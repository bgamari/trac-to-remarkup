{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Char
import Control.Monad
import Control.Monad.Catch hiding (bracket, onError)
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import System.Environment

import Database.PostgreSQL.Simple
import Network.HTTP.Client.TLS as TLS
import Servant.Client

import Logging (LoggerM (..), Logger, makeStdoutLogger, writeLog, liftLogger)
import qualified Logging

import Utils
import GitLab.Common
import Trac.Db as Trac
import Trac.Db.Types as Trac
import qualified Trac.Scraper as Scraper
import qualified Trac.Convert
import Settings
import UserLookup
import TicketImport
import MilestoneImport
import ImportState
import WikiImport
import AttachmentImport
import TicketFixup

gitlabApiBaseUrl :: BaseUrl
gitlabApiBaseUrl =
  gitlabBaseUrl { baseUrlPath = "api/v4" }

main :: IO ()
main = do
    logger <- makeStdoutLogger
    args <- getArgs
    let opts = S.fromList . filter (isPrefixOf "-") $ args
        skipMilestones = "-skip-milestones" `S.member` opts
        skipAttachments = "-skip-attachments" `S.member` opts
        skipWiki = "-skip-wiki" `S.member` opts
        skipTickets = "-skip-tickets" `S.member` opts
        skipWikiHistory = "-skip-wiki-history" `S.member` opts
        skipTicketFixup = "-skip-ticket-fixup" `S.member` opts
        testParserMode = "-test-parser" `S.member` opts
        testScraperMode = "-test-scraper" `S.member` opts
        keepWikiGit = "-keep-wiki-git" `S.member` opts
    let ticketNumbers = S.fromList
                      . map (TicketNumber . read)
                      . filter (all isDigit)
                      . filter (not . isPrefixOf "-")
                      $ args
        scrapeUrls = filter (not . all isDigit)
                   . filter (not . isPrefixOf "-")
                   $ args
    conn <- connectPostgreSQL tracDsn
    mgr <- TLS.newTlsManagerWith $ TLS.mkManagerSettings tlsSettings Nothing
    let env = mkClientEnv mgr gitlabApiBaseUrl
    userIdOracle <- mkUserIdOracle logger conn env

    (finishedMutations, finishMutation) <- openStateFile mutationStateFile

    (commentCache, storeComment) <- openCommentCacheFile logger commentCacheFile

    if testParserMode
      then do
        wpBody <- getContents
        mdBody <- printParseError logger (T.pack wpBody) $
          let dummyLookupAnchor :: String -> Maybe String
              dummyLookupAnchor = Just
          in Trac.Convert.convert
                      logger
                      (showBaseUrl gitlabBaseUrl)
                      gitlabOrganisation
                      gitlabProjectName
                      Nothing
                      (Just "stdin")
                      dummyGetCommentId
                      dummyLookupAnchor
                      wpBody
        putStr mdBody
      else if testScraperMode
        then forM_ scrapeUrls $ \url -> do
          src <- Scraper.httpGet logger url
          (dst, amap) <- Scraper.scrape
            mempty
            logger
            (showBaseUrl gitlabBaseUrl)
            gitlabOrganisation
            gitlabProjectName
            Nothing
            (Just "stdin")
            dummyGetCommentId
            src
          putStrLn dst

      else do
        milestoneMap <- either (error . show) id <$> runClientM (makeMilestones logger (not skipMilestones) conn) env

        unless skipTickets $ Logging.withContext logger "tickets" $ printErrors logger $ do
          writeLog logger "Making tickets" ""
          mutations <- filter (\m -> not $ m `S.member` finishedMutations) .
                       filter (\m -> ticketMutationTicket m `S.member` ticketNumbers || S.null ticketNumbers)
                       <$> Trac.getTicketMutations conn
          let makeMutations' ts = do
                  runClientM
                    (makeMutations
                      logger
                      conn
                      milestoneMap
                      userIdOracle
                      commentCache
                      finishMutation
                      storeComment
                      ts)
                    env >>= throwLeft
                  writeLog logger "makeMutations' done" ""
          makeMutations' mutations

        unless skipAttachments
          $ Logging.withContext logger "attachments"
          $ printErrors logger
          $ runClientM (makeAttachments logger conn userIdOracle) env
            >>= throwLeft

        unless skipWiki
          $ Logging.withContext logger "wiki" $ printErrors logger
          $ void
          $ runClientM (buildWiki logger skipWikiHistory keepWikiGit commentCache conn) env >>= throwLeft

        unless skipTicketFixup
          $ Logging.withContext logger "ticket fixup" $ printErrors logger
          $ fixupLastUpdated logger

    where
      throwLeft :: (Exception e, Monad m, MonadThrow m) => Either e a -> m a
      throwLeft = either throwM return

      printErrors :: Logger -> IO () -> IO ()
      printErrors logger action =
        action `catch` (\(err :: SomeException) -> writeLog logger "EXCEPTION" (displayException err))

dummyGetCommentId :: Int -> Int -> IO CommentRef
dummyGetCommentId _t _c = pure MissingCommentRef

makeMutations :: Logger
              -> Connection
              -> MilestoneMap
              -> UserIdOracle
              -> CommentCacheVar
              -> (TicketMutation -> IO ())
              -> StoreComment
              -> [Trac.TicketMutation]
              -> ClientM ()
makeMutations logger' conn milestoneMap userIdOracle commentCache finishMutation storeComment mutations = do
  mapM_ makeMutation' mutations
  where
    logger :: LoggerM ClientM
    logger = liftLogger logger'

    makeMutation' m =
      handleAll onError $
      flip catchError onError $
      Logging.withContext logger (show . getTicketNumber . ticketMutationTicket $ m) $ do
        result <- withTimeout 10000 $ case ticketMutationType m of
          -- Create a new ticket
          Trac.CreateTicket -> do
            writeLog logger "CREATING" $ show $ ticketMutationTicket m
            ticket <- liftIO $ fromMaybe (error "Ticket not found") <$> Trac.getTicket (ticketMutationTicket m) conn
            iid@(IssueIid issueID) <- createTicket logger' milestoneMap userIdOracle commentCache ticket
            if ((fromIntegral . getTicketNumber . ticketNumber $ ticket) == issueID)
              then
                return ()
              else
                (writeLog logger "TICKET NUMBER MISMATCH" $ show (ticketNumber ticket) ++ " /= " ++ show iid)

          -- Apply a ticket change
          Trac.ChangeTicket -> do
            writeLog logger "MUTATING" $ show $ ticketMutationTicket m
            changes <- liftIO $ Trac.getTicketChanges conn (ticketMutationTicket m) (Just $ ticketMutationTime m)
            writeLog logger "NOTICE" $ show changes
            let iid = IssueIid (fromIntegral . getTicketNumber . ticketMutationTicket $ m)

            createTicketChanges
                logger'
                milestoneMap
                userIdOracle
                commentCache
                storeComment
                iid $ collapseChanges changes
            return ()

        case result of
          Just _  -> liftIO $ finishMutation m
          Nothing -> return ()

      where
        onError :: (Show a) => a -> ClientM ()
        onError err =
            writeLog logger "ERROR" $ "Failed to execute ticket mutation: " ++ show err

collapseChanges :: [TicketChange] -> TicketChange
collapseChanges tcs = TicketChange
    { changeTime = changeTime $ head tcs
    , changeAuthor = changeAuthor $ head tcs
    , changeFields = foldl1 collapseFields (map changeFields tcs)
    , changeComment = listToMaybe $ catMaybes $ map changeComment tcs
    }

