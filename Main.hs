{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char
import Control.Applicative
import Control.Monad
import Control.Monad.Catch hiding (bracket)
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Control
import Control.Concurrent
import Control.Exception.Lifted (bracket)
import Control.Concurrent.Async.Lifted (mapConcurrently_, race_, race)
import Data.Default (def)
import Data.Foldable
import Data.Function
import Data.Functor.Identity
import Data.List
import Data.String
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import System.IO
import System.FilePath
import System.Directory
import System.Environment
import Debug.Trace
import Text.Printf (printf)
import Text.Megaparsec.Error (ParseError, parseErrorPretty)
import Network.HTTP.Client.Internal as HTTP (HttpException (..), HttpExceptionContent (..), Response (..))

import Database.PostgreSQL.Simple
import Network.HTTP.Client.TLS as TLS
import Network.Connection (TLSSettings(..))
import Network.HTTP.Types.Status
import Servant.Client
import Data.Aeson.Text as Aeson

import qualified Git
import Git (git, git_, GitException)
import Logging (LoggerM (..), Logger, makeStdoutLogger, writeLog, liftLogger)
import qualified Logging

import Utils
import GitLab.Tickets
import GitLab.Common
import GitLab.Project
import GitLab.UploadFile
import GitLab.Users
import qualified Trac.Web
import Trac.Db as Trac
import Trac.Db.Types as Trac
import Trac.Convert (LookupComment, tracWikiBaseNameToGitlab)
import qualified Trac.Scraper as Scraper
import Trac.Writer (mkDifferentialLink)
import qualified Trac.Convert
import qualified Trac.Parser as Trac
import Settings
import UserLookup
import TicketImport
import MilestoneImport
import ImportState
import WikiImport

gitlabApiBaseUrl =
  gitlabBaseUrl { baseUrlPath = "api/v4" }

tee :: Monad m => (a -> m ()) -> a -> m a
tee f x = f x >> pure x

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
    conn <- connectPostgreSQL dsn
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

    where
      throwLeft :: (Exception e, Monad m, MonadThrow m) => Either e a -> m a
      throwLeft = either throwM return

      printErrors :: Logger -> IO () -> IO ()
      printErrors logger action =
        action `catch` (\(err :: SomeException) -> writeLog logger "EXCEPTION" (displayException err))

dummyGetCommentId :: Int -> Int -> IO CommentRef
dummyGetCommentId t c = pure MissingCommentRef

divide :: Int -> [a] -> [[a]]
divide n xs = map f [0..n-1]
  where
    f i = mapMaybe (\(j,x) -> if j `mod` n == i then Just x else Nothing)
          $ zip [0..] xs

makeAttachment :: Logger -> UserIdOracle -> Attachment -> ClientM ()
makeAttachment logger userIdOracle (Attachment{..})
  | TicketAttachment ticketNum <- aResource = do
        liftIO $ writeLog logger "ATTACHMENT" $ show (aResource, aFilename)
        mgr <- manager <$> ask
        content <- liftIO $ Trac.Web.fetchTicketAttachment tracBaseUrl ticketNum aFilename
        uid <- findOrCreateUser userIdOracle aAuthor
        msg <- if ".hs" `T.isSuffixOf` aFilename && BS.length content < 30000
            then mkSnippet uid ticketNum content
            else mkAttachment uid ticketNum content
        mkComment uid (IssueIid $ fromIntegral $ getTicketNumber ticketNum) msg
  | otherwise = return ()
  where
    mkSnippet, mkAttachment :: UserId -> TicketNumber -> BS.ByteString -> ClientM Text
    mkSnippet uid ticketNum content = do
        let title = T.unwords [ aFilename, "from ticket"
                              , "#" <> T.pack (show $ getTicketNumber ticketNum)
                              ]
            cs = CreateSnippet { csTitle = title
                               , csFileName = aFilename
                               , csDescription = Just $ T.unlines [ aDescription
                                                                  , ""
                                                                  , title
                                                                  ]
                               , csCode = TE.decodeUtf8 content
                               , csVisibility = Public
                               }
        sid <- GitLab.Project.createSnippet gitlabToken (Just uid) project cs
        return $ T.unlines
            [ "Attached file `" <> aFilename <> "` ($" <> T.pack (show $ getSnippetId sid) <> ")."
            , ""
            , aDescription
            ]
    mkAttachment uid ticketNum content = do
        url <- GitLab.UploadFile.uploadFile gitlabToken (Just uid) project aFilename content
        return $ T.unlines
            [ "Attached file `" <> aFilename <> "` ([download](" <> url <> "))."
            , ""
            , aDescription
            ]

    mkComment :: UserId -> IssueIid -> Text -> ClientM ()
    mkComment uid iid msg = do
        let note = CreateIssueNote { cinBody = msg
                                   , cinCreatedAt = Just aTime
                                   }
        void $ createIssueNote gitlabToken (Just uid) project iid note

makeAttachments :: Logger -> Connection -> UserIdOracle -> ClientM ()
makeAttachments logger conn userIdOracle = do
    attachments <- liftIO $ getAttachments conn
    (finishedAttachments, finishAttachment) <-
        liftIO $ openStateFile attachmentStateFile
    let makeAttachment' a
          | aIdent `S.member` finishedAttachments = return ()
          | otherwise = handleAll onError $ flip catchError onError $ do
            makeAttachment logger userIdOracle a
            liftIO $ finishAttachment aIdent
          where
            aIdent = (aResource a, aFilename a, aTime a)
            onError :: (MonadIO m, Show a) => a -> m ()
            onError err =
                liftIO $ writeLog logger "ERROR" $ "Failed to create attachment " ++ show a ++ ": " ++ show err
    mapM_ makeAttachment' $ attachments

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

