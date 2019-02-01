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
import Data.Time
import Data.Time.Format
import Text.Megaparsec.Error (ParseError, parseErrorPretty)
import Data.Void
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

import GitLab.Tickets
import GitLab.Common
import GitLab.Project
import GitLab.UploadFile
import GitLab.Users
import qualified Trac.Web
import Trac.Db as Trac
import Trac.Db.Types as Trac
import Trac.Convert (LookupComment, tracWikiBaseNameToGitlab)
import Trac.Writer (mkDifferentialLink)
import qualified Trac.Convert
import qualified Trac.Parser as Trac
import qualified Trac.Scraper as Scraper
import Settings
import UserLookup
import TicketImport
import MilestoneImport
import ImportState


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

nthMay :: Int -> [a] -> Maybe a
nthMay n = listToMaybe . drop n

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

withTimeout :: (MonadIO m, MonadBaseControl IO m) => Int -> m a -> m (Maybe a)
withTimeout delayMS action =
  either Just (const Nothing) <$> race action (liftIO reaper)
  where
    reaper :: IO ()
    reaper = do
      threadDelay (delayMS * 1000)

buildWiki :: Logger -> Bool -> Bool -> CommentCacheVar -> Connection -> ClientM ()
buildWiki logger fast keepGit commentCache conn = do
  bracket
    (liftIO $ do
      Git.clone logger wikiRemoteUrl
    )
    (\wc -> liftIO $ do
        if keepGit
          then do
            writeLog logger "GIT" $ printf "Keeping git working copy at %s" wc
          else
            Git.deleteWorkingCopy logger wc >>= writeLog logger "GIT"
    )
    $ \wc -> do
      liftIO $ writeLog logger "WIKI BUILD DIR" wc
      pages <- liftIO $ (if fast then getWikiPagesFast else getWikiPages) conn
      anchorMapVar <- liftIO $ newMVar mempty
      forM_ pages (buildPage anchorMapVar wc)
      liftIO $ do
        (git_ logger wc "pull" [] >>= writeLog logger "GIT")
              `catch`
              (\(err :: GitException) -> do
                case Git.gitExcExitCode err of
                  1 ->
                    -- upstream has no master branch yet - we'll ignore this and
                    -- just push.
                    pure ()

                  _ ->
                    -- something else went wrong, let's report it
                    throwM err
              )
        git_ logger wc "push" ["origin", "master"] >>= writeLog logger "GIT"
  where
    getCommentId :: Int -> Int -> IO CommentRef
    getCommentId t c = fromMaybe MissingCommentRef . (>>= nthMay (c - 1)) . M.lookup t <$> readMVar commentCache

    buildPage :: MVar Scraper.AnchorMap -> Git.WorkingCopy -> WikiPage -> ClientM ()
    buildPage anchorMapVar wc WikiPage{..} =
      Logging.withContext (liftLogger logger) (T.unpack wpName) $ do
        liftIO $ do
          writeLog logger "INFO" $ (show wpTime) ++ " " ++ (T.unpack wpName) ++ " v" ++ (show wpVersion)
          hFlush stdout
        let baseFilename = wc </> (tracWikiBaseNameToGitlab . T.unpack $ wpName)
            filename = baseFilename <.> "md"
            tracFilename = baseFilename <.> "trac"
        liftIO $ do
          let url =
                printf
                  "https://ghc.haskell.org/trac/ghc/wiki/%s?version=%i"
                  wpName
                  wpVersion

          mbody <- dealWithHttpError logger 0 .  withTimeout 100000 $ do
                      src <- Scraper.httpGet logger url
                      anchorMap <- takeMVar anchorMapVar
                      (dst, manchorMap') <-
                        printScraperError logger $
                          Scraper.scrape
                            anchorMap
                            logger
                            (showBaseUrl gitlabBaseUrl)
                            gitlabOrganisation
                            gitlabProjectName
                            Nothing
                            (Just filename)
                            getCommentId
                            src
                      let anchorMap' = fromMaybe anchorMap manchorMap'
                      putMVar anchorMapVar anchorMap'
                      return dst
          writeLog logger "INFO" $ printf "Create file %s in directory %s\n"
            (show filename)
            (show $ takeDirectory filename)
          hFlush stdout
          createDirectoryIfMissing True (takeDirectory filename)
          T.writeFile tracFilename wpBody
          case mbody of
            Just body ->
              writeFile filename body
            Nothing -> do
              writeLog logger "CONVERSION-ERROR" ""
              writeLog logger "INFO" (T.unpack wpBody)
              writeFile filename $
                printf
                  "CONVERSION ERROR\n\nOriginal source:\n\n```trac\n%s\n```\n"
                  wpBody
        muser <- do
          (pure $ findKnownUser wpAuthor)
            |$| (listToMaybe <$> findUsersByUsername gitlabToken wpAuthor)
            |$| (listToMaybe <$> findUsersByEmail gitlabToken wpAuthor)
            |$| do
              -- no user found, let's fake one
              liftIO $ writeLog logger "AUTHOR-MISMATCH" (T.unpack wpAuthor)
              pure . Just $ User (UserId 0) wpAuthor wpAuthor Nothing
        let User{..} = fromMaybe (User (UserId 0) "notfound" "notfound" Nothing) muser
        let juserEmail = fromMaybe ("trac-" ++ T.unpack userUsername ++ "@haskell.org") (T.unpack <$> userEmail)
        let commitAuthor = printf "%s <%s>" userName juserEmail
        let commitDate = formatTime defaultTimeLocale (iso8601DateFormat Nothing) wpTime
        let msg = fromMaybe ("Edit " ++ T.unpack wpName) (T.unpack <$> wpComment >>= unlessNull)
        liftIO $ printGitError logger $ do
          status <- git_ logger wc "status" ["--porcelain"]
          writeLog logger "GIT-STATUS" (show status)
          unless (all isSpace status) $ do
            git_ logger wc "add" ["."] >>= writeLog logger "GIT"
            git_ logger wc "commit" ["-m", msg, "--author=" ++ commitAuthor, "--date=" ++ commitDate] >>= writeLog logger "GIT"

printGitError :: Logger -> IO () -> IO ()
printGitError logger action = action `catch` h
  where
    h :: GitException -> IO ()
    h err = do
      writeLog logger "GIT-EXCEPTION" $ displayException err

printScraperError :: forall a. Logger -> IO (String, a) -> IO (String, Maybe a)
printScraperError logger action = action' `catch` h
  where
    action' :: IO (String, Maybe a)
    action' = do
      (x, a) <- action
      return (x, Just a)

    h :: Scraper.ConversionError -> IO (String, Maybe a)
    h err@(Scraper.ConversionError msg) = do
      writeLog logger "SCRAPER-EXCEPTION" $ displayException err
      return
        ( printf "Conversion error:\n\n```\n%s\n```\n\n" msg
        , Nothing
        )

printParseError :: Logger -> Text -> IO String -> IO String
printParseError logger body action = action `catch` h
  where
    h :: ParseError Char Void -> IO String
    h err = do
      writeLog logger "PARSER-ERROR" $ parseErrorPretty err
      return $ printf "Parser error:\n\n```\n%s\n```\n\nOriginal source:\n\n```trac\n%s\n```\n"
        (parseErrorPretty err) body

dealWithHttpError :: Logger -> Int -> IO (Maybe String) -> IO (Maybe String)
dealWithHttpError logger n action = action `catch` h
  where
    h :: HttpException -> IO (Maybe String)
    h e@(HttpExceptionRequest
          _
          (StatusCodeException
            HTTP.Response { responseStatus = Status { statusCode = 404 } }
            _
          )
        ) = do
      writeLog logger "HTTP-ERROR" $ displayException e
      return Nothing
    h e@(HttpExceptionRequest _ ConnectionFailure {}) =
      retry
    h e@(HttpExceptionRequest _ ResponseTimeout {}) =
      retry

    h e = do
      writeLog logger "HTTP-ERROR" $ displayException e
      return Nothing

    retry =
      if n >= 7 then do
        writeLog logger "HTTP-ERROR" $ "Max number of retries exceeded, skipping."
        return Nothing
      else do
        let delaySecs = 2 ^ n
        writeLog logger "HTTP-ERROR" $ "Network error, retrying in " ++ show delaySecs ++ " seconds"
        threadDelay (delaySecs * 1000000)
        dealWithHttpError logger (succ n) action


unlessNull :: (Eq a, Monoid a) => a -> Maybe a
unlessNull xs
  | xs == mempty = Nothing
  | otherwise = Just xs

(|$|) :: (Monad m) => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
a |$| b = do
  x <- a
  if isJust x then pure x else b
