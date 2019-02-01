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
import Control.Monad.Trans.State
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
import Data.Text (Text)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import System.IO
import System.Directory
import System.FilePath
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

type MilestoneMap = M.Map Text MilestoneId

openStateFile :: forall a. (Ord a, Read a, Show a)
              => FilePath -> IO (S.Set a, a -> IO ())
openStateFile stateFile = do
    stateFileExists <- doesFileExist stateFile
    !finished <-
        if stateFileExists
        then S.fromList . map read . lines <$> readFile stateFile
        else return mempty

    stateFile <- openFile stateFile AppendMode
    hSetBuffering stateFile LineBuffering
    let finishItem :: a -> IO ()
        finishItem = hPutStrLn stateFile . show
    return (finished, finishItem)

openCommentCacheFile :: Logger -> FilePath -> IO (CommentCacheVar, StoreComment)
openCommentCacheFile logger stateFile = do
    stateFileExists <- doesFileExist stateFile
    !cacheEntries <-
        if stateFileExists
        then map readEntry . lines <$> readFile stateFile
        else return []

    let storeItem :: Int -> CommentRef -> IO ()
        storeItem t mn = do
          writeLog logger "TRACE" $ "Store comment ref: " ++ show t ++ " / " ++ show mn
          stateFile <- openFile stateFile AppendMode
          hSetBuffering stateFile LineBuffering
          hPutStrLn stateFile $! showEntry (t, mn)
          hClose stateFile

    cache <- newMVar $! foldr'
              (\(t,n) -> M.insertWith (flip (++)) t [n])
              mempty
              cacheEntries
    return (cache, storeItem)

    where
      readEntry :: String -> (Int, CommentRef)
      readEntry str =
        case words str of
          ('(':_):_ ->
            -- support legacy state file
            let (rt, rn) = read str in (rt, NoteRef rn)
          [t] ->
            (read t, MissingCommentRef)
          [t,"git",h] ->
            (read t, CommitRef h Nothing)
          [t, "git", h, repo] ->
            (read t, CommitRef h (Just repo))
          [t,n] ->
            (read t, NoteRef (read n))

      showEntry :: (Int, CommentRef) -> String
      showEntry (t, MissingCommentRef) = show t
      showEntry (t, NoteRef n) = unwords . map show $ [t, n]
      showEntry (t, CommitRef h Nothing) = unwords [show t, "git", h]
      showEntry (t, CommitRef h (Just repo)) = unwords [show t, "git", h, repo]

attachmentStateFile :: FilePath
attachmentStateFile = "attachments.state"

ticketStateFile :: FilePath
ticketStateFile = "tickets.state"

mutationStateFile :: FilePath
mutationStateFile = "mutations.state"

commentCacheFile :: FilePath
commentCacheFile = "comments.state"

type CommentCacheVar = MVar CommentCache

type CommentCache = (M.Map Int [CommentRef])

type StoreComment = Int -> CommentRef -> IO ()

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
                    Trac.Convert.convert
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

dummyLookupAnchor :: String -> Maybe String
dummyLookupAnchor = Just

divide :: Int -> [a] -> [[a]]
divide n xs = map f [0..n-1]
  where
    f i = mapMaybe (\(j,x) -> if j `mod` n == i then Just x else Nothing)
          $ zip [0..] xs

type Username = Text

type Email = Text

knownUsersList :: [(Username, Username)]
knownUsersList =
    [ "Krzysztof Gogolewski <krz.gogolewski@gmail.com>" .= "monoidal"
    , "Ben Gamari <ben@smart-cactus.org>" .= "bgamari"
    , "Austin Seipp <aust@well-typed.com>" .= "thoughtpolice"
    , "ian@well-typed.com" .= "igloo"
    , "Jan Stolarek <jan.stolarek@p.lodz.pl>" .= "jstolarek"
    , "marlowsd@gmail.com" .= "simonmar"
    , "Richard Eisenberg <eir@cis.upenn.edu>" .= "goldfire"
    , "p.capriotti@gmail.com" .= "pcapriotti"
    , "Thomas Miedema <thomasmiedema@gmail.com>" .= "thomie"
    , "pho@cielonegro.org" .= "pho_at_cielonegro.org"
    , "Favonia" .= "favonia"
    , "andygill" .= "AndyGill"
    ]
  where (.=) = (,)

knownUsers :: M.Map Email Username
knownUsers = M.fromList knownUsersList

knownEmails :: M.Map Username Email
knownEmails = M.fromList $ map flipPair knownUsersList

findKnownUser :: Username -> Maybe User
findKnownUser username = do
  email <- M.lookup username knownEmails
  return $ User (UserId 0) username username (Just email)

flipPair :: (a, b) -> (b, a)
flipPair (a, b) = (b, a)

sanitizeUsername :: Username -> Username
sanitizeUsername n
  | Just _ <- T.find (== '<') n =
    sanitizeUsername $ T.takeWhile (/= '>') $ T.tail $ T.dropWhile (/= '<') n
  | otherwise =
    T.map fixChars $ T.takeWhile (/= '@') n
  where
    fixChars '_' = '_'
    fixChars '-' = '-'
    fixChars '.' = '.'
    fixChars c
      | isLetter c = c
      | isDigit c  = c
    fixChars c = '_'

data UserIdOracle = UserIdOracle { findOrCreateUser :: Username -> ClientM UserId
                                 , findUser :: Username -> ClientM (Maybe UserId)
                                 }

withMVarState :: forall s m a. (MonadMask m, MonadIO m)
              => MVar s -> StateT s m a -> m a
withMVarState var action =
    fst . fst <$> generalBracket (liftIO $ takeMVar var) release (runStateT action)
  where
    release :: s -> ExitCase (a, s) -> m ()
    release s0 exit = do
        liftIO $ putMVar var $ case exit of
          ExitCaseSuccess (_, s') -> s'
          _ -> s0

type UserLookupM = MaybeT (StateT UserIdCache ClientM)

type UserIdCache = M.Map Username UserId

mkUserIdOracle :: Logger -> Connection -> ClientEnv -> IO UserIdOracle
mkUserIdOracle logger conn clientEnv = do
    cacheVar <- newMVar mempty
    let runIt :: Bool -> Username -> StateT UserIdCache IO (Maybe UserId)
        runIt create username = StateT $ \cache -> do
            res <- runClientM (runStateT (runMaybeT $ getUserId create $ T.strip username) cache) clientEnv
            writeLog logger "RESOLVE USER" $ show username ++ " -> " ++ show (fmap fst res)
            either throwM pure res
    let oracle :: Bool -> Username -> ClientM (Maybe UserId)
        oracle create = liftIO
                        . withMVarState cacheVar
                        . runIt create
    return $ UserIdOracle { findOrCreateUser = \user -> fromMaybe (error "couldn't resolve user id") <$> oracle True user
                          , findUser = oracle False
                          }
  where
    tee :: (Show a, Monad m, MonadIO m) => String -> m a -> m a
    tee msg a = do
      x <- a
      liftIO . writeLog logger "INFO" $ msg ++ show x
      return x

    getUserId :: Bool -> Username -> UserLookupM UserId
    getUserId create username =
            tee "tryEmptyUserName" tryEmptyUserName
        <|> tee "tryCache" tryCache
        <|> cacheIt (tee "tryLookupName - " tryLookupName)
        <|> cacheIt (tee "tryLookupEmail - " tryLookupEmail)
        <|> cacheIt (tee "tryCreate - " tryCreate)
      where
        username'
          | Just u <- M.lookup username knownUsers = u
          | otherwise = "trac-"<>sanitizeUsername username

        tryEmptyUserName :: UserLookupM UserId
        tryEmptyUserName
          | username == "" = getUserId create "unknown"
          | otherwise      = empty

        tryCache :: UserLookupM UserId
        tryCache = do
            cache <- lift get
            MaybeT $ pure $ M.lookup username cache

        tryLookupName :: UserLookupM UserId
        tryLookupName = do
            liftIO . writeLog logger "FIND USER BY NAME" $ T.unpack username'
            fmap userId $ MaybeT $ lift $ findUserByUsername gitlabToken username'

        tryLookupEmail :: UserLookupM UserId
        tryLookupEmail = do
            m_email <- liftIO $ getUserAttribute conn Trac.Email username
            let cuEmail = fromMaybe ("trac+"<>username'<>"@haskell.org") m_email
            liftIO . writeLog logger "FIND USER BY EMAIL" $ T.unpack cuEmail
            fmap userId $ MaybeT $ lift $ tee "user by email" $ findUserByEmail gitlabToken cuEmail

        tryCreate :: UserLookupM UserId
        tryCreate
          | not create = empty
          | otherwise = do
            m_email <- liftIO $ getUserAttribute conn Trac.Email username
            uidMay <- lift $ do
              let cuEmail = case m_email of
                              Nothing -> "trac+"<>cuUsername<>"@haskell.org"
                              Just email -> email
                  cuName = username
                  cuUsername = case M.lookup username knownUsers of
                                 Just u -> u
                                 Nothing -> username'
                  cuSkipConfirmation = True
              liftIO $ writeLog logger "CREATE USER" $ show username <> " (" <> show cuEmail <> ")"
              lift $ createUserMaybe gitlabToken CreateUser {..}
            case uidMay of
              Nothing ->
                fail "User already exists"
              Just uid -> do
                liftIO $ writeLog logger "USER CREATED" $ show username <> " as " <> show uid
                lift . lift $ addProjectMember gitlabToken project uid Reporter
                return uid

        cacheIt :: UserLookupM UserId -> UserLookupM UserId
        cacheIt action = do
            uid <- action
            lift $ modify' $ M.insert username uid
            return uid

makeMilestones :: Logger -> Bool -> Connection -> ClientM MilestoneMap
makeMilestones logger actuallyMakeThem conn = do
    when actuallyMakeThem $ do
      milestones <- liftIO $ Trac.getMilestones conn
      mconcat <$> mapM createMilestone' milestones
      pure ()
    foldMap (\(GitLab.Tickets.Milestone a b) -> M.singleton a b)
        <$> listMilestones gitlabToken project
  where
    createMilestone' :: Trac.Milestone -> ClientM MilestoneMap
    createMilestone' Trac.Milestone{..} =
      handleAll onError $ flip catchError onError $ do
        mid <- createMilestone logger gitlabToken Nothing project
            $ CreateMilestone { cmTitle = mName
                              , cmDescription = mDescription
                              , cmDueDate = mDue
                              , cmStartDate = Nothing
                              }
        return $ M.singleton mName mid

    onError :: (MonadIO m, Exception a) => a -> m MilestoneMap
    onError err = do
        liftIO $ writeLog logger "FAILED TO CREATE MILESTONE" $ displayException err
        return M.empty

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
            ticket <- liftIO $ fromMaybe (error "Ticket not found") <$> Trac.getTicket (ticketMutationTicket m) conn
            iid@(IssueIid issueID) <- createTicket logger' milestoneMap userIdOracle commentCache ticket
            if ((fromIntegral . getTicketNumber . ticketNumber $ ticket) == issueID)
              then
                return ()
              else
                (writeLog logger "TICKET NUMBER MISMATCH" $ show (ticketNumber ticket) ++ " /= " ++ show iid)

          -- Apply a ticket change
          Trac.ChangeTicket -> do
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

tracToMarkdown :: Logger -> CommentCacheVar -> TicketNumber -> Text -> IO Text
tracToMarkdown logger commentCache (TicketNumber n) src =
      T.pack <$> Trac.Convert.convertIgnoreErrors
        logger
        (showBaseUrl gitlabBaseUrl)
        gitlabOrganisation
        gitlabProjectName
        (Just $ fromIntegral n)
        Nothing
        getCommentId
        dummyLookupAnchor
        (T.unpack src)
      where
        getCommentId :: Int -> Int -> IO CommentRef
        getCommentId t c = fromMaybe MissingCommentRef . (>>= nthMay (c - 1)) . M.lookup t <$> readMVar commentCache

nthMay :: Int -> [a] -> Maybe a
nthMay n = listToMaybe . drop n

-- | Eliminate metadata changes that are mapped to native GitLab metadata.
-- Ensures that we don't produce redundant comments containing only JSON metadata.
filterFieldChanges :: Fields Update -> Fields Update
filterFieldChanges fields =
    fields { ticketType = noUpdate
           , ticketOwner = noUpdate
           , ticketSummary = noUpdate
           , ticketStatus = noUpdate
           }
  where
    noUpdate = Update Nothing Nothing

createTicket :: Logger
             -> MilestoneMap
             -> UserIdOracle
             -> CommentCacheVar
             -> Ticket
             -> ClientM IssueIid
createTicket logger' milestoneMap userIdOracle commentCache t = do
    let logger = liftLogger logger'
    writeLog logger "TICKET-NR" . show $ ticketNumber t
    creatorUid <- findOrCreateUser userIdOracle $ ticketCreator t
    descriptionBody <- liftIO $
          tracToMarkdown logger' commentCache (ticketNumber t) $
          runIdentity $
          ticketDescription (ticketFields t)
    let extraRows = [] -- [ ("Reporter", ticketCreator t) ]
        description = T.unlines
            [ descriptionBody
            , ""
            , fieldsTable extraRows fields
            , ""
            , fieldsJSON fields
            ]
        fields = ticketFields t
    writeLog logger "FIELDS" . show $ fields
    ownerUid <- case runIdentity . ticketOwner $ fields of
                  Unowned -> return []
                  OwnedBy user -> (:[]) <$> findOrCreateUser userIdOracle (getTracUser user)
    let iid = ticketNumberToIssueIid $ ticketNumber t
        issue = CreateIssue { ciIid = Just iid
                            , ciTitle = runIdentity $ ticketSummary fields
                            , ciLabels = Just $ additions $ fieldLabels $ hoistFields (Update Nothing . Just . runIdentity) fields
                            , ciCreatedAt = Just $ ticketCreationTime t
                            , ciDescription = Just description
                            , ciMilestoneId = Just $ M.lookup (runIdentity $ ticketMilestone fields) milestoneMap
                            , ciWeight = Just $ prioToWeight $ runIdentity $ ticketPriority fields
                            , ciAssignees = Just ownerUid
                            }
    writeLog logger "ISSUE" . show $ issue
    let ignore404 (FailureResponse resp)
          | 404 <- statusCode $ responseStatusCode resp
          = return ()
        ignore404 e
          = throwError e
    deleteIssue gitlabToken Nothing project iid `catchError` ignore404
    ir <- createIssue gitlabToken (Just creatorUid) project issue
    liftIO . writeLog logger' "ISSUE-CREATED" . show $ ir
    return $ irIid ir

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


ticketNumberToIssueIid (TicketNumber n) =
  IssueIid $ fromIntegral n

unlessNull :: (Eq a, Monoid a) => a -> Maybe a
unlessNull xs
  | xs == mempty = Nothing
  | otherwise = Just xs

justWhen :: Bool -> a -> Maybe a
justWhen True x = Just x
justWhen _ _ = Nothing

isCommitComment :: TicketChange -> Bool
isCommitComment tc =
  isJust (changeComment tc) &&
  isJust (T.find (== '@') $ changeAuthor tc)

withFieldDiff :: Ord a => Update (S.Set a) -> (S.Set a -> S.Set a -> ClientM b) -> ClientM (Maybe b)
withFieldDiff (Update old new) handler =
  if isJust new
    then do
      let new' = fromMaybe mempty new
          old' = fromMaybe mempty old
          toAdd = S.difference new' old'
          toRemove = S.difference old' new'
      Just <$> handler toAdd toRemove
    else
      return Nothing

createTicketChanges :: Logger
                    -> MilestoneMap
                    -> UserIdOracle
                    -> CommentCacheVar
                    -> StoreComment
                    -> IssueIid
                    -> TicketChange
                    -> ClientM ()
createTicketChanges logger' milestoneMap userIdOracle commentCache storeComment iid tc = do
    writeLog logger "TICKET-CHANGE" . show $ tc

    handleIssueRelations
    mhash <- handleCommitComments

    -- Here we drop changes to CC field that we accounted for by way of
    -- subscription or unsubscription.
    fields' <- fromMaybe (changeFields tc)
               <$> withFieldDiff (ticketCC $ changeFields tc) handleSubscriptions

    meid <- handleGitLabMetadata fields'

    let trivialUpdate = isTrivialFieldUpdate fields'
        trivialComment = isNothing (changeComment tc)
        trivial = trivialComment && trivialUpdate
    mcinId <- if trivial
        then return Nothing
        else createNote fields'


    let mid = fromMaybe MissingCommentRef $ mcinId <|> meid <|> mhash
    updateCommentCache mid
  where
    logger = liftLogger logger'
    t = case iid of IssueIid n -> TicketNumber $ fromIntegral n


    handleGitLabMetadata :: Fields Update -> ClientM (Maybe CommentRef)
    handleGitLabMetadata fields' = do
        authorUid <- findOrCreateUser userIdOracle $ changeAuthor tc

        -- Field updates. Figure out which fields to update.
        let fields = hoistFields newValue fields'
        let status = case ticketStatus fields of
                      Nothing         -> Nothing
                      Just New        -> Just ReopenEvent
                      Just Assigned   -> Just ReopenEvent
                      Just Patch      -> Just ReopenEvent
                      Just Merge      -> Just ReopenEvent
                      Just Closed     -> Just CloseEvent
                      Just InfoNeeded -> Just ReopenEvent
                      Just Upstream   -> Just ReopenEvent

            notNull :: Maybe Text -> Maybe Text
            notNull (Just s) | T.null s = Nothing
            notNull s = s

        description <-
              liftIO $
              maybe (return Nothing)
                    (fmap Just . tracToMarkdown logger' commentCache t)
                    (ticketDescription fields)

        ownerUid <- case ticketOwner fields of
          Just Unowned  -> return $ Just []
          Just (OwnedBy user) -> Just . (:[]) <$> findOrCreateUser userIdOracle (getTracUser user)
          Nothing -> return Nothing

        currentLabels <- irLabels <$> getIssue gitlabToken project iid
        let labelAddRemoves = fieldLabels fields'
            labels' = (currentLabels `diffLabels` removals labelAddRemoves) <> additions labelAddRemoves

        let edit = EditIssue { eiTitle = notNull $ ticketSummary fields
                             , eiDescription = description
                             , eiMilestoneId = fmap (`M.lookup` milestoneMap) (ticketMilestone fields)
                             , eiLabels = Just labels'
                             , eiStatus = status
                             , eiUpdateTime = Just $ changeTime tc
                             , eiWeight = prioToWeight <$> ticketPriority fields
                             , eiAssignees = ownerUid
                             , eiKeywords = toList <$> ticketKeywords fields
                             }
        writeLog logger "ISSUE-EDIT" . show $ edit
        meid <- if nullEditIssue edit
                  then
                    return Nothing
                  else do
                    writeLog logger "TRACE" $ "Issue edit: " ++ show edit
                    void $ editIssue gitlabToken (Just authorUid) project iid edit
                    meid <- fmap inrId <$> getNewestIssueNote gitlabToken (Just authorUid) project iid
                    writeLog logger "CREATE-ISSUE-EDIT" $ show meid
                    return $ NoteRef <$> meid
        return meid

    -- Handle commit comments. A commit comment is a comment that has been
    -- added automatically when someone mentioned the ticket number in a
    -- commit. Commit comments in Trac replicate the entire commit message;
    -- this is useless to us, because we already have the commit in gitlab, so
    -- we will not create an actual note, but rather just remember which commit
    -- this points to, and turn Trac links to this comment into gitlab links
    -- directly to the commit.
    handleCommitComments :: ClientM (Maybe CommentRef)
    handleCommitComments
       | not $ isCommitComment tc = return Nothing
       | otherwise = do
          writeLog logger "COMMIT-COMMENT" $ (fromMaybe "(no comment)" $ show <$> changeComment tc)
          let mcommitInfo = extractCommitHash =<< changeComment tc
          writeLog logger "COMMIT-COMMENT" $ fromMaybe "???" (fst <$> mcommitInfo)
          return $ do
            (hash, mrepo) <- mcommitInfo
            return $ CommitRef hash mrepo


    -- Translate issue link lists to link/unlink events.
    handleIssueRelations :: ClientM ()
    handleIssueRelations = do
        authorUid <- findOrCreateUser userIdOracle $ changeAuthor tc
        void $ withFieldDiff (ticketRelated $ changeFields tc) $ \toLink toUnlink -> do
          forM_ toLink $ \(TicketNumber n) -> do
            let otherIid = IssueIid . fromIntegral $ n
            writeLog logger "LINK-ISSUES" $ show iid ++ " <-> " ++ show otherIid
            linkResult <- createIssueLink
              logger'
              gitlabToken
              (Just authorUid)
              project
              iid
              (CreateIssueLink project iid project otherIid)
            writeLog logger "LINKED" $ show linkResult

          writeLog logger "LINK" (show toLink)
          writeLog logger "UNLINK" (show toUnlink)

    -- Translate CC field changes to subscribe/unsubscribe events.
    handleSubscriptions :: S.Set Username -> S.Set Username -> ClientM (Fields Update)
    handleSubscriptions toSubscribe toUnsubscribe = do
      writeLog logger "SUBSCRIBE" $ (show toSubscribe)
      writeLog logger "UNSUBSCRIBE" $ (show toUnsubscribe)

      let lookupUsers :: S.Set Username -> ClientM (M.Map Username UserId)
          lookupUsers users =
              M.mapMaybe id <$> mapM (findUser userIdOracle) (M.fromSet id users)
      toSubscribe' <- lookupUsers toSubscribe
      toUnsubscribe' <- lookupUsers toUnsubscribe

      forM_ toSubscribe' $ \uid -> do
        writeLog logger "SUBSCRIBE-USER" $ show uid
        result <- subscribeIssue
                    logger'
                    gitlabToken
                    (Just uid)
                    project
                    iid
        writeLog logger "SUBSCRIBED" $ show result

      forM_ toUnsubscribe' $ \uid -> do
        writeLog logger "UNSUBSCRIBE-USER" $ show uid
        result <- unsubscribeIssue
                    logger'
                    gitlabToken
                    (Just uid)
                    project
                    iid
        writeLog logger "UNSUBSCRIBED" $ show result

      let oldCC = ticketCC (changeFields tc)
          -- Remove the changes we've made via subscription to avoid producing
          -- unnecessary metadata tables
          newCC = oldCC { newValue = fmap (\s -> (s `S.union` M.keysSet toSubscribe') `S.difference` M.keysSet toUnsubscribe') (newValue oldCC) }
      return $ (changeFields tc) { ticketCC = newCC }

    -- Figure out the comment number. We have to do this by counting how many
    -- comments there are on this ticket so far, because Trac doesn't actually
    -- store comment numbers.
    findCommentNumber :: ClientM Int
    findCommentNumber = liftIO $ do
      items <- M.lookup (unIssueIid iid) <$> readMVar commentCache
      case items of
        Nothing -> return 1
        Just xs -> return (length xs + 1)

    createNote :: Fields Update  -- fields with subscriptions removed
               -> ClientM (Maybe CommentRef)
    createNote fields' = do
        authorUid <- findOrCreateUser userIdOracle $ changeAuthor tc
        commentNumber <- findCommentNumber

        -- Compose a note body. In some cases, we don't want to create a note,
        -- because the information we could put in there isn't useful, or because
        -- we already store it elsewhere. In those cases, we leave the body empty,
        -- and check for that later.
        mrawBody <- liftIO $
                      maybe
                          (return Nothing)
                          (fmap Just . tracToMarkdown logger' commentCache t)
                          (changeComment tc)
        let body = T.unlines . catMaybes $
                [ mrawBody >>= justWhen (not . isCommitComment $ tc)
                , justWhen (not $ isTrivialFieldUpdate fields')
                    $ fieldsTable
                        mempty
                        -- [ ("User", changeAuthor tc) -- ]
                        fields'
                , justWhen (not $ isTrivialFieldUpdate $ filterFieldChanges $ changeFields tc)
                    $ fieldsJSON $ filterFieldChanges $ changeFields tc
                ]

        writeLog logger "NOTE" $ show body
        let discard = T.all isSpace body
        mcinId <- if discard
                      then do
                        writeLog logger "COMMENT-SKIPPED" ""
                        return Nothing
                      else do
                        let note = CreateIssueNote { cinBody = body
                                                   , cinCreatedAt = Just $ changeTime tc
                                                   }
                        cinResp <- createIssueNote gitlabToken (Just authorUid) project iid note
                        writeLog logger "NOTE-CREATED" $ show commentNumber ++ " -> " ++ show cinResp
                        return (Just . NoteRef . inrId $ cinResp)
        return mcinId

    -- Update our comment cache. Depending on what we did above, we pick an
    -- apppropriate note reference (pointing to a Note, a Commit, or marking
    -- the comment as missing), and append it to both the in-memory store and
    -- the state file.
    updateCommentCache :: CommentRef -> ClientM ()
    updateCommentCache mid =
        (liftIO $ do
          modifyMVar_ commentCache $
            return .
            M.insertWith (flip (++)) (unIssueIid iid) [mid]
          storeComment (unIssueIid iid) mid
          (M.lookup (unIssueIid iid) <$> readMVar commentCache))
          >>= writeLog logger "STORE-COMMENT" . show


-- | Maps Trac keywords to labels
keywordLabels :: Text -> Labels
keywordLabels =
    fromMaybe mempty . flip M.lookup labelMapping
  where
    labelMapping :: M.Map Text Labels
    labelMapping =
      M.fromList
      $ [ passthru "newcomer"
        , ("newcomers", "newcomer")
        , passthru "TypeInType"
        , passthru "TypeFamilies"
        , passthru "PatternSynonyms"
        , passthru "Deriving"
        , passthru "Generics"
        , passthru "pattern match warnings"
        , passthru "Inlining"
        , passthru "QuantifiedConstraints"
        , passthru "TypeApplications"
        , passthru "levity polymorphism"
        , passthru "CodeGen"
        , passthru "GADTs"
        , "JoinPoints" .= "join points"
        , passthru "Typeable"
        , ("Typeable", "Typeable")
        , ("ORF", "OverloadedRecordFields")
        , ("hs-boot", "hs-boot")
        , passthru "SpecConstr"
        , passthru "ApplicativeDo"
        , passthru "FunDeps"
        , "TypedHoles" .= "typed holes"
        , passthru "CSE"
        , ("TypeCheckerPlugins", "plugins")
        , ("deriving-perf", "Deriving" <> "compiler perf")
        , passthru "CUSKs"
        , passthru "PolyKinds"
        , ("performance", "runtime perf")
        , ("ci-breakage", "CI breakage")

        , ("DWARF", "debug information")
        , passthru "SafeHaskell"
        , passthru "CustomTypeErrors"
        , passthru "StaticPointers"
        , passthru "Unicode"
        , ("warnings", "error messages")
        , passthru "Arrows"
        , passthru "SIMD"
        , passthru "TemplateHaskell"
        ]
      where
        passthru x = (x, mkLabel x)
        (.=) = (,)

typeOfFailureLabels :: TypeOfFailure -> Labels
typeOfFailureLabels t =
    case t of
      BuildingGhcFailed         -> "GhcBuildFailure"
      CompileTimeCrash          -> "CompilerCrash"
      CompileTimePerformance    -> "CompilerPerformance"
      IncorrectDebugInformation -> "DebugInformation"
      DocumentationBug          -> "Documentation"
      InvalidProgramAccepted    -> mempty
      GhcDoesn'tWork            -> mempty
      GhciCrash                 -> "GHCi"
      ValidProgramRejected      -> mempty
      IncorrectAPIAnnotation    -> "APIAnnotations"
      PoorErrorMessage          -> "ErrorMessages"
      IncorrectWarning          -> "ErrorMessages"
      IncorrectResultAtRuntime  -> "IncorrectResultAtRuntime"
      InstallationFailure       -> mempty
      RuntimeCrash              -> "RuntimeCrash"
      RuntimePerformance        -> "RuntimePerformance"
      OtherFailure              -> mempty

ticketTypeLabel :: TicketType -> Labels
ticketTypeLabel Bug = "bug"
ticketTypeLabel Task = "task"
ticketTypeLabel FeatureRequest = "feature request"
ticketTypeLabel MergeReq = "backport request"

fieldLabels :: Fields Update -> AddRemove Labels
fieldLabels fields =
    add "Trac import" <> keywordLbls <> failureLbls <> typeLbls <> statusLbls
  where
    keywordLbls = toAddRemove id $ fmap (foldMap keywordLabels) (ticketKeywords fields)
    typeLbls = toAddRemove ticketTypeLabel (ticketType fields)
    failureLbls = toAddRemove typeOfFailureLabels (ticketTypeOfFailure fields)
    statusLbls = toAddRemove toStatusLabel (ticketStatus fields)

    toStatusLabel Merge = "backport"
    toStatusLabel Upstream = "upstream"
    toStatusLabel _ = mempty


data AddRemove a = AddRemove { additions, removals :: a }

instance Semigroup a => Semigroup (AddRemove a) where
    AddRemove a b <> AddRemove c d = AddRemove (a<>c) (b<>d)

instance Monoid a => Monoid (AddRemove a) where
    mempty = AddRemove mempty mempty

add :: Monoid a => a -> AddRemove a
add x = mempty { additions = x }

remove :: Monoid a => a -> AddRemove a
remove x = mempty { removals = x }

toAddRemove :: Monoid b => (a -> b) -> Update a -> AddRemove b
toAddRemove f (Update old new) =
    AddRemove { removals = maybe mempty f old, additions = maybe mempty f new }


fieldsJSON :: forall f. (FieldToJSON f, Functor f, ConcatFields f, Show (Fields f))
           => Fields f -> T.Text
fieldsJSON fields =
    TL.toStrict $ "<!-- " <> Aeson.encodeToLazyText fields <> " -->"

fieldsTable :: forall f. (Functor f, ConcatFields f, Show (Fields f))
            => [(Text, Text)] -> Fields f -> T.Text
fieldsTable extraRows (f@Fields{..})
  | null rows = ""
  | otherwise = T.unlines $
    [ "<details><summary>Trac metadata</summary>"
    , ""
    , table
    , "</details>"
    ]
  where
    unless :: (a -> Bool) -> Maybe a -> Maybe a
    unless p (Just x) | p x = Nothing
    unless _ x = x

    row :: Text -> Maybe Text -> Maybe (Text, Text)
    row name val = fmap (\v -> (name,v)) val

    rows :: [(Text, Text)]
    rows =
        catMaybes
        [ row "Version" $ concatFields ticketVersion
        , row "Type" $ concatFields $ T.pack . show <$> ticketType
        , row "TypeOfFailure" $ concatFields $ T.pack . show <$> ticketTypeOfFailure
        , row "Priority" $ concatFields $ toPriorityName <$> ticketPriority
        -- , row "Milestone" $ concatFields ticketMilestone
        , row "Component" $ concatFields ticketComponent
        , row "Test case" $ concatFields $ ticketTestCase
        , row "Differential revisions" $ concatFields $ renderTicketDifferentials . toList <$> ticketDifferentials
        -- , row "Status" $ concatFields $ T.pack . show <$> ticketStatus
        -- , row "Description" $ concatFields $ const "description changed" <$> concatFields ticketDescription
        -- , row "Keywords" $ concatFields $ T.intercalate ", " <$> ticketKeywords
        , row "BlockedBy" $ concatFields $ renderTicketNumbers . toList <$> ticketBlockedBy
        , row "Related" $ concatFields $ renderTicketNumbers . toList <$> ticketRelated
        , row "Blocking" $ concatFields $ renderTicketNumbers . toList <$> ticketBlocking
        , row "CC" $ concatFields $ T.intercalate ", " . toList <$> ticketCC
        , row "Operating system" $ concatFields ticketOperatingSystem
        , row "Architecture" $ concatFields ticketArchitecture
        -- , row "ALL" $ Just . T.pack . show $ f
        ] ++ extraRows

    formatTicketNumber :: TicketNumber -> Text
    formatTicketNumber (TicketNumber n) = "#" <> T.pack (show n)

    header :: (Text, Text)
    header = ("Trac field", "Value")

    widths = ( maximum $ map (T.length . fst) (header:rows)
             , maximum $ map (T.length . snd) (header:rows)
             )

    table = T.unlines $
        [ renderRow header
        , renderRow (T.replicate (fst widths) "-", T.replicate (snd widths) "-")
        ] ++ map renderRow rows

    renderRow :: (Text, Text) -> Text
    renderRow (a,b) = mconcat
        [ "| "
        , T.justifyLeft (fst widths) ' ' a
        , " | "
        , T.justifyLeft (snd widths) ' ' b
        , " |"
        ]

renderTicketNumbers :: [TicketNumber] -> Text
renderTicketNumbers xs = T.intercalate ", " . map toLink $ xs
  where
    toLink (TicketNumber n) =
        T.pack $ "#" ++ show n

renderTicketDifferentials :: [Differential] -> Text
renderTicketDifferentials diffs =
  T.intercalate ", " $
    map (mkDifferentialLink . getDifferentialNumber) diffs

toPriorityLabel :: Priority -> Labels
toPriorityLabel p = case p of
  PrioLowest  -> "P-lowest"
  PrioLow     -> "P-low"
  PrioNormal  -> "P-normal"
  PrioHigh    -> "P-high"
  PrioHighest -> "P-highest"

toPriorityName :: IsString s => Priority -> s
toPriorityName p = case p of
  PrioLowest  -> "lowest"
  PrioLow     -> "low"
  PrioNormal  -> "normal"
  PrioHigh    -> "high"
  PrioHighest -> "highest"

prioToWeight :: Priority -> Weight
prioToWeight PrioLowest  = Weight 0
prioToWeight PrioLow     = Weight 3
prioToWeight PrioNormal  = Weight 5
prioToWeight PrioHigh    = Weight 7
prioToWeight PrioHighest = Weight 10

extractCommitHash :: Text -> Maybe (CommitHash, Maybe RepoName)
extractCommitHash t = do
  eparsed <- either (const Nothing) Just $ Trac.parseTrac Nothing (T.unpack t)
  listToMaybe $ Trac.walk extractFromInline eparsed
  where
    extractFromInline :: Trac.Inline -> Maybe (CommitHash, Maybe RepoName)
    extractFromInline (Trac.GitCommitLink hash mrepo _) =
      Just (hash, mrepo)
    extractFromInline _ =
      Nothing

isHexChar c = isDigit c || (c `elem` ['a'..'f'])

(|$|) :: (Monad m) => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
a |$| b = do
  x <- a
  if isJust x then pure x else b
