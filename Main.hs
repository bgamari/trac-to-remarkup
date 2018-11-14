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
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Concurrent
import Control.Concurrent.Async (mapConcurrently_)
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
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO
import System.Directory
import System.Environment

import Database.PostgreSQL.Simple
import Network.HTTP.Client.TLS as TLS
import Network.Connection (TLSSettings(..))
import Network.HTTP.Types.Status
import Servant.Client

import GitLab.Tickets
import GitLab.Common
import GitLab.Project
import GitLab.UploadFile
import GitLab.Users
import qualified Trac.Web
import Trac.Db as Trac
import Trac.Db.Types as Trac
import Trac.Convert (LookupComment)
import Trac.Writer (mkDifferentialLink)
import qualified Trac.Convert
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

openCommentCacheFile :: FilePath -> IO (CommentCacheVar, Int -> Int -> IO ())
openCommentCacheFile stateFile = do
    stateFileExists <- doesFileExist stateFile
    !cacheEntries <-
        if stateFileExists
        then map read . lines <$> readFile stateFile
        else return []

    let storeItem :: Int -> Int -> IO ()
        storeItem t n = do
          stateFile <- openFile stateFile AppendMode
          hSetBuffering stateFile LineBuffering
          hPutStrLn stateFile $! show (t, n)
          hClose stateFile

    cache <- newMVar $! foldr'
              (\(t,n) -> M.insertWith (flip (++)) t [n])
              mempty
              cacheEntries
    return (cache, storeItem)

attachmentStateFile :: FilePath
attachmentStateFile = "attachments.state"

ticketStateFile :: FilePath
ticketStateFile = "tickets.state"

mutationStateFile :: FilePath
mutationStateFile = "mutations.state"

commentCacheFile :: FilePath
commentCacheFile = "comments.state"

type CommentCacheVar = MVar (M.Map Int [Int])

type CommentCache = (M.Map Int [Int])

main :: IO ()
main = do
    args <- getArgs
    print args
    let ticketNumbers = S.fromList . map (TicketNumber . read) $ args
    print ticketNumbers
    conn <- connectPostgreSQL dsn
    mgr <- TLS.newTlsManagerWith $ TLS.mkManagerSettings tlsSettings Nothing
    let env = mkClientEnv mgr gitlabBaseUrl
    getUserId <- mkUserIdOracle env
    milestoneMap <- either (error . show) id <$> runClientM (makeMilestones conn) env

    (finishedMutations, finishMutation) <- openStateFile mutationStateFile

    (commentCache, storeComment) <- openCommentCacheFile commentCacheFile

    putStrLn "Making tickets"
    mutations <- filter (\m -> not $ m `S.member` finishedMutations) .
                 filter (\m -> ticketMutationTicket m `S.member` ticketNumbers || S.null ticketNumbers)
                 <$> Trac.getTicketMutations conn
    let makeMutations' ts = do
            runClientM
              (makeMutations
                conn
                milestoneMap
                getUserId
                commentCache
                finishMutation
                storeComment
                ts)
              env >>= print
            putStrLn "makeMutations' done"
    makeMutations' mutations

    putStrLn "Making attachments"
    runClientM (makeAttachments conn getUserId) env >>= print

divide :: Int -> [a] -> [[a]]
divide n xs = map f [0..n-1]
  where
    f i = mapMaybe (\(j,x) -> if j `mod` n == i then Just x else Nothing)
          $ zip [0..] xs

type Username = Text

knownUsers :: M.Map Username Username
knownUsers = M.fromList
    [ "Krzysztof Gogolewski <krz.gogolewski@gmail.com>" .= "int-index"
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

type UserIdOracle = Username -> ClientM UserId

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

mkUserIdOracle :: ClientEnv -> IO UserIdOracle
mkUserIdOracle clientEnv = do
    cacheVar <- newMVar mempty
    let runIt :: Username -> StateT UserIdCache IO (Maybe UserId)
        runIt username = StateT $ \cache -> do
            res <- runClientM (runStateT (runMaybeT $ getUserId $ T.strip username) cache) clientEnv
            liftIO $ putStrLn $ "Resolve user " ++ show username ++ " -> " ++ show res
            either throwM pure res
    return $ liftIO
           . fmap (fromMaybe $ error "couldn't resolve user id")
           . withMVarState cacheVar
           . runIt
  where
    tee :: (Show a, Monad m, MonadIO m) => String -> m a -> m a
    tee msg a = do
      x <- a
      liftIO . putStrLn $ msg ++ show x
      return x

    getUserId :: Username -> UserLookupM UserId
    getUserId username =
            tee "tryCache" tryCache
        <|> cacheIt (tee "tryLookupName - " tryLookupName)
        <|> cacheIt (tee "tryLookupEmail - " tryLookupEmail)
        <|> cacheIt (tee "tryCreate - " tryCreate)
      where
        cuUsername
          | Just u <- M.lookup username knownUsers = u
          | otherwise = "trac-"<>sanitizeUsername username

        tryCache :: UserLookupM UserId
        tryCache = do
            cache <- lift get
            MaybeT $ pure $ M.lookup username cache

        tryLookupName :: UserLookupM UserId
        tryLookupName = do
            liftIO . putStrLn $ "Find by username: " ++ T.unpack cuUsername
            fmap userId $ MaybeT $ lift $ findUserByUsername gitlabToken cuUsername

        tryLookupEmail :: UserLookupM UserId
        tryLookupEmail = do
            let cuEmail = "trac+"<>cuUsername<>"@haskell.org"
            liftIO . putStrLn $ "Find by email: " ++ T.unpack cuEmail
            fmap userId $ MaybeT $ lift $ tee "user by email" $ findUserByEmail gitlabToken cuEmail

        tryCreate :: UserLookupM UserId
        tryCreate = do
            uidMay <- lift $ do
              let cuEmail = "trac+"<>cuUsername<>"@haskell.org"
                  cuName = username
                  cuSkipConfirmation = True
              liftIO $ putStrLn $ "Creating user " <> show username <> " (" <> show cuEmail <> ")"
              lift $ createUserMaybe gitlabToken CreateUser {..}
            case uidMay of
              Nothing ->
                fail "User already exists"
              Just uid -> do
                liftIO $ putStrLn $ "User created " <> show username <> " as " <> show uid
                lift . lift $ addProjectMember gitlabToken project uid Reporter
                return uid

        cacheIt :: UserLookupM UserId -> UserLookupM UserId
        cacheIt action = do
            uid <- action
            lift $ modify' $ M.insert username uid
            return uid

makeMilestones :: Connection -> ClientM MilestoneMap
makeMilestones conn = do
    milestones <- liftIO $ Trac.getMilestones conn
    --mconcat <$> mapM createMilestone' milestones
    foldMap (\(GitLab.Tickets.Milestone a b) -> M.singleton a b)
        <$> listMilestones gitlabToken project
  where
    createMilestone' :: Trac.Milestone -> ClientM MilestoneMap
    createMilestone' Trac.Milestone{..} = do
        mid <- createMilestone gitlabToken Nothing project
            $ CreateMilestone { cmTitle = mName
                              , cmDescription = mDescription
                              , cmDueDate = Just mDue
                              , cmStartDate = Nothing
                              }
        return $ M.singleton mName mid

makeAttachment :: UserIdOracle -> Attachment -> ClientM ()
makeAttachment getUserId (Attachment{..})
  | TicketAttachment ticketNum <- aResource = do
        liftIO $ putStrLn $ "Attachment " ++ show (aResource, aFilename)
        mgr <- manager <$> ask
        content <- liftIO $ Trac.Web.fetchTicketAttachment tracBaseUrl ticketNum aFilename
        uid <- getUserId aAuthor
        msg <- if ".hs" `T.isSuffixOf` aFilename
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

makeAttachments :: Connection -> UserIdOracle -> ClientM ()
makeAttachments conn getUserId = do
    attachments <- liftIO $ getAttachments conn
    (finishedAttachments, finishAttachment) <-
        liftIO $ openStateFile attachmentStateFile
    let makeAttachment' a
          | aIdent `S.member` finishedAttachments = return ()
          | otherwise = handleAll onError $ flip catchError onError $ do
            makeAttachment getUserId a
            liftIO $ finishAttachment aIdent
          where
            aIdent = (aResource a, aFilename a, aTime a)
            onError :: (MonadIO m, Show a) => a -> m ()
            onError err =
                liftIO $ putStrLn $ "Failed to create attachment " ++ show a ++ ": " ++ show err
    mapM_ makeAttachment' $ attachments

makeMutations :: Connection
              -> MilestoneMap
              -> UserIdOracle
              -> CommentCacheVar
              -> (TicketMutation -> IO ())
              -> (Int -> Int -> IO ())
              -> [Trac.TicketMutation]
              -> ClientM ()
makeMutations conn milestoneMap getUserId commentCache finishMutation storeComment mutations = do
  mapM_ makeMutation' mutations
  where
    makeMutation' m = handleAll onError $ flip catchError onError $ do
      liftIO $ print m
      case ticketMutationType m of
        -- Create a new ticket
        Trac.CreateTicket -> do
          ticket <- liftIO $ fromMaybe (error "Ticket not found") <$> Trac.getTicket (ticketMutationTicket m) conn
          iid@(IssueIid issueID) <- createTicket milestoneMap getUserId commentCache ticket
          if ((fromIntegral . getTicketNumber . ticketNumber $ ticket) == issueID)
            then
              (liftIO $ finishMutation m)
            else
              (liftIO $ putStrLn $ "TICKET NUMBER MISMATCH: " ++ show (ticketNumber ticket) ++ " /= " ++ show iid)
            
        -- Apply a ticket change
        Trac.ChangeTicket -> do
          changes <- liftIO $ Trac.getTicketChanges conn (ticketMutationTicket m) (Just $ ticketMutationTime m)
          let iid = IssueIid (fromIntegral . getTicketNumber . ticketMutationTicket $ m)
          createTicketChanges
              milestoneMap
              getUserId
              commentCache
              storeComment
              iid $ collapseChanges changes
          liftIO $ finishMutation m

      where
        onError :: (MonadIO m, Show a) => a -> m ()
        onError err =
            liftIO $ putStrLn $ "Failed to execute ticket mutation: " ++ show err

collapseChanges :: [TicketChange] -> TicketChange
collapseChanges tcs = TicketChange
    { changeTime = changeTime $ head tcs
    , changeAuthor = changeAuthor $ head tcs
    , changeFields = foldl1 collapseFields (map changeFields tcs)
    , changeComment = listToMaybe $ catMaybes $ map changeComment tcs
    }

tracToMarkdown :: CommentCacheVar -> TicketNumber -> Text -> IO Text
tracToMarkdown commentCache (TicketNumber n) src =
      T.pack <$> Trac.Convert.convert
        gitlabOrganisation
        gitlabProjectName
        (fromIntegral n)
        getCommentId
        (T.unpack src)
      where
        getCommentId t c = ((>>= nthMay (c - 1)) . M.lookup t <$> readMVar commentCache)

nthMay :: Int -> [a] -> Maybe a
nthMay n = listToMaybe . drop n

createTicket :: MilestoneMap
             -> UserIdOracle
             -> CommentCacheVar
             -> Ticket
             -> ClientM IssueIid
createTicket milestoneMap getUserId commentCache t = do
    liftIO $ print $ ticketNumber t
    creatorUid <- getUserId $ ticketCreator t
    descriptionBody <- liftIO $
          tracToMarkdown commentCache (ticketNumber t) $
          runIdentity $
          ticketDescription (ticketFields t)
    let extraRows = [] -- [ ("Reporter", ticketCreator t) ]
        description = T.unlines
            [ descriptionBody
            , ""
            , fieldsTable extraRows fields
            ]
        fields = ticketFields t
        iid = ticketNumberToIssueIid $ ticketNumber t
        issue = CreateIssue { ciIid = Just iid
                            , ciTitle = runIdentity $ ticketSummary fields
                            , ciLabels = Just $ fieldLabels $ hoistFields (Just . runIdentity) fields
                            , ciCreatedAt = Just $ ticketCreationTime t
                            , ciDescription = Just description
                            , ciMilestoneId = Just $ M.lookup (runIdentity $ ticketMilestone fields) milestoneMap
                            , ciWeight = Just $ prioToWeight $ runIdentity $ ticketPriority fields
                            }
    let ignore404 (FailureResponse resp)
          | 404 <- statusCode $ responseStatusCode resp
          = return ()
        ignore404 e
          = throwError e
    deleteIssue gitlabToken Nothing project iid `catchError` ignore404
    ir <- createIssue gitlabToken (Just creatorUid) project issue
    liftIO $ print ir
    return $ irIid ir

ticketNumberToIssueIid (TicketNumber n) =
  IssueIid $ fromIntegral n

createTicketChanges :: MilestoneMap
                    -> UserIdOracle
                    -> CommentCacheVar
                    -> (Int -> Int -> IO ())
                    -> IssueIid
                    -> TicketChange
                    -> ClientM ()
createTicketChanges milestoneMap getUserId commentCache storeComment iid tc = do
    liftIO $ print tc
    authorUid <- getUserId $ changeAuthor tc
    let t = case iid of IssueIid n -> TicketNumber $ fromIntegral n
    commentNumber <- liftIO $ do
      items <- M.lookup (unIssueIid iid) <$> readMVar commentCache
      case items of
        Nothing -> return 1
        Just xs -> return (length xs + 1)
    rawBody <- liftIO $ tracToMarkdown commentCache t $ fromMaybe "(no text)" $ changeComment tc
    let body = T.unlines
            [ rawBody
            , ""
            , "(Trac comment: " <> (T.pack . show) commentNumber <> ")"
            , fieldsTable
                mempty
                -- [ ("User", changeAuthor tc) -- ]
                (changeFields tc)
            ]
        note = CreateIssueNote { cinBody = body
                               , cinCreatedAt = Just $ changeTime tc
                               }

    liftIO $ putStrLn $ "NOTE: " ++ show body
    cinResp <- createIssueNote gitlabToken (Just authorUid) project iid note
    liftIO $ putStrLn $ "NOTE CREATED: " ++ show cinResp
    liftIO $ do
      modifyMVar_ commentCache $
        return .
        M.insertWith (flip (++)) (unIssueIid iid) [inrId cinResp]
      storeComment (unIssueIid iid) (inrId cinResp)
      (M.lookup (unIssueIid iid) <$> readMVar commentCache) >>= print

    let fields = hoistFields newValue $ changeFields tc
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
          maybe (return $ Nothing) (fmap Just . tracToMarkdown commentCache t) $
          ticketDescription fields
    let edit = EditIssue { eiTitle = notNull $ ticketSummary fields
                         , eiDescription = description
                         , eiMilestoneId = fmap (`M.lookup` milestoneMap) (ticketMilestone fields)
                         , eiLabels = Just $ fieldLabels fields
                         , eiStatus = status
                         , eiUpdateTime = Just $ changeTime tc
                         , eiWeight = prioToWeight <$> ticketPriority fields
                         }
    liftIO $ print edit
    when (not $ nullEditIssue edit)
        $ void $ editIssue gitlabToken (Just authorUid) project iid edit
    return ()

-- | Maps Trac keywords to labels
keywordLabels :: M.Map Text Labels
keywordLabels = M.fromList
    $ [ passthru "newcomer"
      , ("newcomers", "newcomer")
      , passthru "TypeInType"
      , passthru "TypeFamilies"
      , passthru "PatternSynonyms"
      , passthru "Deriving"
      , passthru "Generics"
      , passthru "PatternMatchWarnings"
      , passthru "Inlining"
      , passthru "QuantifiedConstraints"
      , passthru "TypeApplications"
      , passthru "LevityPolymorphism"
      , passthru "CodeGen"
      , passthru "GADTs"
      , passthru "JoinPoints"
      , passthru "Typeable"
      , ("Typeable", "typeable")
      , ("ORF", "OverloadedRecordFields")
      , ("hs-boot", "hs-boot")
      , passthru "SpecConstr"
      , passthru "ApplicativeDo"
      , passthru "FunDeps"
      , passthru "TypedHoles"
      , passthru "CSE"
      , ("TypeCheckerPlugins", "Plugins")
      , ("deriving-perf", "Deriving" <> "CompilerPerformance")
      , passthru "CUSKs"
      , passthru "PolyKinds"
      , ("performance", "RuntimePerformance")
      , ("ci-breakage", "CI-Breakage")

      , ("DWARF", "DebugInformation")
      , passthru "SafeHaskell"
      , passthru "CustomTypeErrors"
      , passthru "StaticPointers"
      , passthru "Unicode"
      , ("warnings", "ErrorMessages")
      , passthru "Arrows"
      , passthru "SIMD"
      , passthru "TemplateHaskell"
      ]
  where
    passthru x = (x, mkLabel x)

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

fieldLabels :: Fields Maybe -> Labels
fieldLabels fields =
    "TracImport" <> keywordLbls <> failureLbls
  where
    keywordLbls = mconcat
        [ lbl
        | Just keywords <- pure $ ticketKeywords fields
        , kw <- keywords
        , Just lbl <- pure $ M.lookup kw keywordLabels
        ]

    failureLbls :: Labels
    failureLbls = maybe mempty typeOfFailureLabels $ ticketTypeOfFailure fields

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
        , row "Milestone" $ concatFields ticketMilestone
        , row "Component" $ concatFields ticketComponent
        , row "Test case" $ concatFields $ ticketTestCase
        , row "Differential revisions" $ concatFields $ renderTicketDifferentials <$> ticketDifferentials
        , row "Status" $ concatFields $ T.pack . show <$> ticketStatus
        , row "Description" $ concatFields $ const "description changed" <$> concatFields ticketDescription
        , row "Keywords" $ concatFields $ T.intercalate ", " <$> ticketKeywords
        , row "BlockedBy" $ concatFields $ renderTicketNumbers <$> ticketBlockedBy
        , row "Related" $ concatFields $ renderTicketNumbers <$> ticketRelated
        , row "Blocking" $ concatFields $ renderTicketNumbers <$> ticketBlocking
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
