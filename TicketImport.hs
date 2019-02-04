{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module TicketImport
  ( createTicket
  , createTicketChanges
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Maybe
import Data.String
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Data.Foldable

import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Network.HTTP.Types.Status
import Data.Aeson.Text as Aeson
import Servant.Client

import GitLab.Tickets
import GitLab.Common
import Trac.Db.Types as Trac
import Trac.Writer (mkDifferentialLink)
import qualified Trac.Convert
import qualified Trac.Parser as Trac
import UserLookup
import MilestoneImport (MilestoneMap)
import Logging
import ImportState
import Settings

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
        , ("easy", "newcomer")
        , passthru "TypeInType"
        , passthru "type families"
        , passthru "pattern synonyms"
        , passthru "deriving"
        , ("Deriving", "deriving")
        , passthru "generics"
        , passthru "pattern match warnings"
        , passthru "inlining"
        , passthru "quantified constraints"
        , passthru "TypeApplications"
        , passthru "levity polymorphism"
        , passthru "code-gen"
        , ("codegen", "code-gen")
        , passthru "GADTs"
        , "JoinPoints" .= "join points"
        , passthru "Typeable"
        , ("Typeable", "Typeable")
        , ("ORF", "OverloadedRecordFields")
        , ("hs-boot", "hs-boot")
        , passthru "spec constr"
        , passthru "ApplicativeDo"
        , passthru "demand analysis"
        , ("FunDeps", "functional dependencies")
        , ("FunctionalDependencies", "functional dependencies")
        , "TypedHoles" .= "typed holes"
        , passthru "CSE"
        , ("TypeCheckerPlugins", "plugins")
        , ("deriving-perf", "deriving" <> "compiler perf")
        , passthru "CUSKs"
        , passthru "PolyKinds"
        , ("performance", "runtime perf")
        , ("ci-breakage", "CI breakage")
        , ("DWARF", "debug information")
        , passthru "SafeHaskell"
        , passthru "custom type errors"
        , passthru "StaticPointers"
        , passthru "Unicode"
        , ("unicode", "Unicode")
        , ("TypeErrorMessages", "error messages")
        , ("warnings", "error messages")
        , ("warning", "error messages")
        , passthru "Arrows"
        , passthru "SIMD"
        , passthru "TemplateHaskell"
        , passthru "backpack"
        , passthru "ImpredicativeTypes"
        , ("Roles", "roles")
        , passthru "exceptions"
        , passthru "debugger"
        , ("synonyms", "pattern synonyms")  -- generally arises from typos
        , passthru "cpp"
        , ("TypedTemplateHaskell", "typed TemplateHaskell")
        , passthru "strings"
        , passthru "clang"
        , ("FloatOut", "float-out")
        , passthru "QuantifiedContexts"
        , passthru "InjectiveFamilies"
        , passthru "performance"
        , ("CPRAnalysis", "CPR analysis")
        , ("StaticArgumentTransformation", "static argument transformation")
        , ("PartialTypeSignatures", "partial type sigs")
        , passthru "SafeHaskell"
        , ("RemoteGHCi", "remote GHCi")
        , passthru "integer-gmp"
        , ("RecompilationCheck", "recompilation checking")
        , ("LateLamLift", "late lambda lifting")
        , ("Simplifier", "simplifier")
        , passthru "CAFs"
        , passthru "rules"
        , passthru "linking"
        , ("linker", "linking")
        ]
      where
        passthru x = (x, mkLabel x)
        (.=) = (,)

typeOfFailureLabels :: TypeOfFailure -> Labels
typeOfFailureLabels t =
    case t of
      BuildingGhcFailed         -> "ghc build failure"
      CompileTimeCrash          -> "compiler crash"
      CompileTimePerformance    -> "compiler perf"
      IncorrectDebugInformation -> "debug information"
      DocumentationBug          -> "documentation"
      InvalidProgramAccepted    -> mempty
      GhcDoesn'tWork            -> mempty
      GhciCrash                 -> "GHCi"
      ValidProgramRejected      -> mempty
      IncorrectAPIAnnotation    -> "API annotations"
      PoorErrorMessage          -> "error messages"
      IncorrectWarning          -> "error messages"
      IncorrectResultAtRuntime  -> "incorrect runtime result"
      InstallationFailure       -> mempty
      RuntimeCrash              -> "runtime crash"
      RuntimePerformance        -> "runtime perf"
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
                      Just Invalid    -> Just CloseEvent

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

fieldsJSON :: forall f. (FieldToJSON f, Functor f, ConcatFields f, Show (Fields f))
           => Fields f -> T.Text
fieldsJSON fields =
    TL.toStrict $ "<!-- " <> Aeson.encodeToLazyText fields <> " -->"

fieldsTable :: forall f. (Functor f, ConcatFields f, Show (Fields f))
            => [(Text, Text)] -> Fields f -> T.Text
fieldsTable extraRows (Fields{..})
  | null rows = ""
  | otherwise = T.unlines $
    [ "<details><summary>Trac metadata</summary>"
    , ""
    , table
    , "</details>"
    ]
  where
    row :: Text -> Maybe Text -> Maybe (Text, Text)
    row name val = fmap (\v -> (name,v)) val

    rows :: [(Text, Text)]
    rows =
        catMaybes
        [ row "Version" $ concatFields ticketVersion
        , row "Type" $ concatFields $ T.pack . show <$> ticketType
        , row "TypeOfFailure" $ concatFields $ T.pack . show <$> ticketTypeOfFailure
        , row "Priority" $ concatFields $ toPriorityName <$> ticketPriority
        , row "Resolution" $ concatFields $ T.pack . show <$> ticketResolution
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

ticketNumberToIssueIid :: TicketNumber -> IssueIid
ticketNumberToIssueIid (TicketNumber n) =
  IssueIid $ fromIntegral n

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

justWhen :: Bool -> a -> Maybe a
justWhen True x = Just x
justWhen _ _ = Nothing

-- | Eliminate metadata changes that are mapped to native GitLab metadata.
-- Ensures that we don't produce redundant comments containing only JSON metadata.
filterFieldChanges :: Fields Update -> Fields Update
filterFieldChanges fields =
    fields { ticketType = noUpdate
           , ticketOwner = noUpdate
           , ticketSummary = noUpdate
           , ticketStatus = noUpdate
           , ticketKeywords = noUpdate
           , ticketMilestone = noUpdate
           }
  where
    noUpdate = Update Nothing Nothing

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

dummyLookupAnchor :: String -> Maybe String
dummyLookupAnchor = Just

