{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Trac.Db where

import Data.Functor.Identity
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Trac.Db.Types
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.Text (Text)
import qualified Data.Set as S
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.SqlQQ
import Debug.Trace (trace)
import Control.Monad (join)


deriving instance FromField TicketNumber
deriving instance ToField TicketNumber

instance FromField TracTime where
    fromField field bs = do
        t <- fromField field bs :: Conversion Integer
        return $ TracTime $ posixSecondsToUTCTime $ realToFrac t / 1000000

-- N.B. Work around tuple width limit
type Row =
    (Integer, Text, TracTime, Text,
     Text, Text, Text) :.
    (Maybe Text, Text, Maybe Text,
     Maybe Text, Maybe Text, TracTime, Maybe Text, Maybe Text)

getTicket :: TicketNumber -> Connection -> IO (Maybe Ticket)
getTicket (TicketNumber t) conn = do
  tickets <- mapM (toTicket conn) =<< query conn
      [sql|SELECT id, type, time, component,
                  priority, reporter, status,
                  version, summary, milestone,
                  keywords, description, changetime,
                  cc, owner
           FROM ticket
           WHERE id = ?
          |]
          (Only t)
  return $ listToMaybe tickets

findOrigField :: (Show a, FromField a) => Connection -> Text -> TicketNumber -> IO (Maybe (Maybe a))
findOrigField conn field (TicketNumber n) = do
    mval <- query conn [sql|SELECT oldvalue
                            FROM ticket_change
                            WHERE ticket = ?
                            AND field = ?
                            ORDER BY time ASC
                            LIMIT 1
                           |]
                  (n, field)
    putStrLn $ "findOrigField " ++ show field ++ ": " ++ show mval
    return $ case mval of
      [] -> Nothing
      [Only x] -> Just x

findOrig :: (Show a, FromField a) => Connection -> Text -> Maybe a -> TicketNumber -> IO (Maybe a)
findOrig conn field def ticketNumber =
  fromMaybe def <$> findOrigField conn field ticketNumber

findOrigDef :: (Show a, FromField a) => Connection -> Text -> a -> TicketNumber -> IO a
findOrigDef conn field def ticketNumber =
  fromMaybe def . join <$> findOrigField conn field ticketNumber

toTicket :: Connection -> Row -> IO Ticket
toTicket conn
         ((n, typ, TracTime ticketCreationTime, component,
           prio, reporter, status) :.
          (mb_version, summary, mb_milestone,
           mb_keywords, mb_description, TracTime ticketChangeTime, mb_cc, mb_owner))
  = do
    let ticketStatus = Identity New
        ticketNumber = TicketNumber n
        ticketCreator = reporter

        i = Identity
    ticketSummary <- i <$> findOrigDef conn "summary" summary ticketNumber
    ticketComponent <- i <$> findOrigDef conn "component" component ticketNumber

    ticketType <- i . toTicketType . fromMaybe "" <$> findOrig conn "type" (Just typ) ticketNumber
    ticketPriority <- i . toPriority . fromMaybe "" <$> findOrig conn "priority" (Just prio) ticketNumber
    ticketVersion <- i . fromMaybe "" <$> findOrig conn "version" mb_version ticketNumber
    ticketMilestone <- i . fromMaybe "" <$> findOrig conn "milestone" mb_milestone ticketNumber
    ticketKeywords <- i . S.fromList . T.words . fromMaybe "" <$> findOrig conn "keywords" mb_keywords ticketNumber
    ticketBlockedBy <- i . maybe mempty parseTicketSet <$> findOrig conn "blockedby" Nothing ticketNumber
    ticketRelated <- i . maybe mempty parseTicketSet <$> findOrig conn "related" Nothing ticketNumber
    ticketBlocking <- i . maybe mempty parseTicketSet <$> findOrig conn "blocking" Nothing ticketNumber
    ticketDifferentials <- i . maybe mempty parseDifferentials <$> findOrig conn "differential" Nothing ticketNumber
    ticketTestCase <- i . fromMaybe "" <$> findOrig conn "testcase" Nothing ticketNumber
    ticketDescription <- i . fromMaybe "" <$> findOrig conn "description" mb_description ticketNumber
    ticketTypeOfFailure <- i . toTypeOfFailure . fromMaybe "" <$> findOrig conn "failure" (Just "") ticketNumber
    ticketCC <- i . S.fromList . commaSep . fromMaybe "" <$> findOrig conn "cc" mb_cc ticketNumber
    ticketOwner <- i . maybe Unowned toTicketOwner <$> findOrig conn "owner" mb_owner ticketNumber
    ticketOperatingSystem <- i . fromMaybe "" <$> findOrig conn "os" mb_owner ticketNumber
    ticketArchitecture <- i . fromMaybe "" <$> findOrig conn "architecture" mb_owner ticketNumber
    let ticketFields = Fields {..}
    return Ticket {..}

commaSep :: T.Text -> [T.Text]
commaSep = map T.strip . T.split (== ',')

parseTicketSet :: T.Text -> S.Set TicketNumber
parseTicketSet = S.fromList . mapMaybe parseTicketNumber . T.words

parseTicketNumber :: T.Text -> Maybe TicketNumber
parseTicketNumber =
    either (const Nothing) (Just . TicketNumber . fst) .
    TR.decimal . T.dropWhile (=='#') . T.strip

parseDifferentials :: T.Text -> S.Set Differential
parseDifferentials = S.fromList . mapMaybe parseDifferential . T.words

parseDifferential :: T.Text -> Maybe Differential
parseDifferential str = do
  let stripped1 = T.strip str
      stripped2 = T.strip . fromMaybe stripped1 $ T.stripSuffix "," stripped1
      stripped3 = fromMaybe stripped2 $ T.stripPrefix "Phab:" stripped2
  stripped4 <- T.stripPrefix "D" stripped3
  either (const Nothing) (Just . Differential . fst) $ TR.decimal stripped4


getTickets :: Connection -> IO [Ticket]
getTickets conn = do
    mapM (toTicket conn) =<< query_ conn
      [sql|SELECT id, type, time, component,
                  priority, reporter, status,
                  version, summary, milestone,
                  keywords, description, changetime
           FROM ticket
          |]

getTicketChanges :: Connection -> TicketNumber -> Maybe RawTime -> IO [TicketChange]
getTicketChanges conn n mtime = do
  let run = case mtime of
              Nothing ->
                query conn
                  [sql|SELECT time, author, field, oldvalue, newvalue
                       FROM ticket_change
                       WHERE ticket = ?
                       ORDER BY time ASC
                      |]
                  (Only n)
              Just t ->
                query conn
                  [sql|SELECT time, author, field, oldvalue, newvalue
                       FROM ticket_change
                       WHERE ticket = ?
                       AND time = ?
                       ORDER BY time ASC
                      |]
                  (n, t)
  map toChange <$> run
  where
    toChange :: (TracTime, Text, Text, Maybe Text, Maybe Text) -> TicketChange
    toChange (TracTime t, author, field, old, new) =
        case field of
          "type"         -> fieldChange $ emptyFieldsUpdate{ticketType = mkUpdate (fmap toTicketType) old new}
          "summary"      -> fieldChange $ emptyFieldsUpdate{ticketSummary = mkUpdate id old new}
          "description"  -> fieldChange $ emptyFieldsUpdate{ticketDescription = Update old new}
          "priority"     -> fieldChange $ emptyFieldsUpdate{ticketPriority = mkUpdate (fmap toPriority) old new}
          "milestone"    -> fieldChange $ emptyFieldsUpdate{ticketMilestone = mkUpdate id old new}
          "testcase"     -> fieldChange $ emptyFieldsUpdate{ticketTestCase = mkUpdate id old new}
          "keywords"     -> fieldChange $ emptyFieldsUpdate{ticketKeywords = mkUpdate (fmap $ S.fromList . T.words) old new}
          "status"       -> fieldChange $ emptyFieldsUpdate{ticketStatus = mkJustUpdate toStatus old new}
          "differential" -> fieldChange $ emptyFieldsUpdate{ticketDifferentials = mkUpdate (fmap parseDifferentials) old new}
          "blocking"     -> fieldChange $ emptyFieldsUpdate{ticketBlocking = mkUpdate (fmap parseTicketSet) old new}
          "blockedby"    -> fieldChange $ emptyFieldsUpdate{ticketBlockedBy = mkUpdate (fmap parseTicketSet) old new}
          "related"      -> fieldChange $ emptyFieldsUpdate{ticketRelated = mkUpdate (fmap parseTicketSet) old new}
          "cc"           -> fieldChange $ emptyFieldsUpdate{ticketCC = mkUpdate (fmap $ S.fromList . commaSep) old new}
          "owner"        -> fieldChange $ emptyFieldsUpdate{ticketOwner = mkUpdate (fmap toTicketOwner) old new}
          "component"    -> fieldChange $ emptyFieldsUpdate{ticketComponent = mkUpdate id old new}
          "version"      -> fieldChange $ emptyFieldsUpdate{ticketVersion = mkUpdate id old new}
          "failure"      -> fieldChange $ emptyFieldsUpdate{ticketTypeOfFailure = mkUpdate (fmap toTypeOfFailure) old new}
          "os"           -> fieldChange $ emptyFieldsUpdate{ticketOperatingSystem = mkUpdate id old new}
          "architecture" -> fieldChange $ emptyFieldsUpdate{ticketArchitecture = mkUpdate id old new}

          -- TODO: The other fields

          "comment"     -> empty {changeComment = Just $ expectJust new}
          _             -> if isSkippableField field
                              then
                                 empty
                              else
                                 trace ("TICKET FIELD NOT IMPLEMENTED: " ++ show field ++ " = " ++ show new) $
                                   empty

      where
        isSkippableField :: Text -> Bool
        isSkippableField "resolution" = True
        isSkippableField "difficulty" = True
        isSkippableField x | "_comment" `T.isPrefixOf` x = True
        isSkippableField _ = False

        mkJustUpdate :: (a -> b) -> Maybe a -> Maybe a -> Update b
        mkJustUpdate f = mkUpdate (Just . f . expectJust)

        mkUpdate :: (a -> Maybe b) -> a -> a -> Update b
        mkUpdate f old new = Update (f old) (f new)

        expectJust Nothing = error $ unlines [ "expected Just newvalue:"
                                             , "  t: " <> show t
                                             , "  field: " <> show field
                                             , "  newvalue: "<>  show new
                                             ]
        expectJust (Just x) = x

        empty = TicketChange { changeTime = t
                             , changeAuthor = author
                             , changeFields = emptyFieldsUpdate
                             , changeComment = Nothing
                             }
        fieldChange flds = empty {changeFields = flds}


getTicketMutations :: Connection -> IO [TicketMutation]
getTicketMutations conn = do
  map toTicketMutation <$> query conn
    [sql|SELECT id as ticket, time, ? as type FROM ticket
         UNION
         SELECT DISTINCT ticket, time, ? as type FROM ticket_change
         ORDER BY time, ticket, type
         |]
     (fromEnum CreateTicket, fromEnum ChangeTicket)
  where
    toTicketMutation :: (TicketNumber, RawTime, Int) -> TicketMutation
    toTicketMutation (ticketMutationTicket, ticketMutationTime, typeIndex) =
      TicketMutation {..}
      where
        ticketMutationType = toEnum typeIndex
    

toStatus :: Text -> Status
toStatus t = case t of
    "new"        -> New
    "assigned"   -> Assigned
    "patch"      -> Patch
    "merge"      -> Merge
    "closed"     -> Closed
    "infoneeded" -> InfoNeeded
    "upstream"   -> Upstream
    "reopened"   -> New
    _            -> error $ "unknown status: " ++ show t

toPriority :: Text -> Priority
toPriority t = case t of
    "lowest"  -> PrioLowest
    "low"     -> PrioLow
    "normal"  -> PrioNormal
    "high"    -> PrioHigh
    "highest" -> PrioHighest
    _ -> PrioNormal

toTicketType :: Text -> TicketType
toTicketType t = case t of
    "bug"  -> Bug
    "task" -> Task
    "merge" -> MergeReq
    "feature request" -> FeatureRequest
    _ -> Bug -- TODO

toTypeOfFailure :: Text -> TypeOfFailure
toTypeOfFailure t = case t of
    "Building GHC failed" -> BuildingGhcFailed
    "Compile-time crash" -> CompileTimeCrash
    "Compile-time crash or panic" -> CompileTimeCrash
    "Compile-time performance bug" -> CompileTimePerformance
    "Debugging information is incorrect" -> IncorrectDebugInformation
    "Documentation bug" -> DocumentationBug
    "GHC accepts invalid program" -> InvalidProgramAccepted
    "GHC doesn't work at all" -> GhcDoesn'tWork
    "GHCi crash" -> GhciCrash
    "GHC rejects valid program" -> ValidProgramRejected
    "Incorrect API annotation" -> IncorrectAPIAnnotation
    "Incorrect error/warning at compile-time" -> IncorrectWarning
    "Incorrect result at runtime" -> IncorrectResultAtRuntime
    "Incorrect warning at compile-time" -> IncorrectWarning
    "Installing GHC failed" -> InstallationFailure
    "None/Unknown" -> OtherFailure
    "Other" -> OtherFailure
    "Poor/confusing error message" -> PoorErrorMessage
    "Runtime crash" -> RuntimeCrash
    "Runtime performance bug" -> RuntimePerformance
    "" -> OtherFailure

data Milestone = Milestone { mName :: Text
                           , mDescription :: Text
                           , mDue :: Maybe UTCTime
                           , mCompleted :: Maybe UTCTime
                           }

getMilestones :: Connection -> IO [Milestone]
getMilestones conn = do
    map f <$> query_ conn
      [sql|SELECT name, due, completed, description
           FROM milestone
          |]
  where
    f (mName, mDueTrac, mCompletedTrac, mDescription) =
        let mDue = tracToUTC <$> nullifyTimestamp mDueTrac
            mCompleted = tracToUTC <$> nullifyTimestamp mCompletedTrac
        in Milestone {..}

nullifyTimestamp :: Maybe TracTime -> Maybe TracTime
nullifyTimestamp Nothing = Nothing
nullifyTimestamp (Just t@(TracTime u))
  | u == posixSecondsToUTCTime 0 = Nothing
  | otherwise = Just t

getAttachments :: Connection -> IO [Attachment]
getAttachments conn = do
    mapMaybe f <$> query_ conn
        [sql|SELECT type, id, filename, time, description, author, ipnr
             FROM attachment |]
  where
    f :: (Text, Text, Text, TracTime, Text, Text, Maybe Text) -> Maybe Attachment
    f (typ, rid, aFilename, TracTime aTime, aDescription, aAuthor, aIpAddr)
      | Just aResource <- theResource = Just Attachment {..}
      | otherwise = Nothing
      where
        theResource = case typ of
          "ticket" -> Just $ TicketAttachment $ TicketNumber $ read $ T.unpack rid
          "wiki"   -> Just $ WikiAttachment $ WikiName rid
          "blog"   -> Nothing
          _        -> error $ "Unknown attachment resource type " ++ show typ

getWikiPages :: Connection -> IO [WikiPage]
getWikiPages conn = do
  mapMaybe f <$> query_ conn
        [sql|SELECT name, time, version, author, text, comment
             FROM wiki w
             ORDER BY time, version |]
  where
    f :: (Text, TracTime, Int, Text, Text, Maybe Text) -> Maybe WikiPage
    f (name, TracTime time, version, author, text, mcomment) =
      Just $ WikiPage name time version text mcomment author

-- | Get only the very first and very last version of each wiki page. This will
-- only record the most recent version, discarding the wiki page's history.
getWikiPagesFast :: Connection -> IO [WikiPage]
getWikiPagesFast conn = do
  mapMaybe f <$> query_ conn
        [sql|SELECT name, time, version, author, text, comment
             FROM wiki w
             WHERE w.version = (SELECT MAX(version) FROM wiki v WHERE v.name = w.name)
             ORDER BY time, version |]
  where
    f :: (Text, TracTime, Int, Text, Text, Maybe Text) -> Maybe WikiPage
    f (name, TracTime time, version, author, text, mcomment) =
      Just $ WikiPage name time version text mcomment author

data UserAttribute = Email

getUserAttribute :: Connection -> UserAttribute -> T.Text -> IO (Maybe Text)
getUserAttribute conn attr user = do
    res <- query conn
        [sql|SELECT value
             FROM session_attribute
             WHERE sid = ? AND name = ?
            |]
        (user, attrName)
    return $ case res of
      [Only val] -> Just val
      _ -> Nothing
  where
    attrName :: Text
    attrName = case attr of
                 Email -> "email"
