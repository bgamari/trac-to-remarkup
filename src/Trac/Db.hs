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
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.SqlQQ
import Debug.Trace (trace)


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
     Maybe Text, Maybe Text, TracTime, Maybe Text)

getTicket :: TicketNumber -> Connection -> IO (Maybe Ticket)
getTicket (TicketNumber t) conn = do
  tickets <- mapM (toTicket conn) =<< query conn
      [sql|SELECT id, type, time, component,
                  priority, reporter, status,
                  version, summary, milestone,
                  keywords, description, changetime,
                  cc
           FROM ticket
           WHERE id = ?
          |]
          (Only t)
  return $ listToMaybe tickets

findOrigField :: FromField a => Connection -> Text -> TicketNumber -> IO (Maybe a)
findOrigField conn field (TicketNumber n) = do
    mval <- query conn [sql|SELECT oldvalue
                            FROM ticket_change
                            WHERE ticket = ?
                            AND field = ?
                            ORDER BY time ASC
                            LIMIT 1
                           |]
                  (n, field)
    return $ case mval of
      [] -> Nothing
      [Only x] -> x

findOrig :: FromField a => Connection -> Text -> a -> TicketNumber -> IO a
findOrig conn field def ticketNumber = fromMaybe def <$> findOrigField conn field ticketNumber


toTicket :: Connection -> Row -> IO Ticket
toTicket conn
         ((n, typ, TracTime ticketCreationTime, component,
           prio, reporter, status) :.
          (mb_version, summary, mb_milestone,
           mb_keywords, mb_description, TracTime ticketChangeTime, mb_cc))
  = do
    let ticketStatus = Identity New
        ticketNumber = TicketNumber n
        ticketCreator = reporter

        i = Identity
    ticketSummary <- i <$> findOrig conn "summary" summary ticketNumber
    ticketComponent <- i <$> findOrig conn "component" component ticketNumber

    ticketType <- i . toTicketType <$> findOrig conn "type" typ ticketNumber
    ticketPriority <- i . toPriority <$> findOrig conn "priority" prio ticketNumber
    ticketVersion <- i <$> findOrig conn "version" (fromMaybe "" mb_version) ticketNumber
    ticketMilestone <- i <$> findOrig conn "milestone" (fromMaybe "" mb_milestone) ticketNumber
    ticketKeywords <- i . T.words <$> findOrig conn "keywords" (fromMaybe "" mb_keywords) ticketNumber
    ticketBlockedBy <- i . maybe [] parseTicketList <$> findOrigField conn "blockedby" ticketNumber
    ticketRelated <- i . maybe [] parseTicketList <$> findOrigField conn "related" ticketNumber
    ticketBlocking <- i . maybe [] parseTicketList <$> findOrigField conn "blocking" ticketNumber
    ticketDifferentials <- i . maybe [] parseDifferentials <$> findOrigField conn "differential" ticketNumber
    ticketTestCase <- i . fromMaybe "" <$> findOrigField conn "testcase" ticketNumber
    ticketDescription <- i <$> findOrig conn "description" (fromMaybe "" mb_description) ticketNumber
    ticketTypeOfFailure <- i . toTypeOfFailure <$> findOrig conn "failure" "" ticketNumber
    ticketCC <- i . T.words <$> findOrig conn "cc" (fromMaybe "" mb_cc) ticketNumber
    let ticketFields = Fields {..}
    return Ticket {..}

parseTicketList :: T.Text -> [TicketNumber]
parseTicketList = mapMaybe parseTicketNumber . T.words

parseTicketNumber :: T.Text -> Maybe TicketNumber
parseTicketNumber =
    either (const Nothing) (Just . TicketNumber . fst) .
    TR.decimal . T.dropWhile (=='#') . T.strip

parseDifferentials :: T.Text -> [Differential]
parseDifferentials = mapMaybe parseDifferential . T.words

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
          "keywords"     -> fieldChange $ emptyFieldsUpdate{ticketKeywords = mkUpdate (fmap T.words) old new}
          "status"       -> fieldChange $ emptyFieldsUpdate{ticketStatus = mkJustUpdate toStatus old new}
          "differential" -> fieldChange $ emptyFieldsUpdate{ticketDifferentials = mkUpdate (fmap parseDifferentials) old new}
          "blocking"     -> fieldChange $ emptyFieldsUpdate{ticketBlocking = mkUpdate (fmap parseTicketList) old new}
          "blockedby"    -> fieldChange $ emptyFieldsUpdate{ticketBlockedBy = mkUpdate (fmap parseTicketList) old new}
          "related"      -> fieldChange $ emptyFieldsUpdate{ticketRelated = mkUpdate (fmap parseTicketList) old new}
          "cc"           -> fieldChange $ emptyFieldsUpdate{ticketCC = mkUpdate (fmap T.words) old new}

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
        isSkippableField "owner" = True
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
