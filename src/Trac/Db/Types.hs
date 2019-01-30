{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Trac.Db.Types where

import GHC.Generics
import Control.Applicative
import Data.Monoid
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Time.Clock
import Data.Maybe
import Data.Aeson as Aeson

newtype TracTime = TracTime { tracToUTC :: UTCTime }
                 deriving (Eq, Ord, Show)

type RawTime = Integer

newtype TracUser = TracUser { getTracUser :: Text }
                 deriving stock (Show, Read, Ord, Eq)
                 deriving newtype (ToJSON)

data TicketOwner = OwnedBy TracUser
                 | Unowned
                 deriving stock (Show, Read, Ord, Eq, Generic)
                 deriving anyclass (ToJSON)

toTicketOwner :: Text -> TicketOwner
toTicketOwner x
  | T.null x = Unowned
  | otherwise = OwnedBy $ TracUser x

newtype TicketNumber = TicketNumber { getTicketNumber :: Integer }
                     deriving stock (Show, Read, Ord, Eq)
                     deriving newtype (ToJSON)

data TicketType = FeatureRequest | Bug | MergeReq | Task
                deriving stock (Eq, Show, Generic)
                deriving anyclass (ToJSON)

data Ticket = Ticket { ticketNumber       :: TicketNumber
                     , ticketCreationTime :: UTCTime
                     , ticketChangeTime   :: UTCTime
                     , ticketFields       :: Fields Identity
                     , ticketCreator      :: Text
                     }
            deriving (Show)

data MutationType = CreateTicket -- ^ Ticket creation
                  | ChangeTicket -- ^ One or more changes
            deriving (Show, Read, Enum, Ord, Eq)

-- | Ticket mutation; this data structure does not, however, give us
-- the complete ticket mutation information, only the ticket number,
-- modification timestamp, and whether it was a ticket creation or
-- a set of ticket changes. Multiple ticket changes with the same timestamp
-- and ticket number are reported as one mutation.
data TicketMutation = TicketMutation { ticketMutationTicket :: TicketNumber
                                     , ticketMutationTime :: RawTime
                                     , ticketMutationType :: MutationType
                                     }
                                     deriving (Show, Read, Ord, Eq)

data Priority = PrioLowest | PrioLow | PrioNormal | PrioHigh | PrioHighest
              deriving stock (Eq, Show, Generic)
              deriving anyclass (ToJSON)

data Status = New | Assigned | Patch | Merge | Closed | InfoNeeded | Upstream
            deriving stock (Eq, Show, Generic)
            deriving anyclass (ToJSON)

data Fields f = Fields { ticketType            :: f TicketType
                       , ticketSummary         :: f Text
                       , ticketComponent       :: f Text
                       , ticketPriority        :: f Priority
                       , ticketVersion         :: f Text
                       , ticketMilestone       :: f Text
                       , ticketDescription     :: f Text
                       , ticketTypeOfFailure   :: f TypeOfFailure
                       , ticketKeywords        :: f (S.Set Text)
                       , ticketBlockedBy       :: f (S.Set TicketNumber)
                       , ticketRelated         :: f (S.Set TicketNumber)
                       , ticketBlocking        :: f (S.Set TicketNumber)
                       , ticketDifferentials   :: f (S.Set Differential)
                       , ticketTestCase        :: f Text
                       , ticketStatus          :: f Status
                       , ticketCC              :: f (S.Set Text)
                       , ticketOwner           :: f TicketOwner
                       , ticketOperatingSystem :: f Text
                       , ticketArchitecture    :: f Text
                       }

isTrivialFieldUpdate :: Fields Update -> Bool
isTrivialFieldUpdate = getAll . foldFields . hoistFields (Const . All . isTrivialUpdate)

instance FieldToJSON f => ToJSON (Fields f) where
    toJSON Fields{..} = object $ concat
        [ "type" .=? ticketType
        , "summary" .=? ticketSummary
        , "component" .=? ticketComponent
        , "version" .=? ticketVersion
        , "milestone" .=? ticketMilestone
        , "description" .=? ticketDescription
        , "type_of_failure" .=? ticketTypeOfFailure
        , "keywords" .=? ticketKeywords
        , "blocked_by" .=? ticketBlockedBy
        , "related" .=? ticketRelated
        , "blocking" .=? ticketBlocking
        , "differentials" .=? ticketDifferentials
        , "test_case" .=? ticketTestCase
        , "status" .=? ticketStatus
        , "cc" .=? ticketCC
        , "owner" .=? ticketOwner
        , "operating_system" .=? ticketOperatingSystem
        , "architecture" .=? ticketArchitecture
        ]
      where
        field .=? value
          | Just x <- fieldToJSON value
          = [field .= x]
          | otherwise
          = []

foldFields :: (Semigroup b) => Fields (Const b) -> b
foldFields Fields{..}=
       getConst ticketType
    <> getConst ticketSummary
    <> getConst ticketComponent
    <> getConst ticketPriority
    <> getConst ticketVersion
    <> getConst ticketMilestone
    <> getConst ticketDescription
    <> getConst ticketTypeOfFailure
    <> getConst ticketKeywords
    <> getConst ticketBlockedBy
    <> getConst ticketRelated
    <> getConst ticketBlocking
    <> getConst ticketDifferentials
    <> getConst ticketTestCase
    <> getConst ticketStatus
    <> getConst ticketCC
    <> getConst ticketOwner
    <> getConst ticketOperatingSystem
    <> getConst ticketArchitecture

-- | The constraints here are quite arbitrary; they are just what we happen to
-- need.
hoistFields :: (forall a. (IsEmpty a, Eq a) => f a -> g a) -> Fields f -> Fields g
hoistFields f Fields{..} =
    Fields { ticketType            = f ticketType
           , ticketSummary         = f ticketSummary
           , ticketComponent       = f ticketComponent
           , ticketPriority        = f ticketPriority
           , ticketVersion         = f ticketVersion
           , ticketMilestone       = f ticketMilestone
           , ticketDescription     = f ticketDescription
           , ticketTypeOfFailure   = f ticketTypeOfFailure
           , ticketKeywords        = f ticketKeywords
           , ticketBlockedBy       = f ticketBlockedBy
           , ticketRelated         = f ticketRelated
           , ticketBlocking        = f ticketBlocking
           , ticketDifferentials   = f ticketDifferentials
           , ticketTestCase        = f ticketTestCase
           , ticketStatus          = f ticketStatus
           , ticketCC              = f ticketCC
           , ticketOwner           = f ticketOwner
           , ticketOperatingSystem = f ticketOperatingSystem
           , ticketArchitecture    = f ticketArchitecture
           }

emptyFieldsOf :: (forall a. f a) -> Fields f
emptyFieldsOf x = Fields
    x x x
    x x x
    x x x
    x x x
    x x x
    x x x
    x

emptyFields :: Fields Maybe
emptyFields = emptyFieldsOf Nothing

emptyFieldsUpdate :: Fields Update
emptyFieldsUpdate = emptyFieldsOf (Update Nothing Nothing)


collapseFields :: Alternative m => Fields m -> Fields m -> Fields m
collapseFields a b =
    Fields { ticketType = ticketType a <|> ticketType b
           , ticketSummary = ticketSummary a <|> ticketSummary b
           , ticketComponent = ticketComponent a <|> ticketComponent b
           , ticketPriority = ticketPriority a <|> ticketPriority b
           , ticketVersion = ticketVersion a <|> ticketVersion b
           , ticketMilestone = ticketMilestone a <|> ticketMilestone b
           , ticketDescription = ticketDescription a <|> ticketDescription b
           , ticketTypeOfFailure = ticketTypeOfFailure a <|> ticketTypeOfFailure b
           , ticketKeywords = ticketKeywords a <|> ticketKeywords b
           , ticketBlockedBy = ticketBlockedBy a <|> ticketBlockedBy b
           , ticketRelated = ticketRelated a <|> ticketRelated b
           , ticketBlocking = ticketBlocking a <|> ticketBlocking b
           , ticketDifferentials = ticketDifferentials a <|> ticketDifferentials b
           , ticketTestCase = ticketTestCase a <|> ticketTestCase b
           , ticketStatus = ticketStatus a <|> ticketStatus b
           , ticketCC = ticketCC a <|> ticketCC b
           , ticketOwner = ticketOwner a <|> ticketOwner b
           , ticketOperatingSystem = ticketOperatingSystem a <|> ticketOperatingSystem b
           , ticketArchitecture = ticketArchitecture a <|> ticketArchitecture b
           }

deriving instance Show (Fields Identity)
deriving instance Show (Fields Maybe)

newtype Differential = Differential { getDifferentialNumber :: Int }
                     deriving stock (Show, Eq, Ord)
                     deriving newtype (ToJSON)

data Update a = Update { oldValue :: Maybe a, newValue :: Maybe a }
  deriving (Show, Functor)

instance Applicative Update where
  pure x = Update (pure x) (pure x)
  Update a b <*> Update c d = Update (a <*> c) (b <*> d)

instance Alternative Update where
  Update a b <|> Update c d = Update (a <|> c) (b <|> d)
  empty = Update empty empty

instance Semigroup a => Semigroup (Update a) where
  Update a b <> Update c d = Update (a <> c) (b <> d)

instance Monoid a => Monoid (Update a) where
  mempty = Update mempty mempty


class IsEmpty a where
    isEmpty :: a -> Bool

instance IsEmpty T.Text where
    isEmpty = T.null

instance IsEmpty TicketOwner where
    isEmpty Unowned = True
    isEmpty _ = False

instance IsEmpty (S.Set a) where
    isEmpty = S.null

instance IsEmpty TicketType where
    isEmpty _ = False

instance IsEmpty Priority where
    isEmpty _ = False

instance IsEmpty TypeOfFailure where
    isEmpty _ = False

instance IsEmpty Status where
    isEmpty _ = False

-- | An update is trivial if it either doesn't change anything or contains no
-- useful information.
isTrivialUpdate :: (IsEmpty a, Eq a) => Update a -> Bool
isTrivialUpdate (Update Nothing Nothing) = True
isTrivialUpdate (Update (Just old) Nothing)
  | isEmpty old = True
isTrivialUpdate (Update Nothing (Just new))
  | isEmpty new = True
isTrivialUpdate (Update old new) = old == new


class ConcatFields f where
  concatFields :: f Text -> Maybe Text

instance ConcatFields [] where
  concatFields [] = Nothing
  concatFields xs = Just $ T.intercalate ", " xs

instance ConcatFields Maybe where
  concatFields = id

instance ConcatFields Update where
  concatFields update | isTrivialUpdate update = Nothing
  concatFields (Update old new) =
    Just $ fromMaybe "-" old <> " → " <> fromMaybe "-" new

instance ConcatFields Identity where
  concatFields (Identity t) = Just t


class FieldToJSON f where
  fieldToJSON :: ToJSON a => f a -> Maybe Aeson.Value

instance FieldToJSON Update where
  fieldToJSON (Update a b) =
      Just $ Aeson.object [ "old" .= a
                          , "new" .= b
                          ]

instance FieldToJSON Identity where
  fieldToJSON (Identity a) = Just $ Aeson.toJSON a

deriving instance Show (Fields Update)


data TicketChange = TicketChange { changeTime    :: UTCTime
                                 , changeAuthor  :: Text
                                 , changeFields  :: Fields Update
                                 , changeComment :: Maybe Text
                                 }

                  deriving (Show)

data TypeOfFailure
    = BuildingGhcFailed
    | CompileTimeCrash
    | CompileTimePerformance
    | IncorrectDebugInformation
    | DocumentationBug
    | InvalidProgramAccepted
    | GhcDoesn'tWork
    | GhciCrash
    | ValidProgramRejected
    | IncorrectAPIAnnotation
    | PoorErrorMessage
    | IncorrectWarning
    | IncorrectResultAtRuntime
    | InstallationFailure
    | RuntimeCrash
    | RuntimePerformance
    | OtherFailure
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

newtype WikiName = WikiName Text
                 deriving (Ord, Eq, Show, Read)

data Attachment = Attachment { aResource    :: AttachmentResource
                             , aFilename    :: Text
                             , aTime        :: UTCTime
                             , aDescription :: Text
                             , aAuthor      :: Text
                             , aIpAddr      :: Maybe Text
                             }
                deriving (Show, Read)

data AttachmentResource = TicketAttachment !TicketNumber
                        | WikiAttachment !WikiName
                        deriving (Ord, Eq, Show, Read)

data WikiPage = WikiPage { wpName :: Text
                         , wpTime :: UTCTime
                         , wpVersion :: Int
                         , wpBody :: Text
                         , wpComment :: Maybe Text
                         , wpAuthor :: Text
                         }
              deriving (Show, Read)

data CommentRef = MissingCommentRef
                | NoteRef Int
                | CommitRef CommitHash (Maybe RepoName)
                deriving (Show)

type CommitHash = String

type RepoName = String
