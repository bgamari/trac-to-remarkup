{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GitLab.Tickets where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Text (Text)
import Data.Semigroup
import Data.Maybe
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Proxy
import Data.String
import Data.Time.Clock
import Servant.API
import Servant.Client
import GitLab.Common
import Control.Monad.IO.Class (liftIO)
import Logging

----------------------------------------------------------------------
-- getIssue
----------------------------------------------------------------------

type GetIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid
    :> Get '[JSON] IssueResp

data IssueResp
    = IssueResp { irProjectId :: ProjectId
                , irIid :: IssueIid
                , irLabels :: Labels
                }
    deriving (Show)

instance FromJSON IssueResp where
    parseJSON = withObject "issue response" $ \o ->
      IssueResp <$> o .: "project_id"
                <*> o .: "iid"
                <*> o .: "labels"

getIssue :: AccessToken -> ProjectId -> IssueIid -> ClientM IssueResp
getIssue tok prj iid =
    client (Proxy :: Proxy GetIssueAPI) (Just tok) prj iid

----------------------------------------------------------------------
-- createIssue
----------------------------------------------------------------------

data CreateIssue
    = CreateIssue { ciIid :: Maybe IssueIid
                  , ciTitle :: Text
                  , ciLabels :: Maybe Labels
                  , ciCreatedAt :: Maybe UTCTime
                  , ciDescription :: Maybe Text
                  , ciMilestoneId :: Maybe (Maybe MilestoneId)
                  , ciWeight :: Maybe Weight
                  , ciAssignees :: Maybe [UserId]
                  }
                  deriving (Show)

instance ToJSON CreateIssue where
    toJSON CreateIssue{..} = object
        [ "iid" .= ciIid
        , "title" .= ciTitle
        , "labels" .= ciLabels
        , "created_at" .= ciCreatedAt
        , "description" .= ciDescription
        , "milestone_id" .= ciMilestoneId
        , "weight" .= ciWeight
        , "assignee_ids" .= ciAssignees
        ]

type CreateIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> ReqBody '[JSON] CreateIssue
    :> SudoParam
    :> Post '[JSON] IssueResp

createIssue :: AccessToken -> Maybe UserId
            -> ProjectId -> CreateIssue -> ClientM IssueResp
createIssue tok sudo prj ci =
    client (Proxy :: Proxy CreateIssueAPI) (Just tok) prj ci sudo

----------------------------------------------------------------------
-- editIssue
----------------------------------------------------------------------

data EditIssue
    = EditIssue { eiTitle       :: Maybe Text
                , eiDescription :: Maybe Text
                , eiMilestoneId :: Maybe (Maybe MilestoneId)
                , eiLabels      :: Maybe Labels
                , eiStatus      :: Maybe StatusEvent
                , eiUpdateTime  :: Maybe UTCTime
                , eiWeight      :: Maybe Weight
                , eiAssignees   :: Maybe [UserId]
                , eiKeywords    :: Maybe [Text]
                }
    deriving (Show)

instance ToJSON EditIssue where
    toJSON EditIssue{..} = object
        $ catMaybes
        [ "title" .=? eiTitle
        , "description" .=? eiDescription
        , "milestone_id" .=? eiMilestoneId
        , "labels" .=? eiLabels
        , "state_event" .=? eiStatus
        , "updated_at" .=? eiUpdateTime
        , "weight" .=? eiWeight
        , "assignee_ids" .=? eiAssignees
        , "keywords" .=? eiKeywords
        ]

type EditIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid
    :> ReqBody '[JSON] EditIssue
    :> SudoParam
    :> Put '[JSON] IssueResp

nullEditIssue :: EditIssue -> Bool
nullEditIssue (EditIssue a b c _ e _ g h i) =
    isNothing a && isNothing b && isNothing c &&
    isNothing e && isNothing g && isNothing h &&
    isNothing i
    -- N.B. Ignore update time and labels

editIssue :: AccessToken -> Maybe UserId
          -> ProjectId -> IssueIid -> EditIssue -> ClientM IssueResp
editIssue tok sudo prj iid ei =
    client (Proxy :: Proxy EditIssueAPI) (Just tok) prj iid ei sudo

----------------------------------------------------------------------
-- createIssueNote
----------------------------------------------------------------------

data CreateIssueNote
    = CreateIssueNote { cinBody :: Text
                      , cinCreatedAt :: Maybe UTCTime
                      }

instance ToJSON CreateIssueNote where
    toJSON CreateIssueNote{..} = object
        [ "body" .= cinBody
        , "created_at" .= cinCreatedAt
        ]

data IssueNoteResp
    = IssueNoteResp { inrId :: Int
                    }
    deriving (Show)

instance FromJSON IssueNoteResp where
    parseJSON = withObject "issue note response" $ \o ->
      IssueNoteResp <$> o .: "id"

type CreateIssueNoteAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid :> "notes"
    :> ReqBody '[JSON] CreateIssueNote
    :> SudoParam
    :> Post '[JSON] IssueNoteResp

createIssueNote :: AccessToken
                -> Maybe UserId
                -> ProjectId
                -> IssueIid
                -> CreateIssueNote -> ClientM IssueNoteResp
createIssueNote tok sudo prj iis cin =
    client (Proxy :: Proxy CreateIssueNoteAPI) (Just tok) prj iis cin sudo

----------------------------------------------------------------------
-- listIssueNotes
----------------------------------------------------------------------

data AscDesc = Desc | Asc
  deriving (Ord, Eq, Enum, Bounded)

instance ToHttpApiData AscDesc where
  toQueryParam Asc = "asc"
  toQueryParam Desc = "desc"
  toUrlPiece Asc = "asc"
  toUrlPiece Desc = "desc"

type ListIssueNotesAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid :> "notes"
    :> QueryParam "sort" AscDesc
    :> QueryParam "page" Int
    :> QueryParam "per_page" Int
    :> SudoParam
    :> Get '[JSON] [IssueNoteResp]

getNewestIssueNote :: AccessToken
                   -> Maybe UserId
                   -> ProjectId
                   -> IssueIid
                   -> ClientM (Maybe IssueNoteResp)
getNewestIssueNote tok sudo proj iis =
  listToMaybe <$> getNewestIssueNotes tok sudo proj iis 1

getNewestIssueNotes :: AccessToken
                    -> Maybe UserId
                    -> ProjectId
                    -> IssueIid
                    -> Int
                    -> ClientM [IssueNoteResp]
getNewestIssueNotes tok sudo prj iis pagesize =
    client (Proxy :: Proxy ListIssueNotesAPI)
      (Just tok) prj iis (Just Desc) (Just 0) (Just pagesize) sudo

listIssueNotesPage :: AccessToken
                   -> Maybe UserId
                   -> ProjectId
                   -> IssueIid
                   -> Int
                   -> ClientM [IssueNoteResp]
listIssueNotesPage tok sudo prj iis page =
    client (Proxy :: Proxy ListIssueNotesAPI)
      (Just tok) prj iis (Just Asc) (Just page) (Just 100) sudo

listIssueNotes :: AccessToken
               -> Maybe UserId
               -> ProjectId
               -> IssueIid
               -> ClientM [IssueNoteResp]
listIssueNotes tok sudo proj iis =
  go 0
  where
    go n = do
      notes <- listIssueNotesPage tok sudo proj iis n
      case notes of
        [] -> return []
        _ -> do
          moreNotes <- go (succ n)
          return $ notes ++ moreNotes

----------------------------------------------------------------------
-- deleteIssue
----------------------------------------------------------------------

type DeleteIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid
    :> SudoParam
    :> Delete '[] NoContent

deleteIssue :: AccessToken -> Maybe UserId -> ProjectId -> IssueIid -> ClientM ()
deleteIssue tok sudo prj iid = void $ client (Proxy :: Proxy DeleteIssueAPI) (Just tok) prj iid sudo

----------------------------------------------------------------------
-- createMilestone
----------------------------------------------------------------------

type CreateMilestoneAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "milestones"
    :> ReqBody '[JSON] CreateMilestone
    :> SudoParam
    :> Post '[JSON] CreateMilestoneResp

data CreateMilestone
    = CreateMilestone { cmTitle :: Text
                      , cmDescription :: Text
                      , cmDueDate :: Maybe UTCTime
                      , cmStartDate :: Maybe UTCTime
                      }
                      deriving (Show)

instance ToJSON CreateMilestone where
    toJSON CreateMilestone{..} = object
        [ "title" .= cmTitle
        , "description" .= cmDescription
        , "due_date" .= cmDueDate
        , "start_date" .= cmStartDate
        ]

data CreateMilestoneResp = CreateMilestoneResp MilestoneId

instance FromJSON CreateMilestoneResp where
    parseJSON = withObject "create milestone response" $ \o -> do
        CreateMilestoneResp <$> o .: "id"

createMilestone :: Logger
                -> AccessToken -> Maybe UserId
                -> ProjectId -> CreateMilestone
                -> ClientM MilestoneId
createMilestone logger tok sudo prj cm = do
    liftIO $ writeLog logger "CREATE-MILESTONE" $ show cm
    CreateMilestoneResp mid <- client (Proxy :: Proxy CreateMilestoneAPI) (Just tok) prj cm sudo
    return mid

----------------------------------------------------------------------
-- editMilestone
----------------------------------------------------------------------

type EditMilestoneAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "milestones"
    :> Capture "milestone id" MilestoneId
    :> ReqBody '[JSON] EditMilestone
    :> SudoParam
    :> Put '[JSON] ()

data MilestoneStateEvent = CloseMilestone | ActivateMilestone
                         deriving (Show)

instance ToJSON MilestoneStateEvent where
  toJSON CloseMilestone = "close"
  toJSON ActivateMilestone = "activate"

data EditMilestone
    = EditMilestone { emTitle :: Maybe Text
                    , emDescription :: Maybe Text
                    , emDueDate :: Maybe UTCTime
                    , emStartDate :: Maybe UTCTime
                    , emStateEvent :: Maybe MilestoneStateEvent
                    }
                    deriving (Show)

instance ToJSON EditMilestone where
    toJSON EditMilestone{..} = object
        $ catMaybes
        [ "title" .=? emTitle
        , "description" .=? emDescription
        , "due_date" .=? emDueDate
        , "start_date" .=? emStartDate
        , "state_event" .=? emStateEvent
        ]

editMilestone :: Logger
              -> AccessToken -> Maybe UserId
              -> ProjectId -> MilestoneId -> EditMilestone
              -> ClientM ()
editMilestone logger tok sudo prj mid em = do
    liftIO $ writeLog logger "EDIT-MILESTONE" $ show em
    client (Proxy :: Proxy EditMilestoneAPI) (Just tok) prj mid em sudo

----------------------------------------------------------------------
-- listMilestones
----------------------------------------------------------------------

type ListMilestonesAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "milestones"
    :> QueryParam "per_page" Int
    :> Get '[JSON] [Milestone]

data Milestone = Milestone Text MilestoneId

instance FromJSON Milestone where
    parseJSON = withObject "milestone" $ \o -> do
        Milestone <$> o .: "title" <*> o .: "id"

listMilestones :: AccessToken
               -> ProjectId -> ClientM [Milestone]
listMilestones tok prj = client (Proxy :: Proxy ListMilestonesAPI) (Just tok) prj (Just 100)


----------------------------------------------------------------------
-- createIssueLink
----------------------------------------------------------------------

type CreateIssueLinkAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "issue_iid" IssueIid :> "links"
    :> ReqBody '[JSON] CreateIssueLink
    :> SudoParam
    :> Post '[JSON] CreateIssueLinkResp

data CreateIssueLink
    = CreateIssueLink { cilProject :: ProjectId
                      , cilIssue :: IssueIid
                      , cilTargetProject :: ProjectId
                      , cilTargetIssue :: IssueIid
                      }
                      deriving (Show)

instance ToJSON CreateIssueLink where
    toJSON CreateIssueLink{..} = object
        [ "id" .= cilProject
        , "issue_iid" .= cilIssue
        , "target_project_id" .= cilTargetProject
        , "target_issue_iid" .= cilTargetIssue
        ]

data CreateIssueLinkResp = CreateIssueLinkResp IssueLinkId

instance FromJSON CreateIssueLinkResp where
    parseJSON = withObject "create issueLink response" $ \o -> do
        CreateIssueLinkResp <$> o .: "id"

createIssueLink :: Logger
                -> AccessToken -> Maybe UserId
                -> ProjectId -> IssueIid
                -> CreateIssueLink
                -> ClientM IssueLinkId
createIssueLink logger tok sudo prj iid cm = do
    liftIO $ writeLog logger "CREATE-ISSUE-LINK" $ show cm
    CreateIssueLinkResp mid <- client (Proxy :: Proxy CreateIssueLinkAPI) (Just tok) prj iid cm sudo
    return mid

----------------------------------------------------------------------
-- listIssueLinks
----------------------------------------------------------------------

type ListIssueLinksAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issue" :> Capture "iid" IssueIid :> "links"
    :> QueryParam "per_page" Int
    :> Get '[JSON] [IssueLink]

data IssueLink = IssueLink IssueLinkId ProjectId IssueIid

instance FromJSON IssueLink where
    parseJSON = withObject "issueLink" $ \o -> do
        IssueLink <$> o .: "issue_link_id"
                  <*> o .: "project_id"
                  <*> o .: "iid"

listIssueLinks :: AccessToken
               -> ProjectId
               -> IssueIid
               -> ClientM [IssueLink]
listIssueLinks tok prj iid = client (Proxy :: Proxy ListIssueLinksAPI) (Just tok) prj iid (Just 100)

----------------------------------------------------------------------
-- subscribeIssue
----------------------------------------------------------------------

type SubscribeIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "issue_iid" IssueIid :> "subscribe"
    :> ReqBody '[JSON] SubscribeIssue
    :> SudoParam
    :> Post '[JSON] SubscribeIssueResp

data SubscribeIssue
    = SubscribeIssue
        deriving (Show)

instance ToJSON SubscribeIssue where
    toJSON SubscribeIssue = object
        []

data SubscribeIssueResp = SubscribeIssueResp

instance FromJSON SubscribeIssueResp where
    parseJSON = withObject "subscribe response" $ \o -> do
        pure SubscribeIssueResp

subscribeIssue :: Logger
               -> AccessToken
               -> Maybe UserId
               -> ProjectId
               -> IssueIid
               -> ClientM ()
subscribeIssue logger tok sudo prj iid = do
    liftIO $ writeLog logger "SUBSCRIBE-ISSUE" $ show iid
    SubscribeIssueResp <- client (Proxy :: Proxy SubscribeIssueAPI) (Just tok) prj iid SubscribeIssue sudo
    return ()


----------------------------------------------------------------------
-- unsubscribeIssue
----------------------------------------------------------------------

type UnsubscribeIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "issue_iid" IssueIid :> "ununsubscribe"
    :> ReqBody '[JSON] UnsubscribeIssue
    :> SudoParam
    :> Post '[JSON] UnsubscribeIssueResp

data UnsubscribeIssue
    = UnsubscribeIssue
        deriving (Show)

instance ToJSON UnsubscribeIssue where
    toJSON UnsubscribeIssue = object
        []

data UnsubscribeIssueResp = UnsubscribeIssueResp

instance FromJSON UnsubscribeIssueResp where
    parseJSON = withObject "unsubscribe response" $ \o -> do
        pure UnsubscribeIssueResp

unsubscribeIssue :: Logger
               -> AccessToken
               -> Maybe UserId
               -> ProjectId
               -> IssueIid
               -> ClientM ()
unsubscribeIssue logger tok sudo prj iid = do
    liftIO $ writeLog logger "UNSUBSCRIBE-ISSUE" $ show iid
    UnsubscribeIssueResp <- client (Proxy :: Proxy UnsubscribeIssueAPI) (Just tok) prj iid UnsubscribeIssue sudo
    return ()
