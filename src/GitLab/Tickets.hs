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

----------------------------------------------------------------------
-- getIssue
----------------------------------------------------------------------

type GetIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid
    :> Post '[JSON] IssueResp

data IssueResp
    = IssueResp { irProjectId :: ProjectId
                , irIid :: IssueIid
                }
    deriving (Show)

instance FromJSON IssueResp where
    parseJSON = withObject "issue response" $ \o ->
      IssueResp <$> o .: "project_id"
                <*> o .: "iid"

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
                  }

instance ToJSON CreateIssue where
    toJSON CreateIssue{..} = object
        [ "iid" .= ciIid
        , "title" .= ciTitle
        , "labels" .= ciLabels
        , "created_at" .= ciCreatedAt
        , "description" .= ciDescription
        , "milestone_id" .= ciMilestoneId
        , "weight" .= ciWeight
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
        ]

type EditIssueAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "issues"
    :> Capture "iid" IssueIid
    :> ReqBody '[JSON] EditIssue
    :> SudoParam
    :> Put '[JSON] IssueResp

nullEditIssue :: EditIssue -> Bool
nullEditIssue (EditIssue a b c d e _ g) =
    isNothing a && isNothing b && isNothing c &&
    isNothing d && isNothing e && isNothing g
    -- N.B. Ignore update time

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

createMilestone :: AccessToken -> Maybe UserId
                -> ProjectId -> CreateMilestone
                -> ClientM MilestoneId
createMilestone tok sudo prj cm = do
    liftIO $ putStrLn $ "Create milestone: " ++ show cm
    CreateMilestoneResp mid <- client (Proxy :: Proxy CreateMilestoneAPI) (Just tok) prj cm sudo
    return mid

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
