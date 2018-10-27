{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module GitLab.Users where

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

----------------------------------------------------------------------
-- createUser
----------------------------------------------------------------------

data CreateUser
    = CreateUser { cuEmail       :: Text
                 , cuUsername    :: Text
                 , cuName        :: Text
                 , cuSkipConfirmation :: Bool
                 }
    deriving (Show)

instance ToJSON CreateUser where
    toJSON CreateUser{..} = object
        [ "email" .= cuEmail
        , "username" .=  cuUsername
        , "name" .=  cuName
        , "reset_password" .= True
        , "skip_confirmation" .= cuSkipConfirmation
        ]

data User = User { userId       :: UserId
                 , userUsername :: Text
                 , userName     :: Text
                 , userEmail    :: Maybe Text
                 }
          deriving (Show)

instance FromJSON User where
    parseJSON = withObject "user" $ \o -> do
        User <$> o .: "id"
             <*> o .: "username"
             <*> o .: "name"
             <*> o .:? "email"

type CreateUserAPI =
    GitLabRoot :> "users"
    :> ReqBody '[JSON] CreateUser
    :> Post '[JSON] User

createUser :: AccessToken -> CreateUser -> ClientM UserId
createUser tok cu = do
    user <- client (Proxy :: Proxy CreateUserAPI) (Just tok) cu
    return $ userId user


----------------------------------------------------------------------
-- findUserByUsername
----------------------------------------------------------------------

type FindUserByUsernameAPI =
    GitLabRoot :> "users"
    :> QueryParam "username" Text
    :> Get '[JSON] [User]

findUserByUsername :: AccessToken -> Text -> ClientM (Maybe User)
findUserByUsername tok username = do
    res <- client (Proxy :: Proxy FindUserByUsernameAPI) (Just tok) (Just username)
    return $ case res of
               [] -> Nothing
               [user] -> Just user
               _ -> error $ "Multiple users with id "<>show username


----------------------------------------------------------------------
-- addProjectMember
----------------------------------------------------------------------

type AddProjectMemberAPI =
    GitLabRoot :> "projects"
    :> Capture "id" ProjectId :> "members"
    :> ReqBody '[JSON] AddProjectMember
    :> Post '[JSON] Object

data AccessLevel = Guest | Reporter | Developer | Maintainer | Owner

instance ToJSON AccessLevel where
    toJSON l = toJSON $ case l of
                          Guest      -> 10 :: Int
                          Reporter   -> 20
                          Developer  -> 30
                          Maintainer -> 40
                          Owner      -> 50

data AddProjectMember = AddProjectMember UserId AccessLevel

instance ToJSON AddProjectMember where
    toJSON (AddProjectMember uid access) = object
        [ "user_id" .= uid
        , "access_level" .= access
        ]

addProjectMember :: AccessToken -> ProjectId
                 -> UserId -> AccessLevel -> ClientM ()
addProjectMember tok prj uid access = do
    client (Proxy :: Proxy AddProjectMemberAPI) (Just tok) prj
        $ AddProjectMember uid access
    return ()
