{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module UserLookup
  ( Username, Email
  , mkUserIdOracle
  , findKnownUser
  , UserIdOracle(..)
  ) where

import Data.Char
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Concurrent
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad.Catch hiding (bracket)
import Control.Monad.Except
import Control.Monad (join)
import Text.Printf (printf)
import Network.HTTP.Types
import qualified Data.Aeson as JSON
import qualified Data.Map.Strict as Map
import Text.Email.Validate (canonicalizeEmail)
import qualified Data.ByteString.UTF8 as UTF8

import Servant.Client
import Database.PostgreSQL.Simple

import Logging (Logger, writeLog)
import GitLab.Common
import GitLab.Users
import Trac.Db as Trac
import Settings

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
    , "Richard Eisenberg <eir@cis.upenn.edu>" .= "rae"
    , "p.capriotti@gmail.com" .= "pcapriotti"
    , "Thomas Miedema <thomasmiedema@gmail.com>" .= "thomie"
    , "pho@cielonegro.org" .= "pho_at_cielonegro.org"
    , "Favonia" .= "favonia"
    , "andygill" .= "AndyGill"
      -- Seems to have multiple accounts with same email
    , "Stefan O'Rear <stefanor@cox.net>" .= "sorear"
    , "stefanor@cox.net <Stefan O'Rear>" .= "sorear"
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
    fixChars _ = '_'

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

extractEmail :: Text -> Maybe Text
extractEmail =
  fmap (T.pack . UTF8.toString) . canonicalizeEmail . UTF8.fromString . T.unpack

usernameToEmail :: Text -> Text
usernameToEmail username
  | Just email <- extractEmail username
  = email
  | otherwise
  = "trac+" <> sanitizeUsername username <> "@haskell.org"

mkUserIdOracle :: Logger -> Connection -> ClientEnv -> IO UserIdOracle
mkUserIdOracle logger conn clientEnv =
  return UserIdOracle {..}
  where
    fixUsername :: Text -> Text
    fixUsername username
        | username == ""
        = "unknown"
        | Just u <- M.lookup username knownUsers
        = u
        | otherwise
        = "trac-" <> sanitizeUsername username

    findUser :: Text -> ClientM (Maybe UserId)
    findUser username = do
      liftIO $ writeLog logger "FIND-USER" (T.unpack username)
      let username' = fixUsername username
      m_email <- liftIO $ getUserAttribute conn Trac.Email username
      let email = fromMaybe (usernameToEmail username) m_email

      runMaybeT $
        findUserBy email <|>
        findUserBy username <|>
        findUserBy username'

    findUserBy :: Text -> MaybeT ClientM UserId
    findUserBy nameOrEmail = do
      liftIO . writeLog logger "FIND-USER-BY" $ T.unpack nameOrEmail
      MaybeT $ fmap userId <$> findUserByUsername gitlabToken nameOrEmail

    catchToMaybe :: forall m a. (Monad m, MonadIO m, MonadCatch m, MonadError ServantError m) => m a -> m (Maybe a)
    catchToMaybe action =
      handleAll h . flip catchError hServant $ (Just <$> action)
      where
        hServant :: ServantError -> m (Maybe a)
        hServant (FailureResponse
                   Response
                     { responseStatusCode = Status { statusCode = 409 }
                     , responseBody = body
                     }
                 )
          | Just bodyMap <- JSON.decode body
          , Just msg <- Map.lookup ("message" :: Text) bodyMap
          = do
              liftIO $ writeLog logger "SERVANT" msg
              return Nothing
        hServant err
          = do
              liftIO $ writeLog logger "SERVANT-ERROR" (displayException err)
              return Nothing

        h :: SomeException -> m (Maybe a)
        h err = do
          liftIO $ writeLog logger "IO-ERROR" (displayException err)
          return Nothing

    findOrCreateUser :: Text -> ClientM UserId
    findOrCreateUser username = do
      liftIO $ writeLog logger "FIND-OR-CREATE-USER" (T.unpack username)
      uid <- findUser username >>= \case
        Nothing -> doCreateUser username
        Just uid -> return uid
      catchToMaybe $ addProjectMember gitlabToken project uid Reporter
      return uid

    doCreateUser :: Text -> ClientM UserId
    doCreateUser username = do
      liftIO $ writeLog logger "CREATE-USER" (T.unpack username)
      m_email <- liftIO $ getUserAttribute conn Trac.Email username
      let cuEmail = fromMaybe (usernameToEmail username) m_email
          cuName = username
          cuUsername = fixUsername username
          cuSkipConfirmation = True
      liftIO $ writeLog logger "CREATE USER" $ show username <> " (" <> show cuEmail <> ")"
      uid <- createUser gitlabToken CreateUser {..}
      return uid

flipPair :: (a, b) -> (b, a)
flipPair (a, b) = (b, a)

