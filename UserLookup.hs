{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

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
import Control.Monad (join)
import Text.Printf (printf)

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
      let email = fromMaybe ("trac+"<>username<>"@haskell.org") m_email

      runMaybeT $
        findUserBy email <|>
        findUserBy username <|>
        findUserBy username'

    findUserBy :: Text -> MaybeT ClientM UserId
    findUserBy nameOrEmail = do
      liftIO . writeLog logger "FIND-USER-BY" $ T.unpack nameOrEmail
      MaybeT $ fmap userId <$> findUserByUsername gitlabToken nameOrEmail

    catchToMaybe :: forall m a. (Monad m, MonadIO m, MonadCatch m) => m a -> m (Maybe a)
    catchToMaybe action =
      handleAll h $ (Just <$> action) `catch` h
      where
        h :: SomeException -> m (Maybe a)
        h err = do
          liftIO $ writeLog logger "ERROR" (displayException err)
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
      let cuEmail = case m_email of
                      Nothing -> "trac+"<>username<>"@haskell.org"
                      Just email -> email
          cuName = username
          cuUsername = case M.lookup username knownUsers of
                         Just u -> u
                         Nothing -> fixUsername username
          cuSkipConfirmation = True
      liftIO $ writeLog logger "CREATE USER" $ show username <> " (" <> show cuEmail <> ")"
      uid <- createUser gitlabToken CreateUser {..}
      return uid

flipPair :: (a, b) -> (b, a)
flipPair (a, b) = (b, a)

