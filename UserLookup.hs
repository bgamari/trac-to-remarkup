{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

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
mkUserIdOracle logger conn clientEnv = do
    cacheVar <- newMVar mempty
    let runIt :: Bool -> Username -> StateT UserIdCache IO (Maybe UserId)
        runIt create username = StateT $ \cache -> do
            res <- runClientM (runStateT (runMaybeT $ getUserId create $ T.strip username) cache) clientEnv
            case res of
              Left exc -> do
                writeLog logger "RESOLVE USER FAILED" $ show username ++ ": " ++ show exc
                throwM exc
              Right res' -> do
                writeLog logger "RESOLVE USER" $ show username ++ " -> " ++ show (fst res')
                return res'

    let oracle :: Bool -> Username -> ClientM (Maybe UserId)
        oracle create = liftIO
                        . withMVarState cacheVar
                        . runIt create
    return $ UserIdOracle { findOrCreateUser = \user -> fromMaybe (error "couldn't resolve user id") <$> oracle True user
                          , findUser = oracle False
                          }
  where
    tee :: (Show a, Monad m, MonadIO m) => String -> m a -> m a
    tee msg a = do
      x <- a
      liftIO . writeLog logger "INFO" $ msg ++ show x
      return x

    getUserId :: Bool -> Username -> UserLookupM UserId
    getUserId create username =
            tee "tryEmptyUserName" tryEmptyUserName
        <|> tee "tryCache" tryCache
        <|> cacheIt (tee "tryOverride" tryOverride)
        <|> cacheIt (tee "tryLookupName - " tryLookupName)
        <|> cacheIt (tee "tryLookupTracName - " tryLookupTracName)
        <|> cacheIt (tee "tryLookupEmail - " tryLookupEmail)
        <|> cacheIt (tee "tryCreate - " tryCreate)
      where
        username'
          | Just u <- M.lookup username knownUsers = u
          | otherwise = "trac-"<>sanitizeUsername username

        tryEmptyUserName :: UserLookupM UserId
        tryEmptyUserName
          | username == "" = getUserId create "unknown"
          | otherwise      = empty

        tryOverride
          | Just u <- username `M.lookup` knownUsers = getUserId create u
          | otherwise      = empty

        tryCache :: UserLookupM UserId
        tryCache = do
            cache <- lift get
            MaybeT $ pure $ M.lookup username cache

        tryLookupName :: UserLookupM UserId
        tryLookupName = tryLookupName' username

        tryLookupTracName :: UserLookupM UserId
        tryLookupTracName = tryLookupName' username'

        tryLookupName' :: Username -> UserLookupM UserId
        tryLookupName' name = do
            liftIO . writeLog logger "FIND USER BY NAME" $ T.unpack name
            fmap userId $ MaybeT $ lift $ findUserByUsername gitlabToken name

        tryLookupEmail :: UserLookupM UserId
        tryLookupEmail = do
            m_email <- liftIO $ getUserAttribute conn Trac.Email username
            let cuEmail = fromMaybe ("trac+"<>username<>"@haskell.org") m_email
            liftIO . writeLog logger "FIND USER BY EMAIL" $ T.unpack cuEmail
            fmap userId $ MaybeT $ lift $ tee "user by email" $ findUserByEmail gitlabToken cuEmail

        tryCreate :: UserLookupM UserId
        tryCreate
          | not create = empty
          | otherwise = do
            m_email <- liftIO $ getUserAttribute conn Trac.Email username
            uidMay <- lift $ do
              let cuEmail = case m_email of
                              Nothing -> "trac+"<>username<>"@haskell.org"
                              Just email -> email
                  cuName = username
                  cuUsername = case M.lookup username knownUsers of
                                 Just u -> u
                                 Nothing -> username'
                  cuSkipConfirmation = True
              liftIO $ writeLog logger "CREATE USER" $ show username <> " (" <> show cuEmail <> ")"
              lift $ createUserMaybe gitlabToken CreateUser {..}
            case uidMay of
              Nothing ->
                fail "User already exists"
              Just uid -> do
                liftIO $ writeLog logger "USER CREATED" $ show username <> " as " <> show uid
                lift . lift $ addProjectMember gitlabToken project uid Reporter
                return uid

        cacheIt :: UserLookupM UserId -> UserLookupM UserId
        cacheIt action = do
            uid <- action
            lift $ modify' $ M.insert username uid
            return uid

flipPair :: (a, b) -> (b, a)
flipPair (a, b) = (b, a)

