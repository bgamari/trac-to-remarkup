{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
module Logging
where

import System.IO
import Control.Concurrent.Async.Lifted
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Maybe
import Control.Exception.Lifted
import Data.List
import Control.Monad.Reader

data LoggerM (m :: * -> *)
  = Logger
      { writeLogRaw :: [String] -> m ()
      , getContext :: m [String]
      , pushContext :: String -> m ()
      , popContext :: m (Maybe String)
      }

type Logger = LoggerM IO

class Monad m => MonadLogger m where
  getLogger :: m (LoggerM m)

instance (MonadBase b m) => MonadLogger (ReaderT (LoggerM b) m) where
  getLogger = liftLogger <$> ask

liftLogger :: (MonadBase b m) => LoggerM b -> LoggerM m
liftLogger (Logger w g s p)
  = Logger
      (liftBase . w)
      (liftBase g)
      (liftBase . s)
      (liftBase p)

withContext :: MonadBaseControl IO m => LoggerM m -> String -> m a -> m a
withContext logger c =
  bracket_
    (pushContext logger c)
    (void $ popContext logger)

writeLog :: MonadBaseControl IO m => LoggerM m -> String -> String -> m ()
writeLog logger prefix' msg = do
  let prefix = if null prefix' then "*" else prefix'
  ctx <- getContext logger
  let prepend = if null ctx then
                  prefix ++ " [-] "
                else
                  prefix ++ " [" ++ intercalate ":" (reverse ctx) ++ "] "
      rawMsg = map (prepend ++) . lines $ msg
  writeLogRaw logger rawMsg

writeLogM :: (MonadLogger m, MonadBaseControl IO m)
          => String -> String -> m ()
writeLogM prefix msg = do
  logger <- getLogger
  writeLog logger prefix msg

pushContextM :: (MonadLogger m)
             => String -> m ()
pushContextM ctx = do
  logger <- getLogger
  pushContext logger ctx

popContextM :: (MonadLogger m)
            => m (Maybe String)
popContextM = do
  logger <- getLogger
  popContext logger

withContextM :: (MonadLogger m, MonadBaseControl IO m)
             => String -> m a -> m a
withContextM c =
  bracket_
    (pushContextM c)
    (void popContextM)

makeStdoutLogger :: forall m. (MonadBase IO m, MonadBaseControl IO m)
                 => m (LoggerM m)
makeStdoutLogger = do
  logVar <- liftBase $ newTMVarIO ()
  contextVar <- liftBase $ newTVarIO []
  let writeLogRaw msg = liftBase $
        bracket_
          (atomically $ takeTMVar logVar)
          (atomically $ putTMVar logVar ())
          (mapM_ putStrLn msg >> hFlush stdout)
      getContext = 
        liftBase $ atomically $ readTVar contextVar
      pushContext c = liftBase $ atomically $ do
        cs <- readTVar contextVar
        writeTVar contextVar (c:cs)
      popContext = liftBase $ atomically $ do
        cs <- readTVar contextVar
        writeTVar contextVar (drop 1 cs)
        pure $ listToMaybe cs
  return (Logger {..} :: LoggerM m)

-- makeStdoutLogger = do
--   logChan <- liftBase newTChanIO
--   contextVar <- liftBase $ newTVarIO []
--   let writeLogRaw msg = 
--         liftBase $ atomically $ writeTChan logChan msg
--       getContext = 
--         liftBase $ atomically $ readTVar contextVar
--       pushContext c = liftBase $ atomically $ do
--         cs <- readTVar contextVar
--         writeTVar contextVar (c:cs)
--       popContext = liftBase $ atomically $ do
--         cs <- readTVar contextVar
--         writeTVar contextVar (drop 1 cs)
--         pure $ listToMaybe cs
--   logWorker <- liftBase . async . forever $ do
--     atomically (readTChan logChan) >>= liftBase . mapM putStrLn
--   link logWorker
--   return (Logger {..} :: LoggerM m)
