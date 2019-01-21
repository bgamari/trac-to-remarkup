{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Logging
where

import System.IO
import Control.Concurrent.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Maybe
import Control.Exception.Lifted
import Data.List

data LoggerM (m :: * -> *)
  = Logger
      { writeLogRaw :: String -> m ()
      , getContext :: m [String]
      , pushContext :: String -> m ()
      , popContext :: m (Maybe String)
      }

type Logger = LoggerM IO

liftLogger :: (MonadBase b m, MonadBaseControl b m) => LoggerM b -> LoggerM m
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
                  prefix ++ " [" ++ intercalate ":" ctx ++ "] "
      rawMsg = unlines . map (prepend ++) . lines $ msg
  writeLogRaw logger rawMsg

makeStdoutLogger :: forall m. (MonadBase IO m, MonadBaseControl IO m)
                 => m (LoggerM m)
makeStdoutLogger = do
  logChan <- newChan
  contextVar <- newMVar []
  let writeLogRaw msg = writeChan logChan msg
      getContext =
        readMVar contextVar
      pushContext c = do
        cs <- takeMVar contextVar
        putMVar contextVar (c:cs)
      popContext = do
        cs <- takeMVar contextVar
        putMVar contextVar (drop 1 cs)
        pure $ listToMaybe cs
  fork . forever $ do
    readChan logChan >>= liftBase . putStrLn
  return (Logger {..} :: LoggerM m)
