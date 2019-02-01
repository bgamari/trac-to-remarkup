module Utils 
  ( withTimeout 
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Concurrent
import Control.Concurrent.Async.Lifted (race)

withTimeout :: (MonadIO m, MonadBaseControl IO m) => Int -> m a -> m (Maybe a)
withTimeout delayMS action =
  either Just (const Nothing) <$> race action (liftIO reaper)
  where
    reaper :: IO ()
    reaper = do
      threadDelay (delayMS * 1000)

