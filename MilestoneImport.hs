{-# LANGUAGE RecordWildCards #-}

module MilestoneImport 
  ( MilestoneMap
  , makeMilestones
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import Control.Monad.Catch hiding (bracket, onError)
import qualified Data.Map.Strict as M
import Data.Text (Text)

import Servant.Client
import Database.PostgreSQL.Simple

import GitLab.Common
import GitLab.Tickets
import Trac.Db as Trac
import Logging
import Settings

type MilestoneMap = M.Map Text MilestoneId

makeMilestones :: Logger -> Bool -> Connection -> ClientM MilestoneMap
makeMilestones logger actuallyMakeThem conn = do
    when actuallyMakeThem $ do
      milestones <- liftIO $ Trac.getMilestones conn
      mconcat <$> mapM createMilestone' milestones
      pure ()
    foldMap (\(GitLab.Tickets.Milestone a b) -> M.singleton a b)
        <$> listMilestones gitlabToken project
  where
    createMilestone' :: Trac.Milestone -> ClientM MilestoneMap
    createMilestone' Trac.Milestone{..} =
      handleAll onError $ flip catchError onError $ do
        mid <- createMilestone logger gitlabToken Nothing project
            $ CreateMilestone { cmTitle = mName
                              , cmDescription = mDescription
                              , cmDueDate = mDue
                              , cmStartDate = Nothing
                              }
        return $ M.singleton mName mid

    onError :: (MonadIO m, Exception a) => a -> m MilestoneMap
    onError err = do
        liftIO $ writeLog logger "FAILED TO CREATE MILESTONE" $ displayException err
        return M.empty

