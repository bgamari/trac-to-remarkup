{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module WikiImport (buildWiki) where

import Control.Monad
import Control.Monad.Catch hiding (bracket)
import Control.Monad.Trans.Except
import Control.Exception.Lifted (bracket)
import Data.Char
import Data.Maybe
import Data.Time
import Control.Concurrent
import Text.Printf
import System.FilePath
import System.Directory
import System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M
import Control.Monad.IO.Class

import Servant.Client
import Network.HTTP.Client.Internal as HTTP (HttpException (..), HttpExceptionContent (..), Response (..))
import Network.HTTP.Types.Status

import Database.PostgreSQL.Simple

import Utils
import Trac.Db as Trac
import Trac.Db.Types as Trac
import qualified Trac.Scraper as Scraper
import Trac.Convert (tracWikiBaseNameToGitlab)
import GitLab.Common
import GitLab.Users
import Logging
import Git
import ImportState
import UserLookup
import Settings

buildWiki :: Logger -> Bool -> Bool -> CommentCacheVar -> Connection -> ClientM ()
buildWiki logger fast keepGit commentCache conn = do
  bracket
    (liftIO $ do
      Git.clone logger wikiRemoteUrl
    )
    (\wc -> liftIO $ do
        if keepGit
          then do
            writeLog logger "GIT" $ printf "Keeping git working copy at %s" wc
          else
            Git.deleteWorkingCopy logger wc >>= writeLog logger "GIT"
    )
    $ \wc -> do
      liftIO $ writeLog logger "WIKI BUILD DIR" wc
      pages <- liftIO $ (if fast then getWikiPagesFast else getWikiPages) conn
      anchorMapVar <- liftIO $ newMVar mempty
      forM_ pages (buildPage anchorMapVar wc)
      liftIO $ do
        (git_ logger wc "pull" [] >>= writeLog logger "GIT")
              `catch`
              (\(err :: GitException) -> do
                case Git.gitExcExitCode err of
                  1 ->
                    -- upstream has no master branch yet - we'll ignore this and
                    -- just push.
                    pure ()

                  _ ->
                    -- something else went wrong, let's report it
                    throwM err
              )
        git_ logger wc "push" ["origin", "master"] >>= writeLog logger "GIT"
  where
    getCommentId :: Int -> Int -> IO CommentRef
    getCommentId t c = fromMaybe MissingCommentRef . (>>= nthMay (c - 1)) . M.lookup t <$> readMVar commentCache

    buildPage :: MVar Scraper.AnchorMap -> Git.WorkingCopy -> WikiPage -> ClientM ()
    buildPage anchorMapVar wc WikiPage{..} =
      Logging.withContext (liftLogger logger) (T.unpack wpName) $ do
        liftIO $ do
          writeLog logger "INFO" $ (show wpTime) ++ " " ++ (T.unpack wpName) ++ " v" ++ (show wpVersion)
          hFlush stdout
        let baseFilename = wc </> (tracWikiBaseNameToGitlab . T.unpack $ wpName)
            filename = baseFilename <.> "md"
            tracFilename = baseFilename <.> "trac"
        liftIO $ do
          let url =
                printf
                  "https://ghc.haskell.org/trac/ghc/wiki/%s?version=%i"
                  wpName
                  wpVersion

          anchorMap <- liftIO $ takeMVar anchorMapVar
          res <- runExceptT $ do
            Just src <- dealWithHttpError logger 0 . withTimeout 100000 $ Scraper.httpGet logger url
            liftException ScraperError $ liftIO
              $ Scraper.scrape
                            anchorMap
                            logger
                            (showBaseUrl gitlabBaseUrl)
                            gitlabOrganisation
                            gitlabProjectName
                            Nothing
                            (Just filename)
                            getCommentId
                            src

          liftIO $ putMVar anchorMapVar $ either (const anchorMap) snd res

          writeLog logger "INFO" $ printf "Create file %s in directory %s\n"
            (show filename)
            (show $ takeDirectory filename)
          hFlush stdout
          createDirectoryIfMissing True (takeDirectory filename)
          T.writeFile tracFilename wpBody

          case res of
            Right (body, _) ->
              writeFile filename body
            Left err -> do
              writeLog logger "CONVERSION-ERROR" (showScrapeError err)
              writeLog logger "INFO" (T.unpack wpBody)
              writeFile filename $
                printf
                  "CONVERSION ERROR\n\nError: %s\n\nOriginal source:\n\n```trac\n%s\n```\n"
                  (showScrapeError err)
                  wpBody
        muser <- do
          (pure $ findKnownUser wpAuthor)
            |$| (listToMaybe <$> findUsersByUsername gitlabToken wpAuthor)
            |$| (listToMaybe <$> findUsersByEmail gitlabToken wpAuthor)
            |$| do
              -- no user found, let's fake one
              liftIO $ writeLog logger "AUTHOR-MISMATCH" (T.unpack wpAuthor)
              pure . Just $ User (UserId 0) wpAuthor wpAuthor Nothing
        let User{..} = fromMaybe (User (UserId 0) "notfound" "notfound" Nothing) muser
        let juserEmail = fromMaybe ("trac-" ++ T.unpack userUsername ++ "@haskell.org") (T.unpack <$> userEmail)
        let commitAuthor = printf "%s <%s>" userName juserEmail
        let commitDate = formatTime defaultTimeLocale (iso8601DateFormat Nothing) wpTime
        let msg = fromMaybe ("Edit " ++ T.unpack wpName) (T.unpack <$> wpComment >>= unlessNull)
        liftIO $ printGitError logger $ do
          status <- git_ logger wc "status" ["--porcelain"]
          writeLog logger "GIT-STATUS" (show status)
          unless (all isSpace status) $ do
            git_ logger wc "add" ["."] >>= writeLog logger "GIT"
            git_ logger wc "commit" ["-m", msg, "--author=" ++ commitAuthor, "--date=" ++ commitDate] >>= writeLog logger "GIT"

data ScrapeError = ScraperError Scraper.ConversionError
                 | HttpError HttpException
                 deriving (Show)

showScrapeError :: ScrapeError -> String
showScrapeError e = show e

printGitError :: Logger -> IO () -> IO ()
printGitError logger action = action `catch` h
  where
    h :: GitException -> IO ()
    h err = do
      writeLog logger "GIT-EXCEPTION" $ displayException err

liftException :: (MonadCatch m, Exception exc)
              => (exc -> err) -> ExceptT err m a -> ExceptT err m a
liftException toError action = action `catch` (throwE . toError)

dealWithHttpError :: forall r. Logger -> Int -> IO r -> ExceptT ScrapeError IO r
dealWithHttpError logger n action = liftIO action `catch` h
  where
    h :: HttpException -> ExceptT ScrapeError IO r
    h e@(HttpExceptionRequest
          _
          (StatusCodeException
            HTTP.Response { responseStatus = Status { statusCode = 404 } }
            _
          )
        ) = do
      liftIO $ writeLog logger "HTTP-ERROR" $ displayException e
      throwE $ HttpError e
    h e@(HttpExceptionRequest _ ConnectionFailure {}) =
      retry e
    h e@(HttpExceptionRequest _ ResponseTimeout {}) =
      retry e

    h e = do
      liftIO $ writeLog logger "HTTP-ERROR" $ displayException e
      throwE $ HttpError e

    retry :: HttpException -> ExceptT ScrapeError IO r
    retry e =
      if n >= 7 then do
        liftIO $ writeLog logger "HTTP-ERROR" $ "Max number of retries exceeded, skipping."
        throwE $ HttpError e
      else do
        let delaySecs = 2 ^ n
        liftIO $ writeLog logger "HTTP-ERROR" $ "Network error, retrying in " ++ show delaySecs ++ " seconds"
        liftIO $ threadDelay (delaySecs * 1000000)
        dealWithHttpError logger (succ n) action

(|$|) :: (Monad m) => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
a |$| b = do
  x <- a
  if isJust x then pure x else b

unlessNull :: (Eq a, Monoid a) => a -> Maybe a
unlessNull xs
  | xs == mempty = Nothing
  | otherwise = Just xs

nthMay :: Int -> [a] -> Maybe a
nthMay n = listToMaybe . drop n

