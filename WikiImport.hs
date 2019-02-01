{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module WikiImport where

import Control.Monad
import Control.Monad.Catch hiding (bracket)
import Control.Exception.Lifted (bracket)
import Data.Char
import Data.Maybe
import Data.Time
import Data.Void
import Control.Concurrent
import Text.Printf
import System.FilePath
import System.Directory
import System.IO

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M
import Control.Monad.IO.Class

import Servant.Client
import Network.HTTP.Client.Internal as HTTP (HttpException (..), HttpExceptionContent (..), Response (..))
import Network.HTTP.Types.Status

import Database.PostgreSQL.Simple
import Text.Megaparsec.Error (ParseError, parseErrorPretty)

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

          mbody <- dealWithHttpError logger 0 .  withTimeout 100000 $ do
                      src <- Scraper.httpGet logger url
                      anchorMap <- takeMVar anchorMapVar
                      (dst, manchorMap') <-
                        printScraperError logger $
                          Scraper.scrape
                            anchorMap
                            logger
                            (showBaseUrl gitlabBaseUrl)
                            gitlabOrganisation
                            gitlabProjectName
                            Nothing
                            (Just filename)
                            getCommentId
                            src
                      let anchorMap' = fromMaybe anchorMap manchorMap'
                      putMVar anchorMapVar anchorMap'
                      return dst
          writeLog logger "INFO" $ printf "Create file %s in directory %s\n"
            (show filename)
            (show $ takeDirectory filename)
          hFlush stdout
          createDirectoryIfMissing True (takeDirectory filename)
          T.writeFile tracFilename wpBody
          case mbody of
            Just body ->
              writeFile filename body
            Nothing -> do
              writeLog logger "CONVERSION-ERROR" ""
              writeLog logger "INFO" (T.unpack wpBody)
              writeFile filename $
                printf
                  "CONVERSION ERROR\n\nOriginal source:\n\n```trac\n%s\n```\n"
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

printGitError :: Logger -> IO () -> IO ()
printGitError logger action = action `catch` h
  where
    h :: GitException -> IO ()
    h err = do
      writeLog logger "GIT-EXCEPTION" $ displayException err

printScraperError :: forall a. Logger -> IO (String, a) -> IO (String, Maybe a)
printScraperError logger action = action' `catch` h
  where
    action' :: IO (String, Maybe a)
    action' = do
      (x, a) <- action
      return (x, Just a)

    h :: Scraper.ConversionError -> IO (String, Maybe a)
    h err@(Scraper.ConversionError msg) = do
      writeLog logger "SCRAPER-EXCEPTION" $ displayException err
      return
        ( printf "Conversion error:\n\n```\n%s\n```\n\n" msg
        , Nothing
        )

printParseError :: Logger -> Text -> IO String -> IO String
printParseError logger body action = action `catch` h
  where
    h :: ParseError Char Void -> IO String
    h err = do
      writeLog logger "PARSER-ERROR" $ parseErrorPretty err
      return $ printf "Parser error:\n\n```\n%s\n```\n\nOriginal source:\n\n```trac\n%s\n```\n"
        (parseErrorPretty err) body

dealWithHttpError :: Logger -> Int -> IO (Maybe String) -> IO (Maybe String)
dealWithHttpError logger n action = action `catch` h
  where
    h :: HttpException -> IO (Maybe String)
    h e@(HttpExceptionRequest
          _
          (StatusCodeException
            HTTP.Response { responseStatus = Status { statusCode = 404 } }
            _
          )
        ) = do
      writeLog logger "HTTP-ERROR" $ displayException e
      return Nothing
    h (HttpExceptionRequest _ ConnectionFailure {}) =
      retry
    h (HttpExceptionRequest _ ResponseTimeout {}) =
      retry

    h e = do
      writeLog logger "HTTP-ERROR" $ displayException e
      return Nothing

    retry =
      if n >= 7 then do
        writeLog logger "HTTP-ERROR" $ "Max number of retries exceeded, skipping."
        return Nothing
      else do
        let delaySecs = 2 ^ n
        writeLog logger "HTTP-ERROR" $ "Network error, retrying in " ++ show delaySecs ++ " seconds"
        threadDelay (delaySecs * 1000000)
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

