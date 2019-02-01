{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ImportState where

import Data.Foldable
import System.Directory
import Control.Concurrent
import System.IO

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Logging
import Trac.Db.Types as Trac

openStateFile :: forall a. (Ord a, Read a, Show a)
              => FilePath -> IO (S.Set a, a -> IO ())
openStateFile stateFile = do
    stateFileExists <- doesFileExist stateFile
    !finished <-
        if stateFileExists
        then S.fromList . map read . lines <$> readFile stateFile
        else return mempty

    stateFile <- openFile stateFile AppendMode
    hSetBuffering stateFile LineBuffering
    let finishItem :: a -> IO ()
        finishItem = hPutStrLn stateFile . show
    return (finished, finishItem)

openCommentCacheFile :: Logger -> FilePath -> IO (CommentCacheVar, StoreComment)
openCommentCacheFile logger stateFile = do
    stateFileExists <- doesFileExist stateFile
    !cacheEntries <-
        if stateFileExists
        then map readEntry . lines <$> readFile stateFile
        else return []

    let storeItem :: Int -> CommentRef -> IO ()
        storeItem t mn = do
          writeLog logger "TRACE" $ "Store comment ref: " ++ show t ++ " / " ++ show mn
          stateFile <- openFile stateFile AppendMode
          hSetBuffering stateFile LineBuffering
          hPutStrLn stateFile $! showEntry (t, mn)
          hClose stateFile

    cache <- newMVar $! foldr'
              (\(t,n) -> M.insertWith (flip (++)) t [n])
              mempty
              cacheEntries
    return (cache, storeItem)

    where
      readEntry :: String -> (Int, CommentRef)
      readEntry str =
        case words str of
          ('(':_):_ ->
            -- support legacy state file
            let (rt, rn) = read str in (rt, NoteRef rn)
          [t] ->
            (read t, MissingCommentRef)
          [t,"git",h] ->
            (read t, CommitRef h Nothing)
          [t, "git", h, repo] ->
            (read t, CommitRef h (Just repo))
          [t,n] ->
            (read t, NoteRef (read n))

      showEntry :: (Int, CommentRef) -> String
      showEntry (t, MissingCommentRef) = show t
      showEntry (t, NoteRef n) = unwords . map show $ [t, n]
      showEntry (t, CommitRef h Nothing) = unwords [show t, "git", h]
      showEntry (t, CommitRef h (Just repo)) = unwords [show t, "git", h, repo]

attachmentStateFile :: FilePath
attachmentStateFile = "attachments.state"

ticketStateFile :: FilePath
ticketStateFile = "tickets.state"

mutationStateFile :: FilePath
mutationStateFile = "mutations.state"

commentCacheFile :: FilePath
commentCacheFile = "comments.state"

type CommentCacheVar = MVar CommentCache

type CommentCache = (M.Map Int [CommentRef])

type StoreComment = Int -> CommentRef -> IO ()

