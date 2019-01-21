{-#LANGUAGE RecordWildCards #-}
module Git
where

import System.Exit
import System.Process
import System.IO
import System.IO.Temp
import Control.Exception
import Text.Printf (printf)
import Logging

type WorkingCopy = FilePath

type Remote = String

data GitException =
  GitException
    { gitExcExitCode :: Int
    , gitExcStdout :: String
    , gitExcStderr :: String
    }
    deriving (Show)

instance Exception GitException where
  displayException GitException{..} =
    printf
      "git exception: git exited with exit code %d\n%s\n%s"
      gitExcExitCode
      gitExcStdout
      gitExcStderr

-- | Make a raw call to git
gitRaw :: Logger -> [String] -> String -> IO String
gitRaw logger args inp = do
  writeLog logger "GIT" $ printf "%s\n" (unwords args)
  hFlush stdout
  (errno, out, err) <- readProcessWithExitCode "git" args inp
  case errno of
    ExitSuccess -> return out
    ExitFailure i -> throw $ GitException i out err

-- | Clone a git repo into a temporary directory
clone :: Logger -> Remote -> IO WorkingCopy
clone logger remote = do
  tmpdir <- getCanonicalTemporaryDirectory
  dirname <- createTempDirectory tmpdir "git"
  gitRaw logger ["clone", remote, dirname] ""
  return dirname

-- | Run a git subcommand in the given working copy
git :: Logger -> WorkingCopy -> String -> [String] -> String -> IO String
git logger w cmd args inp =
  gitRaw logger (["-C", w] ++ (cmd:args)) inp

git_ :: Logger -> WorkingCopy -> String -> [String] -> IO String
git_ logger w cmd args = git logger w cmd args ""
