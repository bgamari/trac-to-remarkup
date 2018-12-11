{-#LANGUAGE RecordWildCards #-}
module Git
where

import System.Exit
import System.Process
import System.IO.Temp
import Control.Exception
import Text.Printf (printf)

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
      "git exception: subprocess exited with exit code %d\n%s"
      gitExcExitCode
      gitExcStderr

-- | Make a raw call to git
gitRaw :: String -> [String] -> String -> IO String
gitRaw cmd args inp = do
  (errno, out, err) <- readProcessWithExitCode "git" (cmd:args) inp
  case errno of
    ExitSuccess -> return out
    ExitFailure i -> throw $ GitException i out err

-- | Clone a git repo into a temporary directory
clone :: Remote -> IO WorkingCopy
clone remote = do
  tmpdir <- getCanonicalTemporaryDirectory
  dirname <- createTempDirectory tmpdir "git"
  gitRaw "clone" [remote, dirname] ""
  return dirname

-- | Run a git subcommand in the given working copy
git :: WorkingCopy -> String -> [String] -> String -> IO String
git w cmd args inp =
  gitRaw cmd (["-C", w] ++ args) inp

git_ :: WorkingCopy -> String -> [String] -> IO String
git_ w cmd args = git w cmd args ""
