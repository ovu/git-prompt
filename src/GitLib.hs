module GitLib
    ( getBranchName
      , getShortRevisionOfHead
    ) where

import System.Process
import System.Exit

getBranchName :: IO String
getBranchName = do
    ( exitCode, symbolicRef, _ ) <- readProcessWithExitCode "git" ["symbolic-ref", "HEAD"] []
    if exitCode == ExitSuccess
    then do
      let branch  = removeFirstElevenChars symbolicRef
      let branchToReturn = removeEndOfLine branch
      return branchToReturn
    else
      return ""
  where
    removeFirstElevenChars = drop 11

getShortRevisionOfHead :: IO String
getShortRevisionOfHead = do
    shortRevision <- readProcess "git" ["rev-parse", "--short", "HEAD"] []
    let shortRevisionToReturn = removeEndOfLine shortRevision
    return shortRevisionToReturn

-- Helper functions
removeEndOfLine :: String -> String
removeEndOfLine = filter (/= '\n')
