module GitLib
    ( getBranchName
      , getShortRevisionOfHead
      , getRemoteName
      , getMergeBranch
    ) where

import System.Process
import System.Exit
import Text.Printf

type BranchName = String

getBranchName :: IO BranchName
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

type RemoteName = String

getRemoteName :: BranchName -> IO RemoteName
getRemoteName branchName = do
    ( exitCode, remoteName, _ ) <- readProcessWithExitCode "git" ["config", printf "branch.%s.remote" branchName] []
    if exitCode == ExitSuccess
    then do
      return $ removeEndOfLine remoteName
    else
      return ""

type MergeName = String

getMergeBranch :: BranchName -> IO MergeName
getMergeBranch branchName = do
        ( exitCode, mergeName, _ ) <- readProcessWithExitCode "git" ["config", printf "branch.%s.merge" branchName] []
        if exitCode == ExitSuccess
        then do
          return $ (removeFirstElevenChars . removeEndOfLine) mergeName
        else
          return ""
  where
    removeFirstElevenChars = drop 11

-- Helper functions
removeEndOfLine :: String -> String
removeEndOfLine = filter (/= '\n')
