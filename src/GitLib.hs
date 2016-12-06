module GitLib
    ( getBranchName
      , getShortRevisionOfHead
      , getRemoteName
      , getMergeName
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
      return "origin"

type MergeName = String

getMergeName :: RemoteName -> BranchName -> IO MergeName
getMergeName remoteName branchName = do
      if remoteName == "origin"
      then 
        return $ printf "refs/heads/%s" branchName
      else do
        ( exitCode, mergeName, _ ) <- readProcessWithExitCode "git" ["config", printf "branch.%s.merge" branchName] []
        if exitCode == ExitSuccess
        then do
          return $ removeEndOfLine mergeName
        else
          return ""

-- Helper functions
removeEndOfLine :: String -> String
removeEndOfLine = filter (/= '\n')
