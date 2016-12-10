module GitLib
    ( getBranchName
      , getShortRevisionOfHead
      , getRemoteName
      , getMergeBranch
      , getDifferenceWithRemote
      , DiffWithRemote (..)
      , getStagedStatus
      , StagedStatus (..)
      , getNumberOfChangedFiles
    ) where

import System.Process
import System.Exit
import Text.Printf
import Data.List

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

type MergeBranch = String

getMergeBranch :: BranchName -> IO MergeBranch
getMergeBranch branchName = do
        ( exitCode, mergeName, _ ) <- readProcessWithExitCode "git" ["config", printf "branch.%s.merge" branchName] []
        if exitCode == ExitSuccess
        then do
          return $ (removeFirstElevenChars . removeEndOfLine) mergeName
        else
          return ""
  where
    removeFirstElevenChars = drop 11

data DiffWithRemote = DiffWithRemote { behindCommits :: Int,
                                       aheadCommits :: Int } deriving Show

getDifferenceWithRemote :: RemoteName -> MergeBranch -> IO DiffWithRemote
getDifferenceWithRemote remoteName mergeName = do
        let remoteRef = ( printf "refs/remotes/%s/%s" remoteName mergeName ) :: String
        ( exitCode, behindAndAheadText, _ ) <- readProcessWithExitCode "git" ["rev-list", "--left-right", "--count", printf "%s...HEAD" remoteRef] []
        if exitCode == ExitSuccess
        then do
          let behindAndAheadArray = words behindAndAheadText
          let behindInt = ( read . head ) behindAndAheadArray
          let aheadInt = ( read . last ) behindAndAheadArray
          return $ DiffWithRemote behindInt aheadInt
        else
          return $ DiffWithRemote 0 0

data StagedStatus = StagedStatus { staged :: Int,
                                   conflicted :: Int } deriving Show

getStagedStatus :: IO StagedStatus
getStagedStatus = do 
        ( exitCode, commandResult, _ ) <- readProcessWithExitCode "git" ["diff", "--staged", "--name-status"] []
        if exitCode == ExitSuccess
        then do
          let stagedLines = lines commandResult
          let conflictedFiles = length $ getConflictedLines stagedLines
          let stagedFiles = length stagedLines - conflictedFiles
          return $ StagedStatus stagedFiles conflictedFiles
        else
          return $ StagedStatus 0 0
    where
      getConflictedLines = filter (\ line -> isPrefixOf "U" line)

getNumberOfChangedFiles :: IO Int
getNumberOfChangedFiles = do
        ( exitCode, commandResult, _ ) <- readProcessWithExitCode "git" ["diff", "--name-status"] []
        if exitCode == ExitSuccess
        then do
          let changedLines = lines commandResult
          let unmergedFiles = length $ getUnmergedLines changedLines
          let changedFiles = length changedLines - unmergedFiles
          return changedFiles
        else
          return 0
    where
      getUnmergedLines = filter (\ line -> isPrefixOf "U" line)

-- Helper functions
removeEndOfLine :: String -> String
removeEndOfLine = filter (/= '\n')
