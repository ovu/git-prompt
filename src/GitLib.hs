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
      , getNumberOfUntrackedFiles
      , isGitRepository
    ) where

import System.Process
import System.Exit
import Text.Printf
import Data.List
import Data.Maybe
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad

isGitRepository :: IO Bool
isGitRepository = do
    ( exitCode, _, _ ) <- readProcessWithExitCode "git" ["rev-parse", "--show-toplevel"] []
    return (exitCode == ExitSuccess)

type BranchName = String

getBranchName :: MaybeT IO BranchName
getBranchName = do
    ( exitCode, symbolicRef, _ ) <- lift $ readProcessWithExitCode "git" ["symbolic-ref", "HEAD"] []
    guard ( exitCode == ExitSuccess )
    let branch  = removeFirstElevenChars symbolicRef
    let branchToReturn = removeEndOfLine branch
    return branchToReturn
  where
    removeFirstElevenChars = drop 11

getShortRevisionOfHead :: IO String
getShortRevisionOfHead = do
    shortRevision <- readProcess "git" ["rev-parse", "--short", "HEAD"] []
    let shortRevisionToReturn = removeEndOfLine shortRevision
    return shortRevisionToReturn

type RemoteName = String

getRemoteName :: BranchName -> MaybeT IO RemoteName
getRemoteName branchName = do
    ( exitCode, remoteName, _ ) <- lift $ readProcessWithExitCode "git" ["config", printf "branch.%s.remote" branchName] []
    guard ( exitCode == ExitSuccess )
    return $ removeEndOfLine remoteName

type MergeBranch = String

getMergeBranch :: BranchName -> MaybeT IO MergeBranch
getMergeBranch branchName = do
        ( exitCode, mergeName, _ ) <- lift $ readProcessWithExitCode "git" ["config", printf "branch.%s.merge" branchName] []
        guard (exitCode == ExitSuccess)
        return $ (removeFirstElevenChars . removeEndOfLine) mergeName
  where
    removeFirstElevenChars = drop 11

data DiffWithRemote = DiffWithRemote { behindCommits :: Int,
                                       aheadCommits :: Int } deriving Show

getDifferenceWithRemote :: RemoteName -> MergeBranch -> MaybeT IO DiffWithRemote
getDifferenceWithRemote remoteName mergeName = do
        let remoteRef = ( printf "refs/remotes/%s/%s" remoteName mergeName ) :: String
        ( exitCode, behindAndAheadText, _ ) <- lift $ readProcessWithExitCode "git" ["rev-list", "--left-right", "--count", printf "%s...HEAD" remoteRef] []
        guard( exitCode == ExitSuccess )
        let behindAndAheadArray = words behindAndAheadText
        let behindInt = ( read . head ) behindAndAheadArray
        let aheadInt = ( read . last ) behindAndAheadArray
        return $ DiffWithRemote behindInt aheadInt

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
      getConflictedLines = filter $ isPrefixOf "U"

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
      getUnmergedLines = filter $ isPrefixOf "U"

getNumberOfUntrackedFiles :: IO Int
getNumberOfUntrackedFiles = do
        ( exitCode, commandResult, _ ) <- readProcessWithExitCode "git" ["status", "-s", "-uall"] []
        if exitCode == ExitSuccess
        then do
          let statusLines = lines commandResult
          let untrackedFiles = length $ getUntrackedLines statusLines
          return untrackedFiles
        else
          return 0
    where getUntrackedLines = filter $ isPrefixOf "??"

-- Helper functions
removeEndOfLine :: String -> String
removeEndOfLine = filter (/= '\n')
