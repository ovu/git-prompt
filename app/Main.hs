module Main where

import GitLib
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

main :: IO ()
main = do
  isGitRepository <- isGitRepository
  if not isGitRepository
  then
    return ()
  else do
    GitRepoStatus headInfo remoteStatus stagedStatus changedFiles  untrackedFiles isRepoClean <- getGitRepoStatus
    case headInfo of
      BranchName name -> putStrLn name
      ShortRevision revision -> putStrLn revision
    case remoteStatus of
      Nothing -> putStrLn $ getDiffWithRemoteText $ DiffWithRemote 0 0
      Just ( RemoteStatus diffWithRemote ) -> putStrLn $ getDiffWithRemoteText diffWithRemote
      Just ( NoRemoteStatus defaultStatus ) -> putStrLn defaultStatus
    print ( staged stagedStatus )
    print ( conflicted stagedStatus )
    print changedFiles
    print untrackedFiles
    if isRepoClean 
    then putStrLn "1"
    else putStrLn "0"

data HeadInfo = BranchName String | ShortRevision String
data RemoteStatus = RemoteStatus DiffWithRemote | NoRemoteStatus String

type NumberOfChangedFiles = Int
type NumberOfUntrackedFiles = Int
type IsRepoClean = Bool
data GitRepoStatus = GitRepoStatus HeadInfo (Maybe RemoteStatus) StagedStatus NumberOfChangedFiles NumberOfUntrackedFiles IsRepoClean

getGitRepoStatus :: IO GitRepoStatus
getGitRepoStatus = do
  branchName <- runMaybeT getBranchName
  headInfo <- getHeadInfo branchName
  remoteStatus <- runMaybeT $ getRemoteStatus branchName
  stagedStatus <- getStagedStatus
  numberOfChangedFiles <- getNumberOfChangedFiles
  numberOfUntrackedFiles <- getNumberOfUntrackedFiles
  let isCleanRepository = isLocalRepoClean stagedStatus numberOfChangedFiles numberOfUntrackedFiles
  return $ GitRepoStatus headInfo remoteStatus stagedStatus numberOfChangedFiles numberOfUntrackedFiles isCleanRepository

type BranchName = String

getHeadInfo :: Maybe BranchName -> IO HeadInfo
getHeadInfo branchName =
  case branchName of
    Nothing -> getShortRevision
    Just branch -> return $ BranchName branch
  where
    getShortRevision = do
     shortRevision <- getShortRevisionOfHead
     return $ ShortRevision shortRevision

getRemoteStatus :: Maybe BranchName -> MaybeT IO RemoteStatus
getRemoteStatus branchName =
  case branchName of 
    Nothing -> return $ NoRemoteStatus "."
    Just branch -> getRemoteStatusWithDifference branch
  where
    getRemoteStatusWithDifference branchName = do
     remoteName <- getRemoteName branchName
     mergeBranch <- getMergeBranch branchName
     differenceWithRemote <- getDifferenceWithRemote remoteName mergeBranch
     return $ RemoteStatus differenceWithRemote

isLocalRepoClean :: StagedStatus -> Int -> Int -> Bool
isLocalRepoClean (StagedStatus 0 0) 0 0 = True
isLocalRepoClean (StagedStatus _ _) _ _ = False

getDiffWithRemoteText :: DiffWithRemote -> String
getDiffWithRemoteText diffWithRemote =
   case diffWithRemote of
    ( DiffWithRemote 0 0 ) -> "."
    ( DiffWithRemote behind 0 ) -> "↓·" ++ show behind
    ( DiffWithRemote 0 ahead ) -> "↑·" ++ show ahead
    ( DiffWithRemote behind ahead ) -> "↓·" ++ show behind ++ "↑·" ++ show ahead
