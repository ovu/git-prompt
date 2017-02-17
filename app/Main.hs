module Main where

import GitLib

main :: IO ()
main = do
  isGitRepository <- isGitRepository
  if isGitRepository == False
  then do 
    return ()
  else do
    GitRepoStatus headInfo remoteStatus stagedStatus changedFiles  untrackedFiles isRepoClean <- getGitRepoStatus
    case headInfo of
      BranchName name -> putStrLn name
      ShortRevision revision -> putStrLn revision
    case remoteStatus of
      RemoteStatus diffWithRemote -> putStrLn $ getDiffWithRemoteText diffWithRemote
      NoRemoteStatus defaultStatus -> putStrLn defaultStatus
    putStrLn $ show ( staged stagedStatus )
    putStrLn $ show ( conflicted stagedStatus )
    putStrLn $ show changedFiles
    putStrLn $ show untrackedFiles
    case isRepoClean of
      True -> putStrLn "1"
      False -> putStrLn "0"

data HeadInfo = BranchName String | ShortRevision String
data RemoteStatus = RemoteStatus DiffWithRemote | NoRemoteStatus String

type NumberOfChangedFiles = Int
type NumberOfUntrackedFiles = Int
type IsRepoClean = Bool
data GitRepoStatus = GitRepoStatus HeadInfo RemoteStatus StagedStatus NumberOfChangedFiles NumberOfUntrackedFiles IsRepoClean

getGitRepoStatus :: IO GitRepoStatus
getGitRepoStatus = do
  branchName <- getBranchName
  headInfo <- getHeadInfo branchName
  remoteStatus <- getRemoteStatus branchName
  stagedStatus <- getStagedStatus
  numberOfChangedFiles <- getNumberOfChangedFiles
  numberOfUntrackedFiles <- getNumberOfUntrackedFiles
  let isCleanRepository = isLocalRepoClean stagedStatus numberOfChangedFiles numberOfUntrackedFiles
  return $ GitRepoStatus headInfo remoteStatus stagedStatus numberOfChangedFiles numberOfUntrackedFiles isCleanRepository

type BranchName = String

getHeadInfo :: BranchName -> IO HeadInfo
getHeadInfo branchName = do
  if branchName /= "" 
  then return $ BranchName branchName
  else do
     shortRevision <- getShortRevisionOfHead
     return $ ShortRevision shortRevision

getRemoteStatus :: BranchName -> IO RemoteStatus
getRemoteStatus branchName = do
  if branchName /= ""
  then do
     remoteName <- getRemoteName branchName
     mergeBranch <- getMergeBranch branchName
     differenceWithRemote <- getDifferenceWithRemote remoteName mergeBranch
     return $ RemoteStatus differenceWithRemote
  else
     return $ NoRemoteStatus "."

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
