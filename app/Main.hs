module Main where

import GitLib

main :: IO ()
main = do
  branchName <- getBranchName
  if branchName /= ""
  then do
    putStrLn branchName
    remoteName <- getRemoteName branchName
    mergeBranch <- getMergeBranch branchName
    differenceWithRemote <- getDifferenceWithRemote remoteName mergeBranch
    putStrLn $ getDiffWithRemoteText differenceWithRemote
    stagedStatus <- getStagedStatus
    let stagedFiles = staged stagedStatus
    let conflictedFiles = conflicted stagedStatus
    putStrLn $ show $ stagedFiles
    putStrLn $ show $ conflictedFiles
    numberOfChangedFiles <- getNumberOfChangedFiles
    putStrLn $ show numberOfChangedFiles
    numberOfUntrackedFiles <- getNumberOfUntrackedFiles
    putStrLn $ show numberOfUntrackedFiles
    let isCleanRepository = isLocalRepoClean stagedFiles conflictedFiles numberOfChangedFiles numberOfUntrackedFiles
    if isCleanRepository == True
    then putStrLn "1"
    else putStrLn "0"
  else do
    shortRevision <- getShortRevisionOfHead
    putStrLn shortRevision
    putStrLn "." -- No remote information when in hash
    stagedStatus <- getStagedStatus
    let stagedFiles = staged stagedStatus
    let conflictedFiles = conflicted stagedStatus
    putStrLn $ show $ stagedFiles
    putStrLn $ show $ conflictedFiles
    numberOfChangedFiles <- getNumberOfChangedFiles
    putStrLn $ show numberOfChangedFiles
    numberOfUntrackedFiles <- getNumberOfUntrackedFiles
    putStrLn $ show numberOfUntrackedFiles
    let isCleanRepository = isLocalRepoClean stagedFiles conflictedFiles numberOfChangedFiles numberOfUntrackedFiles
    if isCleanRepository == True
    then putStrLn "1"
    else putStrLn "0"

isLocalRepoClean :: Int -> Int -> Int -> Int -> Bool
isLocalRepoClean 0 0 0 0 = True
isLocalRepoClean _ _ _ _ = False

getDiffWithRemoteText :: DiffWithRemote -> String
getDiffWithRemoteText diffWithRemote =
   case diffWithRemote of
    ( DiffWithRemote 0 0 ) -> "."
    ( DiffWithRemote behind 0 ) -> "↓·" ++ show behind
    ( DiffWithRemote 0 ahead ) -> "↑·" ++ show ahead
    ( DiffWithRemote behind ahead ) -> "↓·" ++ show behind ++ "↑·" ++ show ahead
