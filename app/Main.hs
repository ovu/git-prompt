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
    putStrLn $ show $ staged stagedStatus
    putStrLn $ show $ conflicted stagedStatus
    numberOfChangedFiles <- getNumberOfChangedFiles
    putStrLn $ show numberOfChangedFiles
    numberOfUntrackedFiles <- getNumberOfUntrackedFiles
    putStrLn $ show numberOfUntrackedFiles
  else do
    shortRevision <- getShortRevisionOfHead
    putStrLn shortRevision
    putStrLn "." -- No remote information when in hash
    stagedStatus <- getStagedStatus
    putStrLn $ show $ staged stagedStatus
    putStrLn $ show $ conflicted stagedStatus
    numberOfChangedFiles <- getNumberOfChangedFiles
    putStrLn $ show numberOfChangedFiles
    numberOfUntrackedFiles <- getNumberOfUntrackedFiles
    putStrLn $ show numberOfUntrackedFiles

getDiffWithRemoteText :: DiffWithRemote -> String
getDiffWithRemoteText diffWithRemote =
   case diffWithRemote of
    ( DiffWithRemote 0 0 ) -> "."
    ( DiffWithRemote behind 0 ) -> "↓·" ++ show behind
    ( DiffWithRemote 0 ahead ) -> "↑·" ++ show ahead
    ( DiffWithRemote behind ahead ) -> "↓·" ++ show behind ++ "↑·" ++ show ahead
