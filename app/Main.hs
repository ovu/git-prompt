module Main where

import GitLib

main :: IO ()
main = do
  branchName <- getBranchName
  if branchName /= ""
  then do
    putStrLn branchName
    remoteName <- getRemoteName branchName
    -- putStrLn remoteName
    mergeBranch <- getMergeBranch branchName
    -- putStrLn mergeBranch
    differenceWithRemote <- getDifferenceWithRemote remoteName mergeBranch
    -- putStrLn $ show differenceWithRemote
    putStrLn $ getDiffWithRemoteText differenceWithRemote
  else do
    shortRevision <- getShortRevisionOfHead
    putStrLn shortRevision
    putStrLn "." -- No remote information when in hash

getDiffWithRemoteText :: DiffWithRemote -> String
getDiffWithRemoteText diffWithRemote =
   case diffWithRemote of 
    ( DiffWithRemote 0 0 ) -> "." 
    ( DiffWithRemote behind 0 ) -> "↓·" ++ show behind
    ( DiffWithRemote 0 ahead ) -> "↑·" ++ show ahead
    ( DiffWithRemote behind ahead ) -> "↓·" ++ show behind ++ "↑·" ++ show ahead
