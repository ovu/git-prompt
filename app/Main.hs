module Main where

import GitLib

main :: IO ()
main = do
  branchName <- getBranchName
  if branchName /= ""
  then do
    putStrLn branchName
    remoteName <- getRemoteName branchName
    putStrLn remoteName
    mergeBranch <- getMergeBranch branchName
    putStrLn mergeBranch
  else do
    shortRevision <- getShortRevisionOfHead
    putStrLn shortRevision
