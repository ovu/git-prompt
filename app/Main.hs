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
    mergeName <- getMergeName remoteName branchName
    putStrLn mergeName
  else do
    shortRevision <- getShortRevisionOfHead
    putStrLn shortRevision
