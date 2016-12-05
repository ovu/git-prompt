module Main where

import GitLib

main :: IO ()
main = do
  branchName <- getBranchName
  if branchName /= ""
  then
    putStrLn branchName
  else do
    shortRevision <- getShortRevisionOfHead
    putStrLn shortRevision
