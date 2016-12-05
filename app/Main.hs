module Main where

import GitLib

main :: IO ()
main = do
  branchName <- getBranchName
  putStrLn branchName
