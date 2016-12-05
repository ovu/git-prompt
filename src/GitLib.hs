module GitLib
    ( getBranchName
    ) where

import System.Process

getBranchName :: IO String
getBranchName = do
    symbolicRef <- readProcess "git" ["symbolic-ref", "HEAD"] []
    let branch  = removeFirstElevenChars symbolicRef
    let branchToReturn = removeEndOfLine branch
    return branchToReturn
  where
    removeFirstElevenChars = drop 11
    removeEndOfLine = filter (/= '\n')
