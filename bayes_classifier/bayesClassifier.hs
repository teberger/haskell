module Main where

import System.Environment
import System.IO
  
main :: IO ()
main = do
  [vocab:dataLoc:_] <- getArgs
  vocabulary <- lift (`zip` [1..]) $ hContents =<< openFile vocab
  return ()



