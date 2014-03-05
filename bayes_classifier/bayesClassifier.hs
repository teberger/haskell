module Main where

import System.Environment
import System.IO
import Control.Monad(liftM)  

main :: IO ()
main = do
  vocab:dataLoc:_ <- getArgs
  vocabulary <- liftM (`zip` [1..]) $ fmap lines $ hGetContents =<< openFile vocab ReadMode
  print $ head vocabulary
  return ()
