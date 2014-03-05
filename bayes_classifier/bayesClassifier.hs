module Main where

import System.Environment
import System.IO
import Control.Monad(liftM)  

type Document = (DocumentNumber, WordIndex, WordCount)
type DocumentNumber = Int
type WordIndex = Int
type WordCount = Int

main :: IO ()
main = do
  vocab:dataLoc:_ <- getArgs
  vocabulary <- liftM (`zip` [1..]) . fmap lines $ hGetContents =<< openFile vocab ReadMode
    
  return ()

types = ["train","test"]
types' = ["data","label","map"]