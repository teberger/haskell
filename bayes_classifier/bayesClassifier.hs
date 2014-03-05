module Main where

import System.Environment
import System.IO
import Control.Monad (liftM)
import Data.List

type Document = [(WordIndex, WordCount)]
type DocumentNumber = Int
type WordIndex = Int
type WordCount = Int

main :: IO ()
main = do
  vocab:dataLoc:_ <- getArgs
  vocabulary <- liftM (`zip` [1..]) . fmap lines $ hGetContents =<< openFile vocab ReadMode
  
  train_data  <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.data" ) ReadMode
  train_label <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.label") ReadMode  
  train_map   <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.map"  ) ReadMode
  
  test_data  <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.data" ) ReadMode
  test_label <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.label") ReadMode  
  test_map   <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.map"  ) ReadMode

  

  return ()

types = ["train","test"]
types' = ["data","label","map"]