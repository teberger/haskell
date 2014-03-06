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
  
  train_data_lines  <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.data" ) ReadMode :: [String]
  train_label_lines <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.label") ReadMode :: [String]
  train_map_lines   <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.map"  ) ReadMode :: [String]
  
  test_data_lines  <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.data" ) ReadMode :: [String]
  test_label_lines <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.label") ReadMode :: [String]
  test_map_lines   <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.map"  ) ReadMode :: [String]
  
  let train_data_temp = 

  print $ train_data_lines
  return ()

types = ["train","test"]
types' = ["data","label","map"]