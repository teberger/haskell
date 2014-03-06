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
  
  train_data_lines  <- liftM (groupBy ((==) . (\x -> [x]) . (!! 0))) $ fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.data" ) ReadMode
  train_label_lines <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.label") ReadMode  
  train_map_lines   <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.map"  ) ReadMode
  
  test_data_lines  <- liftM (groupBy ((==) . (\x -> [x] ). (!! 0))) $ fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.data" ) ReadMode
  test_label_lines <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.label") ReadMode  
  test_map_lines   <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.map"  ) ReadMode
  
  

--  let train_data_temp = 
--      train_data = map (groupBy ((==) . fst)) 

  return ()

types = ["train","test"]
types' = ["data","label","map"]