module Main where

import System.Environment
import System.IO
import Control.Monad (liftM)
import Data.List
import Data.Array.Base
import Data.Array.Unboxed

type Instance = (LabelIdx, Document)

type Label = String
type LabelIdx = Int

type Document = [(WordIdx, WordCount)]
type DocumentNumber = Int

type Word = String
type WordIdx = Int
type WordCount = Int

main :: IO ()
main = do
  vocab:labels:dataLoc:_ <- getArgs
  vocabulary <- fmap lines $ hGetContents =<< openFile vocab ReadMode
  labels_ln  <- fmap lines $ hGetContents =<< openFile labels ReadMode
  
  train_data_lines  <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.data" ) ReadMode 
  train_label_lines <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.label") ReadMode 
  train_map_lines   <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.map"  ) ReadMode 
  
  test_data_lines  <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.data" ) ReadMode
  test_label_lines <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.label") ReadMode
  test_map_lines   <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.map"  ) ReadMode
  
  let vocab  = listArray (1,(length vocabulary)) vocabulary :: Array Int Word
      labels = listArray (1,(length labels_ln)) labels_ln   :: Array Int Label
      
      f = (\x y -> (fst x) == (fst y))           
      train_data_temp = groupBy f (map (break (== ' ')) train_data_lines)
      test_data_temp  = groupBy f (map (break (== ' ')) test_data_lines )
      
      trainDocs = map makeDoc train_data_temp 
      testDocs  = map makeDoc test_data_temp
      
      trainData = zip train_label_lines trainDocs :: [Instance]
      testData  = zip test_label_lines testDocs   :: [Instance]

  print $ numElements vocab
  return ()

makeDoc :: [(String, String)] -> Document
makeDoc ls = map ((\(x,y) -> (read x :: Int, read (tail y) :: Int)) . 
                  break (== ' ') . 
                  tail .  
                  snd)
             ls

