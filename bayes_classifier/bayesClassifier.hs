module Main where

import System.Environment
import System.IO
import Control.Monad (liftM)
import Data.List
import qualified Data.Vector as V
import Data.List.Extras.Argmax

--INSTANCE TYPE SYNONYM
type Instance = (LabelIdx, Document)
--LABEL TYPE SYNONYM
type Label = String
type LabelIdx = Int
--DOCUMENT TYPE SYNONYM
type Document = [(WordIdx, WordCount)]
type DocumentNumber = Int
--WORD TYPE SYNONYM
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
  
  let vocab  = V.fromList vocabulary                 :: V.Vector Word
      labels = V.fromList labels_ln                  :: V.Vector Label
      alpha  = 1 / (fromIntegral $ V.length vocab) :: Double      
      
      f = (\x y -> (fst x) == (fst y))           
      train_data_temp = groupBy f (map (break (== ' ')) train_data_lines)
      test_data_temp  = groupBy f (map (break (== ' ')) test_data_lines )
      
      trainDocs = map makeDoc train_data_temp 
      testDocs  = map makeDoc test_data_temp
      
      trainData = (map (read :: String -> Int) train_label_lines) `zip` trainDocs :: [Instance]
      testData  = (map (read :: String -> Int) test_label_lines)  `zip` testDocs  :: [Instance]

  print $ V.length vocab
  return ()

makeDoc :: [(String, String)] -> Document
makeDoc ls = map ((\(x,y) -> (read x :: Int, read (tail y) :: Int)) . 
                  break (== ' ') . 
                  tail .  
                  snd)
             ls

--Renaming methods to make it easier to read
getLabel = fst
getDocument = snd
getWordIdx = fst
getWordCount = snd
