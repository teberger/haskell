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

type DocumentClassDistribution = V.Vector Double
type WordDistribution = V.Vector Double

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
  
  let vocab  = V.fromList vocabulary               :: V.Vector Word
      labels = V.fromList labels_ln                :: V.Vector Label
      nVocab = fromIntegral (V.length vocab)
      nLabel = fromIntegral (V.length labels)
      alpha  = 1 / nVocab
      
      f = (\x y -> (fst x) == (fst y))
      train_data_temp = groupBy f (map (break (== ' ')) train_data_lines)
      test_data_temp  = groupBy f (map (break (== ' ')) test_data_lines )
      
      trainDocs = map makeDoc train_data_temp 
      testDocs  = map makeDoc test_data_temp
      
      trainData = (map read train_label_lines) `zip` trainDocs :: [Instance]
      testData  = (map read test_label_lines ) `zip` testDocs  :: [Instance]
      initClassDist = V.replicate (nLabel :: Int) (1.0 / (nLabel :: Double))        :: DocumentClassDistribution
      initWordsDist = V.replicate (nVocab :: Int) (1.0 / (nVocab :: Double))        :: WordDistribution
  print $ V.length vocab
  return ()

makeDoc :: [(String, String)] -> Document
makeDoc ls = map ((\(x,y) -> (read x :: Int, read (tail y) :: Int)) . 
                  break (== ' ') . 
                  tail .  --removes the leading ' '
                  snd)
             ls

--Renaming methods to make it easier to read
getLabel = fst
getDocument = snd
getWordIdx = fst
getWordCount = snd


