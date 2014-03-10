{-# LANGUAGE TypeOperators #-}

module Main where

import System.Environment
import System.IO
import Control.Monad (liftM)
import Data.Array.Repa
import Data.List.Extras.Argmax
import GSL.Random.Dist
import qualified Prelude as P 

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
  vocabulary <- fmap lines $ hGetContents P.(=<<) openFile vocab ReadMode
  labels_ln  <- fmap lines $ hGetContents P.(=<<) openFile labels ReadMode
  
  train_data_lines  <- fmap lines $ hGetContents P.(=<<) openFile (dataLoc ++ "train.data" ) ReadMode 
--  train_label_lines <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.label") ReadMode 
--  train_map_lines   <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.map"  ) ReadMode 
  
--  test_data_lines  <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.data" ) ReadMode
--  test_label_lines <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.label") ReadMode
--  test_map_lines   <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.map"  ) ReadMode
  
  let vocab  = fromListUnboxed (Z :. (P.length vocabulary :: Int)) vocabulary  :: Array U (Z :. Int) String
{-      labels = R.fromList labels_ln       
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
      initClassDist = V.replicate (V.length labels) (1.0 / nLabel)
      initWordsAlphas = V.replicate (V.length vocab) (1 + alpha)

  print $ V.length vocab
-}
  P.return ()

--makeDoc :: [(String, String)] -> Document
--makeDoc ls = P.map ((\(x,y) -> (P.read x :: Int, P.read (P.tail y) :: Int)) . 
--                  break (== ' ') . 
--                  P.tail .  --removes the leading ' '
--                  P.snd)
--             ls

--Renaming methods to make it easier to read
--getLabel = P.fst
--getDocument = P.snd
--getWordIdx = P.fst
--getWordCount = P.snd


