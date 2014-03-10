{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}

module Main where

import System.Environment
import System.IO
import Control.Monad (liftM)
import Data.List
import Data.Array.Repa hiding ((++), map, zipWith)
import Data.Array.Repa.Eval (fromList)
import qualified Data.Array.Repa as R ((++), map, zipWith)
import Data.Array.Repa.Repr.Vector
import Data.List.Extras.Argmax
import GSL.Random.Dist

--INSTANCE TYPE SYNONYM
type Instance = (LabelIdx, Document)
--LABEL TYPE SYNONYM
type Label = String
type LabelIdx = Int
--DOCUMENT TYPE SYNONYM
type Document = Array U (Z :. WordIdx) WordCount
type DocumentNumber = Int
--WORD TYPE SYNONYM
type Word = String
type WordIdx = Int
type WordCount = Int

main :: IO ()
main = do
  vocab:labels:dataLoc:_ <- getArgs
  vocabulary <- liftM lines $ hGetContents =<< openFile vocab ReadMode
  labels_ln  <- liftM lines $ hGetContents =<< openFile labels ReadMode
  
  train_data_lines  <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.data" ) ReadMode 
  train_label_lines <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.label") ReadMode 
  train_map_lines   <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "train.map"  ) ReadMode 
  
  test_data_lines  <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.data" ) ReadMode
  test_label_lines <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.label") ReadMode
  test_map_lines   <- fmap lines $ hGetContents =<< openFile (dataLoc ++ "test.map"  ) ReadMode
  
  let vocab = fromListVector (Z :. (length vocabulary :: Int)) vocabulary :: Array V (Z :. WordIdx) String
      labels = fromListVector (Z :. (length labels_ln :: Int)) labels_ln :: Array V (Z :. LabelIdx) String
      nVocab = size (extent vocab)  :: Int
      nLabel = size (extent labels) :: Int
      alpha  = 1 / (fromIntegral nVocab) :: Double
      
      f = (\x y -> (fst x) == (fst y))
      train_data_temp = groupBy f (map (break (== ' ')) train_data_lines)
      test_data_temp  = groupBy f (map (break (== ' ')) test_data_lines )      
      
      trainDocs = map (makeDoc nVocab) train_data_temp
      testDocs  = map (makeDoc nVocab) test_data_temp
      
      trainData =  (map read train_label_lines) `zip` trainDocs :: [Instance]      
      testData  = (map read test_label_lines ) `zip` testDocs   :: [Instance]
      
      zeros = fromFunction (Z :. nVocab) (\(Z :. wi) -> 0) :: Array D (Z :. WordIdx) Int
      
      likelyhoods = foldl' (R.++) id $ [foldl' (\x y -> x +^ (snd y)) zeros (filter ((/= i) . fst) trainData) | i <- [0..nLabel]]
--      likelyhoods is append after I sum across all indexes -> Array U (Z :. labelIdx) (Array U (Z :. WordIdx) Int)
      
      
--TODO: Fold over the trainData and come up with the wordLikelyhood list
  print $ length trainDocs
  return ()

makeDoc :: Int -> [(String, String)] -> Document
makeDoc nVocab ls = computeS $ fromFunction (Z :. nVocab) f 
  where f (Z :. i) = case lookup i ls' of 
                        Just x  -> x
                        Nothing -> 0
                        
        ls' = map ((\(x,y) -> (read x :: Int, read (tail y) :: Int)) . 
                                break (== ' ') . 
                                tail .  --removes the leading ' '
                                snd)
               ls

--Renaming methods to make it easier to read
--getLabel = P.fst
--getDocument = P.snd
--gOetWordIdx = P.fst
--getWordCount = P.snd
