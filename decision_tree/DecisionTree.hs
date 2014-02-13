{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module DecisionTree(mkInstance,
                    buildTree,
                    Instance,
                    DecisionTree,
                    classify,
--                    entropy,
--                    infoGain,
                    ) where

import Data.List
import Data.List.Extras.Argmax
--import Data.Array.IArray
--import qualified Data.Set as S
--import Statistics.Test.ChiSquared
--import qualified Data.Map as M

type FeatureSelector a = ([a] -> a)

--Defining an instance data type to use for 
data Instance a c = (Enum a, Eq c, Show a) =>  I { attributes :: [a],
                                                   clazz      :: c  
                                                 }
                      
mkInstance :: (Enum a, Eq c, Show a) => [a] -> c -> Instance a c
mkInstance a c = I a c

data DecisionTree a c = Root a [DecisionTree a c] | Leaf a c
data StoppingCriteria = ChiSquared Double | Purity | Size Int
data BuildMethod = Accuracy | InformationGain

infoGain :: (Eq c, Enum a) => FeatureSelector a -> [Instance a c] -> Double
infoGain att instances = (entropy instances) - (sum  . map (\x -> (lengthd x) / n * (entropy x))) separated
  where separated = [filter ((== x) . fromEnum . att . attributes) instances | x <- bins]
        n = lengthd instances
        bins = foldl' (\ls x -> if (fromEnum . att . attributes $ x) `elem` ls 
                                then ls 
                                else (fromEnum . att . attributes $ x):ls
                      )
                      []
                      instances

entropy :: (Eq c) => [Instance a c] -> Double
entropy inst = sum $ map (\x -> ( (x/n)) * (- logBase 2 (x/n))) ls
  where ls = [lengthd . filter ((== cs) . clazz) $ inst | cs <- classes ]
        n = lengthd inst
        classes = foldl' (\ls x -> if (clazz x) `elem` ls then ls else (clazz x):ls) [] inst


buildTree :: (Enum a, Eq c) => BuildMethod -> StoppingCriteria -> [Instance a c] -> DecisionTree a c
buildTree method criteria dataset = undefined
  where selectionFunction = case method of
                              Accuracy -> accuracy
                              InformationGain -> argmax (\x -> infoGain x dataset) [(!! x) | x <- [1..(length dataset)]]

accuracy = undefined

createTree :: (Instance a c -> a) -> ([[Instance a c]] -> Bool) -> [Instance a c] -> DecisionTree a c
createTree = undefined

classify :: DecisionTree a c -> Instance a c -> c
classify = undefined

--takes the length of a list and returns it as a num so it can be viewed as a double 
--casting is weird in haskell due to the type system...
lengthd :: (Num a) => [b] -> a
lengthd = fromIntegral . length
