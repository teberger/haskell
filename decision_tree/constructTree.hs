module Main where

import qualified Data.Vector as V
import Data.List
import Data.List.Extras.Argmax
import Data.Maybe
import Control.Monad
import System.IO
import System.Environment

--standard rose tree implementation with an additional bit of information storing the
--edge class
data DecisionTree a b c = Node a [(b, DecisionTree a b c)] | Leaf c deriving (Show)

data Nucleotide = A | C | G | T deriving (Show, Eq)

toNuc 'a' = A
toNuc 'c' = C
toNuc 'g' = G
toNuc 't' = T

nucs = [A,C,T,G]

-- So it makes sense to read, a promoter is just a boolean value
type Promoter = Bool
-- An instance is just a pairing of a Vector of nucleotides and a boolean class
type Instance = (V.Vector Nucleotide, Promoter)
--lists of ints can also be named exclusions
type Exclusions = [Int]

main :: IO ()
main = do
  training_file:validation_file:_ <- getArgs
  
  --reads the data in and breaks them into lists of lines
  train_content <- liftM lines . hGetContents =<< openFile training_file ReadMode
  valid_content <- liftM lines . hGetContents =<< openFile validation_file ReadMode
  
  -- constructs pairs of lists of nucleotides and the classification of the list of nucleotides
  let train_data = map ((\(x,y) -> (V.fromList (map toNuc x), (" +" == y))) . break (== ' ')) train_content :: [Instance]
      valid_data = map ((\(x,y) -> (V.fromList (map toNuc x), (" +" == y))) . break (== ' ')) valid_content :: [Instance]
      
      --constructs the tree with no exlcusions at the top level yet
      tree = buildTree train_data [] :: DecisionTree Int Nucleotide Promoter
      --classifies each instance of the validation set using the constructed tree
      validation = map (classify tree) valid_data :: [Promoter]
      --pairs each determined class with its actual class and checks to see if they are equal
      paired = zipWith (\x y -> (x == snd y)) validation valid_data :: [Bool]
  
  putStrLn "Validation Accuracy: " 
  --sums the number of correct and outputs the percentage of accuracy given by the tree
  print $ (foldl' (\x y -> x + (fromIntegral (fromEnum y))) 0 paired) / (genericLength paired)
  
--classifies an instance by recursively decending the DecisionTree until it hits a leaf node  
classify :: DecisionTree Int Nucleotide Promoter -> Instance -> Promoter
classify (Leaf c) _ = c
classify (Node i ls) inst = classify tree inst
  where p = (fst inst) V.! i                            :: Nucleotide
        -- subtree corresponding to the instance's Nucleotide at the appropriate index
        tree = snd . head $ filter ((== p) . fst) ls    :: DecisionTree Int Nucleotide Promoter 


--Recursively constructs the tree one node at a time with the given exclusions
buildTree :: [Instance] -> Exclusions -> DecisionTree Int Nucleotide Promoter
buildTree ls exclusions = case pure ls of
                            True  -> Leaf mostPrevalent
                            False -> Node bestIdx (map func splitData)
                            
  where mostPrevalent = if (length $ filter snd ls) > (length $ filter (not . snd) ls) 
                        then True 
                        else False            :: Promoter
        bestIdx = bestInfoGain exclusions ls  :: Int
        splitData = splitOn ls bestIdx        :: [(Nucleotide, [Instance])] 
        func (p,ls') = if length ls' == 0 
                       then (p,Leaf mostPrevalent) 
                       else (p, buildTree ls' (bestIdx:exclusions)) 
        
--Determines if a list of instances all agree on the same promoter
pure :: [Instance] -> Bool
pure ls =  (length ls) == (length $ filter ((== promoter) . snd) ls)
  where promoter = snd $ head ls 


--Determines the best index for information gain excluding some indeces
bestInfoGain :: Exclusions -> [Instance] -> Int
bestInfoGain exc ls = argmax (\x -> if x `elem` exc then 0.0 else (infoGain ls x)) [0..(V.length . fst . head $ ls) - 1]

--calculates the information gain at a give index for a list of instances
infoGain :: [Instance] -> Int -> Double
infoGain ls index = (entropy ls) - sum (map func separated)
  where separated = map snd $ splitOn ls index
        n = genericLength ls
        func ls = let x = genericLength ls 
                  in if x == 0 then 0 else (x / n) * (entropy ls)

--Calculates the entropy of the list of instances
entropy :: [Instance] -> Double
entropy ls = (func t) + (func f)
  where func x = if x == 0 then 0 else (x/n) *(- logBase 2 (x/n)) :: Double
        n = genericLength ls                                   :: Double
        t = genericLength trues                                :: Double
        f = genericLength falses                               :: Double
        trues = filter snd ls                                  :: [Instance]
        falses = filter (not . snd) ls                         :: [Instance]
        
--splits the data on a given index, return pairs of Nucleotides and the instances        
--which have the nucleotide at that index
splitOn :: [Instance] -> Int -> [(Nucleotide,[Instance])]
splitOn ls idx = [(x, filter ((== x) . (V.! idx) . fst) ls) | x <- nucs]

--Test dataset
pos1 = repeat (V.fromList nucs, True)
pos2 = repeat (V.fromList [G,C,A,T], True)
neg  = repeat (V.fromList [C,A,G,T], False)
testData = (take 5 pos1) ++ (take 3 pos2) ++ (take 9 neg)
