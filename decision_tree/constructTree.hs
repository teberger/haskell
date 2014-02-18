module Main where

import qualified Data.Vector as V
import Data.List
import Data.List.Extras.Argmax
import Data.Maybe
import Control.Monad
import System.IO
import System.Environment

data DecisionTree a b c = Node a [(b, DecisionTree a b c)] | Leaf c deriving (Show)

data Nucleotide = A | C | G | T deriving (Show, Eq)

toNuc 'a' = A
toNuc 'c' = C
toNuc 'g' = G
toNuc 't' = T

nucs = [A,C,T,G]

type Promoter = Bool
type Instance = (V.Vector Nucleotide, Promoter)
type Exclusions = [Int]

main :: IO ()
main = do
  training_file:validation_file:_ <- getArgs
  
  train_content <- liftM lines . hGetContents =<< openFile training_file ReadMode
  valid_content <- liftM lines . hGetContents =<< openFile validation_file ReadMode
  
  let train_data = map ((\(x,y) -> (V.fromList (map toNuc x), (" +" == y))) . break (== ' ')) train_content :: [Instance]
      valid_data = map ((\(x,y) -> (V.fromList (map toNuc x), (" +" == y))) . break (== ' ')) valid_content :: [Instance]
      tree = buildTree train_data []
      validation = map (classify tree) valid_data
      paired = zipWith (\x y -> (x == snd y)) validation valid_data
  
  putStrLn "Validation Accuracy: " 
  print $ (foldl' (\x y -> x + (fromIntegral (fromEnum y))) 0 paired) / (genericLength paired)
--  print $ 
  
  
classify :: DecisionTree Int Nucleotide Promoter -> Instance -> Promoter
classify (Leaf c) _ = c
classify (Node i ls) inst = classify tree inst
  where p = (fst inst) V.! i  :: Nucleotide
        tree = snd . head $ filter ((== p) . fst) ls


buildTree :: [Instance] -> Exclusions -> DecisionTree Int Nucleotide Promoter
buildTree ls exclusions = case pure ls of
                            True  -> Leaf mostPrevalent
                            False -> Node bestIdx (map func splitData)
  where mostPrevalent = if (length $ filter snd ls) > (length $ filter (not . snd) ls) then True else False 
        bestIdx = bestInfoGain exclusions ls
        splitData = splitOn ls bestIdx
        func (p,ls') = if length ls' == 0 then (p,Leaf mostPrevalent) else (p, buildTree ls' (bestIdx:exclusions))
        
pure :: [Instance] -> Bool
pure ls =  (length ls) == (length $ filter ((== promoter) . snd) ls)
  where promoter = snd $ head ls 
                            

bestInfoGain :: Exclusions -> [Instance] -> Int
bestInfoGain exc ls = argmax (\x -> if x `elem` exc then 0.0 else (infoGain ls x)) [0..(V.length . fst . head $ ls) - 1]

infoGain :: [Instance] -> Int -> Double
infoGain ls index = (entropy ls) - sum (map func separated)
  where separated = map snd $ splitOn ls index
        n = genericLength ls
        func ls = let x = genericLength ls 
                  in if x == 0 then 0 else (x / n) * (entropy ls)

entropy :: [Instance] -> Double
entropy ls = (func t) + (func f)
  where func x = if x == 0 then 0 else (x/n) *(- logBase 2 (x/n)) :: Double
        n = genericLength ls                                   :: Double
        t = genericLength trues                                :: Double
        f = genericLength falses                               :: Double
        trues = filter snd ls                                  :: [Instance]
        falses = filter (not . snd) ls                         :: [Instance]
        
        
splitOn :: [Instance] -> Int -> [(Nucleotide,[Instance])]
splitOn ls idx = [(x, filter ((== x) . (V.! idx) . fst) ls) | x <- nucs]

--Test dataset
pos1 = repeat (V.fromList nucs, True)
pos2 = repeat (V.fromList [G,C,A,T], True)
neg  = repeat (V.fromList [C,A,G,T], False)
testData = (take 5 pos1) ++ (take 3 pos2) ++ (take 9 neg)

tdata = map (\(x,y) -> (V.fromList (map toNuc x), y == " +")) $ map (break (== ' ')) $ (lines stringData)

stringData = "cagaaacgttttattcgaacatcgatctcgtcttgtgttagaattctaacatacggt +\n" ++
  "tagagggtgtactccaagaagaggaagatgaggctagacgtctctgcatggagtatg -\n" ++
  "gcaaaaataaatgcttgactctgtagcgggaaggcgtattatgcacaccccgcgccg +\n" ++
  "tctgaaatgagctgttgacaattaatcatcgaactagttaactagtacgcaagttca +\n" ++
  "aaattaaaattttattgacttaggtcactaaatactttaaccaatataggcatagcg +\n" ++
  "taacattaataaataaggaggctctaatggcactcattagccaatcaatcaagaact -\n" ++
  "catgtcagcctcgacaacttgcataaatgctttcttgtagacgtgccctacgcgctt -\n" ++
  "cgaccgaagcgagcctcgtcctcaatggcctctaaacgggtcttgaggggttttttg -\n" ++
  "tgtgcagtttatggttccaaaatcgccttttgctgtatatactcacagcataactgt +\n" ++
  "tcgttgtatatttcttgacaccttttcggcatcgccctaaaattcggcgtcctcata +\n" ++
  "tttctacaaaacacttgatactgtatgagcatacagtataattgcttcaacagaaca +\n" ++
  "gccaatcaatcaagaacttgaagggtggtatcagccaacagcctgacatccttcgtt -\n" ++
  "tccagtataatttgttggcataattaagtacgacgagtaaaattacatacctgcccg +\n" ++
  "ttcgtctccgcgactacgatgagatgcctgagtgcttccgttactggattgtcacca -\n" ++
  "ttactgtgaacattattcgtctccgcgactacgatgagatgcctgagtgcttccgtt -\n" ++
  "caatggcctctaaacgggtcttgaggggttttttgctgaaaggaggaactatatgcg -\n" ++
  "tggggacgtcgttactgatccgcacgtttatgatatgctatcgtactctttagcgag +\n" ++
  "tgctatcctgacagttgtcacgctgattggtgtcgttacaatctaacgcatcgccaa +\n" ++
  "agacgtctctgcatggagtatgagatggactacggtgggtacaatatgctggatgga -\n" ++
  "gagagcatgtcagcctcgacaacttgcataaatgctttcttgtagacgtgccctacg -\n" ++
  "tactagcaatacgcttgcgttcggtggttaagtatgtataatgcgcgggcttgtcgt +\n" ++
  "aattgtgatgtgtatcgaagtgtgttgcggagtagatgttagaatactaacaaactc +\n" ++
  "tattctcaacaagattaaccgacagattcaatctcgtggatggacgttcaacattga -\n" ++
  "cactaatttattccatgtcacacttttcgcatctttgttatgctatggttatttcat +\n" ++
  "ttcgcatatttttcttgcaaagttgggttgagctggctagattagccagccaatctt +\n" ++
  "ccttgaaaaagaggttgacgctgcaaggctctatacgcataatgcgccccgcaacgc +\n" ++
  "atataaaaaagttcttgctttctaacgtgaaagtggtttaggttaaaagacatcagt +\n" ++
  "cagcggcagcacgtttccacgcggtgagagcctcaggattcatgtcgatgtcttccg -\n" ++
  "aaccattccggttgactcaatgagcatctcgatgcagcgtactcctacatgaataga -\n" ++
  "tgcacgggttgcgatagcctcagcgtattcaggtgcgagttcgatagtctcagagtc -\n" ++
  "cgaacgagtcaatcagaccgctttgactctggtattactgtgaacattattcgtctc -\n" ++
  "gtattctcaacaagattaaccgacagattcaatctcgtggatggacgttcaacattg -\n" ++
  "gccttctccaaaacgtgttttttgttgttaattcggtgtagacttgtaaacctaaat +\n" ++
  "atatgaacgttgagactgccgctgagttatcagctgtgaacgacattctggcgtcta -\n" ++
  "taaaaaactaacagttgtcagcctgtcccgcttataagatcatacgccgttatacgt +\n" ++
  "tgtaaactaatgcctttacgtgggcggtgattttgtctacaatcttacccccacgta +\n" ++
  "ctgttgttcagtttttgagttgtgtataacccctcattctgatcccagcttatacgg +\n" ++
  "aaacaatttcagaatagacaaaaactctgagtgtaataatgtagcctcgtgtcttgc +\n" ++
  "tattggcttgctcaagcatgaactcaaggctgatacggcgagacttgcgagccttgt -\n" ++
  "ctatatgcgctcatacgatatgaacgttgagactgccgctgagttatcagctgtgaa -\n" ++
  "tctcaacgtaacactttacagcggcgcgtcatttgatatgatgcgccccgcttcccg +\n" ++
  "aggggcaaggaggatggaaagaggttgccgtataaagaaactagagtccgtttaggt +\n" ++
  "aggaggaactacgcaaggttggaacatcggagagatgccagccagcgcacctgcacg -\n" ++
  "ggtgttttgcgcaatgttaatcgctttgtacacctcaggcatgtaaacgtcttcgta -\n" ++
  "ttagcggatcctacctgacgctttttatcgcaactctctactgtttctccatacccg +\n" ++
  "ttttaaatttcctcttgtcaggccggaataactccctataatgcgccaccactgaca +\n" ++
  "ttgacctactacgccagcattttggcggtgtaagctaaccattccggttgactcaat -\n" ++
  "atagtctcagagtcttgacctactacgccagcattttggcggtgtaagctaaccatt -\n" ++
  "gtactagagaactagtgcattagcttatttttttgttatcatgctaaccacccggcg +\n" ++
  "aactcaaggctgatacggcgagacttgcgagccttgtccttgcggtacacagcagcg -\n" ++
  "atgcaattttttagttgcatgaactcgcatgtctccatagaatgcgcgctacttgat +\n" ++
  "cgctaggactttcttgttgattttccatgcggtgttttgcgcaatgttaatcgcttt -\n" ++
  "aacgcatacggtattttaccttcccagtcaagaaaacttatcttattcccacttttc +\n" ++
  "acagttatccactattcctgtggataaccatgtgtattagagttagaaaacacgagg +\n" ++
  "cgtagcgcatcagtgctttcttactgtgagtacgcaccagcgccagaggacgacgac -\n" ++
  "gaggtggctatgtgtatgaccgaacgagtcaatcagaccgctttgactctggtatta -\n" ++
  "ctacggtgggtacaatatgctggatggagatgcgttcacttctggtctactgactcg -\n" ++
  "cctgaaattcagggttgactctgaaagaggaaagcgtaatatacgccacctcgcgac +\n" ++
  "atgcatttttccgcttgtcttcctgagccgactccctataatgcgcctccatcgaca +\n" ++
  "tgctgaaaggaggaactatatgcgctcatacgatatgaacgttgagactgccgctga -\n" ++
  "tggatggacgttcaacattgaggaaggcataacgctactacctgatgtttactccaa -\n" ++
  "gcaaataatcaatgtggacttttctgccgtgattatagacacttttgttacgcgttt +\n" ++
  "aggcatgtaaacgtcttcgtagcgcatcagtgctttcttactgtgagtacgcaccag -\n" ++
  "cagggggtggaggatttaagccatctcctgatgacgcatagtcagcccatcatgaat +\n" ++
  "acgctaacgcagatgcagcgaacgctcggcgtattctcaacaagattaaccgacaga -\n" ++
  "gaagaccacgcctcgccaccgagtagacccttagagagcatgtcagcctcgacaact -\n" ++
  "taggcaccccaggctttacactttatgcttccggctcgtatgttgtgtggaattgtg +\n" ++
  "tcgataattaactattgacgaaaagctgaaaaccactagaatgcgcctccgtggtag +\n" ++
  "catcctcgcaccagtcgacgacggtttacgctttacgtatagtggcgacaatttttt +\n" ++
  "atgcgcaacgcggggtgacaagggcgcgcaaaccctctatactgcgcgccgaagctg +\n" ++
  "tatgaccgaacgagtcaatcagaccgctttgactctggtattactgtgaacattatt -\n"