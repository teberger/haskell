module Main where

import System.IO
import System.Environment
import Data.List
import System.Random
import Control.Applicative


main :: IO ()
main = do
  wordsFilename:_ <- getArgs
  handle <- openFile wordsFilename ReadMode
  contents <- hGetContents handle
  let allWords = splitBy '|' contents
  randomIndex <- getStdRandom (randomR (0, (length allWords) -1))
  let selected = allWords !! randomIndex
      g = Game 0 selected (map (const '?') selected) "" True
  putStrLn logo
  end <- loop g
  if (not ('?' `elem` (guessWord end)))
    then do {putStrLn ("Correct! Your word: " ++ (word end)); putStrLn "You Win!"}
    else putStrLn ("You Lose! Your word was: " ++ (word end) ++ ". Try again.")

--Main game loop
loop :: Game -> IO Game
loop g = do
  putStrLn . hangman . numGuesses $ g
  putStrLn ("Word to guess: " ++ (guessWord g))
  putStrLn ("Guessed letters: " ++ (guessedLetters g))
  putStr "\nEnter a letter(s): "
  hFlush stdout
  c <- getLine
  let gs = foldr (\c g' -> testLetter g' c) g c
  if (playing gs) then loop gs else return gs
  
--Game data operators
testLetter :: Game -> Char -> Game
testLetter g c = if and [c `elem` (word g), (numGuesses g) < maxGuesses]
                 then Game (numGuesses g)
                      (word g)
                      newGuess
                      (guessedLetters g)
                      ('?' `elem` newGuess)
                 else Game ((numGuesses g) + 1)
                      (word g)
                      (guessWord g)
                      (c:(guessedLetters g))
                      ((numGuesses g) + 1 < maxGuesses)
  where newGuess = replace c (word g) (guessWord g)

replace :: Char -> String -> String -> String
replace c word guess = let is = elemIndices c word
                       in foldr (\i s -> (take i s) ++ [c] ++ (drop (i+1) s)) guess is

--Utility function
splitBy :: Char -> [Char] -> [[Char]]
splitBy c ls = case dropWhile ((==) c) ls of
  "" -> []
  ls' -> w : splitBy c ls''
    where (w, ls'') = break ((==) c) ls'

--Game portion
data Game = Game {numGuesses :: Int,
                  word :: String,
                  guessWord :: String,
                  guessedLetters :: String,
                  playing :: Bool
                 } 
             
maxGuesses :: Int
maxGuesses = 10

-- Logo and hangman display
logo :: String
logo = "--------------------------------------------\n"++
       "| #  #   #   #   #  #### #   #   #   #   # |\n"++
       "| #  #  # #  ##  # #     ## ##  # #  ##  # |\n"++
       "| #### ##### # # # #  ## # # # ##### # # # |\n"++
       "| #  # #   # #  ## #   # #   # #   # #  ## |\n"++
       "| #  # #   # #   #  ###  #   # #   # #   # |\n"++
       "--------------------------------------------\n" ++
       "Your word has been chosen automatically.\n"

hangman :: Int -> String
hangman x = case x of
              0 ->  "Amount of wrong letters: 0\n\n" ++
                    "\n" ++ 
                    "\n" ++ 
                    "\n" ++ 
                    "\n" ++ 
                    "\n" ++ 
                    "\n" ++ 
                    "____________\n\n"

              1 -> "Amount of wrong letters: 1\n\n" ++
                   "\n" ++ 
                   "  |\n" ++ 
                   "  |\n" ++ 
                   "  |\n" ++ 
                   "  |\n" ++ 
                   "  |\n" ++ 
                   "__|_________\n\n"

              2 ->  "Amount of wrong letters: 2\n\n" ++
                    "  _______\n" ++ 
                    "  |\n" ++ 
                    "  |\n" ++ 
                    "  |\n" ++ 
                    "  |\n" ++ 
                    "  |\n" ++ 
                    "__|_________\n\n"
              3 -> "Amount of wrong letters: 3\n\n" ++ 
                   "  _______\n" ++ 
                   "  |/\n" ++ 
                   "  |\n" ++ 
                   "  |\n" ++ 
                   "  |\n" ++ 
                   "  |\n" ++ 
                   "__|_________\n\n"
              4 -> "Amount of wrong letters: 4\n\n" ++ 
                   "  _______\n" ++ 
                   "  |/   | \n" ++ 
                   "  |    O \n" ++ 
                   "  |\n" ++ 
                   "  |\n" ++ 
                   "  |\n" ++ 
                   "__|_________\n\n"

              5 -> "Amount of wrong letters: 5\n\n" ++ 
                   "  _______\n" ++ 
                   "  |/   | \n" ++ 
                   "  |    O \n" ++ 
                   "  |    |\n" ++ 
                   "  |    |\n" ++ 
                   "  |\n" ++ 
                   "__|_________\n\n"
              6 -> "Amount of wrong letters: 6\n\n" ++ 
                   "  _______\n" ++ 
                   "  |/   | \n" ++ 
                   "  |    O \n" ++ 
                   "  |   \\|\n" ++ 
                   "  |    | \n" ++ 
                   "  |\n" ++ 
                   "__|_________\n\n"

              7 -> "Amount of wrong letters: 7\n\n" ++ 
                   "  _______\n" ++ 
                   "  |/   | \n" ++ 
                   "  |    O \n" ++ 
                   "  |   \\|/\n" ++ 
                   "  |    | \n" ++ 
                   "  |\n" ++ 
                   "__|_________\n\n"

              8 ->  "Amount of wrong letters: 8\n\n" ++ 
                    "  _______\n" ++ 
                   "  |/   | \n" ++ 
                   "  |    O \n" ++ 
                   "  |   \\|/\n" ++ 
                   "  |    | \n" ++ 
                   "  |   /\n" ++ 
                   "__|_________\n\n"
              9 ->"Amount of wrong letters: 9\n\n" ++ 
                  "  _______\n" ++ 
                  "  |/   | \n" ++ 
                  "  |    O \n" ++ 
                  "  |   \\|/\n" ++ 
                  "  |    | \n" ++ 
                  "  |   / \\\n" ++ 
                  "__|_________\n\n"

              10 -> "Amount of wrong letters: 10\n\n" ++ 
                    "  _______\n" ++ 
                    "  |/   | \n" ++ 
                    "  |    X \n" ++ 
                    "  |   \\|/\n" ++ 
                    "  |    | \n" ++ 
                    "  |   / \\\n" ++ 
                    "__|_________\n\n"
              _ -> "end"

