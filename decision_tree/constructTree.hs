module Main where

import Control.Monad
import System.Environment
import System.IO

type DnaSequence = (String, Bool)


--Note: Hasekll is a whitespace significant language. All lines starting indented from 'main' are executed 
--as part of the main function. Also, 'return' is not used to exit the function. The last statement
--in a Haskell function definition is the value that is returned. If there are any questions, email
--me at tberge01@cs.unm.edu
main :: IO ()
main = do
     -- Gets the arguments passed into the program. The program should be run with two arguments,
     -- the first being the training data the second being the validation set. The rest of the
     -- arguments will be ignored
     training:validation:_ <- getArgs

     --Reads each file in, splits the instances by line, and turns each line into a tuple where the
     --first part of the tuple is the DNA sequence and the second part is the classification
     trainSetString <- liftM (map (span (/= ' ')) . lines) . hGetContents =<< openFile training ReadMode
     validSetString <- liftM (map (span (/= ' ')) . lines) . hGetContents =<< openFile validation ReadMode

     -- tuple_func turns the " +" and " -" into boolean True or Falses
     -- Since we only have 2 classes, it will be more efficient to use
     -- T/F rather than Strings
     let tuple_func = (\(x,y) -> case y of
                                      " -" -> (x, False)
                                      " +" -> (x, True)
                         ) 
         train_set = map tuple_func trainSetString :: [DnaSequence] -- list of (String, Bool) tuples
         valid_set = map tuple_func validSetString :: [DnaSequence] -- list of (String, Bool) tuples
     
     
     return ()


