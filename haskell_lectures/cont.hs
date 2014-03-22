module Main where

import Control.Monad.Trans.Cont

square :: Int -> (Int -> Int)
square = (*)  
