module Main where

import Control.Monad.Trans.Cont

square :: Int -> (Int -> Int)
square = (*) $ \x1 x2 -> x1 x2 
