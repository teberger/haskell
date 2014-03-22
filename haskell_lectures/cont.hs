module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

square :: Int -> (a -> Int) -> (Int -> Int) 
square = \x -> (*) $ \x1 -> x1 x  --(\x1 -> x1 x x)