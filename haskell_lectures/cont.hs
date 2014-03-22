module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

square :: Int -> Int
square = \x -> (*) $ \x1 -> id $ \x2 -> x <*> id $ \x3 -> x 
