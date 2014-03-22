module Main where

import Control.Monad.Trans.Cont

--square :: Int -> ((Int -> a) -> a)
square = \x -> (*) $ \x1 -> id $ \x2 -> id $ \x3 -> x 
