module Main where

import Control.Monad.Trans.Cont

square :: Int -> (Int -> Int)
square x = (*) $ (\x1 -> x1 x)
