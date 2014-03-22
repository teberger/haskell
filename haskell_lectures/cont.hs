module Main where

import Control.Monad.Trans.Cont

square :: Int -> (Int -> a) -> a
square x = (*) $ (\x1 -> x1 x)
