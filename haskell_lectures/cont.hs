module Main where

import Control.Monad.Trans.Cont

square :: Int -> (Int -> a)
square x = (*) x $ (\x1 -> x1 x)
