module Main where

import Control.Monad.Trans.Cont

square :: Int -> Int
square x = (*) x $ \x1 -> x
