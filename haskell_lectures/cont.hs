module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

square :: Int -> ((a -> Int) -> Int)
square x = (div x) . (\y -> y x)