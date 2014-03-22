module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

square :: Int -> ((Int -> Int) -> a)
square x = (div x) . (\y -> y x)