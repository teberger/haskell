module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

square :: Int -> ((Int -> a) -> a)
square x = (div x) . (\y -> y x)