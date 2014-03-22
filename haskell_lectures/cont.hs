module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

square :: ((Int -> Int) -> Int) -> ((Int -> Int) -> Int)
square = div . (\f -> f x)  . (\y -> y x)