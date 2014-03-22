module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

square :: Num (t0 -> Int) => Int -> (t0 -> Int) -> t0 -> Int
square = \x -> (*) $ \x1 -> x  --(\x1 -> x1 x x)