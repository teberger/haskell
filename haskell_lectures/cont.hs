module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

square :: Int -> Int
square = \x -> (*) $ \x0 x1 x2 -> x0 x x --(\x1 -> x1 x x)