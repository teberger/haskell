module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

square :: Int -> Int -> a
square = \x -> (*) $ (\x1 -> x1 x x)