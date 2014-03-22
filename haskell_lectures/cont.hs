module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

square :: Int -> Int
square = \x -> (*) $ (\x1 -> x1 x) $ (\x2 -> x2 x)