module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

square :: Int -> Int
square x = mult x x