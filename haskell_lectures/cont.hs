module Main where

import Control.Monad.Trans.Cont

square :: Int -> (Int -> a) -> a
square x foo =  $ \x1 -> (*) x1 $ \x2 -> foo x2
