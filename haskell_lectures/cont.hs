module Main where

import Control.Monad.Trans.Cont

square :: Int -> (Int -> a) -> a
square x foo = foo $ (\x1 -> (*) x1 $ \x2 -> x2)
