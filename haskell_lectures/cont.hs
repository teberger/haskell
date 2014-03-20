module Main where

import Control.Monad.Trans.Cont

square :: Int -> (Int -> a) -> a
square x foo = foo (((*) x) x)

fib :: Int -> (Int -> a) -> a
fib x foo = if (x < 2) 
            then x 
            else (fib (x-1)) + (fib (x-2))