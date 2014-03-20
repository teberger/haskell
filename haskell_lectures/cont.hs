module Main where

import Control.Monad.Trans.Cont

square :: Int -> (a -> Int) -> Int
square x foo = foo (((*) x) x)

fib :: Int -> Int
fib x = if (x < 2) 
        then x 
        else (fib (x-1)) + (fib (x-2))