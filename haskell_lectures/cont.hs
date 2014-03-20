module Main where

import Control.Monad.Trans.Cont

square :: Int -> Int
square x = ((*) x) x

fib x :: Int -> Int
fib x = if (x < 2) then x else (fib (x-1)) + (fib (x-2))