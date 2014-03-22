module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

-- Continuation is typed as (a -> r) -> r
-- Cont a r

add_1 :: Int -> Int
add_1 x = x + 1

add_1' :: Int -> ((Int -> a) -> a)
add_1' x = \f -> f (x + 1)

add_cont :: Int -> Int -> ((Int -> a) -> a)
add_cont x y = \f -> f (x + y)

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib x = fib (x-1) + fib (x-2)