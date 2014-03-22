module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

-- Continuation is typed as (a -> r) -> r
-- Cont a r

one :: Int
one = 1

one_cps :: (Int -> a) -> a
one_cps = \f -> f 1

add_1 :: Int -> Int
add_1 x = x + 1

add1_cps :: Int -> ((Int -> a) -> a)
add1_cps x = \f -> f (x + 1)

add1_cps' :: ((Int -> Int) -> a) -> a
add1_cps' = \f -> f (\x -> (x + 1))

add_cont :: Int -> Int -> ((Int -> a) -> a)
add_cont x y = \f -> f (x + y)

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib x = fib (x-1) + fib (x-2)