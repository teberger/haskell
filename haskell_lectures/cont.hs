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
add1_cps' = \f -> f $ \x -> (x + 1)

add x y = x + y
add' x = \y -> x + y
add'' = \x -> \y -> x + y
add''' = (\f -> (\x y -> f x y)) (+)

op = \operator -> (\x -> (\y -> operator x y))

op' = \operator -> (\x -> 
                   (\y -> 
                   (\context -> context $ operator x y)))

--op' x y operator = \c -> c $ operator x y

add_cont = op' (+)
mult_cont = op' (*)

--fib ??
--fib :: Int -> (Int -> a) -> a

--fact ??
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

--now pulling out the context? 
fact' 0 = \c -> c 1
fact' n = \c -> fact' (n-1) $ \x -> c (x * n) -- <=== look familiar? 

fact'' 0 = return 1
fact'' n = do
  x <- fact'' (n-1)
  return (x * n)
  
fact''' 0 = return 1  
fact''' n = return . ((*) n) =<< fact (n-1)

fib 0 = \c -> c 0 
fib 1 = \c -> c 1
fib n = \c -> fib (n-1) $ \x -> 
              fib (n-2) $ \y -> 
              c (x + y)

fib' :: (Num r) => Int -> (Int -> r) -> r
fib' 0 = return 0 
fib' 1 = return 1
fib' n = do
  x <- fib (n-1)
  y <- fib (n-2)
  return (x + y)

foo n = callCC $ \k -> do
  let n' = n ^ 2 + 3
  return (n' - 4)
  
fib'' 0 = callCC $ \k -> k 0
fib'' 1 = callCC $ \k -> k 1
fib'' n = callCC $ \k -> do
  x1 <- fib'' (n-1)
  x2 <- fib'' (n-2)
  k (x1 + x2)
