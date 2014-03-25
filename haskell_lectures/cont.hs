module Main where

import Control.Monad.Trans.Cont
import Control.Monad (when, join)
import Data.Char

-- Continuation is typed as (a -> r) -> r
-- Cont a r

one :: Int
one = 1

one_cps :: (Int -> a) -> a
one_cps = \f -> f 1

-- :t cont
--    = ((a -> r) -> r) -> Cont r a 

one_cps' :: Cont a Int
one_cps' = cont (\c -> c 1)

add_1 :: Int -> Int
add_1 x = x + 1

add1_cps :: Int -> ((Int -> a) -> a)
add1_cps x = \f -> f (x + 1)

add x y = x + y
add' x = \y -> x + y
add'' = \x -> \y -> x + y

op = \operator -> (\x -> (\y -> operator x y))

op' = \operator -> (\x -> 
                   (\y -> 
                   (\context -> context $ operator x y)))

--op' x y operator = \c -> c $ operator x y
add_cont = op' (+)
mult_cont = op' (*)

--fact ??
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

--now pulling out the context? 
fact' 0 = \c -> c 1
fact' n = \c -> fact' (n-1) $ \x -> c (x * n) -- <=== look familiar? 

-- :t return ? 
-- Monad m => a -> m a
fact'' 0 = return 1  
fact'' n = fact'' (n-1) >>= return . ((*) n)

fact''' 0 = return 1
fact''' n = do
  x <- fact'' (n-1)
  return (x * n)
  
--now what about fib?
--fib :: Int -> (Int -> a) -> a

--lambda style
fib 0 = \c -> c 0 
fib 1 = \c -> c 1
fib n = \c -> fib (n-1) $ \x -> 
              fib (n-2) $ \y -> 
              c (x + y)

-- in monadic form?
fib' :: (Num r) => Int -> (Int -> r) -> r
fib' 0 = return 0 
fib' 1 = return 1
fib' n = do
  x <- fib (n-1)
  y <- fib (n-2)
  return (x + y)

-- so, continuation functions are monadic in nature however, this is 
-- kind of a disappointment, because we hid the continuation
-- functions with the monadic style of code. Where did (\c -> c ...) go?
  
-- :t callCC  
-- k is a unit function  
foo n = callCC $ \k -> do
  let n' = n ^ 2 + 3
--  k n
  return (n' - 4)
  
fib'' 0 = callCC $ \k -> k 0
fib'' 1 = callCC $ \k -> k 1
fib'' n = callCC $ \k -> do
  x1 <- fib'' (n-1)
  x2 <- fib'' (n-2)
  k (x1 + x2)

-- k is our continuation function. It acts as a return from
-- the continuation monad.
realRoot n = callCC $ \k -> do
  when (n < 0) $ k "Error"
  k (show $ sqrt n)
  return "error" --note: this is never 'returned'

--mother of all monads! We can simulate our favorite monads with continuations.
-- consider the list monad:
listMonad = do
  a <- [1,2]
  b <- [10,20]
  return $ a+b
 

listMonad' = do
  a <- cont (\c -> join [c 1, c 2])
  -- orai
  b <- cont (\c -> [10,20] >>= c)
  return $ a+b

testList = listMonad
testList' = runCont listMonad' return

--more importantly, this shows us where the monadic values get unwrapped
--in relation to the cont monad

--lets write an String -> [Integer] parser using Cont
testString :: String
testString = "123 321 1 32" -- parse should yield: [123,321,1,32]

badString :: String
badString = "123 321 1 & 1" -- parse should yield: [123,321,1]

parse :: String -> Cont a [Int]
parse s = callCC $ \k -> do 
  (i,s') <- integer s 
  when (s' == "Error") $ k i >> return i:[]
  s'' <- spaces s'
  is <- parse s''
  return (i:is)
  
integer :: String -> Cont a (Int, String)
integer [] = callCC $ \exit1 -> exit1 (0,"Done")
integer s = do
  i <- cont $ (\c -> c $ read (takeWhile isDigit s))
  return (i, (dropWhile isDigit s))
  
isDigit' :: Char -> Cont a Int  
isDigit' c = return $ digitToInt c 

spaces :: String -> Cont a String
spaces s = cont (\c -> c $ dropWhile (== ' ') s)