module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

-- Continuation is typed as (a -> r) -> r
-- Cont a r

add_cont :: Int -> Int -> Cont a Int
add_cont x y = return (add x y)
