module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

-- Continuation is typed as (a -> r) -> r
-- Cont a r

add :: Int -> Int -> Cont a Int
add = undefined