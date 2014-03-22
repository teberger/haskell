module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

square :: ((Int -> Int) -> Int) -> ((Int -> Int) -> Int)
square = \f y -> div f y