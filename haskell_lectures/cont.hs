module Main where

import Control.Monad.Trans.Cont
import Control.Applicative

square :: Num a => _
square = \x -> (*) $ (\x1 -> x1 x $ (\x2 -> x2 x))