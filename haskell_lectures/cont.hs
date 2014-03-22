module Main where

import Control.Monad.Trans.Cont

square x = (*) $ \x2 -> x2 x $ \x3 -> x3
