module Util.MonadRandom
  ( module Control.Monad.Random
  , module Control.Monad.Random.Class
  , randomElem
  ) where

import Control.Monad.Random
import Control.Monad.Random.Class


randomElem :: RandomGen g => [a] -> Rand g a
randomElem xs = do
  idx <- getRandomR (0,length xs - 1)
  return (xs !! idx)
