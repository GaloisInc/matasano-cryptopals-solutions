module Common
  ( module Common
  , module Control.Monad
  , module Data.Bits
  , module Data.Char
  , module Data.List
  , module Data.Map.Strict
  , module Data.Monoid
  , module Data.Word
  , module Debug.Trace
  , module Text.Printf
  ) where

-- import qualified Data.ByteString as BS
import Control.Monad ( forM_, replicateM, when )
import Data.Bits
import Data.Char
import Data.List ( foldl', foldl1', scanl', sort, sortOn, transpose )
import Data.Map.Strict ( Map, fromList, (!) )
import Data.Maybe ( maybe )
import Data.Monoid ( (<>) )
import Data.Word
import Debug.Trace
import Text.Printf

----------------------------------------------------------------

type Raw = [Word8]
type Base16 = [Char]
type Base64 = [Char]

-- Copied from http://hackage.haskell.org/package/TraceUtils.
traceId' :: Show a => String -> a -> a
traceId' prefix x = trace (prefix++show x) x

-- I'm on the plane and I don't remember how Haskell's native
-- assertions work.
--
-- UPDATE: @Control.Exception.assert :: Bool -> a -> a@ is disabled
-- when compiling with optimization, or with @-fignore-asserts@.
assert :: String -> Bool -> a -> a
assert msg condition x | condition = x
                       | otherwise = error $ "Assertion failed: " ++ msg
