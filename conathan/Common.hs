module Common
  ( module Common
  , module Control.Monad
  , module Data.Bits
  , module Data.Char
  , module Data.List
  , module Data.Map
  , module Data.Word
  , module Debug.Trace
  , module Text.Printf
  ) where

-- import qualified Data.ByteString as BS
import Control.Monad ( forM_ )
import Data.Bits
import Data.Char ( chr, isAlphaNum, isAscii, isPrint, isPunctuation
                 , isSymbol, ord )
import Data.List ( sort, sortOn )
import Data.Map ( Map, fromList, (!) )
import Data.Word
import Debug.Trace
import Text.Printf

type Raw = [Word8]
type Base16 = [Char]
type Base64 = [Char]
