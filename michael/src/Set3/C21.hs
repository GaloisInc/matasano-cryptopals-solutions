module Set3.C21
  ( mersenneTwister
  , Word32(..)
  , temper
  , untemper
  , t0, t1, t2, t3
  , u0, u1, u2, u3
  ) where


import qualified Control.Monad.ST.Lazy as SL
import           Data.Bits
import           Data.STRef
import           Data.Word
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as M


{-

--------------------------------------------------------------------------------
-- Mersenne Twister 32-bit version
--------------------------------------------------------------------------------

The coefficients for MT19937 are:

(w, n, m, r) = (32, 624, 397, 31)
a = 9908B0DF16
(u, d) = (11, FFFFFFFF16)
(s, b) = (7, 9D2C568016)
(t, c) = (15, EFC6000016)
l = 18

f = 1812433253


# Initialization
x0 = seed
xi = f * (x_(i-1) `xor` (x_(i-1) >> (w-2))) + i


# Tempering
y := x ⊕ ((x >> u) & d)
y := y ⊕ ((y << s) & b)
y := y ⊕ ((y << t) & c)
z := y ⊕ (y >> l)


---

this implementation uses the neat (and new to me) ST.Lazy monad to
create an infinite stream of random Word32s. In practice you wouldn't
want this exact behavior since you wouldn't be able to support multiple
types, but it's pretty neat.

by running a loop forever and lazily emitting values we can return a
pure list of Word32s which the user can draw from. meanwhile we hold
a MVector of Word32s and iterate across it, looping around when we go
off the end. reads are modded in this way as well, so that we generate
the sequence in place.


-}

mersenneTwister :: Word32 -> [Word32]
mersenneTwister seed = SL.runST (runIt st0)
  where
    st0 = setupState seed

----
-- constants
----
w = 32
n = 624
m = 397
r = 31
--
a = 0x9908B0DF
u = 11
d = 0xFFFFFFFF
s = 7
b = 0x9D2C5680
t = 15
c = 0xEFC60000
--
l = 18
f = 1812433253
--
maskU :: Word32
maskU = foldl setBit 0 [r..(w-1)]
maskL :: Word32
maskL = foldl setBit 0 [0..(r-1)]


toSL = SL.strictToLazyST

----
-- functions
----
runIt :: Vector Word32 -> SL.ST s [Word32]
runIt p_st = do
  -- idx here is `k` from wikipedia's descriptions
  idxR <- toSL (newSTRef 0)
  st <- toSL (V.thaw p_st)
  let stLen = M.length st
      modReadSL n = toSL (M.read st (n `mod` stLen))
      modIncIdx = do
        idx <- toSL (readSTRef idxR)
        toSL (writeSTRef idxR ((idx+1) `mod` stLen))
        return idx
  sequence $ repeat $ do
    idx <- modIncIdx
    --
    val_idx_m <- modReadSL (idx+m)
    val_idx   <- modReadSL idx
    val_idx_1 <- modReadSL (idx+1)
    let newVal = val_idx_m `xor` (mulA ((val_idx .&. maskU) .|. (val_idx_1 .&. maskL)))
    --
    toSL (M.write st idx newVal)
    return (temper newVal)

setupState :: Word32 -> Vector Word32
setupState seed = V.fromList (take n (genState seed 1))
  where
    genState xPrev i =
      let xI = f * (xPrev `xor` (xPrev `shift` (2-w))) + i
       in xPrev : (genState xI (i+1))

temper :: Word32 -> Word32
temper = t3 . t2 . t1 . t0

t0 :: Word32 -> Word32
t0 v = v `xor` ((v `shift` (-u)) .&. d)
--
t1 :: Word32 -> Word32
t1 v = v `xor` ((v `shift`    s) .&. b)
--
t2 :: Word32 -> Word32
t2 v = v `xor` ((v `shift`    t) .&. c)
--
t3 :: Word32 -> Word32
t3 v = v `xor`  (v `shift` (-l))

untemper :: Word32 -> Word32
untemper = u0 . u1 . u2 . u3


--------------------------------------------------------------------------------
-- the rationale behind the numbers of shifts:
-- given that we shifted something right by `k` bits and XORed it
-- with itself:
-- at the start, we know that the first `k` bits are correct, since
-- they were XORed with all 0s (logical shift). the second group of
-- `k` bits depend on the first `k` bits, and we get those after 1
-- shift. the 3rd `k` bits depend on the 2nd, so we get those after 2
-- shifts. hence the rationale that we do n rounds where
-- `k * (1+n) > 32`.
--
-- see PrzemekM's comment on https://adriftwith.me/coding/2010/08/13/reversing-the-mersenne-twister-rng-temper-function/
--------------------------------------------------------------------------------


-- u = 11
u0 :: Word32 -> Word32
u0 v = f . f $ v
-- iterate 2x since 11*(1+1) < 32 < 11*(1+2)
  where
    f x = v `xor` ((x `shift` (-u)) .&. d)
--
-- s = 7
u1 :: Word32 -> Word32
u1 v = f . f . f . f $ v
-- iterate 4x times since 7*(1+3) < 32 < 7*(1+4)
  where
    f x = v `xor` ((x `shift` s) .&. b)
--
-- t = 15
u2 :: Word32 -> Word32
u2 v = f . f $ v
-- iterate 2x since 15*(1+1) < 32 < 15*(1+2)
  where
    f x = v `xor` ((x `shift` t) .&. c)
--
u3 :: Word32 -> Word32
u3 = t3
-- iterate 1x since 18*(1+0) < 32 < 18*(1+1)

mulA :: Word32 -> Word32
mulA x = case testBit x 0 of
           False ->  x `shift` (-1)
           True  -> (x `shift` (-1)) `xor` a
