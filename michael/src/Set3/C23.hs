module Set3.C23 where


import Data.Bits
import Data.Word

import Set3.C21 (mersenneTwisterFromState, untemper)


-- this type is not instructive. but the input list is 624 elements long, and
-- is the first 624 elements of the output of a mersenne twister. we untemper
-- these outputs to get the original seed, which we feed into our new cipher,
-- which we then return the output of. so the input list is finite, but the
-- outputted list is infinite.
--
-- it's important to note that the cloned cipher will start 624 elements
-- after the original cipher. this is because we don't reconstruct the
-- initial state, but rather the second state (once the original 624
-- vector has been rewritten, completely, one time).
cloneCipher :: [Word32] -> [Word32]
cloneCipher xs = if length xs /= 624
                    then error "input must be length 624"
                    else newCipher
  where
    state = map untemper xs
    newCipher = mersenneTwisterFromState state
