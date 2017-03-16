module System.Random.Xorshift32 (RNGState, seed, int32) where

import Data.Int.Bits ((.^.), shl, shr)
import Data.Tuple (Tuple(..))

newtype RNGState = RNGState Int

-- | Note: seeding with 0 will result in only generating 0s.
seed :: Int -> RNGState
seed = RNGState

int32 :: RNGState -> Tuple Int RNGState
int32 (RNGState x) =
  let x'   = x   .^. (x `shl` 13)
      x''  = x'  .^. (x' `shr` 17)
      x''' = x'' .^. (x'' `shl` 5)
  in  Tuple x''' (RNGState x''')
