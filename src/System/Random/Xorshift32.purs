module System.Random.Xorshift32 (int32) where

import Data.Int.Bits ((.^.), shl, shr)

-- | Note: int32 0 == 0
int32 :: Int -> Int
int32 x =
  let x'   = x   .^. (x `shl` 13)
      x''  = x'  .^. (x' `shr` 17)
   in x'' .^. (x'' `shl` 5)
