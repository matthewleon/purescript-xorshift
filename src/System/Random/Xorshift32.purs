module System.Random.Xorshift32 (Seed, seed, getSeed, int32) where

import Prelude

import Data.Int.Bits ((.^.), shl, shr)
import Data.Maybe (Maybe(..))

newtype Seed = Seed Int

instance showSeed :: Show Seed where
  show (Seed i) = "(Seed " <> show i <> ")"

seed :: Int -> Maybe Seed
seed 0 = Nothing
seed i = Just (Seed i)

getSeed :: Seed -> Int
getSeed (Seed i) = i

int32 :: Seed -> Seed
int32 (Seed x) =
  let x'   = x   .^. (x `shl` 13)
      x''  = x'  .^. (x' `shr` 17)
   in Seed (x'' .^. (x'' `shl` 5))
