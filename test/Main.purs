module Test.Main where

import Prelude

import Control.Monad.Eff (Eff, foreachE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.ST (pureST)
import Data.Array (replicate)
import Data.Array.ST (modifySTArray, thaw, unsafeFreeze)
import Data.Int (floor, toNumber)
import Data.Maybe (Maybe(..))
import Data.Unfoldable (iterateN)
import Partial.Unsafe (unsafePartial)
import System.Random.Xorshift32 (Seed, getSeed, int32, seed)

scipyChisquare :: forall e. Array Int -> Eff e Unit
scipyChisquare = scipyChisquareImpl <<< show

foreign import scipyChisquareImpl :: forall e. String -> Eff e Unit

bin :: Int -> Int -> Int -> Array Int -> Array Int
bin min max k xs = pureST do
  let minN = toNumber min
      range = (toNumber max) - minN
      binSize = range / (toNumber k)
  -- TODO: unsafeThaw
  bins <- thaw $ replicate k 0
  foreachE xs $ \x ->
    let index = floor (((toNumber x) - minN) / binSize)
    in void $ unsafePartial modifySTArray bins index (_ + 1)
  unsafeFreeze bins

randomSeed :: forall e. Eff (random :: RANDOM | e) Seed
randomSeed =
  seed <$> randomInt bottom top >>= case _ of
    Nothing -> randomSeed
    Just s  -> pure s

main :: Eff (console :: CONSOLE, random :: RANDOM) Unit
main = do
  seed <- randomSeed
  log $ "random seed: " <> show seed
  let arraySize = 1000000
      arr = getSeed <$> iterateN arraySize int32 seed
      bins = bin bottom top 256 arr
  scipyChisquare bins
