module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Console.Timer (Timer, time, timeEnd)
import Data.List (length, reverse)
import Data.List.Types (List(..), (:))
import Data.Tuple (Tuple(..))

import System.Random.Xorshift (RNGState, seed, int32)

main :: Eff (console :: CONSOLE) Unit
main = do
  timer :: Timer "generation" <- time
  let ints = randomInts 100 1000000
  timeEnd timer
  log $ "generated list of " <> show (length ints) <> " random ints."

  where
    randomInts :: Int -> Int -> List Int
    randomInts s length = reverse $ go (seed s) Nil 0
      where
        go :: RNGState -> List Int -> Int -> List Int
        go state accum index -- tail recursive, builds backward
          | (index < length) =
            case int32 state
              of (Tuple i state') -> go state' (i : accum) (index + 1)
          | otherwise = accum
