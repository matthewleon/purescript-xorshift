module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Console.BrowserSpecific.Timer (time, timeEnd)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.List (List, length)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import System.Random.Xorshift32 (RNGState, seed, int32)

main :: Eff (console :: CONSOLE, exception :: EXCEPTION) Unit
main = do
  timer <- time "generation"
  let ints = randomInts 100 1000000
  timeEnd timer
  log $ "generated list of " <> show (length ints) <> " random ints."
  where
  randomInts :: Int -> Int -> List Int
  randomInts s = unfoldr go <<< Tuple (seed s)
    where
    go :: (Tuple RNGState Int) -> Maybe (Tuple Int (Tuple RNGState Int))
    go (Tuple rngState n)
      | n > 0     = Just $ (_ `Tuple` n - 1) <$> int32 rngState
      | otherwise = Nothing
