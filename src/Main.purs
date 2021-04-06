module Main where

import Prelude
import Data.Array (mapMaybe)
import Data.Array.NonEmpty (NonEmptyArray, filter, range, zip, (!!))
import Data.Foldable (length, minimum)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.String (Pattern(..), split)
import Data.String.NonEmpty (fromString)
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = do
  input <- split (Pattern "\n") <$> readTextFile UTF8 "input.txt"
  log <<< show $ fromInput input
  where
  fromInput :: Array String -> Maybe Int
  fromInput [ tape, word ] = do
    t <- fromString tape
    w <- fromString word
    ?callSolveHere

  fromInput _ = Nothing

newtype Tape a
  = Tape (NonEmptyArray a)

distanceTo :: Int -> Int -> Int
distanceTo a b = abs $ a - b

findPositions :: forall a. Eq a => Tape a -> a -> Array Int
findPositions (Tape arr) element =
  map fst
    $ filter ((element == _) <<< snd)
    $ zip (range 0 (length arr - 1)) arr

solve :: forall a. Eq a => Tape a -> NonEmptyArray a -> Int -> Int -> Maybe Int
solve tape@(Tape arr) word i x = case word !! (i + 1) of
  Nothing -> Just 0
  Just element -> minimum $ mapMaybe goto $ findPositions tape element
    where
    goto newX = do
      distanceFromNext <- solve tape word (i + 1) newX
      pure $ distanceFromNext + x `distanceTo` newX
