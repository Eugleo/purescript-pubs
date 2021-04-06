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
  log <<< show $ solveInput input
  where
  solveInput :: Array String -> Maybe Int
  solveInput [ pubs, beers ] = do
    p <- fromString pubs
    b <- fromString beers
    -- máme dva stringy, t a w, které odpovídají dvěma řádkům v souboru input.txt
    ?callSolveHere

  solveInput _ = Nothing

{-

data Pubs = ...

-}
{-

solve vrátí
- Nothing pokud nemůžeme daná piva vypít
- (Just l) pokud je můžeme vypít na nějaké cestě délky l

solve ... -> Maybe Int
solve ... = ...

-}
