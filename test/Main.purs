module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (unsafeFromString)
import Data.String.NonEmpty.CodeUnits (toNonEmptyCharArray)
import Effect (Effect)
import Main (solve, Tape(..))
import Partial.Unsafe (unsafePartial)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main =
  runTest do
    suite "diagonal" do
      let
        tape = Tape $ nonempty "ABCDEFGHIJKLMNO"
      test "AHOJ" do
        let
          word = nonempty "AHOJ"
        Assert.equal (Just 19) (solve tape word 0 0)
      test "FENA" do
        let
          word = nonempty "FENA"
        Assert.equal (Just 26) (solve tape word 0 0)
  where
  nonempty = toNonEmptyCharArray <<< (unsafePartial $ unsafeFromString)
