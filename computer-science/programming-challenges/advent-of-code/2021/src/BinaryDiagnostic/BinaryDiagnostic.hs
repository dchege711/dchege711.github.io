{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module BinaryDiagnostic.BinaryDiagnostic
  ( BinaryDiagnostics (..),
    powerConsumption,
    lifeSupportRating,
  )
where

import Control.DeepSeq (NFData, rnf)
import Data.Bits (Bits (testBit))

-- Without the underscore prefix, I need to add `diagWidth` and `diagNums` to the
-- export list to avoid `Wunused-top-binds` [1]. The field names share the top
-- level namespace with ordinary variables and classes. [2] That's kinda
-- inconvenient.
--
-- [1]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html?highlight=unused-top-binds#ghc-flag--Wunused-top-binds
-- [2]: https://www.haskell.org/tutorial/moretypes.html#sect6.2
data BinaryDiagnostics = BinaryDiagnostics {diagWidth :: Int, diagNums :: [Int]}

-- For the `($!!)` operator to work on `BinaryDiagnostics`, we need to be an
-- instance of `NFData`. [1] ([2] for syntax)
--
-- [1]: https://hackage.haskell.org/package/deepseq-1.4.6.1/docs/Control-DeepSeq.html#t:NFData
-- [2]: https://stackoverflow.com/a/31478918/7812406
instance NFData BinaryDiagnostics where
  rnf BinaryDiagnostics {..} = rnf diagWidth `seq` rnf diagNums

-- | `toBitList n b` returns a `[Int]` representing the b-least significant
-- | bits of `n`, e.g. `toBitList 22 5 == [1, 0, 1, 1, 0]`.
toBitList :: Int -> Int -> [Int]
toBitList n numBits =
  map (\i -> if testBit n i then 1 else 0) [(numBits -1), (numBits -2) .. 0]

-- | `fromBitList ds` returns the `Int` formed when `ds` is treated like a bit
-- | representation of an integer, e.g. `fromBitList [1, 0, 1, 1, 0] == 22`.
fromBitList :: [Int] -> Int
fromBitList ds = fst $ foldr f (0, 1) ds
  where
    f d (s, powerOf2) = (s + powerOf2 * d, powerOf2 * 2)

-- | Flips any `0` to `1` and anything else to `0`. Assumes the input `[Int]` is
-- | just zeros and ones.
flipZerosAndOnes :: [Int] -> [Int]
flipZerosAndOnes = map (\num -> if num == 0 then 1 else 0)

-- | `bitFrequencies numBits nums` returns an `[Int]` of size `numBits` where
-- | the i'th element encodes the relative frequency of the bit at position
-- | `numBits - i`.
bitFrequencies :: Int -> [Int] -> [Int]
bitFrequencies numBits = foldr updateCumulativeFrequencies (replicate numBits 0)
  where
    zerosToNegativeOnes :: Int -> Int
    zerosToNegativeOnes i = if i == 0 then -1 else i

    updateCumulativeFrequencies :: Int -> [Int] -> [Int]
    updateCumulativeFrequencies num cumulativeBitFrequencies =
      zipWith
        (+)
        cumulativeBitFrequencies
        ( map
            zerosToNegativeOnes
            ( toBitList num numBits
            )
        )

-- | Convert frequency values to either 0 or 1 depending on whether the value
-- | is negative or positive. In case the value is zero, `tieValue` is used.
frequenciesToZeroOne :: Int -> [Int] -> [Int]
frequenciesToZeroOne tieValue = map f
  where
    f :: Int -> Int
    f freq
      | freq < 0 = 0
      | freq > 0 = 1
      | otherwise = tieValue

type BitFrequencySummarizer = Int -> [Int] -> [Int]

-- | `majorityBits numBits tieValue nums` returns an `[Int]` where the element
-- at index `i` is the most common bit at position `i`. In case of a tie,
-- `tieBreaker` is placed into `i`.
majorityBits :: BitFrequencySummarizer
majorityBits numBits nums =
  frequenciesToZeroOne 1 (bitFrequencies numBits nums)

-- | `majorityBits numBits tieValue nums` returns an `[Int]` where the element
-- at index `i` is the least common bit at position `i`. In case of a tie,
-- `tieBreaker` is placed into `i`.
minorityBits :: BitFrequencySummarizer
minorityBits numBits nums =
  frequenciesToZeroOne 0 (map (* (-1)) (bitFrequencies numBits nums))

-- The BinaryLiterals language extension adds syntactic sugar for `0b11001001`
-- [1]. But I didn't end up using binary representation. `Data.Bits` provides
-- utilities for working with `Int`, so why not? [2]
--
-- [1]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/binary_literals.html
-- [2]: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Bits.html

-- | Solution for Advent of Code Day 3: Part I.
powerConsumption :: BinaryDiagnostics -> Int
powerConsumption BinaryDiagnostics {..} =
  let majorities = majorityBits diagWidth diagNums
      gammaRate = fromBitList majorities
      epsilonRate = fromBitList (flipZerosAndOnes majorities)
   in gammaRate * epsilonRate

-- | `lastNumStanding nums width f positionToCheck` recursively finds an `Int`
-- | that matches the rules of AoC Day 3: Part II.
lastNumStanding :: [Int] -> Int -> BitFrequencySummarizer -> Int -> Int
lastNumStanding [] _ _ _ = 0
lastNumStanding [x] _ _ _ = x
lastNumStanding nums width f positionToCheck =
  let expectedValues = f width nums
      shouldBeSet = (expectedValues !! positionToCheck) == 1 -- (*/_＼)
      bitIndex = length expectedValues - positionToCheck - 1
      matchingNums = filter (\n -> testBit n bitIndex == shouldBeSet) nums
   in if null matchingNums
        then last nums
        else lastNumStanding matchingNums width f (positionToCheck + 1)

-- | Solution for Advent of Code Day 3: Part II.
lifeSupportRating :: BinaryDiagnostics -> Int
lifeSupportRating BinaryDiagnostics {..} =
  -- Partial application allows us to pass less than the full number of args to
  -- a function that takes multiple arguments.
  --
  -- [1]: https://wiki.haskell.org/Partial_application
  let f = lastNumStanding diagNums diagWidth
   in f majorityBits 0 * f minorityBits 0
