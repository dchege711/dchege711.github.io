{-# OPTIONS_GHC -Wall #-}

module SonarSweep.SonarSweep (numIncreases, num3MeasurementIncreases) where

import Data.Maybe (isJust, catMaybes)
import Text.Read (readMaybe)

-- Computing `numIncreases` imperatively is rather straightforward, e.g.
--
-- ```py
-- prev_val = math.inf
-- count = 0
--
-- for line in s.split("\n"):
--   val = int(line)
--   if val > prev_val: count += 1
--   prev_val = val
--
-- return count
-- ```
--
-- How do I do it in Haskell? Variables are immutable, so I can't accumulate
-- to some value. Furthermore, [1] says, "If you think of a `Text` value as an
-- array of `Char` values (which it is not), you run the risk of writing
-- inefficient code." My mindset is still on `Text` as a `[Char]`. We'll see!
--
-- Pattern matching on lists looks promising. [2]
--
-- [1]: https://hackage.haskell.org/package/text-2.0/docs/Data-Text.html#g:24
-- [2]: https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/1-haskell-basics#functions-on-lists
numIncreases :: [String] -> Int
numIncreases [] = 0 -- The empty list does not have a delta
numIncreases [_] = 0 -- The single item list does not have a delta
numIncreases (x:y:zs) = total where
  xInt = readMaybe x :: Maybe Int
  yInt = readMaybe y :: Maybe Int
  contribution = if isJust xInt && isJust yInt && yInt > xInt then (1 :: Int) else (0 :: Int)
  total = contribution + numIncreases (y:zs)

num3MeasurementIncreases :: [String] -> Int
-- More on pattern matching on lists. [1] offers a more concise syntax than the
-- one I used in `numIncreases`. It takes advantage of the fact that pattern
-- matching starts from the top.
--
-- [1]: https://en.wikibooks.org/wiki/Haskell/Pattern_matching#Why_does_it_work_with_lists?
num3MeasurementIncreases (u:w:x:y:zs) = total where
  -- Skipping error handling is not remarkably shorter as we get a
  -- `[(a, String)]`, and the code seems less readable. Good on Haskell for not
  -- granting my request for a footgun.
  uInt = readMaybe u :: Maybe Int
  wInt = readMaybe w :: Maybe Int
  xInt = readMaybe x :: Maybe Int
  yInt = readMaybe y :: Maybe Int
  areAllValidInts = isJust uInt && isJust wInt && isJust xInt && isJust yInt

  -- Looks like I don't need `(0 :: Int)` when the `then` part is clear.
  prevWindow = if areAllValidInts then sum (catMaybes [uInt, wInt, xInt]) else 0
  currWindow = if areAllValidInts then sum (catMaybes [wInt, xInt, yInt]) else 0
  contribution = if currWindow > prevWindow then (1 :: Int) else 0

  total = contribution + num3MeasurementIncreases (w:x:y:zs)

num3MeasurementIncreases _ = 0 -- Any list with less than 4 items doesn't have a delta
