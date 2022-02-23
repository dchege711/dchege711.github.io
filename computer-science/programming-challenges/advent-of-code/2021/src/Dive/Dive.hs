{-# OPTIONS_GHC -Wall #-}

module Dive.Dive
  ( productOfFinalPosition,
    productOfFinalPositionWithNewIntepretation,
  )
where

-- One consideration in Haskell is the third-party library that I should use,
-- given that standard GHC seems quite lean. [1] should help in picking up
-- libraries that have lots of usage. For example, [2] helped me pick [3], which
-- uses Posix regex, and is fast, native, stable and lazy.
--
-- [1]: https://wiki.haskell.org/Category:Libraries
-- [2]: https://wiki.haskell.org/Regular_expressions
-- [3]: https://hackage.haskell.org/package/regex-tdfa

import Data.Maybe (fromJust, isJust, mapMaybe)
import Text.Read (readMaybe)
import Text.Regex.TDFA ( (=~) )

-- [1] suggests that I should be fine without deriving from the `Enum` data
-- type. Deriving from `Enum` helps me when I care about the mapping to an
-- underlying type, e.g. Int. [2]
--
-- [1]: https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/5-tokenizer-data-types#enumerated-data-types
-- [2]: https://stackoverflow.com/questions/6000511/better-way-to-define-an-enum-in-haskell/6000520
-- [3]: https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/2-algebraic-data-types
data DiveDirection = Up | Down | Forward deriving (Eq, Show)

diveDirection :: String -> Maybe DiveDirection
diveDirection "forward" = Just Forward
diveDirection "up" = Just Up
diveDirection "down" = Just Down
diveDirection _ = Nothing

parseSubMatches :: [String] -> Maybe (DiveDirection, Int)
parseSubMatches [dir, mag] =
  let direction = diveDirection dir
      magnitude = readMaybe mag :: Maybe Int
      parsedDirAndMag =
        if isJust direction && isJust magnitude
          then Just (fromJust direction, fromJust magnitude)
          else Nothing
   in parsedDirAndMag
parseSubMatches _ = Nothing

-- Surprised that defining the constant as `STEP_REGEX` does not compile:
--
--    Not in scope: data constructor ‘FORWARD’typecheck
--
-- Variable names must start with a lowercase letter, and anything that is
-- uppercase is interpreted as a Data Constructor. [1]
--
-- [1]: https://stackoverflow.com/a/28381111/7812406
stepRegex :: [Char]
stepRegex = "([a-z]+) ([0-9]+)" -- regex-tdfa doesn't have \d for digits

directionAndMagnitude :: String -> Maybe (DiveDirection, Int)
directionAndMagnitude s =
  -- The format is (beforeMatch, firstMatch, afterMatch, [subMatches])
  let (_, _, _, subMatches) = s =~ stepRegex :: (String, String, String, [String])
   in -- Haskell lists are ordinary single-linked lists. Except operations on the
      -- first element (e.g. prepend, get, remove), the rest of the operations
      -- (including getting the length and indexing) are linear-time.
      --
      -- [1]: https://wiki.haskell.org/How_to_work_on_lists
      parseSubMatches subMatches

applySign :: (DiveDirection, Int) -> Int
applySign (Up, i) = - i
applySign (Down, i) = i
applySign (Forward, i) = i

-- It's common in Haskell to use the apostrophe to signify a relationship to a
-- previously defined item. Similar to how apostrophes are used in math.
--
-- [1]: https://stackoverflow.com/a/5673954/7812406
applySign' :: Maybe (DiveDirection, Int) -> Int
-- Hlint saved me here. My initial implementation was:
--
--    if isJust m then applySign $ fromJust m else 0
--
-- ... and Hlint suggested:
--
--    maybe 0 applySign m
--
-- ... and further suggested dropping the `m` citing Eta reduction. This
-- reduction leads to the cleaner Pointfree style which omits the names of the
-- variables. Pointfree style puts the spotlight on composing functions (high
-- level), rather than shuffling data (low level). [1] [2].
--
-- [1]: https://wiki.haskell.org/Eta_conversion
-- [2]: https://wiki.haskell.org/Pointfree
-- [3]: https://www.schoolofhaskell.com/user/school/starting-with-haskell/introduction-to-haskell/4-higher-order-programming-and-type-inference#wholemeal-programming
applySign' = maybe 0 applySign

isForward :: (DiveDirection, Int) -> Bool
isForward (Forward, _) = True
isForward _ = False

cumulativeSums :: [Int] -> [Int]
cumulativeSums [] = []
cumulativeSums [x] = [x]
cumulativeSums [x, y] = [x, x + y]
cumulativeSums (x : y : zs) = x : cumulativeSums (x + y : zs)

productOfFinalPosition :: [String] -> Int
productOfFinalPosition steps =
  -- In general, use `where` for locally-defined functions, and `let` for
  -- intermediate values. [1]
  --
  -- I don't fully get the nuance in [2], but it might come in handy later as
  -- I get more experience with Haskell. A broad stroke would be `let` places
  -- fewer restrictions, and `where` is more readable, but `where` may obscure
  -- inefficient code that redefines local functions.
  --
  -- [1]: http://www.cse.unsw.edu.au/~cs3161/14s2/StyleGuide.html#sec-5-1-1
  -- [2]: https://wiki.haskell.org/Let_vs._Where
  let parsedSteps = mapMaybe directionAndMagnitude steps

      -- `fst` and `snd` are utility functions for the first and second members
      -- of a pair, respectively. [1]
      --
      -- [1]: https://en.wikibooks.org/wiki/Haskell/Lists_and_tuples

      -- How do I compute `horizontalPos` and `verticalPos` while iterating
      -- through `parsedSteps` once? From an imperative programming background,
      -- this looks pretty inefficient! Update: [1] suggests the `foldl`
      -- package.
      --
      -- [1]: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Foldable.html#g:18
      finalHorizontalPos = sum $ map applySign $ filter isForward parsedSteps

      -- The function composition operator is handy when inverting the filter
      -- predicate.
      --
      -- [1]: https://techoverflow.net/2014/01/03/haskell-invert-filter-predicate/
      finalVerticalPos = sum $ map applySign $ filter (not . isForward) parsedSteps
   in finalHorizontalPos * finalVerticalPos

productOfFinalPositionWithNewIntepretation :: [String] -> Int
productOfFinalPositionWithNewIntepretation steps =
  let parsedSteps = mapMaybe directionAndMagnitude steps
      finalHorizontalPos = sum $ map applySign $ filter isForward parsedSteps
      -- Hlint suggestion on using map once was illuminating. I had code like:
      --
      --    l = ([1..10] :: [Int])
      --    map even $ map negate l
      --
      -- ... for which Hlint suggested:
      --
      --    map (even . negate) l
      --
      -- ... and that makes sense. Composition strikes again!
      --
      -- That said, the (.) can be unwieldy because we normally read from left
      -- to right, but (.) composition is read from right to left. For extra
      -- readability, we can use the (>>>) operator from `Control.Arrow`, e.g.
      --
      --    map (negate >>> even) l
      --
      -- ... as it flips the (.) operator. [1]
      --
      -- [1]: https://byorgey.wordpress.com/2019/04/24/competitive-programming-in-haskell-basic-setup/
      aimDeltas =
        map
          (applySign' . (\x -> if (not . isForward) x then Just x else Nothing))
          parsedSteps

      -- How do I do cumulative sums, i.e. [1, 2, 3, 4] -> [1, 3, 6, 10]? The end
      -- result is a list, so maybe something to do with `map`, but how do I
      -- reference what came before? Maybe a standalone function with pattern
      -- matching?
      cumulativeAims = cumulativeSums aimDeltas

      forwardDeltas =
        map
          (applySign' . (\x -> if isForward x then Just x else Nothing))
          parsedSteps

      -- In other languages, moving from a list to a single value is referred to
      -- as a reduction. Haskell uses the term "folding". On lists, one can
      -- either recursively combine the 1st element with the result of combining
      -- the rest (right fold), or recursively combine the results of combining
      -- all but the last element with the last element (left fold). Some
      -- initial value is provided that is combined with the first item in the
      -- list. There are some nuances:
      --
      --   `foldr` lazily evaluates the recursive case of folding over the rest
      --   of the list. This allows it to handle computations on infinite lists
      --   that either produce some result without referencing the recursive
      --   case, or short-circuit (e.g. `||` short-circuits on the first
      --   `True`).
      --
      --
      --   `foldl` immediately calls itself with new params until it reaches the
      --   end of the list (and thus can't handle infinite loops). However, its
      --   tail recursiveness (the final result of the recursive call is the
      --   final result of the function itself) can be efficiently compiled as a
      --   as a loop.
      --
      --
      --   `foldl` does not evaluate the initial parameter before the recursive
      --   call is made. At the end of the list, we may end up with a gigantic
      --   expression that causes stack overflow. Haskell provides the `foldl'`
      --   function that forces evaluation of the initial parameter before
      --   making the recursive call.
      --
      -- From [1] [2]. [3] and [4] goes into way more detail, and are worth a
      -- closer read.
      --
      -- That said, I still don't know how to summarize `(zip forwardDeltas
      -- aimDeltas)`. `foldl` and `foldr` accept operators, but there's no
      -- operator for "increase depth value by aim multiplied by X". Seems like
      -- `foldMap` is the place to be, but WHAT IS A MONOID?
      --
      -- Update: Misread the docs. `fold` and `foldMap` need monoids, but
      -- `foldl` and `foldr` have examples with lambdas. Nice!
      --
      -- [1]: https://wiki.haskell.org/Fold
      -- [2]: https://wiki.haskell.org/Tail_recursion
      -- [3]: https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Foldable.html#overview
      -- [4]: https://www.schoolofhaskell.com/user/school/starting-with-haskell/introduction-to-haskell/6-laziness

      -- My version had:
      --
      --    (fst p * snd p)
      --
      -- ... and Hlint proposed the use of uncurry, which converts a curried
      -- function to a function on pairs. That's pretty neat.
      --
      -- [1]: https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:uncurry

      -- Lambda abstractions can also have multiple arguments. For example:
      --
      --    \x y z -> [x, 2 * y, 3 * z]
      --
      -- ... is an anonymous function that takes 3 arguments.
      --
      -- [1]: https://www.schoolofhaskell.com/user/school/starting-with-haskell/introduction-to-haskell/4-higher-order-programming-and-type-inference#anonymous-functions
      finalDepth =
        foldr
          (\p depthSoFar -> depthSoFar + uncurry (*) p)
          0
          (zip forwardDeltas cumulativeAims)
   in finalHorizontalPos * finalDepth
