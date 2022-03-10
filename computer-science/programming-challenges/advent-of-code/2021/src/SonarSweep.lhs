%include polycode.fmt
---
date: 2022-02-18
domains:
- adventofcode.com
- github.com
- jhidding.github.io
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2021/src/SonarSweep/01-sonar-sweep/
title: 'AoC 2021 Day 01: Sonar Sweep'
weight: 1
---

{{< citation
  id="AoC2021-01"
  title="Day 1 - Advent of Code 2021: Sonar Sweep"
  url="https://adventofcode.com/2021/day/1"
  accessed="2022-02-18" >}}

\## Part One

*As the submarine drops below the surface of the ocean, it automatically
performs a sonar sweep of the nearby sea floor. On a small screen, the
sonar weep report (your puzzle input) appears: each line is a
measurement of the sea floor depth as the sweep looks further and
further away from the submarine.*

*The first order of business is to figure out how quickly the depth
increases, just so you know what you're dealing with - you never know if
the keys will get carried into deeper water by an ocean current or a
fish or something.*

*To do this, count **the number of times a depth measurement increases**
from the previous measurement. (There is no measurement before the
first measurement.)*

***How many measurements are larger than the previous measurement?***

\begin{code}
{-# OPTIONS_GHC -Wall #-}

module SonarSweep (numIncreases, num3MeasurementIncreases) where

import Data.Maybe (isJust, catMaybes)
import Text.Read (readMaybe)
\end{code}

Computing `numIncreases` imperatively is rather straightforward, e.g.

```py
prev_val = math.inf
count = 0

for line in s.split("\n"):
  val = int(line)
  if val > prev_val: count += 1
  prev_val = val

return count
```

How do I do it in Haskell? Variables are immutable, so I can't accumulate
to some value. Furthermore, {{% cite Data.Text %}} says, "If you think of a
`Text` value as an array of `Char` values (which it is not), you run the risk of
writing inefficient code." My mindset is still on `Text` as a `[Char]`. We'll
see! Pattern matching on lists looks promising. {{% cite YorgeyHaskellBasics %}}

\begin{code}
numIncreases :: [String] -> Int
numIncreases [] = 0 -- The empty list does not have a delta
numIncreases [_] = 0 -- The single item list does not have a delta
numIncreases (x:y:zs) = total where
  xInt = readMaybe x :: Maybe Int
  yInt = readMaybe y :: Maybe Int
  contribution = if isJust xInt && isJust yInt && yInt > xInt then (1 :: Int) else (0 :: Int)
  total = contribution + numIncreases (y:zs)
\end{code}

\## Part Two

*Considering every single measurement isn't as useful as you expected:
there's just too much noise in the data.*

{{% comment %}}

I think going forward, it'll be useful for me to guess what part two of
the problem will be, and see how my guess holds up.

In this case, taking rolling statistics is a technique for smoothening
out noise.

{{% /comment %}}

*Instead, consider sums of a **three-measurement sliding window**.*

*Your goal now is to count **the number of times the sum of measurements
in this sliding window increases** from the previous sum. Stop when
there aren't enough measurements left to create a new three-measurement
sum.*

*Consider sums of a three-measurement sliding window. **How many sums are
larger than the previous sum?***

More on pattern matching on lists. {{% cite HaskellWikiPatternMatching %}}
offers a more concise syntax than the one I used in `numIncreases`. It takes
advantage of the fact that pattern matching starts from the top.

\begin{code}
num3MeasurementIncreases :: [String] -> Int
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
\end{code}

\## Learning from Others' Solutions

Without the parsing of the `[String]` into an `[Int]`, my solution is
basically:

\begin{spec}
numIncreases :: [Int] -> Int
numIncreases (x:y:zs) = total where
  contribution = if y > x then (1 :: Int) else 0
  total = contribution + numIncreases (y:zs)
numIncreases _ = 0

num3MeasurementIncreases :: [Int] -> Int
num3MeasurementIncreases (u:w:x:y:zs) = total where
  contribution = if (w + x + y) > (u + w + x) then (1 :: Int) else 0
  total = contribution + num3MeasurementIncreases (w:x:y:zs)
num3MeasurementIncreases _ = 0
\end{spec}

{{% cite HiddingAoC2021-01 %}} basically does:

\begin{spec}
diff :: [Int] -> [Int]
diff (a1:a2:as) = a2 - a1 : diff (a2:as)
diff _ = []

numIncreases :: [Int] -> Int
numIncreases = length . filter (> 0) . diff

slidingSum :: [Int] -> [Int]
slidingSum (a1:a2:a3:as) = a1 + a2 + a3 : slidingSum (a2:a3:as)
slidingSum _ = []

num3MeasurementIncreases :: [Int] -> Int
num3MeasurementIncreases = numIncreases . slidingSum
\end{spec}

Comparing with {{% cite HiddingAoC2021-01 %}}'s solution, mine has hints
of imperative programming. They are working with whole lists, while I'm
more concerned about data shuffling.

{{% cite LeAoC2021-01 %}} notes that combining `drop` and `zipWith`
gives us a way of working with consecutive values:

\begin{spec}
numIncreases :: [Int] -> Int
numIncreases xs = length (filter (== True) (zipWith (<) xs (drop 1 xs)))
\end{spec}

{{% comment %}}

I still have a lot of ground to cover before I shift how I think about
programs. {{% cite HiddingAoC2021-01 %}}'s approach was not on my radar.
Maybe if I wasn't pre-occupied with `[String] -> [Int]` parsing, I'd
have considered something that was more functional-oriented.

{{% /comment %}}

{{% cite HiddingAoC2021-01 %}} goes further and notes that for
`num3MeasurementIncreases`, the middle terms in the finite difference
drop out, and therefore:

\begin{spec}
diff3 :: [Int] -> [Int]
diff3 (a1:a2:a3:a4:as) = a4 - a1 : diff3 (a2:a3:as)
diff3 _ = []

num3MeasurementIncreases :: [Int] -> Int
num3MeasurementIncreases = length . filter (> 0) . diff3
\end{spec}

{{% comment %}}

My `num3MeasurementIncreases` has `if (w + x + y) > (u + w + x)`, and it
didn't occur to me that I can omit `w + x` from both sides of the
inequality check. Huh!

{{% /comment %}}

{{% cite LeAoC2021-01 %}} uses the same idea, comparing `[(a1, a4), (a2,
a5), ...]`:

\begin{spec}
num3MeasurementIncreases :: [Int] -> Int
num3MeasurementIncreases xs = length (filter (== True) (zipWith (<) xs (drop 3 xs)))
\end{spec}

`zipWith` is pretty handy!

I also like the `(x1:x2:x3:xs)` convention over `(w:x:y:zs)`. The former
is more readable.

\## References

1. {{< citation
  id="HiddingAoC2021-01"
  title="Advent of Code 2021: Day 1: Sonar Sweep"
  url="https://jhidding.github.io/aoc2021/#day-1-sonar-sweep"
  accessed="2022-02-20" >}}

1. {{< citation
  id="LeAoC2021-01"
  title="advent-of-code-2021/reflections.md at master · mstksg/advent-of-code-2021"
  url="https://github.com/mstksg/advent-of-code-2021/blob/master/reflections.md#day-1"
  accessed="2022-02-22" >}}

1. {{< citation
  id="Data.Text"
  title="Data.Text"
  url="https://hackage.haskell.org/package/text-2.0/docs/Data-Text.html#g:24"
  date="2022-02-18" >}}

1. {{< citation
  id="YorgeyHaskellBasics"
  author="Brent Yorgey"
  title="Haskell Basics: Functions on Lists"
  url="https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/1-haskell-basics#functions-on-lists"
  date="2022-02-18" >}}

1. {{< citation
  id="HaskellWikiPatternMatching"
  title="Pattern Matching: Why does it work with lists?"
  url="https://en.wikibooks.org/wiki/Haskell/Pattern_matching#Why_does_it_work_with_lists?"
  date="2022-02-18" >}}
