%include polycode.fmt
---
title: "AoC 2021 Day 08: Seven Segment Search"
date: 2022-03-07
weight: 8
---

{{< citation
    id="AoC2021-08"
    title="Day 8 - Advent of Code 2021"
    url="https://adventofcode.com/2021/day/8"
    author="Eric Wastl"
    accessed="2022-03-07" >}}

\## Part I Description

*You barely reach the safety of the cave when the whale smashes into the cave
mouth, collapsing it. Sensors indicate another exit to this cave at a much
greater depth, so you have no choice but to press on.*

*As your submarine slowly makes its way through the cave system, you notice that
the four-digit seven-segment displays in your submarine are malfunctioning;
they must have been damaged during the escape. You'll be in a lot of trouble
without them, so you'd better figure out what's wrong.*

*Each digit of a seven-segment display is rendered by turning on or off any of
the seven segments named `a` through `g`:*

```md
  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
```

*So, to render a `1`, only segments `c` and `f` would be turned on; the rest
would be off. To render a `7`, only segments `a`, `c`, and `f` would be turned
on.*

*The problem is that the signals which control the segments have been mixed up
on each display. The submarine is still trying to display numbers by producing
output on signal wires `a` through `g`, but those wires are connected to
segments randomly. Worse, the wire/segment connections are mixed un separately
for each four-digit display! (All of the digits within a display use the same
connections, though.)*

*So, you might know that only signal wires `b` and `g` are turned on, but that
doesn't mean segments `b` and `g` are turned on: the only digit that uses two
segments is `1`, so it must mean segments `c` and `f` are meant to be on. With
just that information, you still can't tell which wire (b/g) goes to which
segment (c/f). For that, you'll need to collect more information.*

*For each display, you watch the changing signals for a while, make a note of
all ten unique signal patterns you see, and then write down a single four-digit
output value (your puzzle input). Using the signal patterns, you should be able
to work out which patterns corresponds to which digit.*

*For example, here's what you might see in a single entry in your notes:*

```txt
acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab || cdfeb fcadb cdfeb cdbaf
```

*Each entry consists of ten unique signal patterns, a `||` delimiter, and
finally the four-digit output value. Within an entry, the same wire/segment
connections are used (but you don't know what the connections actually are). The
unique signal patterns correspond to the ten different ways the submarine tries
to render a digit using the current wire/segment connections. Because `7` is the
only digit that uses three segments, `dab` in the above example means that to
render `7`, signal lines `d`, `a`, and `b` are on. Because `4` is the only digit
that uses four segments, `eafb` means that to render a `4`, signal lines `e`,
`a`, `f`, and `b` are on.*

*Using this information, you should be able to work out which combination of
signal wires corresponds to each of the ten digits. Then, you can decode the
four digit output value. Unfortunately, in the above example, all of the digits
in the output value `cdfeb fcadb cdfeb cdbaf` use five segments and are more
difficult to deduce.*

*For now, focus on the easy digits. Because the digits `1`, `4`, `7`, and `8`
each use a unique number of segments, you should be able to tell which
combinations of signals correspond to those digits.*

***In the output values (the part after the `||` on each line), how many times
do digits `1`, `4`, `7`, or `8` appear?***

\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module AoC2021.SevenSegmentSearch
    (
        SevenSegmentDisplay(..),
        numOf1478AppearancesInOutput,
        sumOfOutputValues
    )
where

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Data.Foldable (Foldable(foldl'))
import Data.Char (ord)

\end{code}

\## Input Representation

The line `be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb || fdgacbe
cefdb cefbgd gcbe` has `cfbegad` matching with `fdgacbe` in the output value, so
I need a representation that allows those two to be linked. Sorting the
characters is sufficient as it gives `abcdefg` in both cases.

The ten signal patterns are in no particular order, so a `[String]` will do. The
output values do not need to be in any particular order, so a `[String]` will
also do.

\begin{code}

data SevenSegmentDisplay = SevenSegmentDisplay{
    uniquePatterns :: [IntSet.IntSet], outputValues :: [IntSet.IntSet]} deriving Show

\end{code}

However, the [solution for Part II]({{< ref "#part-ii-solution" >}}) makes use
of set operations, e.g. intersection and subtraction, and therefore, using a
`[Set Char]` instead of a `[String]` makes more sense. [Even better, an
`[IntSet]`](#PatriciaTreesSection).

\## Part I Solution

\begin{code}

numActiveSegmentsToDigits :: IntMap.IntMap [Int]
numActiveSegmentsToDigits = IntMap.fromList
    [(6, [0, 6, 9]), (2, [1]), (5, [2, 3, 5]), (4, [4]), (3, [7]), (7, [8])]

nonAmbiguousLengths :: IntSet.IntSet
-- There is also `Map.keysSet` but that returns an
nonAmbiguousLengths = IntSet.fromList $ IntMap.keys $
    IntMap.filter (\t -> length t == 1) numActiveSegmentsToDigits

\end{code}

The `containers` package provides `IntMap` and `IntSet` in addition to the
general `Map` and `Set` data structures. <a id="PatriciaTreesSection"></a> {{%
cite containersHaskell %}} This distinction is motivated by {{% cite
Okasaki1998 %}}'s work on finite maps that are based on {{% cite Morrison1968
%}}'s Patricia trees, instead of the usual base of balanced binary search trees.
While both bases have fast lookups and inserts, Patricia trees have fast merges
of two containers. {{% cite Okasaki1998 %}}

{{% comment %}}

I've been getting the vibe that Haskell is more explicit in its connection to
academia, e.g. foundational papers being linked from API docs, and library
writers and maintainers being faculty in CS departments.

{{% /comment %}}

\begin{code}

numOf1478AppearancesInOutput :: [SevenSegmentDisplay] -> Int
numOf1478AppearancesInOutput = foldr f 0 where
    f :: SevenSegmentDisplay -> Int -> Int
    f SevenSegmentDisplay{ outputValues=outputs } prevSum =
        prevSum + length (
            filter
            (\s -> IntSet.member (IntSet.size s) nonAmbiguousLengths)
            outputs)

\end{code}

Pattern-matching using `SevenSegmentDisplay{ outputValues=outputValues }` leads
to a `Wname-shadowing` HLint warning on the second `outputValues`. {{% cite
LeventErkok2020 %}} notes that either of the `NamedFieldPuns` or
`RecordWildCards` extensions allows shadowing of field names. However, adding
either language extension results in HLint warning that the `LANGUAGE` pragma is
unused.

{{% comment %}}

Compared to other Part I's, this one felt too straightforward. Most of the
difficulty was in [using `parsec` to parse the input line]({{< ref
"/computer-science/programming-challenges/advent-of-code/2021/src/AoC2021InputParser#day-08-seven-segment-search"
\>}}).

{{% /comment %}}

\## Part II Description

{{% priors %}}

Part II might feature additional information to distinguish additional digits.
Maybe the output values follow some pattern, e.g. the digits are always
increasing from right to left, not all digits are possible for a given output,
etc.

Update: I was wrong. We *do* have enough information to deduce all of the
digits. I don't see how this is always possible.

{{% /priors %}}

*Through a little deduction, you should now be able to determine the remaining
digits. Consider again the first example above:*

```txt
acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab || cdfeb fcadb cdfeb cdbaf
```

*After some careful analysis, the mapping between signal wires and segments only
make sense in the following configuration:*

```md
 dddd
e    a
e    a
 ffff
g    b
g    b
 cccc
```

*So, the unique signal patterns would correspond to the following digits:*

```md
acedgfb: 8
cdfbe: 5
gcdfa: 2
fbcad: 3
dab: 7
cefabd: 9
cdfgeb: 6
eafb: 4
cagedb: 0
ab: 1
```

*Then, the four digits of the output value can be decoded:*

```md
cdfeb: 5
fcadb: 3
cdfeb: 5
cdbaf: 3
```

*Therefore, the output value for this entry is `5353`.*

*For each entry, determine all of the wire/segment connections and decode the
four-digit output values. **What do you get if you add up all of the output
values?***

\## Part II Solution

In the non-jumbled up case, the matching of digits to segments is:

```md
1 -   c  f
4 -  bcd f
7 - a c  f
8 - abcdefg

0 - abc efg
2 - a cde g
3 - a cd fg
5 - ab d fg
6 - ab defg
9 - abcd fg
```

From Part I, the jumbled representations of `1`, `4`, `7`, and `8` can be
identified by counting the number of segments. We can go further and note that
there are shared segments, for example, `0` and `1` both have segments `c` and
`f`. So, solving Part II comes down to building up from the base provided by
`1`, `4`, `7`, and `8`.

The union of the active segments in `147` is `abcdf`, but that doesn't seem
helpful. `8` isn't helpful because it has all of the segments active. The union
of `1x`'s segments (for \\(x \in [4, 7, 8]\\)) doesn't seem helpful because it
will be the same as the active segments for \\(x\\). The union of `47`'s active
segments is `abcdf`, which also doesn't correspond to a digit.

If we go one level deeper, we might deduce an additional digit. With regard to
`x`'s active segments,  \\(x \in [1, 4, 7, 8]\\), being a subset of another
number `y`'s active segments, \\(y \in [0, 2, 3, 5, 6, 9]\\):

```md
1: 0, 3, 9
4: 9
7: 0, 3, 9
```

Nothing jumps out yet. Maybe looking at `x`s and `y`s that share segments might
be illuminating? `1`'s `cf` shares at least one active segment with all `y`s
and therefore given that the union of `1x`'s active segments is the same as that
of `x`, proceeding further doesn't seem useful. Maybe looking at non-shared
segments between `x`s and `y`s helps? Nah, that wouldn't provide additional
info than what I got from the shared segments analysis.

Maybe I can do something with the `y`s:

```md
0 - abc efg (6)
6 - ab defg (6)
9 - abcd fg (6)

2 - a cde g (5)
3 - a cd fg (5)
5 - ab d fg (5)
```

The union of `069`'s active segments is `abcdefg`, and so is the union of
`235`'s active segments, so that's not helpful.

The intersection of `069`'s active segments is `abfg`, and the complement of
this intersection is `cde`; nothing useful yet.

{{% comment %}}

I'm mostly talking in set terminology, so maybe instead of using `String` to
represent a display digit, I should have used a `Set Char`.

{{% /comment %}}

The intersection of `253`'s active segments is `adg`, and the complement of this
intersection is `bcef`; nothing useful yet.

The union of the previous two intersections (`abfg` and `adg`) is `abdfg`, and
for once we have something useful as that equals to `5`'s active segments! The
complement of `abdfg` is `ce`, but that's not useful.

Of `23`'s active segments, the union is `acdefg`, the complement of the union is
`b`, the intersection is `acdg`, and the complement of this intersection is
`bef`. None of these look helpful.

The knowns and unknowns are currently:

```md
8 - abcdefg (7)

1 -   c  f  (2)
4 -  bcd f  (4)
7 - a c  f  (3)
5 - ab d fg (5)

0 - abc efg (6)
6 - ab defg (6)
9 - abcd fg (6)

2 - a cde g (5)
3 - a cd fg (5)
```

I had already computed combinations of `147`, but maybe there's new info now
that `5` is also known.

The unknowns, \\([0, 6, 9, 2, 3]\\) have at least 5 active segments, so taking
operations which give at least 5 elements will be most useful.

The union of `15`s active segments is `abcdfg`, which matches the active
segments of `9`. Sweet!

The union of `45`'s active segments is `abcdfg`, which we've already determined
to be `9`. The union of `75`'s active segments doesn't yield anything new
either.

The knowns and unknowns are currently:

```md
8 - abcdefg (7)

1 -   c  f  (2)
4 -  bcd f  (4)
7 - a c  f  (3)
5 - ab d fg (5)
9 - abcd fg (6)

0 - abc efg (6)
6 - ab defg (6)

2 - a cde g (5)
3 - a cd fg (5)
```

The difference between `0` and `6` is that the former has a `c`, while the
latter has a `d`. Subtracting `5 abdfg` from `9 abcdfg` gives `c`, and therefore
distinguishes `0` from `6`.

The difference between `2` and `3` is that the former has an `e`, while the
latter has an `f`. The complement of the union of `14759`'s active segments is
`e`, and that can be used to distinguish `2` from `3`.

{{% comment %}}

Tedious exercise, but rewarding in the end. I feel like Sherlock Holmes.

{{% /comment %}}

{{% cite Mazon2021-08 %}} has simpler logic though. For convenience, here is the
non-scrambled mapping:

```md
1 -   c  f  (2)
4 -  bcd f  (4)
7 - a c  f  (3)
8 - abcdefg (7)

0 - abc efg (6)
6 - ab defg (6)
9 - abcd fg (6)

2 - a cde g (5)
3 - a cd fg (5)
5 - ab d fg (5)
```

\\([1, 4, 7, 8]\\) are identifiable from their unique lengths. <a
id="Mazon2021-08-LogicDeduction"></a> `6` is the 6-segment digit digit that does
not include `1`. `9` is the 6-segment digit that includes `4`. `0` is the
remaining 6-segment digit. `3` is the 5-segment digit that includes `1`. `5`
is the 5-segment digit that's included in `6`. `2` is the remaining 5-segment
digit. {{% cite Mazon2021-08 %}}

{{% comment %}}

The major shortcoming of how I went about deducing the mapping was thinking in
terms of unions and intersections, and not considering subsets. Once I found a
working approach, I hastened to implement it and call it a day.

{{% /comment %}}

\begin{code}

type Segment = IntSet.IntSet
type SegmentsMapping = Map.Map Segment Int

allSegmentsActive :: IntSet.IntSet
allSegmentsActive = (IntSet.fromList . map ord) "abcdefg"

deduceMappings :: [Segment] -> SegmentsMapping
deduceMappings input =
    Map.fromList [(repr0, 0), (repr1, 1), (repr2, 2), (repr3, 3), (repr4, 4), (repr5, 5), (repr6, 6), (repr7, 7), (repr8, 8), (repr9, 9)] where
        getSegmentsOfSize :: Int -> [Segment]
        getSegmentsOfSize n = filter (\s -> IntSet.size s == n) input

        repr1 = head (getSegmentsOfSize 2)
        repr4 = head (getSegmentsOfSize 4)
        repr7 = head (getSegmentsOfSize 3)
        repr8 = allSegmentsActive

        reprs069 = getSegmentsOfSize 6
        intersection069 = foldl' IntSet.intersection allSegmentsActive reprs069

        reprs235 = getSegmentsOfSize 5
        intersection235 = foldl' IntSet.intersection allSegmentsActive reprs235

        repr5 = IntSet.union intersection069 intersection235
        repr9 = IntSet.union repr1 repr5

        reprs06 = filter (/= repr9) reprs069
        difference95 = IntSet.difference repr9 repr5

        repr6 = head (filter (IntSet.disjoint difference95) reprs06)
        repr0 = head (filter (/= repr6) reprs06)

        union14579 = foldl' IntSet.union IntSet.empty [repr1, repr4, repr5, repr7, repr9]
        complement14579 = IntSet.difference allSegmentsActive union14579

        reprs23 = filter (/= repr5) reprs235
        repr3 = head (filter (IntSet.disjoint complement14579) reprs23)
        repr2 = head (filter (/= repr3) reprs23)


parseInt :: SegmentsMapping -> [Segment] -> Int
parseInt mapping = foldl' (\acc segment -> acc * 10 + (mapping Map.! segment)) 0

sumOfOutputValues :: [SevenSegmentDisplay] -> Int
sumOfOutputValues = foldl' (\acc display -> acc + extractOutputValue display) 0 where
    extractOutputValue :: SevenSegmentDisplay -> Int
    extractOutputValue SevenSegmentDisplay{ .. } = parseInt (deduceMappings uniquePatterns) outputValues

\end{code}

{{% comment %}}

`parseInt` adapted from [AoC 21 #03: Binary Diagnostic > Converting Binary
Representation to Decimal]({{< ref
"/computer-science/programming-challenges/advent-of-code/2021/src/BinaryDiagnostic/03-binary-diagnostic#converting-binary-representation-to-decimal"
\>}}).

{{% /comment %}}

{{% cite Mazon2021-08 %}} approaches the problem differently. They note that the
search space is small enough, \\(7! = 5{,}040\\), that there is [no need to use
first-order logic](#Mazon2021-08-LogicDeduction).

{{% open-comment %}}

Why is the search space \\(7!\\) instead of \\(6!\\), given that we already know
the representations of \\([1, 4, 7, 8]\\)?

Answer: We're looking at a seven-segment display.

Why \\(7!\\) though. \\(7!\\) is the number of ways \\(7\\) items can be
ordered. I don't understand why the search space also corresponds to this.

{{% /open-comment %}}

```hs
-- From https://xn--sant-epa.ti-pun.ch/posts/2021-12-aoc/day08.html

type Segment = Int
newtype Observation = Observation { view :: [Segment] }

type Wire = Int
newtype Digit = Digit [Wire] deriving (Eq, Ord)
```

{{% comment %}}

So far I've been using `data` to define custom types; `newtype` is new to me.

{{% /comment %}}

The syntax and usage of `newtype` and `data` are identical. Replacing `newtype`
with `data` will compile and most probably work, but `data` can only be replaced
with `newtype` if the type has exactly one constructor with exactly one field
inside it. The restrictions for `newtype` imply that the new type and the type
of field are in direct correspondence (isomporphic), and therefore after the
type is checked at compile time, at run time the two types can be treated
essentially the same, without the overhead or indirection normally associated
with a data constructor. {{% cite haskellWikiNewtype %}}

{{% comment %}}

Note that there is more subtle reasoning  on the nuance between `newtype` and
`data` (e.g. why not use `newtype` everywhere you can?) in
{{% cite haskellWikiNewtype %}} and in [A Gentle Introduction to Haskell: Types,
Again](https://www.haskell.org/tutorial/moretypes.html#sect6.1). This nuance
doesn't seem useful to me at this stage.

{{% /comment %}}

```hs
-- From https://xn--sant-epa.ti-pun.ch/posts/2021-12-aoc/day08.html

combine :: Observation -> Digit
combine = Digit . sort . view
```

This syntax is possible because `Observation { view :: [Segment] }` creates an
accessor function called `view` of type `Observation -> [Segment]` {{% cite
wikiHaskellNamedFields %}}. `Segment` and `Wire` are synonyms for `Int`, and are
thus entirely compatible, and that's why `Digit` can accept a `[Segment]`
{{% cite haskellWikiTypeSynonym %}}.

```hs
-- From https://xn--sant-epa.ti-pun.ch/posts/2021-12-aoc/day08.html

reference :: [Digit]
reference = map (combine . readObservation)
    [ "abcefg", "cf", "acdeg", "acdfg", "bcdf"
    , "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"
    ]

solve :: [Observation] -> [Observation] -> [Int]
solve obsDigits obsDisplay =
    let permute p = map (combine . Observation . map (p !!) . view)
        Just perm = find ((== sort reference) . sort . flip permute obsDigits)
                    (permutations [0..6])
    in map (fromJust . (`elemIndex` reference)) (permute perm obsDisplay)
```

{{% open-comment %}}

I don't yet follow why `solve` works, and I don't think dissecting it further
is worthwhile. Maybe I can leave it at "exhaustive search is feasible for this
problem"?

{{% /open-comment %}}

{{% comment %}}

New API: `flip :: (a -> b -> c) -> b -> a -> c`, where `flip f` takes its
(first) two arguments in the reverse order of `f`, e.g.

```hs
 >>> flip (++) "hello" "world"
"worldhello"
```

{{% cite Prelude %}}

{{% /comment %}}

\## References

1. {{< citation
    id="containersHaskell"
    title="containers: Assorted concrete container types"
    url="https://hackage.haskell.org/package/containers"
    url_2="https://haskell-containers.readthedocs.io/en/latest/"
    url_3="https://github.com/haskell-perf/sets"
    accessed="2022-03-13" >}}

1. {{< citation
    id="Okasaki1998"
    title="Fast Mergeable Integer Maps"
    authors="Okasaki, Chris; Andy Gill"
    affiliations="Columbia University; Semantic Designs"
    publication="Workshop on ML, pp. 77-86"
    year="1998"
    url="http://ittc.ku.edu/~andygill/papers/IntMap98.pdf"
    url_2="https://scholar.google.com/scholar?hl=en&as_sdt=0%2C48&q=Fast+Mergeable+Integer+Maps+(1998)&btnG="
    cited_by_count="91"
    cited_by_count_last_mod="2022-03-13"
    accessed="2022-03-13" >}}

1. {{< citation
    id="Morrison1968"
    author="Morrison, Donald R"
    title="PATRICIA - practical algorithm to retrieve information coded in alphanumeric."
    publication="Journal of the ACM, Vol. 15, No. 4 (1968): 514-534."
    affiliation="Sandia Laboratory"
    url="https://scholar.google.com/scholar?hl=en&as_sdt=0%2C48&q=PATRICIA%E2%80%94practical+algorithm+to+retrieve+information+coded+in+alphanumeric&btnG="
    cited_by_count="1370"
    cited_by_count_last_mod="2022-03-13"
    accessed="2022-03-13" >}}

1. {{< citation
    id="LeventErkok2020"
    title="Question: Confusion between `-Wall` and `NamedFieldPuns` (#18246) · Issues · Glasgow Haskell Compiler / GHC · GitLab"
    date="2020-05-27"
    url="https://gitlab.haskell.org/ghc/ghc/-/issues/18246"
    url_2="https://github.com/ndmitchell/hlint/issues/1250"
    accessed="2022-03-13" >}}

1. {{< citation
    id="Mazon2021-08"
    author="Jean-Baptiste Mazon"
    title="AoC Day 8: Seven Segment Search"
    url="https://xn--sant-epa.ti-pun.ch/posts/2021-12-aoc/day08.html"
    accessed="2022-03-15" >}}

1. {{< citation
    id="wikiHaskellNamedFields"
    title="Haskell/More on datatypes - Wikibooks, open books for an open world"
    url="https://en.wikibooks.org/wiki/Haskell/More_on_datatypes#Named_Fields_(Record_Syntax)"
    accessed="2022-03-15" >}}

1. {{< citation
    id="haskellWikiTypeSynonym"
    title="Type synonym - HaskellWiki"
    url="https://wiki.haskell.org/Type_synonym"
    accessed="2022-03-15" >}}

1. {{< citation
    id="haskellWikiNewtype"
    title="Newtype - HaskellWiki"
    url="https://wiki.haskell.org/Newtype"
    accessed="2022-03-15" >}}

1. {{< citation
    id="Data.List"
    title="Data.List"
    url="https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-List.html"
    accessed="2022-03-15" >}}

1. {{< citation
    id="Prelude"
    title="Prelude"
    url="https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:flip"
    accessed="2022-03-15" >}}
