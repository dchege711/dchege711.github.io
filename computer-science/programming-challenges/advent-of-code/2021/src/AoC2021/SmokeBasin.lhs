%include polycode.fmt
---
title: "AoC 2021 Day 09: Smoke Basin"
date: 2022-03-16
weight: 9
summary: "Multi-dimensional arrays using `massiv`; Fusion; Box vs. Unboxed; Connected Components; `Data.Set`; Monadic map"
---

{{< citation
    id="AoC2021-09"
    title="Day 9 - Advent of Code 2021"
    url="https://adventofcode.com/2021/day/9"
    author="Eric Wastl"
    accessed="2022-03-16" >}}

\## Part I {{% cite AoC2021-09 %}}

These caves seem to be lava tubes. Parts are even still volcanically active;
small hydrothermal vents release smoke into the caves that slowly settles like
like rain.

If you can model how the smoke flows through the caves, you might be able to
avoid it and be that much safer. The submarine generates a height-map of the
floor of the nearby caves for you (your puzzle input).

Smoke flows to the lowest point of the area it's in. Your first goal is to find
the low points - the locations that are lower than any of its adjacent
locations. Most locations have four adjacent locations (up, down, left, and
right); locations on the edge or corner of the map have three or two adjacent
locations, respectively. (Diagonal locations do not count as adjacent.)

The risk level of a low point is `1` plus its height. Find all of the low points
on your height-map. **What is the sum of the risk levels of all low points on
your height-map?**

{{% comment %}}

The modeling of the risk level as `1 + height` is unintuitive. I'd think that
the lowest point on the map has the greatest risk, e.g. if there is a lot of
smoke flow, the global low point will get the greatest depth of smoke. So I'd
have modeled the risk level as `1 / (1 + height)`.

{{% /comment %}}

\begin{code}
{-# OPTIONS_GHC -Wall #-}

module AoC2021.SmokeBasin
    (
        HeightMap,
        sumOfRiskLevelsOfLowPoints,
        productOf3LargestBasins
    )
where

import Data.Massiv.Core.Index (Ix2(..), Sz(..), Border(..), unSz)
import qualified Data.Massiv.Array as A
    (
        Array, P(..), computeAs, sum, (!?), (!), flatten, size, zeroIndex,
        (..:), toList
    )
import Data.Massiv.Array.Stencil (Stencil, makeStencil, mapStencil)
import Data.List (foldl', sort)
import Data.Maybe (isJust, fromJust, maybeToList)
import qualified Data.Set as Set
    (
        Set, fromList, union, empty, singleton, null, deleteFindMin, insert,
        difference, size, member
    )

\end{code}

\## Input Representation

The data is an \\(n \times n\\) grid. In another language, using a
one-dimensional array might have reaped me the benefits of cache locality, but
lists in Haskell are linked lists that lack guaranteed locality.

In [Day 04: Giant Squid]({{< ref
"/computer-science/programming-challenges/advent-of-code/2021/src/GiantSquid#HiddingAoC2021-04"
\>}}), Hidding used the {{% cite Massiv %}} array library, whose tagline is
"multi-dimensional arrays with [fusion](#MassiveFusion),
[stencils](#MassiveStencil) and parallel computation". It's at least worth
checking out in the problem.

\## Notable Design Decisions in `massiv`

For example, `foo = length . filter p . map g . map f . concat` would be pretty
inefficient if executed literally. Fusion refers to program transformations
aimed at removing intermediate data structures. GHC has transformation rules
that enable fusion. {{% cite haskellWikiFusion %}}  <a id="MassiveFusion"></a>

{{% comment %}}

Also encountered stream fusion [in the `Data.Vector` library]({{< ref
"../BinaryDiagnostic/03-binary-diagnostic.md#Data.Vector.StreamFusion" >}}). A
*stream* represents the traversal of a list-like structure. All the list
functions become stream functions --  but crucially. stream operations are
non-recursive, and can therefore be glued together. {{% cite haskellWikiFusion
%}}

{{% /comment %}}

[Like `Data.Vector`]({{< ref
"/computer-science/programming-challenges/advent-of-code/2021/src/BinaryDiagnostic/03-binary-diagnostic#efficiency-of-collections-types"
\>}}), `Massiv` also supports boxed elements (may point to a thunk), unboxed
elements, and storable types. However, `Massiv` has optimized containers for
instances of [the `Prim`
class](https://hackage.haskell.org/package/massiv-1.0.1.1/docs/Data-Massiv-Array-Manifest.html#t:Prim).
{{% cite Massiv %}}

{{% comment %}}

A bit surprised that `Boolean` is not an instance of the `Prim` type. Other
languages tend to have Boolean as one of primitive types.

The size of a `bool` in C++ is defined from the implementation and may be
greater than 1 (byte) {{% cite cppFundamentalTypes %}}. However, the `sizeof`
for `char`, `signed char`, `unsigned char`, `std::byte` (C++17), and `char8_t`
(C++20) will always evaluate to 1 (byte). {{% cite cppSizeOf %}}. A tad
counter-intuitive that a `bool` is at least 8 bits in C++. {{% cite
cpp8bitsForBoolean %}} attributes this to every C++ data type needing to be
addressable, and most CPU architectures are designed with a 8-bit chunks as the
smallest addressable memory.

While `std::vector<bool>` is allowed to be space-efficient,
it loses some guarantees of `std::vector` e.g. safely modifying elements
concurrently in a multi-threaded context, contiguous storage (that allows
pointer arithmetic), etc. {{% cite cppVectorBool %}}

{{% /comment %}}

`Massiv` has delayed arrays which do not exist in memory, and are instead
defined as a function or a composition of functions. This allows to operate on
a massive array in constant memory. {{% cite Massiv %}}

`Massiv` has a wrapping data type for indices, e.g. `makeArrayR D Seq
(Sz (3 :. 5)) (\ (i :. j) -> i * j)`, which creates a 2D array with 3 arrays,
each with 5 elements, where the element at index `i :. j` is computed as
`i * j`. There is a constructor, `IxN` that supports N-dimensional arrays, e.g.
`10 :> 20 :> 30 :. 40` is an index into a 4D array. {{% cite Massiv %}}

A stencil is a function that can read the neighboring elements of the stencil's
center (the zero index), and only those, and then outputs a new value for the
center element. {{% cite Massiv %}} <a id="MassiveStencil"></a>

{{% comment %}}

{{% cite Massiv %}} mentions "Moore neighborhood", which led to me to the Von
Neumann neighborhood (the cell itself and cells at a Manhattan distance of `1`)
which happens to be the kind of neighborhood being evaluated in this problem.
{{% cite wikiVonNeumannNeighborhood %}}

{{% /comment %}}

\## Input Representation (Cont'd)

\begin{code}

-- `A.P` because the underlying representation (Int) is an instance of the
-- `Prim` type class.
type HeightMap = A.Array A.P Ix2 Int

\end{code}

{{% comment %}}

For a while, I was stuck thinking of \\(a = [[0,1,2], [1,2,5], ..., [3,2,6]]\\)
as an N-dimensional array, where \\(N\\) can only be determined after reading
the whole file. Gleaning at {{% cite HiddingAoC21-09 %}} made me see \\(a\\) as
the 2D array that it is.

{{% /comment %}}

\## Part I Solution

\begin{code}
vonNeumannNeighborhood :: (Ix2 -> Int) -> [Int]
vonNeumannNeighborhood get =
    [get (-1 :. 0), get (1 :. 0), get (0 :. -1), get (0 :. 1)]

riskIfLowPoint :: (Ix2 -> Int) -> Int
riskIfLowPoint get =
    let centerPoint = get (0 :. 0)
        lowPoint = foldl' min (maxBound :: Int) (vonNeumannNeighborhood get)
        risk
            | centerPoint < lowPoint = 1 + centerPoint
            | otherwise              = 0
    in  risk

riskIfLowPointStencil :: Stencil Ix2 Int Int
riskIfLowPointStencil = makeStencil (Sz (3 :. 3)) (1 :. 1) riskIfLowPoint
{-# INLINE riskIfLowPointStencil #-}

sumOfRiskLevelsOfLowPoints :: HeightMap -> Int
sumOfRiskLevelsOfLowPoints heightMap =
    let borderHandling = Fill (maxBound :: Int)
        risksArrayDW = mapStencil borderHandling riskIfLowPointStencil heightMap
        riskArray = A.computeAs A.P risksArrayDW
    in A.sum riskArray

\end{code}

{{% comment %}}

Most of my time was spent trying to get the syntax and types right. I'm spending
a lot of time deciphering `massiv`'s syntax as opposed to learning Haskell.

{{% /comment %}}

\### Learnings from Others' Part I Solutions

{{% cite HiddingAoC21-09 %}} has more succinct solution:

```hs
type Array2' r a = A.Array r Ix2 a
type Array2 a = Array2' A.U a
```

The `U` is the `Unbox` type class. The array is usually just as fast as `P`, but
can work with a wider range of data types. {{% cite Massiv %}} Using `Array2`
allowed {{% cite HiddingAoC21-09 %}} to define more types, e.g. `Array2 Int` for
the height map, `Array2 Int` for the risk values, `Array2 (Int, Int)` in Part
II, and so forth. However, the `Array2'` type was unncessary IMO.

```hs
neighbours :: [Ix2]
neighbours = [-1 :. 0, 1 :. 0, 0 :. -1, 0 :. 1]
```

My analogous function, `vonNeumannNeighborhood :: (Ix2 -> Int) -> [Int]`, could
have been simplified into {{% cite HiddingAoC21-09 %}}'s. After all, I kept on
using the `get` parameter for all 4 elements, and that's not in tune with
the wholemeal programming advocated for in Haskell. My lack of understanding of
`get` as a function that knows how to fetch values made me blind to {{% cite
HiddingAoC21-09 %}} use of `get` in `findMinStencil`.

```hs
findMinStencil :: Stencil Ix2 Int Int
findMinStencil = makeStencil (Size (3 :. 3)) (1 :. 1) go
    where go get
            || all ((centerPoint <) . get) neighbors = value + 1
            || otherwise                             = 0
            where centerPoint = get (0 :. 0)
```

`all` returns `True` if all elements of a `Foldable` structure satisfy the
predicate (and interestingly, `all (> 3) [] == True`) {{% cite Prelude %}}. It's
also possible to nest `where` definitions. Neat!

```hs
sumOfRiskLevelsOfLowPoints :: Array2 Int -> Int
sumOfRiskLevelsOfLowPoints heightMap = A.sum riskArray
    where riskArray :: Array2 Int
          riskArray = A.compute $ mapStencil (Fill 10) findMinStencil heightMap
```

My use of `Fill (maxBound :: Int)` for border handling was unncessary. The
max height is `9` so `10` suffices. However, I also think that `maxBound :: Int`
is more self explanatory.

\## Part II {{% cite AoC2021-09 %}}

Next, you need to find the largest basins so you know what areas are most
important to avoid.

A basin is all locations that eventually flow downward to a single low point.
Therefore, every low point has a basin, although some basins are very small.
Locations of height `9` do not count as being in any basin, and all other
locations will always be part of exactly one basin.

The size of a basin is the number of locations within the basin, including the
low point.

What do you get if you multiply together the sizes of the three largest basins?

\## Part II Solution

\begin{code}

maxHeight :: Int
maxHeight = 9

\end{code}

I initially tried to be "efficient" about this problem by only considering
right and bottom neighbors to prevent double-counting.

```hs
buggyBasinSize :: HeightMap -> Ix2 -> Int
buggyBasinSize heightMap ix@@(row :. col) =
    let localBasinSize height
            || height == maxHeight = 0
            || otherwise = 1 + basinSize heightMap (row+1 :. col)
                            + basinSize heightMap (row :. col +1)
    in localBasinSize (A.defaultIndex maxHeight heightMap ix)
```

Even if the above implementation were correct, the size of a basin would depend
on what the initial `(i, j)` are. If `(i, j)` are not the top-left cell in the
basin, then we might not get the correct answer as the locations to the left and
above the starting location are omitted.

This question reduces to the connected components problem in graph theory. Two
neighboring locations are considered connected if their heights are both less
than `maxheight = 9`.

{{% comment %}}

{{% cite AoC2021-09 %}} is not explicit about what counts as a neighbor. In this
height map:

```md
19
92
```

... are `1` and `2` considered to be in the same basin? Maybe not because the
two `9`s form a wall that prevents water in `1` from flowing to `2`. If it does
flow, then all `9`s are submerged, and we have one huge basin, which is not in
the spirit of the problem.

{{% /comment %}}

Is there a way of finding the connected components using stencils? ~~It feels
like there should, given `vonNeumannNeighborhood`...~~ Stencils make sense when
we're applying a function to each element while taking its neighbors into
account. In the case of finding neighbors, I don't think a stencil will be
useful.

\begin{code}

isInABasin :: Int -> Bool
isInABasin height = height < maxHeight

computeBasinNeighbors :: HeightMap -> Ix2 -> Locations
computeBasinNeighbors heightMap (row :. col) =
    let candidates = [row-1 :. col, row+1 :. col, row :. col-1, row :. col+1]
        heights = map (heightMap A.!?) candidates
        isNeighbor (_, maybeHeight) =
            isJust maybeHeight && isInABasin (fromJust maybeHeight)
    in Set.fromList (map fst $ filter isNeighbor (zip candidates heights))

\end{code}

{{% open-comment %}}

Is there a more idiomatic way of checking that a given index is within bounds?

{{% /open-comment %}}

\begin{code}

type Locations = Set.Set Ix2

getBasin :: HeightMap -> Ix2 -> Maybe Locations
getBasin heightMap location
    | not (isInABasin (heightMap A.! location)) = Nothing
    | otherwise = Just (getLocationsInBasin Set.empty (Set.singleton location)) where
        getLocationsInBasin :: Locations -> Locations -> Locations
        getLocationsInBasin visited visiting
            | Set.null visiting = visited
            | otherwise =
                let (currLocation, otherLocations) = Set.deleteFindMin visiting
                    visited' = Set.insert currLocation visited
                    currNeighbors = computeBasinNeighbors heightMap currLocation
                    unseenLocations = Set.difference currNeighbors visited
                    visiting' = Set.union otherLocations unseenLocations
                in getLocationsInBasin visited' visiting'

collectBasins :: HeightMap -> [Ix2] -> Locations -> [Locations]
collectBasins heightMap (location:locations) visited =
    let maybeBasin
            | Set.member location visited = Nothing
            | otherwise                   = getBasin heightMap location
        visited'
            | isJust maybeBasin = Set.union visited (fromJust maybeBasin)
            | otherwise         = Set.insert location visited
    in maybeToList maybeBasin ++ collectBasins heightMap locations visited'
collectBasins _ [] _ = []

\end{code}

{{% comment %}}

I'm spending too much time writing down code for collecting the connected
components. Two sources of delay:

* In my head, I have a vague idea of how I'd solve this problem in Python, and
I'm trying to translate that into Haskell, which demands a functional style with
immutability. I need to learn how to think in functional terms, i.e. going from
the algorithm to functional implementation, instead of going from an imperative
implementation to a functional one.

* Not understanding the documentation at {{% cite Massiv %}}. I'm struggling
with things that are relatively straightforward in other languages' docs, e.g.
given an N-dimensional array, how do I construct a list of all the valid
indices? Or am I asking questions that only an imperative thinker would? How do
I verify that an index into a given array is valid, without trying to fetch the
element at that index?

{{% /comment %}}

The `Set` library does not offer a generic `Set.pop` API. Instead, it has
`Set.lookupMin` and `Set.lookupMax`, which both run in \\(O(log\ n)\\) time.
These APIs make it apparent that the `Set` container is ordered.

{{% comment %}}

C++17 introduced `std::set<Key,Compare,Allocator>::extract`, which is amortized
\\(O(1)\\) when passing in an iterator. {{% cite cppSetExtract %}}
`std::unordered_set`, which hashes items into buckets, has its `extract` method
taking \\(O(1)\\) in the average case, and \\(O(n)\\) in the worst case.
{{% cite cppUnorderedSetExtract %}}

Python's `set.pop` specifies that an arbitrary element is removed and returned.
There aren't explicit guidelines, but I assume the underlying implementation has
similar complexity. {{% cite pySetPop %}}

{{% /comment %}}

\begin{code}

productOf3LargestBasins :: HeightMap -> Int
productOf3LargestBasins heightMap =
    let indexes = A.toList $ A.flatten $ (A.zeroIndex :: Ix2) A...: unSz (A.size heightMap)
        basinSizes = map Set.size (collectBasins heightMap indexes Set.empty)
    in (product . take 3 . reverse . sort) basinSizes

\end{code}

\### Learnings from Others' Part II Solutions

{{% cite HiddingAoC21-09 %}}'s approach was different: mark the minima, and then
repeatedly grow neighborhood around each minimum until two patches meet.

Given an `Array2 Int` where minima are denoted as non-zero elements, {{% cite
HiddingAoC21-09 %}} labels each minima with a unique integer, with the help of
{{% cite RIO.State %}} (which re-exports {{% cite Control.Monad %}}).

```hs
-- `evalState :: State s a -> s -> a` evaluates a state computation with the
-- initial state, and returns the final value, discarding the final state.
--
-- `State s a` is a state-passing computation to execute.
-- `s` is the initial value.
-- `a` is the return value of the state computation.

markBasins :: Array2 Int -> Array2 Int
markBasins a = evalState (A.mapM markNonZero a) 0 -- Initial ID is zero.
    where markNonZero :: Int -> State Int Int
          markNonZero x
            -- `modify` maps an old state to a new state inside a state monad;
            -- the old state is thrown away. `modify'` is its strict variant.
            --
            -- `get` returns the state from the internals of the monad.
            --
            -- So whenever x is a minimum, we increase the state, and then
            -- assign the value to that array element. That's why the IDs are
            -- all greater than zero and unique.
            || x /= 0    = modify (+ 1) >> get
            || otherwise = return 0
```

{{% comment %}}

The `(>>)` "and then" operator sequentially composes two actions, discarding
any value produced by the first. [Previous encounter with `(>>)` operator]({{<
ref "../../haskell-meta#monad" >}}).

{{% /comment %}}

{{% comment %}}

The technique used in `markBasins` looks promising for future endeavors where I
need to process a collection whle keeping track of some state. Previously, I've
found such problems tricky because I'm used to state being in a mutable
variable. The monad state feels mutable, but somehow abstracted from me.

{{% /comment %}}

\## Misc. Learnings from Others' Solutions

{{% cite Qualia91AoC21-09 %}} used the {{% cite Debug.Trace %}} library, and it
looks promising, there are functions in the package that are not "referentially
transparent", e.g. `trace :: String -> a -> a` whose type indicates purity, but
it actually has a side effect of outputting the trace message. I didn't know
this was possible.

\## References

1. {{< citation
    id="Massiv"
    title="massiv: Massiv (Массив) is an Array Library."
    author="Alexey Kuleshevich"
    url="https://hackage.haskell.org/package/massiv"
    url_2="https://hackage.haskell.org/package/massiv#stencil"
    accessed="2022-03-16" >}}

1. {{< citation
    id="wikiVonNeumannNeighborhood"
    title="Von Neumann neighborhood - Wikipedia"
    url="https://en.wikipedia.org/wiki/Von_Neumann_neighborhood"
    accessed="2022-03-16" >}}

1. {{< citation
    id="haskellWikiFusion"
    title="GHC optimisations - HaskellWiki"
    url="https://wiki.haskell.org/GHC_optimisations#Fusion"
    accessed="2022-03-16" >}}

1. {{< citation
    id="cppFundamentalTypes"
    title="Fundamental types - cppreference.com"
    url="https://en.cppreference.com/w/cpp/language/types#Boolean_type"
    accessed="2022-03-28" >}}

1. {{< citation
    id="cppSizeOf"
    title="sizeof operator - cppreference.com"
    url="https://en.cppreference.com/w/cpp/language/sizeof"
    accessed="2022-03-28" >}}

1. {{< citation
    id="cpp8bitsForBoolean"
    title="boolean - C++ : why bool is 8 bits long? - Stack Overflow"
    url="https://stackoverflow.com/questions/2064550/c-why-bool-is-8-bits-long"
    accessed="2022-03-28" >}}

1. {{< citation
    id="cppVectorBool"
    title="std::vector<bool> - cppreference.com"
    url="https://en.cppreference.com/w/cpp/container/vector_bool"
    accessed="2022-03-28" >}}

1. {{< citation
    id="HiddingAoC21-09"
    author="Johan Hidding"
    title="Advent of Code 2021: Day 9: Smoke Basin"
    url="https://jhidding.github.io/aoc2021/#day-9-smoke-basin"
    accessed="2022-03-29" >}}

1. {{< citation
    id="haskellWikiSectionInfixOp"
    title="Section of an infix operator - HaskellWiki"
    url="https://wiki.haskell.org/Section_of_an_infix_operator"
    accessed="2022-04-08" >}}

1. {{< citation
    id="cppSetExtract"
    title="std::set<Key,Compare,Allocator>::extract - cppreference.com"
    url="https://en.cppreference.com/w/cpp/container/set/extract"
    accessed="2022-04-08" >}}

1. {{< citation
    id="pySetPop"
    title="Built-in Types — Python 3.10.4 documentation"
    url="https://docs.python.org/3/library/stdtypes.html#frozenset.pop"
    accessed="2022-04-08" >}}

1. {{< citation
    id="cppUnorderedSetExtract"
    title="std::unordered_set - cppreference.com"
    url="https://en.cppreference.com/w/cpp/container/unordered_set"
    accessed="2022-04-08" >}}

1. {{< citation
    id="Qualia91AoC21-09"
    author="BOC Dev"
    title="AdventOfCode2021/day9.hs at master · Qualia91/AdventOfCode2021"
    url="https://github.com/Qualia91/AdventOfCode2021/blob/master/Day9/day9.hs"
    accessed="2022-04-15" >}}

1. {{< citation
    id="Debug.Trace"
    title="Debug.Trace"
    url="https://hackage.haskell.org/package/base-4.16.1.0/docs/Debug-Trace.html"
    accessed="2022-04-15" >}}

1. {{< citation
    id="Prelude"
    title="Prelude"
    url="https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#v:all"
    accessed="2022-04-15" >}}

1. {{< citation
    id="RIO.State"
    title="RIO.State"
    author="Michael Snoyman"
    url="https://hackage.haskell.org/package/rio-0.1.21.0/docs/RIO-State.html"
    accessed="2022-04-15" >}}

1. {{< citation
    id="Control.Monad"
    author="Andy Gill; Edward Kmett"
    title="mtl: Monad classes, using functional dependencies"
    url="https://hackage.haskell.org/package/mtl"
    accessed="2022-04-15" >}}
