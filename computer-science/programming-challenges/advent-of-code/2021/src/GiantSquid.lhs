%include polycode.fmt
---
date: 2022-02-25
domains:
- adventofcode.com
local_url: http://localhost:1313/computer-science/programming-challenges/advent-of-code/2021/src/GiantSquid/04-giant-squid/
title: 'AoC 2021 Day 04: Giant Squid'
weight: 4
---

{{< citation
  id="AoC2021-04"
  title="Day 4 - Advent of Code 2021"
  url="https://adventofcode.com/2021/day/4"
  accessed="2022-02-25" >}}

\## Part One

*You're already almost 1.5km (almost a mile) below the surface of the
ocean, already so deep that you can't see any sunlight. What you **can**
see, however, is a giant squid that has attached itself to the outside
of your submarine.*

*Maybe it wants to play
[bingo](https://en.wikipedia.org/wiki/Bingo_(American_version))?*

*Bingo is played on a set of boards each consisting of a 5x5 grid of
numbers. Numbers are chosen at random, and the chosen number is
**marked** on all boards on which it appears. (Numbers may not appear on
all boards.) If all numbers in any row or any column of a board are
marked, that board **wins**. (Diagonals don't count.)*

*The submarine has a **bingo subsystem** to help passengers (currently,
you and the giant squid) pass the time. It automatically generates a
random order in which to draw numbers and a random set of boards (your
puzzle input).*

*The **score** of the winning board can be calculated. Start by finding
the **sum of all unmarked numbers** on that board. Then multiply that
sum by **the number that was just called** when the board won, to get
the final score.*

*To guarantee victory against the giant squid, figure out which board
will win first. **What will your final score be if you choose that
board?***

\begin{code}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module GiantSquid
  ( DrawnNumbers,
    Tile,
    Board,
    scoreOfFirstWinningBoard,
    scoreOfLastWinningBoard,
  )
where

import qualified Data.Vector as V
import Data.Maybe (fromJust)
\end{code}

File format: the first line contains the numbers to draw. The rest is a new
line followed by a 5x5 grid of numbers representing a board.

```txt
7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6
```

The numbers to draw can be represented as an `[Int]`.

A board can be represented as a flat list-like of `(Int, Bool)` tuples, with
helper functions accessing the cell at `(i, j)`. Haskell types are immutable,
so updating a board involves changing the entire board. I'll need to do a lot
of indexing. From [the evaluation for various containers]({{< ref
"/computer-science/programming-challenges/advent-of-code/2021/src/BinaryDiagnostic/03-binary-diagnostic#efficiency-of-collections-types"
\>}}), candidates are `List`, `Data.Sequence`, and `Data.Vector.*`.

Typical operations will be updating a matching tile (if immutable, then
creating new 25-element list), and then checking if the board wins (at most
10 lookups). The need for `fmap` ({{% cite Data.Vector.Primitive %}}) and
indexing makes me choose `Data.Vector` for the `Board` type.

Does changing a board in a list of `Board`s recreate the whole vector?
Seems like it would because I'd be using `map`. Presumably though, only one
`[Board]` will be created because all `Boards` will be processed first.

\begin{code}
type DrawnNumbers = [Int]

type Tile = (Int, Bool)

type Tiles = V.Vector Tile

type Board = (Tiles, Bool)
\end{code}

{{% cite HiddingAoC2021-04 %}} remarks that Haskell is not well-suited for
multi-dimensional array processing, and therefore they use the `Massiv` library.
In my case though, I represented the 5x5 board as a one-dimensional vector of 25
elements.

The `Massiv` also provided convenient APIs for the problem, which {{% cite
HiddingAoC2021-04 %}} was able to utilize, e.g. traversing rows and columns,
updating an entry that matches a predicate, etc.

{{% comment %}}

`Massiv`'s convenient APIs made me also look at APIs from {{% cite Data.Vector
%}}. Till then, I mostly assumed that `Data.Vector` shared the same API as
`List`, only that the under-the-hood implementation was more efficient for my
needs, e.g. indexing. Now that I've looked at the docs, nothing stands out.
There are a couple of mentions of "monadic actions", e.g. `Data.Vector.mapM`
which "applies the monadic action to all elements of the vector, yielding a
vector of results", but monadic actions can wait a bit for now. I'm yet to get
the hang on monads in code.

{{% /comment %}}

\begin{code}
boardWinsFromIndex :: Tiles -> Int -> Bool
boardWinsFromIndex tiles idx =
  -- TODO: Add error handling? This function crashes with idx = 25
  let columnTiles = [idx - 5, idx - 10 .. 0] ++ [idx] ++ [idx + 5, idx + 10 .. 24]
      mod5 = idx `mod` 5
      rowTiles = [idx - mod5, idx - mod5 + 1 .. idx + 5 - mod5 - 1]

      -- | Return `True` iff the tiles in the indices are all marked.
      bingo :: [Int] -> Bool
      bingo (j : js) = let tileIsMarked = snd (tiles V.! j)
                       in if null js
                           then tileIsMarked
                           else tileIsMarked && bingo js
      bingo []       = False

   in bingo columnTiles || bingo rowTiles

type TileWithIndex = (Tile, Int)
\end{code}

Unexpected infinite loop.

```hs
l1 = [1, 2, 3]
l2 = [1 .. ]
zip l1 l2              -- [(1,1), (2,2), (3,3)]
v1 = [1, 2, 3]
v2 = [1 .. ]
V.zip v1 v2            -- Infinite loop!
```

\begin{code}
indicesWithMatch :: Tiles -> Int -> V.Vector Int
indicesWithMatch tiles num =
  let tileMatches :: Int -> TileWithIndex -> Bool
      tileMatches numToMatch (tile, _) = fst tile == numToMatch

      tilesWithIndex :: Tiles -> V.Vector TileWithIndex
      tilesWithIndex t = V.zip t (V.fromList [0 .. (length t)])

      matchingTilesWithIndex :: V.Vector TileWithIndex
      matchingTilesWithIndex = V.filter (tileMatches num) (tilesWithIndex tiles)
   in V.map snd matchingTilesWithIndex

playRoundOnBoard :: Int -> Board -> Board
playRoundOnBoard num board =
  let updateTile :: Int -> Tile -> Tile
      updateTile n tile = if fst tile == n then (n, True) else tile

      updatedTiles = V.map (updateTile num) (fst board)
      matchedIndices = V.toList (indicesWithMatch updatedTiles num)

      boardWins :: Tiles -> [Int] -> Bool
      boardWins _ [] = False
      boardWins tiles [i] = boardWinsFromIndex tiles i
      boardWins tiles (i : is) = boardWinsFromIndex tiles i || boardWins tiles is
   in (updatedTiles, boardWins updatedTiles matchedIndices)

type BoardWithLastCalledNum = (Board, Int)

playBingo :: DrawnNumbers -> [Board] -> BoardWithLastCalledNum
playBingo (n : nums) currBoards =
  let boardsAfterRound = map (playRoundOnBoard n) currBoards
      winner = filter snd boardsAfterRound
   in -- What is the idiomatic way of saying `if true`?
      if null winner then playBingo nums boardsAfterRound else (head winner, n)
playBingo [] currBoards = (head currBoards, 0)

scoreOfBoard :: BoardWithLastCalledNum -> Int
scoreOfBoard (board, lastCalledNum) =
  let unmarkedTiles :: Tiles
      unmarkedTiles = V.filter (not . snd) (fst board)
   in sum (V.map fst unmarkedTiles) * lastCalledNum

scoreOfFirstWinningBoard :: (DrawnNumbers, [Board]) -> Int
scoreOfFirstWinningBoard (drawnNums, boards) =
  scoreOfBoard (playBingo drawnNums boards)
\end{code}

\## Part Two

*On the other hand, it might be wise to try a different strategy: let the
giant squid win.*

*You aren't sure how many bingo boards a giant squid could play at once,
so rather than waste time counting its arms, the safe thing to do is to
**figure out which board will win last** and choose that one. That way,
no matter which boards it picks, it will win for sure.*

*Figure out which board will win last. **Once it wins, what would its
final score be?***

\begin{code}
playLastBoardBingo :: DrawnNumbers -> [Board] -> [BoardWithLastCalledNum] -> Maybe BoardWithLastCalledNum

playLastBoardBingo [n] boards winningBoards =
    let boardsAfterRound = map (playRoundOnBoard n) boards
        currWinners = reverse (filter snd boardsAfterRound)
        lastWinningBoardWithNum = if null currWinners
            then head winningBoards
            else (head currWinners, n)
    in Just lastWinningBoardWithNum

playLastBoardBingo (n:ns) boards winningBoards =
    let boardsAfterRound = map (playRoundOnBoard n) boards
        currWinners = reverse (filter snd boardsAfterRound)
        pendingBoards = filter (not . snd) boardsAfterRound
        allWinners = map (\b -> (b, n)) currWinners ++ winningBoards
    in playLastBoardBingo ns pendingBoards allWinners

playLastBoardBingo _ _ _ = Nothing

scoreOfLastWinningBoard :: (DrawnNumbers, [Board]) -> Int
scoreOfLastWinningBoard (drawnNums, boards) =
    -- TODO: error handling?
    scoreOfBoard(fromJust $ playLastBoardBingo drawnNums boards [])

\end{code}

\## References

1. {{< citation
  id="Data.Vector.Primitive"
  title="Data.Vector.Primitive"
  url="https://hackage.haskell.org/package/vector-0.12.3.1/docs/Data-Vector-Primitive.html#t:Prim" >}}

1. {{< citation
  id="HiddingAoC2021-04"
  title="Advent of Code 2021: Day 04: Giant Squid"
  url="https://jhidding.github.io/aoc2021/#day-4-giant-squid"
  accessed="2022-03-02" >}}

1. {{< citation
  id="Data.Vector"
  title="Data.Vector"
  url="https://hackage.haskell.org/package/vector-0.12.3.1/" >}}
