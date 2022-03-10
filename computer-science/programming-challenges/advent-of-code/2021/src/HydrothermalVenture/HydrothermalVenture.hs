{-# OPTIONS_GHC -Wall #-}

module HydrothermalVenture.HydrothermalVenture
  ( pointsWithAtLeastTwoRightSegmentOverlaps,
    pointsWithAtLeastTwoSegmentOverlaps,
    LineSegment (..),
    Point (..),
  )
where

import Data.List (group, sort)

-- The input text is of the form:
--
-- 0,9 -> 5,9
-- 8,0 -> 0,8
--
-- The points don't make sense individually, so grouping them together as a line
-- segment makes sense.
--
-- Kinda wild that deriving `Eq` and `Ord` simply works!
data Point = Point {x :: Int, y :: Int} deriving (Eq, Ord, Show)

data LineSegment = LineSegment {p1 :: Point, p2 :: Point}

type LineSegmentDiscretizer = LineSegment -> [Point]

discretizeRightLineSegment :: LineSegmentDiscretizer
discretizeRightLineSegment LineSegment {p1 = Point {x = x1, y = y1}, p2 = Point {x = x2, y = y2}}
  | x1 == x2 || y1 == y2 =
    let steps :: Int -> Int -> [Int]
        steps i1 i2 = if i2 > i1 then [i1 .. i2] else [i2 .. i1]
        xs = steps x1 x2
        ys = steps y1 y2
     in map (\p -> Point {x = head p, y = p !! 1}) (sequence [xs, ys])
  | otherwise = []

groupSegmentOverlaps :: LineSegmentDiscretizer -> [LineSegment] -> [[Point]]
groupSegmentOverlaps discretizer lineSegments =
  -- In an imperative language with mutable data, I'd have had a mapping from
  -- points to counts, and then looped over the map to get those w/ counts >=
  -- 2. But we don't have mutability...
  --
  -- Suppose we had a list of points with a vent, e.g. [(x1, y1), (x2, y2),
  -- (x3, y3), (x1, y1), (x4, y4)]. How do I get counts without mutability?
  --
  -- Seems like some version of folding, where the accumulator is of the form
  -- [(x1, x2, 2), (x2, y2, 1), (x3, y3, 1), (x4, y4, 1)]. Feels pretty
  -- inefficient. Or we can sort the list, and then keeping count doesn't need
  -- take quadratic(?) time.
  let expandSegmentsToPoints :: [LineSegment] -> [Point]
      expandSegmentsToPoints [] = []
      expandSegmentsToPoints (seg : segs) =
        discretizer seg ++ expandSegmentsToPoints segs
   in group (sort (expandSegmentsToPoints lineSegments))

pointsWithAtLeastTwoRightSegmentOverlaps :: [LineSegment] -> Int
pointsWithAtLeastTwoRightSegmentOverlaps segs =
  length
    ( filter
        (\ps -> length ps > 1)
        ( groupSegmentOverlaps
            discretizeRightLineSegment
            segs
        )
    )

discretizeDiagonalLineSegment :: LineSegmentDiscretizer
discretizeDiagonalLineSegment LineSegment {p1 = Point {x = x1, y = y1}, p2 = Point {x = x2, y = y2}}
  | abs (x1 - x2) == abs (y1 - y2) =
    let absDiff = abs (x1 - x2)
        xs = [x1, x1 + step .. x2]
          where
            step = (x2 - x1) `div` absDiff
        ys = [y1, y1 + step .. y2]
          where
            step = (y2 - y1) `div` absDiff
     in zipWith (\x' y' -> Point {x = x', y = y'}) xs ys
  | otherwise = []

discretizeLineSegments :: LineSegmentDiscretizer
discretizeLineSegments lineSeg@LineSegment {p1 = Point {x = x1, y = y1}, p2 = Point {x = x2, y = y2}}
  | x1 == x2 || y1 == y2 = discretizeRightLineSegment lineSeg
  | abs (x1 - x2) == abs (y1 - y2) = discretizeDiagonalLineSegment lineSeg
  | otherwise = []

pointsWithAtLeastTwoSegmentOverlaps :: [LineSegment] -> Int
pointsWithAtLeastTwoSegmentOverlaps segs =
  length
    ( filter
        (\ps -> length ps > 1)
        ( groupSegmentOverlaps
            discretizeLineSegments
            segs
        )
    )
