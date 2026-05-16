%include polycode.fmt
---
title: "AoC 2021 Solution Runner"
date: 2022-04-11
weight: 96
---

\begin{code}

{-# OPTIONS_GHC -Wall #-}

module AoC2021 (runSolution) where

-- https://cabal.readthedocs.io/en/3.4/cabal-package.html#accessing-data-files-from-package-code

import AoC2021InputParser
    ( parseBinaryDiagnosticInput, parseBingoInput, parseHydrothermalVents,
      parseLanternfishInternalTimers, parseHorizontalCrabPositions,
      parseSevenSegmentsDisplay, parseHeightMap )
import BinaryDiagnostic.BinaryDiagnostic (lifeSupportRating, powerConsumption)
import Data.String (IsString (fromString))
import Dive.Dive (productOfFinalPosition, productOfFinalPositionWithNewIntepretation)
import GiantSquid (scoreOfFirstWinningBoard, scoreOfLastWinningBoard)
import HydrothermalVenture.HydrothermalVenture
  ( pointsWithAtLeastTwoRightSegmentOverlaps,
    pointsWithAtLeastTwoSegmentOverlaps,
  )
import qualified AoC2021.Lanternfish (numOfFishIn80Days, numOfFishIn256Days)
import qualified AoC2021.TreacheryOfWhales as TreacheryOfWhales
  (
    minFuelForAlignmentWithConstantBurnRate,
    minFuelForAlignmentWithIncreasingBurnRate
  )
import Paths_advent_of_code_y2021 (getDataFileName)
import SonarSweep ( num3MeasurementIncreases, numIncreases )
import System.IO (IOMode (ReadMode), hGetContents, withFile)
import qualified AoC2021.SevenSegmentSearch as SevenSegmentSearch
  (numOf1478AppearancesInOutput, sumOfOutputValues)
import qualified AoC2021.SmokeBasin as SmokeBasin
  (
    sumOfRiskLevelsOfLowPoints,
    productOf3LargestBasins
  )
import Text.Printf (printf)

allSolutions :: IO ()
allSolutions = do
  solution01
  solution02
  solution03
  solution04
  solution05
  solution06
  solution07
  solution08
  solution09

runSolution :: Int -> IO ()
runSolution 0 = do allSolutions
runSolution 1 = do solution01
runSolution 2 = do solution02
runSolution 3 = do solution03
runSolution 4 = do solution04
runSolution 5 = do solution05
runSolution 6 = do solution06
runSolution 7 = do solution07
runSolution 8 = do solution08
runSolution 9 = do solution09
runSolution x = do putStrLn ("Day " ++ show x ++ " has no associated solution.")

solution01 :: IO ()
solution01 = do
  -- hGetContents is lazy in that data is only read as the characters are
  -- processed. The lazy evaluation of the string is transparent, and so it can
  -- be passed to pure functions without any issues. However, if we try to hold
  -- onto `s` past the call to `numIncreases`, then we lose the memory
  -- efficiency as the compiler is forced to keep its value in memory for future
  -- use. Note that closing a handle before fully consuming its results will
  -- make you miss on the stream's data that had not been evaluated before the
  -- handle's close. [1] [2]
  --
  -- [1]: http://book.realworldhaskell.org/read/io.html#io.lazy.hGetContents
  -- [2]: https://hackage.haskell.org/package/base-4.16.0.0/docs/System-IO.html#v:hGetContents

  fp <- getDataFileName "src/scratchpad/01-sonar-sweep.input.txt"
  putStrLn "Day 01. Sonar Sweep"
  withFile
    fp
    ReadMode
    ( \h -> do
        s <- hGetContents h
        putStr "\tPart 1: Number of measurements larger than previous measurement: "
        print (SonarSweep.numIncreases (lines (fromString s)))
        putStr "\tPart 2: Number of 3-measurements larger than previous 3-measurement: "
        print (SonarSweep.num3MeasurementIncreases (lines (fromString s)))
    )

solution02 :: IO ()
solution02 = do
  fp <- getDataFileName "src/Dive/scratchpad/input.txt"
  putStrLn "Day 02. Dive!"
  withFile
    fp
    ReadMode
    ( \h -> do
        s <- hGetContents h
        putStr "\tPart 1: Product of final horizontal position and final depth: "
        print (productOfFinalPosition (lines (fromString s)))
        putStr "\tPart 2: Product of final horizontal position and final depth "
        putStr "with new instructions of : "
        print (productOfFinalPositionWithNewIntepretation (lines (fromString s)))
    )

solution03 :: IO ()
solution03 = do
  putStrLn "Day 03. Binary Diagnostic"
  putStr "\tPart 1: Power Consumption: "
  input <- parseBinaryDiagnosticInput "src/BinaryDiagnostic/scratchpad/input.txt"
  print (powerConsumption input)
  putStr "\tPart 2: Life Support Rating: "
  print (lifeSupportRating input)

solution04 :: IO ()
solution04 = do
  putStrLn "Day 04. Giant Squid"
  input <- parseBingoInput "src/scratchpad/04-giant-squid.input.txt"

  putStr "\tPart 1: Score of first winning board: "
  print (GiantSquid.scoreOfFirstWinningBoard input)

  putStr "\tPart 2: Score of last winning board: "
  print (GiantSquid.scoreOfLastWinningBoard input)

solution05 :: IO ()
solution05 = do
  putStrLn "Day 05. Hydrothermal Venture"
  input <- parseHydrothermalVents "src/HydrothermalVenture/scratchpad/input.txt"

  putStr "\tPart 1: Num Points Where >= 2 Right Lines Overlap: "
  print (pointsWithAtLeastTwoRightSegmentOverlaps input)

  putStr "\tPart 2: Num Points Where >= 2 Lines Overlap: "
  print (pointsWithAtLeastTwoSegmentOverlaps input)

solution06 :: IO ()
solution06 = do
  putStrLn "Day 06. Lanternfish"
  input <- parseLanternfishInternalTimers "src/scratchpad/06-lanternfish.input.txt"

  putStr "\tPart 1: Number of lanternfish after 80 days: (360610) "
  print (AoC2021.Lanternfish.numOfFishIn80Days input)

  putStr "\tPart 2: Number of lanternfish after 256 days: (1631629590423) "
  print (AoC2021.Lanternfish.numOfFishIn256Days input)

solution07 :: IO ()
solution07 = do
  putStrLn "Day 07. The Treachery of Whales"
  input <- parseHorizontalCrabPositions "src/scratchpad/07-treachery-of-whales.input.txt"

  putStr "\tPart 1: Min fuel needed to align horizontal positions: (329389) "
  print (TreacheryOfWhales.minFuelForAlignmentWithConstantBurnRate input)

  putStr "\tPart 2: Min fuel needed to align with increasing burn rate: "
  print (TreacheryOfWhales.minFuelForAlignmentWithIncreasingBurnRate input)

printCheckedSolution :: (Eq a, Show a) => a -> a -> IO ()
printCheckedSolution actual expected = do
  let actualStr = show actual
  let mark | actual == expected = "✅"
           | otherwise          = "❌ (expected " ++ show expected ++ ")"
  printf "%s %s\n" actualStr mark

solution08 :: IO ()
solution08 = do
  putStrLn "Day 08. Seven Segment Search"
  input <- parseSevenSegmentsDisplay "src/scratchpad/08-seven-segment-search.input.txt"

  putStr "\tPart 1: Number of times the digits 1, 4, 7, or 8 appear in output: "
  printCheckedSolution (SevenSegmentSearch.numOf1478AppearancesInOutput input) 330

  putStr "\tPart 2: Sum of output values: "
  printCheckedSolution (SevenSegmentSearch.sumOfOutputValues input) 1010472

solution09 :: IO ()
solution09 = do
  putStrLn "Day 09. Smoke Basin"
  input <- parseHeightMap "src/scratchpad/09-smoke-basin.input.txt"

  putStr "\tPart 1: Sum of the risk levels of all low points on heightmap: "
  printCheckedSolution (SmokeBasin.sumOfRiskLevelsOfLowPoints input) 417

  putStr "\tPart 2: Product of the 3 largest basins: "
  printCheckedSolution (SmokeBasin.productOf3LargestBasins input) 1148965

\end{code}
