{-# OPTIONS_GHC -Wall #-}

module AoC2021 (allSolutions) where

-- https://cabal.readthedocs.io/en/3.4/cabal-package.html#accessing-data-files-from-package-code

-- https://cabal.readthedocs.io/en/3.4/cabal-package.html#accessing-data-files-from-package-code
import AoC2021InputParser
    ( parseBinaryDiagnosticInput,
      parseBingoInput,
      parseHydrothermalVents,
      parseLanternfishInternalTimers,
      parseHorizontalCrabPositions, )
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

allSolutions :: IO ()
allSolutions = do
  solution01
  solution02
  solution03
  solution04
  solution05
  solution06
  solution07

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
