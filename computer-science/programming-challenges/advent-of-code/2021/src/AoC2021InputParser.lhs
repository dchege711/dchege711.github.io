%include polycode.fmt
---
title: "AoC 2021 Input Parser"
date: 2022-03-07
weight: 26
---

\begin{code}

{-# OPTIONS_GHC -Wall #-}

-- {-# LANGUAGE FlexibleContexts #-}

-- The `(..)` syntax represents all of the constructors for the data type. [1]
-- Without that export, we can pattern-match in BinaryDiagnostic.hs because we
-- run into a "Not in scope: data constructor ‘BinaryDiagnostics’" error.
--
-- [1]: https://stackoverflow.com/a/34548070/7812406
module AoC2021InputParser
  ( parseBinaryDiagnosticInput,
    parseBingoInput,
    parseHydrothermalVents,
    parseLanternfishInternalTimers,
    parseHorizontalCrabPositions,
    parseSevenSegmentsDisplay,
    parseHeightMap,
  )
where

import BinaryDiagnostic.BinaryDiagnostic (BinaryDiagnostics (..), diagNums, diagWidth)
import Control.DeepSeq (($!!))
import Data.Char (digitToInt, ord)
import Data.Maybe (isJust, listToMaybe)
import Data.String (IsString (fromString))
import qualified Data.List.Split as Split
import qualified Data.Vector as V
import GiantSquid (Board, DrawnNumbers, Tile)
import qualified HydrothermalVenture.HydrothermalVenture as HV (LineSegment (..), Point (..))
import Paths_advent_of_code_y2021 (getDataFileName)
import System.IO (IOMode (ReadMode), hGetContents, withFile)
import Text.Parsec (endOfLine)
import Text.ParserCombinators.Parsec
import Text.Read (readMaybe)
import qualified AoC2021.SevenSegmentSearch as SevenSegmentSearch (SevenSegmentDisplay (..))
import qualified Data.IntSet as IntSet
import qualified AoC2021.SmokeBasin as SmokeBasin (HeightMap)
import qualified Data.Massiv.Array as MassivArray (empty, fromLists')
import qualified Data.Massiv.Core as MassivCore (Comp(Seq))

\end{code}

\## Day 03: Binary Diagnostic

\begin{code}

-- The `Numeric` module has a `readBin` function [1], but for some reason, I get
-- a "Variable not in scope: readBin" error. However, `readDec`, `readOct` and
-- `readHex` work...
--
-- [1]: https://hackage.haskell.org/package/base-4.16.0.0/docs/Numeric.html#v:readBin
readBin' :: String -> Int
readBin' binString = fst $ foldr f (0, 1) binString
  where
    f c (s, powerOf2) = (s + powerOf2 * digitToInt c, powerOf2 * 2)

-- The file is a list of binary digits of the same width, e.g.
--
-- 00100
-- 11110
parseBinaryDiagnosticInput :: FilePath -> IO BinaryDiagnostics
parseBinaryDiagnosticInput fp = do
  validatedFP <- getDataFileName fp
  withFile
    validatedFP
    ReadMode
    ( \h -> do
        s <- hGetContents h
        let ls = lines (fromString s)
        let width = maybe 0 length (listToMaybe ls)
        -- With lazy I/O, `h` gets closed as soon as we leave `withFile`. This
        -- introduces errors of the form "hGetContents: illegal operation
        -- (delayed read on closed handle)". [1]
        --
        -- In the expression `f $!! x`, `x` is fully evaluated before `f` is
        -- applied to it. [1] [2]
        --
        -- Alternatively, one can use `readFile` which holds the file open until
        -- it has finished reading the file [1]. However, [3] says that
        -- `readFile` reads the file lazily. Hmm...
        --
        -- [1]: https://stackoverflow.com/a/26949379/7812406
        -- [2]: https://hackage.haskell.org/package/deepseq-1.4.6.1/docs/Control-DeepSeq.html#v:-36--33--33-
        -- [3]: https://hackage.haskell.org/package/base-4.16.0.0/docs/html#v:readFile
        return $!! (BinaryDiagnostics {diagWidth = width, diagNums = map readBin' ls})
    )

\end{code}

\## Day 04: Giant Squid

\begin{code}

-- `endBy` expects the very last item to be followed by the separator. It
-- continues parsing until it can't parse any more content. [1]
--
-- TODO: Is it possible to define `bingoFile` as `endBy bingoSection doubleEOL`?
-- The last double EOL is partly consumed by `bingoElementSeparator` that also
-- matches a new line.
--
-- [1]: http://book.realworldhaskell.org/read/using-parsec.html
bingoFile :: Parser [[String]]
bingoFile = endBy bingoSection endOfLine

-- `sepBy` takes two functions as arguments. The first function parses some sort
-- of content, while the second function parses a separator. `sepBy` starts by
-- trying to parse content, then separators, and alternates back and forth until
-- it can't parse a separator. It returns a list of all the content it was able
-- to parse. [1]
--
-- [1]: http://book.realworldhaskell.org/read/using-parsec.html
bingoSection :: Parser [String]
bingoSection = sepBy bingoElement bingoElementSeparator

bingoElement :: Parser String
bingoElement = many digit

bingoElementSeparator :: Parser Char
bingoElementSeparator = try (char ',') <|> try (char ' ') <?> "separator for element"

-- `try` applies a parser, and if it fails, then `try` behaves as if it hadn't
-- consumed any input at all, and tries the option on the right of the `<|>`.
-- Note that `try` only has an effect if its on the left side of a `<|>`. [1]
--
-- `<?>` tries the parser on its left. In the event of a failure, it presents an
-- error message instead of trying another parser. The error message should
-- complete the sentence, "Expecting...". [1]
--
-- [1]: http://book.realworldhaskell.org/read/using-parsec.html
-- eol = try (string "\n\r\n\r")
--         <|> try (string "\r\n\r\n")
--         <|> try (string "\n\n")
--         <|> string "\r\r"
--         <?> "end of line followed by empty line"

-- File format: the first line contains the numbers to draw. The rest is a new
-- line followed by a 5x5 grid of numbers representing a board.
--
-- 7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
--
-- 22 13 17 11  0
--  8  2 23  4 24
-- 21  9 14 16  7
--  6 10  3 18  5
--  1 12 20 15 19
--
--  3 15  0  2 22
--  9 18 13 17  5
-- 19  8  7 25 23
-- 20 11 10 24  4
-- 14 21 16 12  6
parseBingoInput :: FilePath -> IO (DrawnNumbers, [Board])
parseBingoInput fp = do
  dataFp <- getDataFileName fp
  -- Objective: Try using `readFile` and see if `($!!)` is needed to fully
  -- evaluate the contents before exiting this function.
  fileContents <- readFile dataFp
  case parse bingoFile "" fileContents of
    Left e -> do
      putStrLn "Error parsing input"
      print e
      return ([], [])
    Right r -> do
      let parseInt :: String -> Int
          parseInt s = read s :: Int

          drawnNumbers = map parseInt (head r)

          tile :: String -> Tile
          tile x = (parseInt x, False)

          -- TODO: Figure out how to parse multiple spaces as
          -- separators, and get rid of the `isValidNum` filter.
          isValidNum :: String -> Bool
          isValidNum s = isJust (readMaybe s :: Maybe Int)

          parseBoards :: [[String]] -> [Board]
          parseBoards ([_] : l1 : l2 : l3 : l4 : l5 : ls) =
            let nums = V.filter isValidNum (V.fromList (l1 ++ l2 ++ l3 ++ l4 ++ l5))
                board = V.map tile nums
             in (board, False) : parseBoards ls
          parseBoards [l1, l2, l3, l4, l5] =
            let nums = V.filter isValidNum (V.fromList (l1 ++ l2 ++ l3 ++ l4 ++ l5))
                board = V.map tile nums
             in [(board, False)]
          parseBoards _ = []

      return (drawnNumbers, parseBoards (tail r))

\end{code}

\## Day 05: Hydrothermal Venture

\begin{code}

hydrothermalFile :: Parser [HV.LineSegment]
hydrothermalFile = endBy hydrothermalLine endOfLine

-- Parses "0,9 -> 5,9" into a `LineSegment`.
hydrothermalLine :: Parser HV.LineSegment
hydrothermalLine =
  do p1 <- commaSeparatedCoordinates -- Underscores to avoid shadowing.
     _ <- string " -> "
     p2 <- commaSeparatedCoordinates
     return HV.LineSegment {HV.p1 = p1, HV.p2 = p2}

-- Parses "0,9" into (0, 9)
commaSeparatedCoordinates :: Parser HV.Point
commaSeparatedCoordinates =
  do x <- many1 digit
     _ <- char ','
     y <- many1 digit
     return HV.Point {HV.x=read x :: Int, HV.y= read y :: Int}

reportError :: ParseError -> IO ()
reportError e = do
  putStrLn "Error parsing input"
  print e

parseHydrothermalVents :: FilePath -> IO [HV.LineSegment]
parseHydrothermalVents fp = do
  dataFp <- getDataFileName fp
  fileContents <- readFile dataFp
  case parse hydrothermalFile "Hydrothermal Parser" fileContents of
    Left e -> do {reportError e; return []}
    Right r -> return r

singleLineCommaDelimitedFile :: Parser [Int]
singleLineCommaDelimitedFile =
  sepBy (do x <- many1 digit; return (read x :: Int)) (char ',')

parseSingleLineCommaDelimitedFile :: FilePath -> IO [Int]
parseSingleLineCommaDelimitedFile fp = do
  dataFp <- getDataFileName fp
  fileContents <- readFile dataFp
  case parse singleLineCommaDelimitedFile "Single Comma-Delimited Int Parser" fileContents of
    Left e  -> do {reportError e; return []}
    Right r -> return r

\end{code}

\## Day 06: Lanternfish

\begin{code}

parseLanternfishInternalTimers :: FilePath -> IO [Int]
parseLanternfishInternalTimers = parseSingleLineCommaDelimitedFile

\end{code}

\## Day 07: Treachery of Whales

\begin{code}

parseHorizontalCrabPositions :: FilePath -> IO [Int]
parseHorizontalCrabPositions = parseSingleLineCommaDelimitedFile

\end{code}

\## Day 08: Seven Segment Search

\begin{code}

-- Sample line:
-- fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
sevenSegmentsDisplayLine :: Parser SevenSegmentSearch.SevenSegmentDisplay
sevenSegmentsDisplayLine = do
  -- TODO: Is there syntax for avoiding the intermediate _allSegments variable?
  _allSegments <- many1 (noneOf "\r\n")
  let uniquePatternsAndOutputs = splitAt 10 $ Split.split (Split.dropDelims . Split.dropInnerBlanks $ Split.oneOf "| ") _allSegments

  let stringsToIntSets = map (IntSet.fromList . map ord) :: [String] -> [IntSet.IntSet]
  return SevenSegmentSearch.SevenSegmentDisplay{
    SevenSegmentSearch.uniquePatterns = stringsToIntSets (fst uniquePatternsAndOutputs),
    SevenSegmentSearch.outputValues = stringsToIntSets (snd uniquePatternsAndOutputs)}

sevenSegmentsDisplayFile :: Parser [SevenSegmentSearch.SevenSegmentDisplay]
sevenSegmentsDisplayFile = endBy sevenSegmentsDisplayLine endOfLine

parseSevenSegmentsDisplay :: FilePath -> IO [SevenSegmentSearch.SevenSegmentDisplay]
parseSevenSegmentsDisplay fp = do
  dataFp <- getDataFileName fp
  fileContents <- readFile dataFp
  case parse sevenSegmentsDisplayFile "Seven Segment Display" fileContents of
    Left e  -> do {reportError e; return []}
    Right r -> do {return r}

\end{code}

I gave up on using `parsec` to parse a line of the form `cg bdaec gdafb agbcfd
gdcbef || cg cg fdcagb cbg` because the `||` and space delimiter kept tripping
up my `endBy sevenSegmentsDisplayLine endOfLine` construction. Interested in how
others parsed this line.

{{% cite Mazon2021-08 %}} did `(digits, _:display) = splitAt 10 (map
readObservation (words l))`. This is more readable than my
`splitAt 10 $ Split.split (Split.dropDelims . Split.dropInnerBlanks $
Split.oneOf "|| ") l`. I was so preoccupied by taking care of the `||` that I
didn't ask whether I could pattern-match it away using `_:display`.

\## Day 09: Smoke Basin

\begin{code}

-- Sample line: "2199943210"
heightMapLine :: Parser [Int]
heightMapLine =
  do heights <- many1 digit
     return (map digitToInt heights)

heightMapFile :: Parser [[Int]]
heightMapFile = endBy heightMapLine endOfLine

parseHeightMap :: FilePath -> IO SmokeBasin.HeightMap
parseHeightMap fp =
  do dataFp <- getDataFileName fp
     fileContents <- readFile dataFp
     case parse heightMapFile "Height Map" fileContents of
       Left e -> do {reportError e; return MassivArray.empty}
       Right r -> do {return (MassivArray.fromLists' MassivCore.Seq r)}

\end{code}

\## References

1. {{< citation
  id="Mazon2021-08"
  author="Jean-Baptiste Mazon"
  title="AoC Day 8: Seven Segment Search"
  url="https://xn--sant-epa.ti-pun.ch/posts/2021-12-aoc/day08.html"
  accessed="2022-03-15" >}}
