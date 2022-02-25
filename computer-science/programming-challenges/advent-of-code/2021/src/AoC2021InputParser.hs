{-# OPTIONS_GHC -Wall #-}

-- The `(..)` syntax represents all of the constructors for the data type. [1]
-- Without that export, we can pattern-match in BinaryDiagnostic.hs because we
-- run into a "Not in scope: data constructor ‘BinaryDiagnostics’" error.
--
-- [1]: https://stackoverflow.com/a/34548070/7812406
module AoC2021InputParser (parseBinaryDiagnosticInput) where

import BinaryDiagnostic.BinaryDiagnostic (BinaryDiagnostics(..), diagNums, diagWidth)
import Control.DeepSeq (($!!))
import Data.String (IsString (fromString))
import Paths_advent_of_code_y2021 (getDataFileName)
import System.IO (IOMode (ReadMode), hGetContents, withFile)
import Data.Maybe (listToMaybe)
import Data.Char (digitToInt)

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
        -- [3]: https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:readFile
        return $!! (BinaryDiagnostics{diagWidth=width, diagNums=map readBin' ls})
    )
