{-# OPTIONS_GHC -Wall #-}

import Data.Char (isAlpha, ord)
import Data.List (sort)
import Data.String (IsString (fromString))
import Data.Text.Lazy (Text, split, unpack)
import System.IO
  ( BufferMode (LineBuffering),
    IOMode (ReadMode),
    hGetLine,
    hSetBuffering,
    withFile,
  )

getNameScore :: (Text, Int) -> Int
getNameScore (name, pos) = score
  where
    startingOrdValue = ord 'A' - 1
    score =
      pos
        * sum
          ( map (\c -> ord c - startingOrdValue) (filter isAlpha (unpack name))
          )

-- In an imperative language, I'd have thoughtlessly written a parameter-less
-- function (hard-corded) to fetch the names from disk. But Haskell's functional
-- nature makes me think about what it means for a function to take no
-- parameters. Something about that feels wrong. Granted, I could make it take
-- the filepath as a parameter. [1] suggests something like
-- `sumOfNameScores :: Int`.
--
-- Not to fret though. I ended up placing the I/O logic in the main function,
-- so `sumOfNameScores` is a proper function with input paramaters.
--
-- [1]: https://stackoverflow.com/a/52349907/7812406
sumOfNameScores :: [Text] -> Int
sumOfNameScores names = score
  where
    sortedNames = sort names

    -- [1] contends that needing an index while processing a list is a code
    -- smell for an inefficient algorithm, and that we should look into using a
    -- vector.
    --
    -- Currying is the process of transforming a function that takes multiple
    -- arguments in a tuple as its argument, into a function that takes just a
    -- single argument and returns another function which accepts further
    -- arguments, e.g. `g :: (a, b) -> c` after currying becomes into `f :: a ->
    -- (b -> c)`. Currying is especially useful as it allows partial
    -- application. [2]
    --
    -- [1]: https://stackoverflow.com/a/16191987/7812406
    -- [2]: https://wiki.haskell.org/Currying
    score = sum (zipWith (curry getNameScore) sortedNames [1 ..])

-- Pure code refers to functions that always return the same result when given
-- the same input, have no side effects (e.g. I/O on files, printing to screen),
-- and never alter state. I/O in Haskell is impure code. [1]
--
-- A side effect is anything that causes the evaluation of an expression to
-- interact with something outside itself. Side effects are time-sensitive, e.g.
--
-- * It matters when one modifies a global variable as it may affect the
--   evaluation of other expressions.
-- * It matters when an item is printed to screen as it may need to be in a
--   certain order w.r.t. other writes to the screen.
-- * It matters when we read from a file or the network as the contents of the
--   file may affect the outcome.
--
-- [2]
--
-- [1]: http://book.realworldhaskell.org/read/io.html
-- [2]: https://www.schoolofhaskell.com/user/school/starting-with-haskell/introduction-to-haskell/6-laziness#side-effects-and-purity
main :: IO ()
main = do
  putStr "The total of all the name scores in names.txt is "

  -- Open file handles ought to be closed. [1] suggested using the `openFile`
  -- function together with the `bracket` function to ensure that the handle got
  -- closed. However, HLint further suggested the `withFile` function, which is
  -- less verbose, and achieves the same safety. [1] does not mention the
  -- `withFile` function (maybe it was introduced later?)
  --
  -- [1]: http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html#find.acquire.use.release
  withFile
    "scratchpad/names.txt"
    ReadMode
    ( \h -> do
        -- Haskell tries to make educated guesses on the buffering used in I/O.
        -- In this case though, we know that names.txt is a list of
        -- comma-delimited names in one line, so `LineBuffering` makes more
        -- sense. The default was `BlockBuffering Nothing`, which means use an
        -- implementation-defined block size.
        --
        -- [1]: http://book.realworldhaskell.org/read/io.html#id615631
        hSetBuffering h LineBuffering

        -- The `<-` binds the result from executing an I/O action to a name.
        --
        -- [1]: http://book.realworldhaskell.org/read/io.html#x_CC
        allNamesStr <- hGetLine h

        -- Surprised that Haskell's core library lacks a string splitter!
        --
        -- The `fromString` is needed to form a `Text` out of the `String` type
        -- (which is an alias for `[Char]`).
        --
        -- [1]: https://stackoverflow.com/a/37895439/7812406

        -- If `?` is an operator, then:
        --
        --    `(?y)` is equivalent to the function `\x -> x ? y`
        --
        --    `(y?)` is equivalent to the function `\x -> y ? x`
        --
        -- [1]: https://www.schoolofhaskell.com/user/school/starting-with-haskell/introduction-to-haskell/4-higher-order-programming-and-type-inference#anonymous-functions
        print $ sumOfNameScores (split (== ',') (fromString allNamesStr))
    )
