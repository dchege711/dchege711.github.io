%include polycode.fmt
---
title: "AoC 2021 Main"
date: 2022-04-11
weight: 95
---

\begin{code}

module Main where

import qualified AoC2021 (runSolution)
import AoC2021Args (Args(..), aocArgParser)

import Options.Applicative

main :: IO ()
main = runAoCSolutions =<< execParser opts
  where
    opts = info (aocArgParser <**> helper)
      ( fullDesc
     <> progDesc "Run solutions for a given DAY of AoC 2021"
     <> header "A module for running solutions for AoC 2021")

runAoCSolutions :: Args -> IO ()
runAoCSolutions (Args d) = AoC2021.runSolution d

\end{code}
