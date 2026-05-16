%include polycode.fmt
---
title: "AoC 2021 Parsing Arguments"
date: 2022-04-11
weight: 99
---

\## Parsing the Command Line

{{% cite haskellWikiCmdLineOpts %}} lists a couple of libraries. {{% cite
optParse %}} and {{% cite CmdArgs %}} are the most popular options. Going with
{{% cite optParse %}} as it's slightly more popular.

\begin{code}
{-# OPTIONS_GHC -Wall #-}

module AoC2021Args (Args(..), aocArgParser) where

import Options.Applicative
import Data.Semigroup ((<>))

data Args = Args
    { day :: Int }

aocArgParser :: Parser Args
aocArgParser = Args
    <$> option auto
        ( long "day"
       <> help "The day to run code for. Use '0' to run all of the solutions."
       <> showDefault
       <> value 0
       <> metavar "DAY") -- The metavariable is displayed in the help text.

\end{code}

A type `a` is a `Semigroup` if it provides an associative function (`<>`) that
lets you combine any two values of type `a` into one, and the following holds:

```hs
(a <> b) <> c == a <> (b <> c)
```

{{% cite Data.Semigroup %}}

{{% comment %}}

Found a way of getting the min/max from a list of numbers!

The `sconcat` function allows us to combine multiple values, e.g.
`sconcat (1 :|| [2, 3, 4]) :: Max Int`, which evaluates to `Max {getMax = 4}`.
`(1 :|| [])` is equivalent to `[1]`, but is guaranteed to be non-empty. The
`sconcat` function requires a non-empty list. {{% cite Data.Semigroup %}}

{{% /comment %}}

With the integration in the `main` functions of [Main]({{< ref "../app/Main"
\>}}) and [AoC2021Test]({{< ref "../test/AoC2021Test" >}}), I'm able to run code
like `time cabal run advent-of-code-y2021 -- --day 9`.

\## References

1. {{< citation
    id="haskellWikiCmdLineOpts"
    title="Command line option parsers - HaskellWiki"
    url="https://wiki.haskell.org/Command_line_option_parsers"
    accessed="2022-04-11" >}}

1. {{< citation
    id="optParse"
    title="optparse-applicative: Utilities and combinators for parsing command line options"
    url="https://hackage.haskell.org/package/optparse-applicative"
    accessed="2022-04-11" >}}

1. {{< citation
    id="CmdArgs"
    title="cmdargs: Command line argument processing"
    url="https://hackage.haskell.org/package/cmdargs"
    accessed="2022-04-11" >}}

1. {{< citation
    id="Data.Semigroup"
    title="Data.Semigroup"
    url="https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Semigroup.html"
    accessed="2022-04-11" >}}
