%include polycode.fmt
---
title: "AoC 2021 Day 10: Syntax Scoring"
date: 2022-04-12
weight: 10
---

{{< citation
    id="AoC2021-10"
    title="Day 10 - Advent of Code 2021"
    url="https://adventofcode.com/2021/day/10"
    author="Eric Wastl"
    accessed="2022-04-12" >}}

\## Part I

You ask the submarine to determine the best route out of the deep-sea cave, but
it only replies:

```shell
Syntax error in navigation subsystem on line: all of them
```

The navigation subsystem is made of several lines containing chunks. There are
one or more chunks on each line, and chunks contain zero or more other chunks.
Adjacent chunks are not separated by any delimiter; if one chunk stops, the next
chunk (if any) can immediately start. Every chunk must open and close with one
of four legal pairs of matching characters:

* If a chunk opens with `(`, it must close with `)`.
* If a chunk opens with `[`, it must close with `]`.
* If a chunk opens with `{`, it must close with `}`.
* If a chunk opens with `<`, it must close with `>`.

So, `()` is a legal chunk that contains no other chunks, as is `[]`. More
complex but valid chunks include `([])`, `{()()()}`, `<([{}])>`,
`[<>({}){}[([])<>]]`, and even `(((((((((())))))))))`.

Some lines are incomplete, but others are corrupted. Find and discard the
corrupted lines first.

A corrupted line is one where a chunk closes with the wrong character - that is,
where the characters it opens and closes with do not form one of the four legal
pairs listed above.

Examples of corrupted chunks include `(]`, `{()()()>`, `(((()))}`, and
`<([]){()}[{}])`. Such a chunk can appear anywhere within a line, and its
presence causes the whole line to be considered corrupted.

To calculate the syntax error score for a line, take the first illegal character
on the line and look it up in the following table:

* `)`: 3 points.
* `]`: 57 points.
* `}`: 1197 points.
* `>`: 25137 points.

Find the first illegal character in each corrupted line of the navigation
subsystem. What is the total syntax error score for those errors?
