%include polycode.fmt
---
title: "AoC 2021 Day 08: Seven Segment Search"
date: 2022-03-07
weight: 8
---

{{< citation
    id="AoC2021-08"
    title="Day 8 - Advent of Code 2021"
    url="https://adventofcode.com/2021/day/8"
    accessed="2022-03-07" >}}

\## Part I Description

*You barely reach the safety of the cave when the whale smashes into the cave
mouth, collapsing it. Sensors indicate another exit to this cave at a much
greater depth, so you have no choice but to press on.*

*As your submarine slowly makes its way through the cave system, you notice that
the four-digit seven-segment displays in your submarine are malfunctioning;
they must have been damaged during the escape. You'll be in a lot of trouble
without them, so you'd better figure out what's wrong.*

*Each digit of a seven-segment display is rendered by turning on or off any of
the seven segments named `a` through `g`:*

```md
  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
```

*So, to render a `1`, only segments `c` and `f` would be turned on; the rest
would be off. To render a `7`, only segments `a`, `c`, and `f` would be turned
on.*

*The problem is that the signals which control the segments have been mixed up
on each display. The submarine is still trying to display numbers by producing
output on signal wires `a` through `g`, but those wires are connected to
segments randomly. Worse, the wire/segment connections are mixed un separately
for each four-digit display! (All of the digits within a display use the same
connections, though.)*

*So, you might know that only signal wires `b` and `g` are turned on, but that
doesn't mean segments `b` and `g` are turned on: the only digit that uses two
segments is `1`, so it must mean segments `c` and `f` are meant to be on. With
just that information, you still can't tell which wire (b/g) goes to which
segment (c/f). For that, you'll need to collect more information.*

*For each display, you watch the changing signals for a while, make a note of
all ten unique signal patterns you see, and then write down a single four-digit
output value (your puzzle input). Using the signal patterns, you should be able
to work out which patterns corresponds to which digit.*

*For example, here's what you might see in a single entry in your notes:*

```txt
acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab || cdfeb fcadb cdfeb cdbaf
```

*Each entry consists of ten unique signal patterns, a `||` delimiter, and
finally the four-digit output value. Within an entry, the same wire/segment
connections are used (but you don't know what the connections actually are). The
unique signal patterns correspond to the ten different ways the submarine tries
to render a digit using the current wire/segment connections. Because `7` is the
only digit that uses three segments, `dab` in the above example means that to
render `7`, signal lines `d`, `a`, and `b` are on. Because `4` is the only digit
that uses four segments, `eafb` means that to render a `4`, signal lines `e`,
`a`, `f`, and `b` are on.*

*Using this information, you should be able to work out which combination of
signal wires corresponds to each of the ten digits. Then, you can decode the
four digit output value. Unfortunately, in the above example, all of the digits
in the output value `cdfeb fcadb cdfeb cdbaf` use five segments and are more
difficult to deduce.*

*For now, focus on the easy digits. Because the digits `1`, `4`, `7`, and `8`
each use a unique number of segments, you should be able to tell which
combinations of signals correspond to those digits.*

***In the output values (the part after the `||` on each line), how many times
do digits `1`, `4`, `7`, or `8` appear?***

