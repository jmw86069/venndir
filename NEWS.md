# venndir version 0.0.4.900

## changes to existing functions

* `venndir()` no longer renders the plot, but calls
`render_venndir()` in order to offer more customized
options. It also allows editing the source data as
needed, like adjusting individual overlap settings.
* `textvenn()` now supports directional Venn counts,
even with `"each"` it uses two columns for all possible
directions for the central 3-way overlaps.

## new functions

* `render_venndir()` renders a Venn or Euler diagram using
the output format from `venndir()`. It displays a warning
at the bottom of the plot whenever a Venn overlap cannot
be displayed, for example with proportional Venn (Euler)
diagrams when an overlap cannot be represented. This label
can be hidden, but by default is displayed so the information
is not lost.


# venndir version 0.0.3.900

## new functions

* `venndir()` the core visual function that displays either
Venn diagram or a Euler diagram for proportional diagrams.

# venndir version 0.0.2.900

The `venndir` package is intended for analyses where direction
is encoded alongside each item in a set. The driving example
is for gene expression analysis, which results in a subset
of differentially expressed genes, where the expression can
either by "up-regulation" or "down-regulation." When comparing
two sets of differentially expressed genes, it is helpful
if not essential to recognize when any overlaps also share
the same direction of change (or opposite direction.)

Although there are some 40,000 gene loci in human (give or take),
it is common to see many of the same few hundred genes
differentially regulated across a very diverse array of
experiments. For two experiments to identify 800 genes each,
with overlap of 400, these outcomes illustrate the utility
of venndir:

* 400 genes overlap, 200 share the same direction, 200 are opposite.
In this case, the overlap may represent response by similar functional
set of genes, but with different biological drivers.
* 400 genes overlap, 396 genes share the same direction. In this case,
the overlap could be seen as highly similar between the two sets.
* 400 genes overlap, only 4 genes share the same direction. In this
case, the overlap could be see as nearly exactly opposite.

## new functions

* `textvenn()` prints a Venn diagram using console text output.
It also supports directionality, the formatting is still being
adjusted for best visual appeal.
* `signed_overlaps()` is the core overlap function for venndir,
it performs directional and direct overlap, with configurable
output. It returns a `data.frame` with counts for each Venn overlap
set, and optionally includes items. The exact output format may
be adjusted in future, currently it returns one row per overlap.
For directional overlaps, it returns one row per combination of
up/down.
* `make_venn_test()` is a convenience function to generate test
sets for Venn diagrams. It can return simple sets, or sets with
directionality. It is mainly intended to help create very large
sets to test efficiency of the process.
