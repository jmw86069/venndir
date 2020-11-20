
# venndir 0.0.10.900

## updates to existing functions

* `render_venndir()` by default groups gridtext labels
together, and draws one contiguous background and border
around them all. It currently only works for R base plots,
since it needs to know the grob sizes upfront.
That said, at this point the base R plot looks
substantially better than ggplot2 when labels are placed on 
the outside of the Venn circles. Hopefully I will
figure out how to do the same for ggplot2 output.


## new functions

* `draw_gridtext_groups()` does the work for grouping
multiple `gridtext::richtext_grob()` labels together,
using `x, y, overlap_set` to define the grouping.
This method also aligns text relative to a line segment
if present, by using `degrees_to_adj()`. A by-product
of the approach is that labels are now centered by
the label width, instead of having left/right
components which may differ in width.


# venndir 0.0.9.900

Began adding `testthat` tests for various functions.


## one more refactor

Proportional Venn diagrams (Euler diagrams) warranted a refactor
in order to label each set even when one set is fully contained
inside another set.


## new functions

* `signed_counts2setlist()` is an import method similar to
`counts2setlist()` but to allow importing directional overlaps.
* `get_sp_buffer()` applies a buffer width to the interior of
a polygon, either with fixed width, or relative width based
upon the minimum width required to make the polygon disappear.
The relative value means `sp_buffer=0.5` can be used to
create a buffer at half-width, for a polygon of any size.
The purpose is to draw a line segment from outside the polygon
to "just inside" the polygon, using an amount appropriate
for the size and shape of the polygon.
As `rgeos::gBuffer()` has no ability to scale relative
to the polygon size and shape, `get_sp_buffer()` quickly
runs a sweep to determine the `width` where an internal
polygon is just barely non-zero, and defines that as
`sp_buffer=-1`.
* `mean_degrees()` takes a vector of angles in degrees and
returns the mean angle based upon the sum of unit vectors.
It adds a small random bit to prevent vectors from exactly
cancelling out, and an argument `seed` that makes the randomness
reproducible.
* `mean_degree_arc()` is similar to `mean_degrees()` except
it takes an ordered set of angles that define an arc, and
returns the mean position along that arc. Use argument `do_plot=TRUE`
to see a visual representation of the options.
It turns out that spreading text labels along an arc only
worked when the mean angle was fixed inside the arc. Otherwise
occassionally the mean angle could be exactly 180 degrees
off, when the angles span more than 180 degrees, and if
multiple labels were present at each end. You'd think that
would be rare, but it happened enough to notice the bug
and to implement better logic.
* `degrees_to_adj()` is a helper function that takes an
angle in degrees, and returns the text `adjx` and `adjy`
values so a text label will be positioned outside the angle.


## functions removed

* `ggrender_venndir()` was removed, use
`render_venndir(plot_style="gg")` in its place. It made sense
to keep the visualization logic inside one function.

## bug fixes

* `spread_degrees()` was not properly maintaining the initial
order of angles when it iteratively spread out a series of
angles. For example one group of angles could be spread
wide enough to overlap another existing angle, in which
case that angle should be kept outside the initial group,
instead of being placed into the middle. The telltale
sign is that line segments wildly criss-crossed inside
the polygon.


## changes to existing functions

* New argument `inside_percent_threshold` added to
`venndir()`, `render_venndir()`, and `venndir_label_style()`
used to position labels outside a polygon when its size
is below this percent of the total polygon size. It works
as a reasonable alternative for determining if the label
visually fits inside a polygon.
* `venndir()` can accept polygon shapes to use as input,
instead of defining its own or using those from
`eulerr::euler()`. New argument `venn_sp` accepts any
`sp::SpatialPolygons` compatible object, which must have
at least as many polygons as there are sets in `setlist`.
* `venndir()` now adds the Venn shapes to the output `venn_spdf`
with `type="set"`; all other polygons have `type="overlap"`.
The shapes will serve as an anchor for set labels, in the
event we want to label one or more circles -- like when one
circle is fully contained inside another.
* `polygon_label_outside()` now determines a reasonable text
alignment `adj` based upon the angle offset, to help minimize
label overlap with the line segment.
* `polygon_label_outside()` was updated to correct some internal
use of coordinates. New argument `sp_buffer` to allow the
line segment to end slightly inside each polygon.
* `get_venn_shapes()` default 4-way Venn ellipses were slightly
adjusted so the middle ellipses broke the outer ellipses
into two polygons. This change helps the automated position
of labels by `polygon_label_outside()` with certain settings.
* `venndir_label_style()` was refactored to be able to
position labels inside or outside the polygon, for four
types of labels:

* `"set"` - e.g. `"set_A"`
* `"overlap"` - e.g. `"set_A&set_B"` normally this label is not shown
* `"count"` - e.g. `10`
* `"items"` - e.g. `c("item1", "item3", "item9")` for each overlap set.

It has some `label_preset` options, which help position set and
count labels inside or outside.


# venndir 0.0.8.900

One more refactor is in progress. Label coordinates will
be pre-calculated inside and outside each polygon, then
labels can be toggled in or out (or moved to a custom
location). The main driver is to be able to label the
Venn circles on the perimeter of the diagram. For proportional
circles, when a set is fully contained inside another
set, there is no polygon with that set alone -- thus
the defaul labeling fails.

## new functions

* `polygon_label_outside()` defines label placement outside
a polygon, one of two methods: "label" positions outside
using relative label positioning; "bbox" positions outside
using relative polygon position within the bounding box.
* `expand_range()` is a simple function to expand a numeric
range by `expand_fraction`, mainly for R plots to have
space to render labels outside a central Venn diagram.
* `spread_degrees()` takes a vector of angles in degrees
as input, and returns a vector where angles are separated
by at least `min_degrees`.
* `mean_degrees()` calculates the mean angle in degrees
for an input vector of angles.
* `diff_degrees()` takes a vector of angles in degrees,
and returns the angular difference. Suitable for cases
with angles like `c(355, 2, 12)` where the difference
between `355` and `2` is `7`.
* `sp_polylabelr()` is a simple wrapper function to
`polylabelr::poi()` for `sp::SpatialPolygons` objects.

# venndir 0.0.7.900

Several refactoring processes being implemented, some
further updates ad bugfixes may be necessary. Pushed
this version for more testing on CentOS where the
`gridtext` package was hard to install for GCC below
version 5.3, but was installed with GCC 5.3.

## new dependencies

* Added dependencies:

   * `sf` package dependency for ggplot2 output
   * `gridtext` and `ggtext` for label output
   * `gridBase` for base R plotting

## bug fixes

* `render_venndir()` added `sp` prefix to `sp::plot()` for
base R plotting.
* `signed_overlaps()` was updated to allow using `sep="|"`.
Previous this delimited was used to make unique rownames,
and needed to be parsed properly to prevent errors.
* `venndir()` new argument `sep` which is now properly passed
to `signed_overlaps()` and `get_venn_shapes()`.
* `find_vennpoly_overlaps()`, `overlaplist2setlist()`, `counts2setlist()`
now call `strsplit(..., fixed=TRUE)` so the `sep` character is not
treated like a regular expression, thus allowing delimiter `sep="|"`
if specified.

## changes to existing functions

* `textvenn()` new argument `sep` to allow customizing
the delimiter between set names, for example the default `sep="&"`
creates overlap names like `set_A&set_B`; but `sep=" | "`
creates overlap names like `set_A | set_B`.
* `venndir()` new argument `show_set` to customize display of each
set name.

   * `show_set="main"` displays only the set name for main sets
   * `show_set="all"` displays all set names in each overlap
   * `show_set="none"` displays no set name
   * `show_set="boundary"` *not yet implemented* will display the set
   label for each circle/ellipse boundary


# venndir 0.0.6.900

Much closer to a release-ready form. It needs the ggplot2
option enabled with the newest `render_venndir()` logic.

## bug fixes / changes

* Fixed obscure bug where set names could fail if not provided
in alphabetic order.
* Changed `venndir()` default `show_zero=FALSE` to hide empty labels.
* Removed `ggrender_venndir()` from the exported functions list,
this function needs to be included inside `render_venndir()`.

## refactoring the details

More refactoring may take place to simplify the workflow,
until the package is publicized.

* `label_df$col` renamed to `label_df$color` for consistency,
and preparing for use by others.
* `ggrender_vendir()` will be retired, or will be converted to
a simple wrapper function which calls `render_venndir()`.

## new features

* `label_polygon_fill()` new arguments `dither_cex` and `dither_color`
which slightly adjust the `cex` and `color` values. The
`cex` is uniformly distributed +/- the `dither_cex` value,
as a fraction of the `cex`, the default is roughly 4% variation
in `cex`. The `dither_color` is similar, it adds some
heterogeneity to `darkFactor` and `sFactor` and calls
`jamba::makeColorDarker()` to adjust brightness and saturation.
* `label_polygon_fill()` can plot the polygon buffer behind the
labels, to review the effect of adjusting `scale_width` for each
polygon.
* `label_df` uses `show_label` and `show_items` to control visibility
of the count label, and item labels, respectively. When the values
are `NA` it applied default rules, which favors showing count
label, unless the count label is moved outside the polygon.
* `label_df` recognizes `x_offset,y_offset` to nudge
count labels somewhere else. Currently manual, this adjustment
may be automated at some point. When a label is moved outside
its parent polygon, a line segment is drawn from the label
to just inside the polygon boundary. When no items are displayed,
the segment is drawn more toward the center.
* `venndir_label_style()` new function to control the visual
style of count labels, with option to fill and draw a border
around labels.
* `polygon_label_segment()` is used when moving a label from inside
a polygon to somewhere that may or may not be inside that polygon.
When it is moved outside, it returns the point on the polygon
boundary along that line segment. It can be used with `sp_buffer`
so the line can be drawn slightly inside the polygon boundary.

# venndir 0.0.5.900

## bug fixes

* `sp_ellipses()` argument renamed from `radius` to `xradius`
and `yradius`.

## display item labels inside Venn polygons

* `ggrender_venndir()` and `render_venndir()` both allow optional
display of items inside the Venn polygons. For relatively small
sets (<500) this works fairly well. You can configure the label
to include the `"sign"`, and/or `"item"`. It works as an
alternative or supplement to using proportional Venn diagrams.


## new render functions

* `ggrender_venndir()` renders a venndir diagram using ggplot2.
Note that this function requires the package `sf`, and in future
other objects internal to venndir may be converted from `sp` to `sf`.
* `label_polyogn_fill()` takes a `sp::SpatialPolygons` object,
a vector of labels, and returns coordinates to place labels
inside the polygon. It has some adjustments to add buffer
around polygon boundaries, but does not directly detect
label overlaps. It either plots labels in base R graphics,
or returns coordinates suitable for ggplot2.

## new `rescale_*()` functions

The `rescale_*()` functions
are intended to allow manipulation of numeric coordinates in
each relevant object type, aimed at `sp::SpatialPolygons`
and the various sub-types.

* `rescale_sp()` manipulates `sp::SpatialPolygons` which
contains one or more `sp:Polygons` objects, and therefore calls:
* `rescale_ps()` manipulates `sp:Polygons` which contains
one or more `sp:Polygon` objects, and therefore calls:
* `rescale_p()` manipulates `sp:Polygon` which contains
numeric `matrix` coordinates, and therefore calls:
* `rescale_coordinates()` manipulates numeric `matrix`
coordinates.

The `rescale_*()` operations enable three types of manipulations:

1. `scale` - multiplies coordinates for each axis
2. `rotate_degrees` - rotates points in degrees
3. `shift` - shifts coordinates by adding a constant to each axis
4. `center` - the `scale` and `rotate_degree` are applied relative
to the `center`, which by default uses the mean bounding box.

## new conversion functions

* `list2im_opt()` - optimized method to convert a list of character
vectors to a logical incidence matrix. The `list` names become
colnames, which represent the names of each set. The rownames
represent items.
* `list2im_value()` - convert list of named vectors to a value
incidence matrix. The list vectors are named by item, and the
vector values are stored in the matrix.
* `im2list()` - convert incidence matrix to list of character vectors.
* `im_value2list()` - convert value incidence matrix to a list
of vectors. Vectors are named by item, and values are from the
incidence matrix.
* `counts2setlist()` - convert vector of overlap counts to setlist,
intended for compatibility with other packages like `eulerr`,
`upsetr`, and `VennDiagram`.

## changes to existing functions

* `venndir()` new argument `alpha_by_counts` adjusts
alpha transparency of each overlap based upon the number
of counts. Implemented to evaluate the visual effect for now.


# venndir 0.0.4.900

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


# venndir 0.0.3.900

## new functions

* `venndir()` the core visual function that displays either
Venn diagram or a Euler diagram for proportional diagrams.

# venndir 0.0.2.900

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
