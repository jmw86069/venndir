
# todo on venndir

## add notice of missing overlaps to `ggrender_venndir()`

## place set names at periphery of Venn circles

* outer label placement will be necessary when displaying
items inside each polygon. Even if I could determine the
count label size inside the polygon to place labels around
it, it seems non-ideal
* this process is different for proportional circles
which may be placed at various locations
* for Venn circles and ellipses the coordinates could
be pre-defined

## place count labels outside a polygon

* when polygons are small and count labels do not fit inside,
we need a method to push the label elsewhere, and draw a
line segment back to the polygon center.
* maybe a label_offset, which moves a label from its normal
position to a new position -- thus triggering a segment
between those two points.

## consider switching default `venndir()` to call ggplot2

* could move `ggrender_venndir()` into `render_venndir()` and
have just the one function serve both purposes.
* note that pkgdown output for most example pages is not
aligned, probably because `gridBase` is not compatible with
the ragg rendering engine used for pkgdown. Mixing base R
with grid R graphics is not going to be well-supported.


## find some better/humorous Venn diagram examples

* bonus points if the items include some sort of image
or icon that could be placed beside the item labels.
