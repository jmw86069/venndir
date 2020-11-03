
# todo on venndir


## small features for roll-out

* DONE: Bug: Fix `polylabelr::poi()` when there is one or more holes
inside a polygon.
* Yes: change all arguments with `angle` to use `degrees` for
consistency, and to make clear the type of angle.
* Yes: refactor to handle `"set name"` separate from
`"main count"` in the count label, so the `"set name"` can
be moved, for example to perimeter or outside the polygon,
leaving the numeric count inside as needed.

   * Yes: Proportional diagram with one circle fully inside
   another does not have set name displayed -- it must be.
   Consider choosing one internal polygon with fewest set overlaps
   with the largest area.
   * need idea for where to store the set label coordinates,
   since the Venn circle/ellipse shape does not have its own
   row.
   * maybe venn shapes need to be stored in another `list`
   element returned by `venndir()`? It makes that output
   a bit too heavy imo.

* Maybe: new function `venndir_bender()` to modify a `venndir`
result. resize, move, rotate venndir circles/ellipses.
* Maybe: change default to `return_items=TRUE` for
`venndir()` and `signed_overlaps()`.

### refactor label position

Design ideas:

* each label has focal point `polylabelr::poi()` inside the polygon
* each label has outer point `polygon_label_outside()` outside the polygon
* label can be `"inside"` or `"outside"`
* label `x_offset,y_offset` can be defined upfront by
`polygon_label_outside()` then may be modified as needed
* labels now have three parts

   1. `"set_name"`
   2. main counts (total venn counts in each overlap set)
   3. signed counts (directional counts in each overlap set)

* label coordinates in `label_df` contain `x,y` and `x_offset,y_offset`

   * it does not handle `"set_name"` differently than `"main counts"`
   * it makes sense to allow `"set_name"` to be outside the polygon
   but the counts inside the polygon
   * maybe we need to store the Venn shapes (full circle) apart
   from the overlap polygons?


### update `polygon_label_outside()` for multiple labels

* The idea is to position labels by angle relative to the
same central point, detect labels "too close" to one another,
then spread them out evenly to assist with spacing between labels.
* Labels would be defined mainly by center point and angle in degrees.
* "Too close" would be defined by user-configurable minimum degrees,
with some suitable default value.


## features for wider roll-out


### make render_venndir() work for ggplot2 output

* Essentially roll `ggrender_venndir()` into `render_venndir()`
then remove `ggrender_venndir()` (or make it a silent alias.)
* Add missing counts text label to ggplot2 output. Plan to use
`ggtext::geom_textbox()` for a wider label in ggplot2.



## Miscellaneous items


### option to place set names at periphery of Venn circles

* This change could be most relevant with proportional Venn
diagrams especially when one circle is fully enclosed inside
another circle.
* outer label placement will be necessary when displaying
items inside each polygon. Even if I could determine the
count label size inside the polygon to place labels around
it, it seems non-ideal
* this process is different for proportional circles
which may be placed at various locations
* for Venn circles and ellipses the coordinates could
be pre-defined


### improve item label efficiency

* large number of labels makes it very inefficient -- find a
way to make it faster.
* one idea: instead of calling `gridtext` for each polygon, aggregate
them into a `data.frame` then display them in one call. Doubtful this
change will be much faster.
* another idea: the `sp::spsample()` function success rate is fairly
low, requiring iterating with higher `n + i` labels in order to
get `n` coordinates inside the polygon area. This iteration might
be rate-limiting with large `n`.


### make output compatible with plotly (or something interactive)

* Currently `ggtext::geom_richtext()` is not compatible with
`plotly::ggplotly()` but probably just lacks the `to_basic`
wrapper function. (See `splicejam::to_basic.GeomShape()` for
an example workaround.)
* This feature may require text labels at 0 or 90 degrees.


### venn meme function

* basic idea is to make it "easy" to create a Venn meme when
needed. The ability is already in place, but seems to have
very consistently different default values than other
Venn styles.


### example of displaying items in large label box beside the Venn diagram

* manual placement of a larger label block where it handles
word-wrapping itself, done by `gridtext`.


### display fully "closed" Venn counts

* "Closed" includes the universe of each input list which
is not represented in the Venn circles. For example:

   * genes tested in set_A that are not hits
   * genes tested in set_B that are not hits

* This option is based upon `limma` package display of total
genes in each set, displayed outside the main Venn circles.


### item labels

* It still sometimes fails to find enough labels inside the
polygon, usually for large number of labels. Workaround might
be for the user to change layout type to something like `"regular"`
or `"nonaligned"` which could improve the search success rate.

   * Add example to help text, and to Rmarkdown vignette.

* DONE: When there is only one item, consider using the center label
instead of choosing a location inside the polygon, which sometimes
chooses something away from the center.

   * Note that sometimes the `polylabelr::poi()` point is outside
   the polygon, which happens sometimes when the polygon has a hole
   or otherwise "weird" shape. The method checks for that and uses
   the buffer polygon `sp::spsample()` approach if the `polylabelr::poi()`
   point is not contained in the source polygon.


### `label_polygon_fill()` with relative buffer scale_width

* add option for scale_width to be relative to the object size
instead of absolute dimensions. For example:

   * `scale_width=-1` would produce no output polygon,
   because `-1` is interpreted `-100%`,
   * `scale_width=-0.99` would produce an extremely small polygon,
   roughly 1% the original width,
   * `scale_width=-0.5` would produce a polygon with roughly
   half width buffer.

* The test example should use a relatively tall-thin ellipse,
rotated to 45 degrees. The bounding box would be quite large,
but the actual width of the polygon would be relatively small.


### `label_polygon_fill()` extra features

* DONE: dither_cex,dither_color,dither_angle to adjust each text
label slightly in three ways: `cex` to make label font slightly
larger or smaller; `color` to make label color slightly lighter
or darker; `angle` to adjust the angle of text slightly more
or less than the original `angle` value. These effects can help
visibly distinguish large number of labels packed into a relatively
small polygon area.


### missing counts warning

* DONE: The warning needs to check both `show_label` and `show_items`
for each overlap set before deciding that a set is not
displayed.



### consider refactoring "base R plot" to use "grid graphics"

* Lower priority evaluation.
* Requires learning grid graphics in much more detail.
* The main benefit would be compatibility with `gridtext::richtext_grob()`
which uses grid graphics. Currently using `gridBase` to combine
`grid` and base R plotting works in interactive R sessions, but
in Rmarkdown it is sometimes glitchy, because Rmarkdown now calls
`ragg::agg_png()` and I'm almost sure this use would not be
recommended or supported. "Pick one method" is a reasonable
thing to ask of me.
* Another benefit is potential to calculate label width in a way
to detect overlaps, and/or resize font size to fit relative to
other labels.
* Another possibility is to re-position labels using relative
units, for example move main/signed labels to the left by
1.2x main label width.


### refactor venn count label styling

* DONE: `venndir_label_style()`

   * currently resides inside `venndir()` which makes it hard to
   change the style in `render_venndir()`. I guess ideally the
   count label style can be updated by a separate function
   to make it easy to change styles.


### calculate concordance coefficient

* Kruskal concordance: (agree - disagree) / (agree + disagree)
* Return concordance for each overlap set


### refactor venndir output

Most of the changes below are DONE.

* each polygon is represented per row in `venn_spdf`

   * label (set overlap)
   * venn_counts
   * count label x,y coordinates
   * count label x_offset,y_offset to displace a label outside the polygon
   (actually the offset will be stored in `label_df` for each label)
   * polygon fill, border
   * alpha, lwd, lty

* each label is represented in label_df, sometimes multiple labels per polygon

   * venn_counts
   * text (visible label)
   * overlap_set (matches venn_spdf$label)
   * type (main for total counts, signed for directional counts)
   * x,y coordinates for each label (they should move together but not required)
   * x_offset,y_offset for each label (make sure to draw each unique line only once)
   * col (label text color), fill (label box fill), border (label box border)
   * fontsize, vjust, hjust, halign, rot
   * show_label - whether to display `text` for this polygon
   * show_items - whether to display `items` for this polygon
   * show_label and show_items are intended to be mutually exclusive
   per polygon, default behavior:
   
      * default `show_label=NA` and `show_items=NA` each row of `label_df`
      * function argument decides behavior for rows with `NA`.
      * if any `show_label=TRUE` for a polygon in `label_df` then
      set `show_items=FALSE`, or in future move the label outside the polygon.
      * if `show_items=TRUE` then if `length(items) > max_items` then
      set `show_items=FALSE` and `show_label=TRUE`.
      * if both `show_labels=TRUE` and `show_items=TRUE` then display both,
      trust the user to adjust coordinates as needed.




### DONE: option to move count labels outside the polygon

* DONE: implement `x_offset,y_offset` for count labels

   * Draw a line to polygon border if needed (`polygon_label_segment()`)
