
# todo on venndir

## bugs

* If input contains `factor` values, they are coerced to `integer`
which is incorrect. It needs to convert to `character` then
follow the proper steps.

## vignette on fonts

From user testing and feedback, there are issues around font display.
For example, `pdf()` output sometimes displays no text labels,
while `png()` does display the text labels. We think this issue
is due to the `fontfamily` argument and/or `family` parameter
referencing a font that is not present on the user environment.

R fonts are sort of a nightmare. There are fonts for several
different output devices, and they are not always consistent.

Choosing a font that does not represent Unicode characters
will result in directional arrows being shown as blank squares,
something like `[]`. This issue is usually caused by one or two
issues:

* Font does not include Unicode symbols at all.
* R locale does not permit Unicode characters.

Both the above must be valid for Unicode arrows to be displayed.

The vignette should describe several different scenarios and
potential workarounds. Include example with Cairo package
and `CairoFonts()`.


## enhancements

### visual issue

Currently the main count label font is always the same color
as the main set label font, even when the main set label is outside
and the main count label is inside the polygon. Visually this
is only a problem when the polygon fill is a dark color, and
the outside background is a light color (usually white).
When this happens, the count label inside the polygon is black
on a dark color, and is very difficult to read.

The best current workaround is to use `poly_alpha=0.2` which should
force the Venn fill to be pale enough that black labels are clearly
visible.

A refactor is required to store the main set name label color
separate from the main count label. This change can be done two ways:

1. adding a row in `label_df`, which will also allow specifying
the set name fill and box independent from the count fill and box.
2. adding a column to `venn_spdf` for font color, note this step
does not allow the outside set name label to have different fill
and box outline than the inside count label, if desired.

Looks like option 1 is preferable.

### bugs

* `render_venndir()` argument `max_items` is being ignored.
For overlaps with too many items, desired behavior is to
display the counts instead of item labels.
* `polygon_label_outside()` is glitchy for weird proportional
cases, for example 4 sets and `shape="ellipse"` might
warrant different default settings. Alternative method
would involve taking boundary point, expanding the
boundary, find nearest point on that boundary, then
proceeding with `spread_degrees()`.

### performance

* Showing a few hundred item labels is still painfully
slow. Consider `text()` as an alternative. Or test options
that remove the dither options for color/degrees/cex in
case that somehow triggers non-vectorized rendering.
* `polygon_label_outside()` for 5-way Venn overlaps sometimes
moves labels to weird positions. Workaround with `min_degrees=8`
but not sure why.


### miscellaneous

* Option to display concordance score below the signed values.
* For `overlap_type="agreement"` hide the signed values for
single-set overlaps, they always "agree" and are not informative.


### prepare for R-shiny

* make `ggtext::geom_textbox()` and `ggtext::geom_richtext()`
work for `plotly::ggplotly()`, by adding `to_basic.*()` functions.
For example, in the package `splicejam` to make `ggforce::geom_shape()`
work, I added the function `to_basic.GeomShape()`.

   * `to_basic.GeomRichText()`
   * `to_basic.GeomTextBox()`


## fixed bugs version 0.0.11.9000

* FIXED: `im2list()` with logical matrix input was incorrectly
assigning all values. The `empty` argument will match the class
of the input `x` to account for this mismatch.
* FIXED: `show_zero=TRUE` is not working properly, it is always hidden.
* FIXED: `alpha_by_counts` appears to be mis-aligned.
* FIXED: Fix issue where directional label colors apply
`make_color_contrast()` without using the correct
`alpha` value. See with `venndir(make_venn_test(100, 3), poly_alpha=0)`



* DONE: Add warning message to ggplot2 output in `render_venndir()`.


## vignettes


### interfacing with VennDetail

* follow VennDetail vignette, show alternative with directional changes


### interfacing with DESeq2/edgeR


### add vignette showing more visual options

* how to change Venn set colors
* how to nudge a Venn circle
* how to rotate the whole Venn diagram
* how to adjust item label buffer, and view the buffer size
* how to create custom curations with `curate_df`
* how to handle gene data in a dataset with multiple probes
per gene


### make displaying items labels easier

* make it "one step" to display items, when `show_items` is
enabled in `venndir()` the default `label_preset` should include
items.
* Option to allow "one column" of labels inside the polygon, which could
be useful for things like a list of pathways between two experiments.


### polish label coordinates inside/outside

* coordinates for inside/outside should really just be
in `venn_spdf` then used by `label_df` when needed
* label styles need polishing. If overlap label is outside
and count label is inside, how can they have two different styles?


### Combine individual labels into one cohesive label

COMPLETE for base R plots.
* TODO: for any label associated with a line segment,
use `degree_to_adj()` to help place the label at the
correct edge of the line segment.
* one option is simply to create one label, but text
alignment for two column output would be difficult.
* prototype using one label for fill/border, and
expand the buffer size so it includes the other labels.

   * calculate label width/height (`grid::ascentDetails()`,
   `grid::widthDetails()`
   * also use grid graphics logic to use rescalable operations
   * requires knowledge about grid graphics I don't currently have
   and was not yet easy to grok from online guides. I was able to
   proof-of-concept a method that used grob dimensions of a left label,
   to produce a right label with border and padding sufficient to
   encompass both sides. When trying to automate, it stopped working.
   Something in the circular logic, that a grob does not have a size
   until placed, so adjusting by size before placement has no usable
   value?


## DONE: Update Rmarkdown README.Rmd and venndir_gene_expression.Rmd

* Use new recommended workflow and function arguments.
* Beware errors in `venndir_gene_expression.Rmd` with unique errors
related to `ragg_png`, see details and workaround suggested
here: https://www.jumpingrivers.com/blog/r-knitr-markdown-png-pdf-graphics/


## DONE: small features for version 0.0.9.9000

* DONE: Some importer for signed overlaps. Given a known directional
Venn, have a method to import to use venndir.

    * `signed_counts2setlist()`

* DONE: Bug: Fix `polylabelr::poi()` when there is one or more holes
inside a polygon.
* DONE: change all arguments with `angle` to use `degrees` for
consistency, and to make clear the type of angle.
* DONE: Refactor to handle `"set name"` separate from
`"main count"` in the count label, so the `"set name"` can
be moved, for example to perimeter or outside the polygon,
leaving the numeric count inside as needed.

   * DONE: Proportional diagram with one circle fully inside
   another does not have set name displayed -- it must be.
   Consider choosing one internal polygon with fewest set overlaps
   with the largest area.
   * DONE: need idea for where to store the set label coordinates,
   since the Venn circle/ellipse shape does not have its own
   row.
   * DONE: maybe venn shapes need to be stored in another `list`
   element returned by `venndir()`? It makes that output
   a bit too heavy imo.

* Maybe: new function `venndir_bender()` to modify a `venndir`
result. resize, move, rotate venndir circles/ellipses.
* DONE: change default to `return_items=TRUE` for
`venndir()` and `signed_overlaps()`.


### DONE: refactor label position

Design idea: Four label types: set, overlap, count, items

* set

   * inside
   * outside
   * none
   * ifhidden
   
* overlap

   * none
   * inside
   * outside

* count

   * inside
   * outside
   * none

* items

   * none
   * inside

Data storage:

* venn_spdf

   * contains each Venn circle
   * contains each Venn overlap polygon

* label_df

   * contains each overlap count label

Progress:

* COMPLETE for base R plots.
* COMPLETE for ggplot.



### DONE: update `polygon_label_outside()` for multiple labels

* DONE: The idea is to position labels by angle relative to the
same central point, detect labels "too close" to one another,
then spread them out evenly to assist with spacing between labels.
* Labels would be defined mainly by center point and angle in degrees.
* "Too close" would be defined by user-configurable minimum degrees,
with some suitable default value.
* return the point of label, and the point at the polygon so a line
segment can be drawn
* return the text adjust (`adj`) value so a text label can be placed
at an appropriate side relative to the incoming line segment.



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


## Completed items


### DONE: option to move count labels outside the polygon

* DONE: implement `x_offset,y_offset` for count labels

   * Draw a line to polygon border if needed (`polygon_label_segment()`)

### DONE: refactor venndir output

The changes below are DONE.

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
