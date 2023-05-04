
# todo on venndir

## 01may2023

**Note the official TODO was moved to `TODO.md`.**

## 03apr2023

* check build issues on R-4.2.3, dependency on data.table

## 06feb2023

* add `venndir_legend()` to summarize number of elements per Venn set.
* `venndir()`

   * consider alternative label layouts:
   
      * position "overlap title" at top-center
      * one column underneath when `overlap_type="overlap"` with counts
      * two columns underneath, total counts on left, signed counts on right
      * or one column with counts, then signed counts in the same column
   
   * output really needs to be its own object type

      * convert from `list` to something like `class="venndir_output"`
      which inherits from `list`, or is an S4 object with proper methods.
      * input to functions like `nudge_venndir_label()` should take
      a proper object instead of `list`.
   
   * make it clear how to display overlap titles, e.g. `set="all"` is not
   working. The `x$label_df[,"overlap"]` column accepts
   `"outside"`, `"inside"`, or `"none"`, and is not being filled correctly.
   * `plot_warning=TRUE` when the label contains more than 3 overlap sets,
   it should just indicate the number of overlap sets rather than list
   each overlap set.

* DONE. scale default `padding` value based upon `font_cex`

   * DONE. currently `padding="4 pt"` is too much when the font is shrunk down small
   * DONE. `padding` should be an argument to `venndir()`

* add option for custom label placement?

   * this option might not be needed if label orientation can be customized,
   e.g. with title centered above all count labels underneath it, instead
   of being offset.
   * center by label box
   * center by coordinate zero (the point between left/right columns in labels)

* `nudge_venndir_label()`

   * add proper function parameter documentation
   * `x_offset`, `y_offset` should be additive to existing values
   * should be possible to set a label `"inside"`, `"outside"`
   * should be possible to set the overlap title `"inside"`, `"outside"`.

* error with `label_style="shaded"` from `colorjam::blend_colors()`
likely caused by blending NA or NULL colors. See `colorjam-blendcolors.R`
line 161.
* store more data inside `venndir()` output object

   * Main goal is for `render_vendir()` to be totally self-sufficient
   when operating on `venndir()` output.
   * Essentially all new parameters used in `render_venndir()` should
   be stored in the object, for example: `plot_warning`

* label segments

   * the segment buffer should use a scale relative to the plot scale
   (e.g. 1% of the overall plot layout range), however when the polygon
   cannot accommodate that buffer, it should use progressively smaller
   scale relative to that polygon.

* outside label positions

   * ideally during rendering, labels should be "dodged" in some way
   to minimize overlaps.


## 20oct2022

* `venndir()` possible bug

   * when displaying `proportional=TRUE, overlap_type="overlap"` it does
   not indicate when a non-overlap cannot be displayed, for example
   items unique to one set. Found with 4-way Euler diagram.
   * Appears to happen sometimes when eulerr returns circular instead of
   ellipse shape, which seems to happen due to internal randomness.

* `min_count` should be stored in `mem` output

   * `mem_enrichment_heatmap()` uses `min_count` with `p_cutoff`
   to define significant pathways
   * `mem_plot_folio` has no argument `min_count` and cannot get it from `mem`
   * `multiEnrichMap()` should store `min_count` in the output `mem`

* new S4 classes

   * `mem`: required for Bioconductor submission (if pursued), and
   useful otherwise
   * `mpf`: or `mem_plots` or `mem_folio` with output from `mem_plot_folio()`

* `mem_enrichment_heatmap()`

   * apparent bug when trying to order rows to match the order in
   `mem_gene_path_heatmap()` - but a "bug" because it is unsupported,
   but it does cause incorrect heatmaps when using
   `ComplexHeatmap::draw(hm[row_index,])`.
   * When supplied with `row_order` this function should properly order
   by `names(row_order)` to match the actual `rownames(enrichIM)`
   used for the heatmap.
   * argument `min_count` is not explicitly passed by `mem_plot_folio()`
   and not stored in the `mem` object.

* Keep track of this URL for installation issues in Mac OSX regarding sf:
https://github.com/r-spatial/sf/issues/1536#issuecomment-727342736

## 20sep2022

* `overlap_type="agreement"` should hide agreement for single-set overlap.
* when overlap and signed overlap are displayed together, the label should
center the main overlap number, with signed labels off to the side.

   * Currently, main overlap and signed overlap are combined into one box,
   the box is centered on the label coordinate. However, inside a polygon
   sometimes the main overlap number is offset outside the polygon,
   and the signed overlap numbers are also offset, so really no numeric
   labels are positioned well. By default the main overlap number should
   be inside the polygon.

* Design idea: nudge label by overlap name.

   * The idea is to make it easy to adjust a label.
   * Something like this:
   ```R
   nudge_label=list(
      `A&B`=c(x=1, y=0),
      `A&B&C`=c(x=0.5, y=0),
      `B`=c(x=0, y=1))
   ```

## 01sep2022

* Small bug fix:

   * `overlap_type="overlap"`,
   * `label_preset="main items"`,
   * `show_items="sign item"`
   * It displays the counts instead of the sign, since there is no sign
   when `overlap_type="overlap"`.
   * when `overlap_type="overlap"`, the `show_items` argument should
   ignore `"sign"` as a possible display value.

* `signed_overlaps()` should default to non-signed overlaps
when all entries in `setlist` have incidence value `1`, `"up"`.

   * when `setlist` is an overlap list, convert with `overlaplist2setlist()`
   to `setlist`.

* Conversion functions, and inputs should be more user-friendly:

   * `signed_overlaps()` argument `setlist` should accept output
   from `label_df$items`.
   * `counts2setlist()` and `signed_counts2setlist()` should
   accept the names from `lengths(label_df$items)`,
   which includes a suffix with the overlap hit values.

      * For example, names include:
      `"GroupA|1 0", "GroupB|0 1", "GroupA&GroupB|1 1"`.
      Simple enough, they should be equivalent to:
      `"GroupA", "GroupB", "GroupA&GroupB"`.
      * Also, for signed overlaps, the suffix indicates the direction,
      and should be honored.


## 31may2022

* Bug with `textvenn()` caused by `jamba::printDebug()`.

## 23may2022

* Bug: `proportional=TRUE` and `show_items="sign item"` appears
to cause it to position labels inside a polygon that does not
exist. See venndir.R line 817 inside `render_venndir()`.
Error: "Error in label_xy[, 1] : subscript out of bounds" seems
like a symptom of a polygon that does not exist in proportional
polygons.
* Why is there such a large gap between overlap and signed counts
in the label? See if there is a padding that can be reduced in
the respective ggtext labels.
* Consider new default: `overlap_type="each"`.
* Consider new default: `label_preset="main outside"`.
* Consider new default: `inside_label_percent=0` or `1`.
* Consider by default positioning labels above/below rather than
radially around the plot, otherwise consider `expand_fraction=0.2`.
* `inside_label_percent` should use the bounding box area as the basis
for the calculation, since the 4-way Venn takes up a larger percentage
of the bounding box than the 3-way Venn. The theory should be the
percentage of screen space available to position a label.

   * Actually, "ideal world" the theory should be whether the label
   fits inside the Venn overlap polygon. The label bounding box will
   generally be larger than the actual label text, so this approach
   is more likely to determine labels are "too big" when they could
   actually fit inside the polygon.
   * Single numerical counts are most intuitive inside the polygon.
   * Signed counts are "busy" - but when they fit beside the main
   overlap count without overstepping the polygon, it works well.

## 15may2022

* scale proportional coordinates to rough the same magnitude as
non-proportional coordinate circles.

   * proportional coordinates tend to have much larger (7x) magnitude
   * other downstream components are based upon absolute coordinates:
   * item_buffer, segment_buffer
   * offset

## 06may2022

* adjust item label size `item_cex` automatically based upon

   1. number of item labels
   2. relative area of the polygon

* `venndir()` and `render_venndir()` should have specific argument
for the item label buffer width

   * it currently uses `label_polygon_fill()` argument `scale_width` which is
   impossible to remember!

* Prepare some easy way to enable Venn diagrams from `sestats`
results from `jamses::se_contrast_stats()`.

   * Consider something similar for `limma::decideTests()` which returns
   an incidence matrix.
   * Consider allowing incidence matrix as valid input to `venndir()`.
   * While we're at it, try to recognize other common inputs, such as
   signed count vector, count vector, incidence matrix,
   signed incidence matrix.

### Convert from SpatialPolygonsDataFrame to sf equivalent

Background:

* The `sp` package is deprecated, to be retired by end of 2023.
* All `sp` methods must be converted to use `sf` format.
* Fortunately, `sf` uses something like a `data.frame`  as its
storage model for polygons. Most if not all functions from `rgeos`
are already available as `sf`-specific functions, the steps just
all need to be ported.
* Unsure if `spsample` is available for `sf`. It would not be too difficult
to wrapper a very simple replacement function:

   * start with square coordinate "grid"
   * rotate grid 30, 45, or 60 degrees
   * overlay grid onto polygon P
   * resize grid progressively smaller until at least N points are inside P
   * optionally perform basic rotations at each sizing to maximize fit


## 28apr2022

* `render_venndir()` does not honor `show_segments=FALSE` with
`plot_style="gg"`.
* In the rare case that two sets completely overlap, proportional Euler
returns two identical circles. We should probably offset them at least
1% so the circle colors are visible.

   * `venndir()` obtains the proportional coordinates,
   `get_venn_shapes()` creates the coordinates - the step that creates
   coordinates should probably test and do the adjustment, so any
   function that calls `get_venn_shapes()` will receive valid coordinates.

* Problem: proportional Venn labels outside are not placed in ideal locations:

   * often placed left and right side, which extend beyond the panel width
   * sometimes still overlap other labels placed outside, partly due to
   irregular label "box", wide text labels, multi-line text labels,
   multi-component labels (set name, counts, signed counts)

* Need a method to display overlap count and item labels in a set.
* When item labels exceed `max_items` it should display the overlap counts,
currently it displays nothing.
* Need a method to display set sizes somewhere in the plot panel:

   * Total items in each set.
   * Total items overall.

* Need method to display overlap percent as alternative to overlap count

   * How do other Venn diagram tools calculate percent overlap?


## 09mar2022

* `textvenn()` is useful but sometimes labels are super long and fill
the screen width (and beyond). It needs some logic to word-wrap.
E.g. wrap by the separator `"&"` as first-pass; potentially word-wrap
each label within that if longer than a defined length. Word-wrap
probably needs to be sensitive to underscores "_", hyphens "-", period ".",
and whitespace " ", including things like "\t\r\n", etc.

## 02feb2022

* Option to supply specific SpatialPolygons objects for each set.
* Option to apply `rotate_degrees` to the polygon representing specific sets.
The function `rescale_sp()` takes `rotate_degrees` as an argument then
applies to all polygons. It could potentially be applied to named
polygons, if `rotate_degrees` also had names that matched the sp object.


## 30jan2022

### Bugs observed

* Sometimes `label_style="shaded box"` throw error and do not display.
* Sometimes `label_preset="main outside"` causes main labels not to
be plotted. The inside labels appear correctly.
* Proportional Venn labels draw a segment to an ambiguous polygon,
sometimes two segments end inside the same polygon.

### Feature enhancements

* `venndir()` option to "recognize" when the input is format:

   * incidence matrix: `im2list()`
   * signed incidence matrix: `im_value2list()`
   * overlap counts: `counts2setlist()`; force `return_items=FALSE`
   * signed overlap counts: `signed_counts2setlist()`; force `return_items=FALSE`

* Option to supply a list where some elements are signed, while
others are not. For example, supplying "dysregulated genes" with
direction, and supplying "genes assayed" which is not directional.
This feature implies certain downstream effects:

   * Concordance cannot be confirmed or denied with non-directional
   list elements. For example, "gene hit down", "protein hit down",
   "protein assayed" should be reported as "concordant", and the label
   should be something like "downArrow hyphen downArrow".

* Need more/better styles for arranging labels.

   * Option to center set name above the counts and signed counts.
   * Option for multi-column signed counts, especially for 3+ signed sets.


## 15nov2021

* indicate total items per set somewhere in the figure
* different label layout strategies, for example:

   * The main set label is placed top-left, main counts bottom-left,
   signed counts placed top-right through bottom-right. The label
   width is calculated and centered inside the Venn polygon, however
   the counts are sometimes pushed to the far right if the main
   set label is too long.
   * Alternative is to place set label top-center,
   then main count labels bottom-left, signed counts bottom-right.


## 02nov2021

* proportional Venn label positioning bugs:

   * when a circle is fully inside another, it tries to label
   the interior circle using the polygon with fewest overlaps;
   however it errantly tries to place the count label for the
   unique set also inside the polygon which is obviously incorrect.
   * it would be good to re-draw Venn circles using the actual
   set color after everything is rendered - to help reinforce
   the set colors

* "Try harder to make Euler proportional Venn diagram layout that
represents all overlaps." A related issue was filed until R "eulerr"
Github here: https://github.com/jolars/eulerr/issues/84

   * basic idea is to manipulate the algorithm `eulerr/src/optim_final.cpp`
   lines 125 to 131. Example code is shown that adjusts the scoring.
   * The current approach ranks solutions by lowest
   residual of `fitted_area - orig_area` - in other words absolute
   area represented. My thought was to add an "area penalty" when
   an entire overlap is not represented - this penalty could be
   a percentage of the total counts that are not represented multiplied
   by a weighting factor, for example 5x.


## 29oct2021

* Lot of examples of Venn diagram includes the percentage either along
with the overlap count, or instead of the overlap count.
* Several examples of Venn diagrams hide/display a subset of labels
of interest.
* Lot of venndir examples where the set label is super long, which
makes the center of the label offset in a weird way.

   * ideal world: have some configurable orientations of labels to
   include in each cell.
   * one example: set title is centered across the label, the overlap
   and signed overlaps all appear below this title.



## 01oct2021

* Make `curate_venn_labels()` options more clearly defined

   * the default data should be documented somewhere, perhaps as data
   to make it easier to customize


## 28sep2021

* I cannot ever remember how to add item labels! If I cannot remember,
nobody else can remember. It needs to be easy to enable.

* Some option to word wrap super long set labels, otherwise the
label is 10x wider than the plot size.

* COMPLETE: Implement `max_items` so item labels are hidden in sections with
too many labels.

* COMPLETE: Item labels should use `text()` or `geom_text()` and not the
`gridtext()` and `geom_gridtext()` counterparts - they are extremely
slow for even moderate (20 to 50) labels on one figure. The gridtext
feature is mostly useful for markdown support, which can be optional.

* It is hard to adjust placement of labels outside the circle.

   * `offset` is intended to help push labels in one direction,
   but does not work reliably, especially for proportional Euler diagrams.
   One option is to scale proportional coordinates to typical
   dimensions used by fixed layouts.
   * Revisit the workflow to nudge or move label positions.

* COMPLETE: `polygon_label_fill()` argument `apply_n_scale=TRUE` should also
adjust polygons by relative size of the polygon compared to the
size of the overall Venn diagram. Smaller polygons with
fewer labels should not be adjusted as much as larger polygons.

   * In general, proportional diagrams should use the same buffer
   for each polygon, since the polygon is already proportional
   to the number of item labels.

* `polygon_label_outside()` should adjust relative buffer consistent
with `polygon_label_fill()`, based upon the plot size.
* More arguments should be added to `venndir()` so these options
are not "hidden" features:

   * shape="ellipse" is used for proportional Euler diagrams
   and allows elliptical ovals instead of circles.
   * `scale_width` is passed to `polygon_label_fill()` when items
   are being displayed.
   * `draw_buffer` - shows item polygon buffer
   * `segment_buffer` - adjusts segment from outside labels to respective
   internal polygon.
   * `apply_n_scale` - adjusts item polygon buffer by number of labels.
   * `center_method`, `vector_method`, `segment_method` passed to
   `polygon_label_outside()` to position labels around the Venn polygons.

* Longer term, I might need to adjust the "optimization settings" used
by the `eulerr` package. It appears to perform a solver minimization
function that maximizes the total overlap count represented.
I think it could be penalized when a set overlap is not represented
to "encourage" it not to skip a potential overlap.

### Venn section labeling bugs

* Problem: sometimes an outside label points to wrong polygon
inside the Venn diagram. Seems limited to proportional Euler diagrams.
* It appears the priority in selecting a polygon is sorted wrong,
for example, it should pick the subsection with fewest overlapping
sets. It seems to choose the whole Venn circle instead of a subsection.

   * A main set label outside may have a segment that
   points to some arbitrary location.
   * Sometimes an overlap count appears in a random location,
   and it appears to be "buggy". Sometimes the overlap count
   should be hidden; other times the overlap count is in the
   wrong location.

* Change `render_venndir()` to draw each Venn set border
after the individual polygons have been drawn, but before the
labels have been added. The goal is for the Venn set borders
to match `set_colors`.

   * Currently when a Venn circle is fully inside another circle,
   the two set colors are blended to produce the polygon fill
   and polygon border. In this case, the border does not match
   any value in `set_colors`.



## 25jun2021

* when input character vectors happen to have names, `signed_overlaps()`
should not assume directional overlap unless specifically requested,
otherwise it takes ages! Workaround is to use `overlap_type="overlap"`.


## usability of custom changes

* document using `offset` so outside labels can be
influenced up or down, left or right in a figure.


## low usability for item labeling

* It's too confusing to add item labels to the diagram,
it needs to be more "automatic" so that when `show_items`
is enabled, `label_preset` is updated consistently.
Also `label_preset` needs to be visible in `venndir()`
and `render_venndir()` instead of being passed via `...`.
* Item labels using `ggtext` and `gridtext` are PAINFULLY
slow to render more than 100 or so labels. Slow in terms
of minutes, compared to the expected sub-second response time.
Maybe the developers are okay with that, but there is
no big reason not to use something orders of magnitude
faster in venndir... like `text()` and `geom_text()`.
It means item labels cannot contain markdown,
unless we allow ggtext and gridtext as an option,
like for `venn_meme()` when there is usually
only one item per overlap.
Using `text()` and `geom_text()` may impact the font
and Unicode compatibility of displaying arrows (again).


## new label preset

* set label and count inside, signed labels outside


## usability of custom label placement

* Use case: move one overlap label inside, another label
outside the polygon.
* Use case: move the signed labels for one overlap
outside, but keep set and main count label inside.


## bugs

* When set label is supposed to be inside, but there is no
portion of the circle unique to that set label, the label
seems to go near a border. However, it often overlaps the
count label, and maybe should be grouped with the largest
internal polygon overlap for that set.
* Item labels ignores the argument `max_items` and should
instead hide item labels and display numeric counts when the number
of items is too high.
* Sometimes with 4-way proportional Venn diagrams the main set
label seems to point to the whole circle, instead of pointing
to the subset of the circle that is specific to the label. Something
seems to be going wrong when choosing the best available
polygon to direct the label. It appears to work for intersect
polygons, but not for main set labels, which suggests the order
of choosing a polygon when more than one has a matching label
is the problem -- and this should only happen with main labels
because they are associated with the full circle, and with any
overlap polygon that only involves that same label.
* Item label `buffer_w` and `buffer_h` appear to use absolute
plot units instead of relative units. Since proportional
coordinates tends to be different range than the fixed Venn
circles, the units are not interchangable, making it confusing.
Also, these buffers should be apparent in `venndir()` and
`render_venndir()` rather than being hidden inside `...`.

* COMPLETE: If input contains `factor` values, they are coerced to `integer`
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
