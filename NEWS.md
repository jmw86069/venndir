# venndir 0.0.57.900

## major changes

* `plot.JamPolygon()`

   * Plot now uses 'native' units by default, controlled with
   argument `do_plot_scale=FALSE`. Previous plots uses 'snpc' units
   which maintained 1:1 aspect ratio but imposed a square viewport,
   creating whitespace when the graphics device was not also square.
   * New plots may be subtly different, but figures generally appear
   larger by virtue of using available space more effectively.
   * Major benefit is that grid objects can be drawn in the viewport
   without first transforming coordinates using 'adjx', 'adjy' functions.
   It was always the goal, I finally worked out a solid approach.

* `venndir()`, `render_venndir()` are affected by `plot.JamPolygon()`

   * default Venn diagrams fill the whitespace more effectively, especially
   2-way Venn diagrams which were naturally wider than they were tall.
   The goal is always to minimize use of `expand_fraction` to adjust the
   Venn figure in the default case.
   * New argument `do_plot_scale=FALSE` can be passed via `'...'` to
   `plot.JamPolygon()` to control coordinate units.
   Revert to previous behavior with `do_plot_scale=TRUE`.
   * `main` now recognizes newline '\n' and converts to '  \n' to force
   linebreak for markdown, only when non-whitespace occurs beforehand.
   To avoid linebreak, leave one space before the newline: ' \n'.


## Changes to existing functions

* `render_venndir()`

   * `expand_fraction` help docs are expanded. When legend is drawn,
   the margin is expanded on the top or bottom using `legend_x`,
   only when it contains either 'top' or 'bottom'.
   * `expand_fraction` is explicitly stored into the Venndir metadata
   when returned.

* `venndir()`

   * Now inherits `metadata()` from `render_venndir()` when `do_plot=TRUE`.

# venndir 0.0.56.900

* Exciting new venndir logo! Courtesy of Christina Ward @csward70,
the real artiste!
* New favicon as well.
* Added svglite; removed hexSticker,ggplot2.

## Bug fixes

* Fixed `spread_degrees()` not correct when wrapping around c(0, 359).
Edit: Still needs work.
* Fixed `label_outside_JamPolygon()` for labels at degrees crossing c(0, 359),
iterative spreading where spreading certain angles interferes with others.
* Added tests for `spread_degrees()`, `mean_degree_arc()`, and
`label_outside_JamPolygon()`.
* `render_venndir()` now properly passes `'...'` ellipses to
`make_color_contrast()` for item label color adjustment, for example:
L_lo, L_hi, C_floor, L_threshold.

## Notable changes in behavior

* `venndir_legender()` args `x_inset`,`y_inset` reduced to **0.5 line**,
from 2 character lines previously. Change was driven by venndir-book where too
many signed Venndir labels overlapped the legend and needed to be nudged.
* New `render_venndir_footnotes()` which also uses `footnote_x_inset`,
`footnote_y_inset` similar to the legend. It displays a single footnote
style character when the diagram cannot display all overlaps.
* New argument `marquee_styles` supports inline styles in markdown text.
* Added examples with custom fonts, and inline image.

## updates to existing functions

* `Venndir-object`

   * New 'metadata' slot entry 'footnotes' with `data.frame` of footnotes,
   the preferred method of indicating "hidden overlaps" that cannot be
   displayed, usually in a Euler diagram with 3 or more sets.

* `venndir()`, `render_venndir()`, `plot()`

   * New argument `draw_footnotes` which calls `render_venndir_footnotes()`
   to render a small footnote symbol in the bottom corner.
   * New argument `marquee_styles` stored inside Venndir metadata, adds
   custom marquee markdown inline styles: `'{.mystyle My Style!}'`.
   Convenient to use custom font, or resize a font, make it bold, etc etc.
   
* `label_outside_JamPolygon()`

   * now enforces default values for `vector_method`, `segment_method`.
   * new argument value `center_method="none"` to use `center` as absolute
   coordinates.
   * Expanded the help docs.

* Removed `hexsticker_venndir()` as a short-lived option, no longer useful.

## new internal functions

* `assign_degree_groups()`, `make_degrees_clockwise()` - used to help
space out degree angles used for outside labels.

   * Previously labels would sometimes criss-cross when spacing out one group
   of angles near another angle, flipping the order of angles.
   Now the order is preserved.
   * Improved the logic of recognizing arcs, so any angles inside the arc
   could be treated with the group.
   * Still one bug with multi-part polygons and outside labels, sometimes
   the label points to the wrong part. It currently draws a line to the
   largest part, but sometimes the label is right beside a smaller part.
   Leaving it TODO against my every instinct.

* `render_venndir_footnotes()`

   * Simple wrapper to draw a footnote symbol onto a Venndir figure when
   footnotes exist. Currently footnotes are only created when overlaps
   cannot be displayed, for example with Euler diagrams where not
   every overlap can be shown. The footnote is a subtle visual cue that
   there is informatio to be reviewed.

* `footnotes()`

   * Function returns a `data.frame` with footnotes.

# venndir 0.0.55.900

Work in progress through 04jun2025, most updates were discovered
while writing the detailed Venndir E-book.

## Notable changes to default behavior

* `venndir()`

   * Default internal base font size changed to `16` from `16.8`.
   The `16.8` was the awkward result of previous `14 * 1.2` when enough
   people suggested making the default a little larger.
   * Default `font_cex` changed to `c(1, 1, 0.75)` from `c(1, 1, 0.7)`,
   mostly so the default font size would become `c(16, 16, 12)`.
   The 16 and 12 are natural, common font sizes, more easily matched
   with other figures for consistency.

## Bug fixes

* Fixed `spread_degrees()` not correct when wrapping around c(0, 359).
* `plot.JamPolygon()`

   * `'label.color'` is properly recognized for label colors.

* `venndir_legender()`

   * Fixed bug that did not utilize metadata.
   * Fixed bug that did not use internal `curate_df` when present.
   * Added argument `curate_df` to make this customization more convenient.
   * Changed default `x="bottomright"` to be consistent with `venndir()`

`legend_signed` to persist this option from the Venndir object.
* `render_venndir()`

   * fixed bug that prevented items and plot title from both being displayed,
   caused by duplicated grid grob name. Ha.
   Plot title is now added last to the `grob_list` so it
   is rendered after all other grobs.
   * Fixed longstanding "bug" feature request by Dr. Theofilatos causing
   fully-internal set labels to be grouped together with the overlap count
   when placed outside. Now there are two labels, only when the set-only
   label cannot point to a set-only overlap region.
   This is an area of ongoing consideration, how best to place labels
   in these specific scenarios.
* `modify_venndir_overlap()`

   * Repaired ability to move a specific label outside, fixed blank
   values for x_offset,y_offset as required to place labels outside.

* `hexsticker_venndir()` fixed obscure bug using internal environment,
which somehow prioritized `globalenv()` instead.
* `venndir_assemble_label()` fixed very miniscule visual glitch by
using `vjust="center"` instead of `"center-ink"` which otherwise caused
labels not to be aligned vertically (by height) when only one label had
hanging character "g".
* `overlaplist2setlist()` adjusted for more robust use of signed input.

## Updates to existing functions

* `venndir()` and `render_venndir()`

   * When drawn, the returned object includes attributes 'adjx', 'adjy'
   which are necessary to add other grid elements to the same coordinate space.
   * Key change in behavior: Outside set names and outside counts are
   grouped together only when the set and counts both refer to
   set-specific region. Otherwise counts are displayed separately, to
   avoid "A 5" implying there are 5 items in "set A" rather than 5 items
   in the overlap of "A" with whatever other set subsumes "A".
   `venndir(counts2setlist(c(B=5, "A&B"=3)), show_labels="NC", proportional=TRUE)`

* `get_venn_polygon_shapes()` new argument `seed` to help `eulerr::euler()`
output be consistent. Use `seed=NULL` for randomness.
It can be passed via `venndir()` using `'...'` ellipses.
* `make_color_contrast()` - added param help text.

# venndir 0.0.54.900

## changes to existing functions

* `warnings()` generic function is now exported.
* `check_systemfonts_family()` added argument `path` with optional
full path to specific font file.
* Added tests for `warnings()` on `Venndir` object.
* `venndir()`

   * Fixed some issues with outside label placement with proportional output.
   * Improved logic for ref_polygon, used internally to label singlet sets
   which are fully enclosed in another set.
   * Removed slot names from print object summary, not necessary.

* `render_venndir()` 

   * Improved label groupings by ref_polygon, edge cases. In future, set
   labels can be independent of the "overlap count set label" - possibly
   allowing set size to be displayed with the outer label, and have
   a separate label with unique counts for the same set.
   It is confusing visually, so for now the labels are intended to mean
   the "Venn overlap label", therefore 'set_A' implies "items unique to set_A".
   * Improved default center for 2-way Venn, biasing labels toward the top
   rather than left-right.
   * Make line segments slightly darker and wider to be more clearly visible.
   
* `label_outside_JamPolygon()`

   * New argument `do_plot` for optional visual summary of label positions.
   * New argument `snap_y_percent=5` will snap y-axis positions when
   multiple labels have a value within this percentage of the plot dimensions,
   used to align labels.
   * Added internal trick to ensure set labels sort top-to-bottom before
   overlap labels for the same set, instead of clockwise, improving
   the left-right symmetry.

* `subset_systemfonts()` - added more help text.
* `check_systemfonts_family()` new argument `debug` to print error
when suppressed internally, new argument `path` for optional full filename.
* `degrees_to_adj()` - new argument `do_fractional=TRUE` with improved
sliding placement of the label instead of snapping to the nearest 45 degree
angle.

## Bug fixes

* `spread_degrees()` - Fixed conditional filter causing it to spread angles
instead of using `min_degrees`. Fixed rare edge case with proportional labels
appearing to be wrongly positioned.

# venndir 0.0.53.900

* Added Enhances: hexSticker, ggplot2
* Added Imports: withr
* Added figure alt text to gene expression vignette.

## changes to existing functions

* `textvenn()`

   * Now hides values for singlet overlap counts as defined by
   `curate_venn_labels()`.
   * New argument `template` with options for "wide" and "tall",
   consistent with `venndir()`
   * New argument `draw_legend=FALSE` to print text table legend.

* `make_venn_test()`

   * New argument `set_names` to make it easier to define custom names.
   * Names are handled slightly differently, using `set_names` then
   `names(sizes)`, then assigning `"set_A"` using LETTERS.

* `curate_venn_labels()`

   * New arg `type="all"` returns three columns: 'sign', 'color', 'hide_singlet'

* `Venndir-class`

   * `print()` method now includes "overlaps not shown" when applicable.
   * new generic `warnings()` to list "overlaps not shown" when applicable.
   * metadata entry `"warn_df"` contains a `data.frame` when there are
   overlap counts that cannot be shown on the venndir plot. It occurs
   sometimes with proportional Euler diagrams with 3 or more sets.
   * `im()` generic method handles unsigned and signed data.

* `JamPolygon-class`

   * `nrow()`, `ncol()` more correctly defined without causing warning.

## New functions

Some helper functions to navigate fonts from systemfonts.

* `subset_systemfonts()` convenience function to navigate the fonts
available to systemfonts and therefore marquee. Adds option to display
the search results. Also filters out "problem fonts" not recognized
by the Freetype API used by 'systemfonts'.
* `check_systemfonts_family()` is the utility function that checks
for 'systemfonts' Freetype compatibility.
* `hexsticker_venndir()` simple utility function to create the
hexsticker. It has a few preset 'inspirations' in development.

# venndir 0.0.52.900

* Removed dependency: vwline

## Changes to existing functions

* `venndir()`

   * Default `fontfamily="Arial"` to avoid invisible text with small
   font sizes with `"sans"` or `"Helvetica"` on Mac raster output.
   * Minor adjustment to color blending to use alpha upfront, overlaps
   tend to become more opaque which intuitively is what was expected.

* `render_venndir()`

   * By default, plot title uses marquee, not gridtext.

* `make_venn_test()` uses 200 items and 3 sets, as better defaults.

## New functions

* `collapse_im()` - convenient function to collapse probe-level,
transcript-level, or peptide-level incidence matrix to gene-level.
Useful for transcriptome (RNA-seq, microarray), proteomics (MS peptide),
or other technologies that may have multiple assays for the same gene.


# venndir 0.0.51.900

## Changes to existing functions

* `venndir()`

   * **Change of behavior**:  
   Now for `overlap_type="agreement"` the sign for 'agreement' is not
   shown for single-set overlaps since it has no meaning when only
   one set is involved.  
   The exception is when displaying items with only the "sign",
   in that case the sign is still shown.
   * **Change of default**: `item_style="marquee"` will now use marquee
   for item label rendering.
   * Added some elements to Venndir metadata: `unicode`, `curate_df`
   * Now calls `get_venndir_curate_df()` to define `curate_df` upfront.

* `venn_meme()` new default `item_style="default"` which thereby uses
marquee, also `"marquee"` is added as a recognized option.

## new functions

* `get_venndir_curate_df()`

   * Convenient way to get the curate_df used to convert signed values
   to Unicode (or non-Unicode) symbols, and associated colors.
   * `curate_venn_labels()` now calls this function to define `curate_df`.

# venndir 0.0.50.900

## Changes

* Big change to labels, now the default uses marquee for consistent Unicode
arrows. Spacing was adjusted in legends and count labels.
* Moved marquee(>= 1.0.0) to Imports, new version avoids potential crash
on MacOS.
* Moved gridtext to Enhances, it is now deprecated.
* Added tests for `marquee::marquee_grob()` across fonts with Unicode arrows,
with `grid::textGrob()` showing arrows consistently for marquee and not grid.

## Changes to existing functions

* `venndir()` - Reduced default border width, it was too distracting.
* `assemble_venndir_label()`

   * New default `text_grob_type="marquee"`
   * Rounded rectangles are scaled to the smallest font size,
   previously they used a small, fixed radius.
   * Label padding is more consistently adjusted for font sizes.

* `venndir_legender()`

   * New default uses marquee.
   * Padding is adjusted by font size, making the table smaller by default.


# venndir 0.0.49.900

## Changes

* Now requires the CRAN jamba, version 1.0.0 or higher. Woot.
* `venndir_legender()`

   * New argument `legend_headers` allows custom legend header labels.
   * Output to `legend_style="data.frame"` is now consistent.
   * Added tests to cover most custom options.

# venndir 0.0.48.900

* bumped `colorjam>=0.0.30.900` to pick up new color blending for
additive alpha blending, useful for repeating a transparent color
and having the color become more intense with each overlap.

## bug fixes

* Fixed error in `venndir_legender()` with empty signed lists.

## changes to existing functions

* `find_venn_overlaps_JamPolygon()` called by `venndir()`

   * Color blending uses an improved algorithm in colorjam for
   additive alpha blending. For example, transparent red used for
   three sets will become progressively less transparent with each
   overlap. It should probably be more dramatic, but it's a good start.

* `venn_meme()`

   * changed order that default text labels are applied to 3-way and 4-way
   Venn, with special case of 4-way proportional Venn (Euler) coded
   so the overlaps are filled clockwise. Effort to make it easy
   to copy Venn memes observed in the wild.

* `label_fill_JamPolygon()` calls updated version of `sample_JamPolygon()`
* `sample_JamPolygon()`

   * Added `pattern="columns"` layout option to enforce all points on the
   same row, an alternative to the default hexagonal/triangular.
   * Updated logic to choose appropriate subset of points when more than `n`
   are defined. Now takes all of the first row when `spread=FALSE`.
   When `spread=TRUE` it spreads points by column, then row, which somehow
   improves the visual result, resulting in fewer weird clusters of points.

* `assemble_venndir_label()`

   * Label assembly was adjusted to add customizable label line
   spacing, and label padding. Added graphical tests.

# venndir 0.0.47.900

* Added to Imports: `eulerr`, `gtable` (lightweight, also used by ggplot2)
* Added to Suggests: `gridtext`, `marquee`
* Count labels use `grid::textGrob()` by default, since `gridtext` (kerning)
and `marquee` (R crash on MacOS) have "limitations".
* Major refactoring of label grouping: uses `gtable` instead of `gTree`.
Faster and more accurate height/width. Fixed labels being slightly cropped.
* Simplified the Venndir data model, columns no longer used for labels.

## new functions

* `assemble_venndir_labels()` - new workhorse function creates grobs,
assembles them into `gtable` format.

   * Reverts `text_grob_type="marquee"` to `text_grob_type="textGrob"`
   on MacOS using R-4.4.1 or older.
   * Applies fontfamily,fontface,fontsize,fontcolor to each type of label,
   and multiple labels, each in order.

## changes to existing functions

* `render_venndir()`

   * Changed default `fontfamily="sans"` to avoid "Arial" which may not
   be present for all graphics devices.
   * Help docs include useful `...` arguments passed to internal functions.
   * Now calls `assemble_venndir_labels()`, not `draw_gridtext_groups()`.
   * Argument `group_labels` is dropped, all labels are grouped.

* `venn_meme()`

   * Changed default `item_style="text"` to avoid kerning issues with gridtext.
* `venndir_legender()`

   * New default shows up/down counts when shown in Venn diagram.
   * New default shows table borders.
   * New argument `legend_color_style` controls color fill and border.
   * The "Size" column is now left-aligned, previously right-aligned.
   * Added light grey shading to "Total" row.
   * Added leading/trailing whitespace to all columns.

## deprecated functions (internal)

* `grobs_exts()`, `grobs_stack()`, `grobs_tile()`, `grobs_xalign()`,
`grobs_yalign()`, `draw_gridtext_groups()`
* removed: `reposition_venn_gridtext_labels()`

# venndir 0.0.46.900

Legend labels can include signed counts and percentage values.

## changes to existing functions

* `venndir_legender()` can optionally include signed counts and percentage.

   * Renamed argument `include_total=TRUE` to `legend_total=TRUE`, for
   consistency with the next two arguments, and to help distinguish
   these options specific to the legend.
   * New argument `legend_signed=FALSE` defines whether to show signed counts.
   When `NULL` or `TRUE` it will display signed counts only when present,
   for example when `overlap_type` is not "overlap".
   `legend_signed=FALSE` will always hide signed counts.
   * New argument `legend_percentage=NULL` detects whether to display percent
   total items, and is only used when `legend_total=TRUE`.
   When `show_labels` includes percentage, it is `TRUE`, otherwise `FALSE`.

* `venndir()`

   * Changed default `segment_distance=0.05` previously `0.1`,
   to shrink the distance of outside labels from the Venn polygons.
   Outside labels are now always justified relative to the plot,
   and so the labels can be placed closer, and the Venn diagram can be
   drawn slightly larger.

* `render_venndir()`

   * Changed default `expand_fraction=NULL` from `expand_fraction=0`
   so it detects whether to adjust the default condition when
   `draw_legend=TRUE`, and `main` is supplied as a plot title.
   It expands the default location, and shifts it slightly upward.
   

# venndir 0.0.45.900

## changes to existing functions

* `render_venndir()`

   * Updated `item_cex` logic again, solidifying the association of
   `item_cex` values to specific, named Venn overlaps, which are also
   stored in `metadata` for persistence, and editing.
   * Related, fixed a bug causing `item_cex` to be applied incorrectly
   when there were empty items. Now it is applied by overlap name to keep
   proper sync.

# venndir 0.0.45.900

## changes to existing functions

* `render_venndir()`

   * Improved the `item_cex` calculations, accounting for relative
   area of each shape, and number of items, also imposing a floor
   to prevent microscopic font sizes.
   * Fixed minor bug causing `item_cex` to be applied incorrectly
   when there were missing/NA labels.

* `venndir()`

   * default argument change: `font_cex=c(1, 1, 0.7)`, 0.7 is down from 0.8.
   * All `font_cex` values are secretly multiplied by 1.2 as a slight
   adjustment for the default condition.

* `sample_JamPolygon()`

   * New algorithm to place points inside a polygon, now using adaptive
   step sizes. Substantially fewer steps, much more accurate, and
   markedly faster. By being more accurate, it is guaranteed to use
   the optimal pattern, which finds the closest match to the number
   of points requested.
   * For `spread=FALSE` it no longer takes the first `n` points, it
   now takes the middle `n` points. Most cases the effect will be minimal.

* `label_fill_JamPolygon()`

   * Now has better working examples, which plot the labels for review.

# venndir 0.0.44.900

## new functions

* `Venndir` has new methods:

   * `show()` to summarize the contents
   * `metadata()` and `metadata<-`
   * `setlist()` - returns the original setlist
   * `overlaplist()`
   * `overlapdf()` - returns `data.frame` of overlap items
   * `signed_counts()`
   * `im()` - return incidence matrix format

* `modify_venndir_overlap()`, `highlight_venndir_overlap()`

   * Customize overlap set visual features, such as fill color, border,
   fontsize, font color.
   * The highlight function streamlines the options to draw a distinctive
   border around one or more overlap sets.

## changes to existing functions

* General changes to remove warnings, for example `minus_JamPolygon()`
was throwing warnings with empty polygon input or output.
* Added tests for `area_JamPolygon()` and `labelr_JamPolygon()` for a
couple weird cases with nested holes, nested-nested solid polygons.
* `venndir()`, `render_venndir()`

   * Now accepts incidence matrix input, sets as colnames, items as rows,
   and `c(-1, 0, 1)` values.
   * Added to metadata slot for persistence, though some might be added
   directly to `label_df` for persistence for each overlap label:
   `main`, `template`, `overlap_type`, `draw_legend`,
   `item_buffer`, `item_cex`, `item_style`, `item_degrees`
   `show_segments`, `segment_buffer`
   * `render_venndir()` defaults are `NULL` so they will pull from the
   "metadata" slot, otherwise it assigns the previous defaults.
   It is now possible to make `Venndir` object, then render separately
   and use the original `show_segments` and `segment_buffer` values.

* `get_venn_polygon_shapes()`

   * Very small nudge to default 4-way elliptical Venn shapes to prevent
   rounding error overlaps at the ends of sets 2 and 3, overlapping the
   side edge of sets 4 and 1, respectively. Same thing, nudging sets
   1 and 4 to prevent rounding error overlaps at the bottom edge.
   Previously there were miniscule "unique" shapes where the cornered edges
   slightly criss-crossed, since the ellipses are drawn with N vertices,
   and not infinite number of vertices.
   Possible side effect is that item fill for sets 1 and 4 may now also
   use the tiny section at the bottom for item labels.

* `signed_overlaps()`

   * Slightly changed how `keep_item_order=TRUE` is implemented, making
   it markedly faster for larger lists.

* `labelr_JamPolygon()`

   * For multi-part polygons, it now uses the largest disconnected polygon
   from the set, instead of using polygons in order. Apparently
   `polylabelr::poi()` uses the first polygon provided, which is sometimes
   comically small.
   * The new method checks for multi-part polygons, then determines whether
   there are multiple parent polygons. For polygons with holes,
   the sub-part polygons (holes, or nested solid polygons within a hole)
   need to be maintained with the appropriate parent. If there is only one
   parent, use them all as-is, since `polylabelr::poi()` works properly
   there. Otherwise, use the parent polygon with the largest net area.
   * This update is an improvement for most Venn diagrams, but may not
   be optimal for polygon purists, since the polygon with largest area
   may not be the "most labelable" polygon. That said, `polylabelr::poi()`
   ignores all but the first polygon anyway.

* `label_fill_JamPolygon()`

   * improved default condition for item buffer adjustment, it now only
   uses the number of items to make the buffer more restrictive,
   no longer uses the relative polygon area. (That adjustment was conflicting
   with the item-shrinkage approach.)

* `nudge_JamPolygon()`

   * New argument `scale` so this function accomplishes all manipulations:
   `nudge`, `rotate_degrees`, and `scale`.

# venndir 0.0.43.900

## changes to existing functions

* `label_fill_JamPolygon()`

   * Minor adjustment to how the item buffer is determined with decreasing
   number of items. Slightly more centralized with 3 or fewer items.

* `render_venndir()` (and `venndir()`)

   * Now accepts `item_buffer` as a vector, recycled to all overlap sets.
   * No longer sorts item labels, instead they are expected to be
   sorted (or not) by `signed_overlaps()`.

* `signed_overlaps()`

   * New argument `keep_item_order=FALSE` by default sorts all items
   using `jamba::mixedSort()` (proper alphanumeric sort).
   When `keep_item_order=TRUE` it will keep items in the same order
   they originally appeared, which allows a fixed item label order
   when displaying item labels.

* `venn_meme()`

   * New argument `keep_item_order=TRUE` whose default will keep each
   overlap label in the order they are provided. Usually there is only
   one label per overlap, but if it is provided as a `list` with multiple
   entries, they will be rendered in the order they are provided.
   Motivated by efforts to "reproduce the Venn diagram from a paper."


# venndir 0.0.42.900

## changes to existing functions

* `check_Venndir()` - validation function for `Venndir` objects.

   * Fixed error when validating `Venndir`, applying logical criteria
   to vector with size > 1.

* `venn_meme()`

   * Changed default `item_cex=1` consistent with improvements to item
   label scaling by Venn overlap polygon size.

# venndir 0.0.41.950

## changes to existing functions

* `render_venndir()`

   * now applies `halign=0.5` for main count labels,
   which helps display percentage labels centered under the main count.
   * Percentage labels are rounded to integer values, except that values
   between 0 and 1 will show one digit, excluding 0 and 1.
   
* `venndir_label_style()`

   * new argument `extra_styles` intended to customize the text style
   for different label types. Currently
   `extra_styles=list(percent=c("***", "***"))` will
   format percentage labels with bold,italic font.

# venndir 0.0.41.900

## overall

* Add plot titles!
* Arrange signed counts in single column, under main counts.
* Outside Venn labels default slightly toward the top, rather than the sides.
* `nudge_venndir_label()` is convenient for moving inside or outside labels.

## changes to existing functions

* `Venndir` S4 object changes.

   * New slot name `"metadata"` to store miscellaneous settings.
   All efforts are made to accept older Venndir objects without
   this slot available.

* `venndir()`

   * New argument `main` to draw a plot title. It uses `gridtext` to
   enable custom Markdown font options.
   When provided, `main` is also stored in `Venndir@metadata$main`
   for persistence.
   * New argument `template="wide"` is used to arrange count and signed
   count labels together:
   `"wide"` places them side-by-side, `"tall"` places them in one column.
   When provided, `template` is also stored in `Venndir@metadata$template`
   for persistence.
   * New argument `center` passed to `label_outside_JamPolygon()`.
   The default `center=c(0, -0.15)` tends to place labels at the top
   rather than the left/right sides by default.
   The default use case is improved.

* `render_venndir()`

   * Now properly converts set names with `"\n"` to use `"<br>"` as
   originally intended, so it "just works".
   * New arguments `main` and `template` as above, except that when
   no argument is defined, it will use `Venndir@metadata$template`
   or `Venndir@metadata$main` if defined.

* `nudge_venndir_label()`

   * New argument `offset_list` to define a list of offsets to one or more
   sets, where set is defined by `names(offset_list)`.
   * New arguments `align_x`,`align_y` to apply uniform alignment of labels
   to the top/bottom/left/right of a set of labels. Useful to ensure
   labels are the same y-position and not slightly different heights.

# venndir 0.0.40.900

* Updated label positioning logic, silenced some unintended verbose output
from `render_venndir()`.

# venndir 0.0.39.950

* Small change to use `gridGeometry` 0.4.1 from Github until released to CRAN,
this version fixes rare error caused by proportional diagram and innerborder,
when the delta from two circles creates a small line with zero area.
Will update again once it is available on CRAN.
Thanks pmur002 for the rapid fix!

   * Updated tests from 0.0.39.900 which skipped the innerborder to
   circumvent the bug in the tests that uncovered the error.

* `buffer_JamPolygon()`

   * default argument `steps=20`, formerly `steps=200` which was (and is)
   the predominant rate-limiting step.

* `nudge_JamPolygon()` now recognizes `rotate_degrees` fixing the bug that
previously did not allow rotating the Venn diagram polygons.

# venndir 0.0.39.900

## changes to existing functions

* `render_venndir()`

   * Signed output now uses `":"` instead of `": "` between the directional
   arrow and the count, for example `"^^:20"` instead of `"^^: 20"`.
   Somehow the whitespace calculation is inconsistent, causing some
   labels to be wider than others with `gridtext::richtext_grob()`,
   with much wider whitespace than anticipated.
   The effect was inconsistent even on the same machine, between RStudio
   and R console, and differed across other machines also.
   Workarounds include choosing different fonts, apparently some provide
   more reliable whitespace calculations.
   The change only occurs during rendering when `grobs` are created.
   The underlying `Venndir` data is not changed.

* `plot.JamPolygon()`, affecting `venndir()` and `render_venndir()`

   * Now renders borders using `gridGeometry` thanks to suggestion from
   pmur002. Borders are more consistent, without small visual artifacts.
   * Added `"outerborder"` as a formal border type.
   * All three types of borders can be rendered, in order: outer, inner, border
   
      * "outerborder" - begins at the outer edge of the boundary
      * "border" - is centered on the boundary itself
      * "innerborder" - begins at the inner edge of the boundary
      * when `border` is not drawn, either the innerborder or outerborder
      are drawn on the border itself to prevent a tiny artifact gap
      between the innerborder and outerborder.

* `venndir()`

   * The `Venndir` object now uses `outerborder` instead of `border`,
   and sets `border` to `NA`.
   * New argument `lwd` controls default border line width.
   * `unicode` (silent argument passed to `curate_venn_labels()` controls
   the Unicode character with up/down arrows, disagreement, etc.
   You can supply `unicode=2` for alternate symbols, though they are
   font-dependent, R-dependent, and terminal-dependent. All things must
   work well together (apparently). Use `unicode=FALSE` for simple text.
   * New default `poly_alpha=0.6` makes background less intense.
   * Now calls `make_color_contrast()` properly without forcing saturation,
   previously caused blue to become cyan instead of light blue.

* `sample_JamPolygon()` changed `n_ratio=1` after testing and disliking
the previous `n_ratio=4`.
* `venndir_to_df()` gained some new features:

   * new argument `df_format` with three formats:
   
      * `"hits"` - essentially a hit matrix with 1, 0, -1 indicating direction
      * `"items"` - each column of a data.frame contains items for a Venn
      overlap.
      * `"wide"` - intended as an RMarkdown summary - it uses grouped rows
      to display items in each Venn overlap. Not sure it works very well.
   
   * argument `return_type`:
   
      * `"data.frame"` (default) - returns a data.frame
      * `"kable"` - returns a colorized `kable` table for RMarkdown HTML

* `textvenn()`

   * now returns column `"color"` mainly used for `venndir_to_df()`
   * calls `colorjam::col_linear_xf()` for better colorization of counts
   when `color_by_counts=TRUE`.

* `get_venn_polygon_shapes()`

   * Changed default `return_type="JamPolygon"` since transition to JamPolygon,
   removed the option for `"polygon_list"`.

* `polygon_circles()`, `polygon_ellipses()` both now return `JamPolygon`.

## new functions

* `nudge_JamPolygon()` - simple function to adjust `JamPolygon` polygons


## added generic functions

* Cleaned up some generic function logic, probably more to do.
* `Venndir` objects

   * `plot()`
   * `length()`

* `rbind2.JamPolygon()`

   * This function was enhanced to be able to combine multiple `JamPolygon`
   objects either in a single `list`, multiple `list`, and will now
   retain all colnames across all `JamPolygon` objects.

## removed deprecated functions, mostly related to polygon_list data format

The polygon_list format loosely conformed to `polyclip` data input/output,
but had several exceptions that motivated me to use `JamPolygon`: simple
polygons encoded as list, complex polygons encoded as nested list;
solid polygon encoded as clockwise points, holes encoded counterclockwise.

* `bbox_polygon_list()`
* `get_largest_polygon_list()`
* `intersect_polygon_list()`
* `labelr_polygon_list()`
* `minus_polygon_list()`
* `plot_polygon_list()`
* `polygon_list_labelr()`
* `rescale_polygon_list()`
* `union_polygon_list()`
* `get_venn_shapes()`
* `eulerr_to_polygon_list()`
* `polygon_areas()`
* `nudge_polygon_coords()`
* `nudge_polygon_list()`
* `polygon_list_to_xy_list()`
* `xy_list_to_polygon_list()`

# venndir 0.0.38.900

## changes to existing functions

* `sample_JamPolygon()`

   * default changed to `spread=TRUE` so that item labels are more
   evenly distributed by default
   * new argument `n_ratio` to control the target number of points, from
   which `spread=TRUE` will choose `n` evenly spaced. For now, the ratio
   is set `n_ratio=5` which seems to perform well.

* `label_fill_JamPolygon()`

   * used to display item labels inside the Venn diagram, passes `...`
   to `sample_JamPolygon()`

* `venndir()`

   * Change: `names(setlist)` will have any characters `":"` changed to `"."`.
   
      * The `":"` character is used by `grid` as a delimited to name `grobs`,
      which means `names(setlist)` that also contain `":"` create confusion.
      Easiest workaround is to maintain `":"` characters in the
      `setlist_labels` and `legend_labels` (for display) but to remove the
      `":"` characters from the internal representation of these names.
      * In principle this should not create new bugs, however it is possible
      something else depends upon the internal names to match the
      original setlist names.


# venndir 0.0.37.900

## changes

* Added tests for hidden count labels, hidden signed count labels,
and corresponding overlap counts for good measure.
* Added laundry list of topics to document.
* Removed `gridtext_richtext_grob()` and supporting functions.

## changes to existing functions

* `plot.JamPolygon()`

   * New argument `render_thin_border` to control whether the thin line
   between inner and outer border is drawn.
   It may be hidden by default in future.
   * `buffer` can now be vectorized, to shift the plot along each side
   in order: (bottom, left, top, right). The default applies buffer to all
   side, but sometimes it is useful to shift the plot away from one side,
   for example `buffer=c(0.1, 0, 0, 0, 0)`. It still maintains proper
   aspect ratio.
   * Added checks for line width 0, and empty `""` line colors, both are
   handled silently by drawing either line with width 0.01, or transparent
   color, respectively.

* `render_venndir()`

   * passes `expand_fraction` to `plot.JamPolygon` using
   argument `buffer` so it can shift the Venn diagram off-center.
   * now passes `...` to `plot.JamPolygon()` so that optional
   arguments will be recognized.

# venndir 0.0.36.900

Substantial changes.

The method of assembling labels now arranges labels adjacent to each
other, removing an important dependency on internal `gridtext` functions
(which are not permitted in CRAN/Bioconductor).
It also allows more flexible labeling in future, e.g.
different overlap metrics, optional overlap names.

Labels use a more aesthetic layout:
* The main set label is always top-center of a group of labels.
* Count labels are split into left/right columns, for main/signed counts,
respectively. They are middle-aligned left-to-right.
* Set label and count labels are arranged top/bottom, also centered.
* One region is defined for the group of labels.
* The previous arrangement can be see with `group_labels=FALSE`.

All graphics are returned as `grid` graphical objects (grobs), which
enables flexible options for drawing the figure. For example it can
be drawn inside a `grid::viewport`, or included with `patchwork` or
`cowplot` as a figure panel. Or, the `grobs` can be directly manipulated.

## Bug fixes

* `venndir_label_style()`

   * Fixed issue that associated a count label to an interior overlap set,
   a rare occurrence. Caused by the method used to associate an orphan set
   label, where there is no "uniquely set_B" overlap polygon so it
   associates "set_B" to an overlap region that contains set_B. That method
   incorrectly also allowed an overlap count to appear for sets that do
   not have an overlap region - which occurs when proportional circles
   fail to find a solution that contains every overlap observed.
   The fix is to hide overlap count labels when the overlap is not represented,
   while still displaying the main set label when some overlap exists
   that contains that set.

## changes to existing functions

* `venndir()`

   * returns attributes `"gtree"`, `"grob_list"`, `"viewport"` when
   `do_plot=TRUE`
   * assigns `padding` and `radius` using the same values.
   
* `render_venndir()`

   * Returns attributes `"gtree"`, `"grob_list"`, `"viewport"`
   * The figure is rendered with one call to `grid::grid.draw()` instead
   of being done piecemeal, making it appear faster.
   * No longer calls custom `gridtext_richtext_grob()`, since that also
   calls functions internal to the gridtext package. It was done in
   order to supply vector of different `padding` values.
   * Instead calls `gridtext::richtext_grob()` in a list.

* `venndir_legender()`

   * Now left-aligns the text label columns, and right-aligns the count
   value column.
   * New argument `legend_padding` to control the table cell size.
   * A bug in `gridExtra::tableGrob()` calculation of padding
   caused left-aligned text to appear at the very left edge
   of the text box, without a padded region shown. The padding is only
   used to extend the box itself, but still allows the label to be
   aligned relative to the extended box, and not the inner region.
   This bug was sidestepped by enfocing leading/trailing whitespace.

* `plot.JamPolygon()`

   * Modified to return `grid` graphical objects, and the `viewport` defined.
   * New argument `do_draw` to control whether the figure is drawn, or
   only the graphical objects should be returned.

* `draw_gridtext_groups()` - substantial changes, most all previous
processing has been changed. It is used internally.

## New internal functions

Not terribly exciting, but useful nonetheless.
Several new functions were added to help manipulate `grid` grobs,
to stack, tile, xalign, yalign grobs relative to each other, using
units that keep them aligned even when the plot device is resized.
These functions are probably dependent upon `gridtext` labels, since they
are used to create individual overlap and count labels, and define
`xext`,`yext` with the "true" `grid` dimensions for each label.
These functions make it possible to center the grouped label on the
original label point, or align the left column with the right column, etc.



# venndir 0.0.35.900

* Added LICENSE and LICENSE.md.

## changes to existing functions

* `venndir()` - relatively minor update, fixes unnamed `setlist` or
`setlist` with duplicate or `NA` names.

   * Validates that `names(setlist)` are defined, otherwise
   integer numbers are defined in order.
   * Validates that `names(setlist)` are unique, and not `NA`,
   otherwise it calls `jamba::makeNames()` to create unique names.

## bug fixes

* Fixed bug in `venndir()` when using `sets` to define a subset from `setlist`
causing the colors to be out of sync.

   * Added tests to confirm duplicated or `NA` for `names(setlist)` are
   handled.

# venndir 0.0.34.900

## changes to existing functions

Main goal is to fix bug associated with empty or nested `setlist` entries
when used with `proportional=TRUE`. This update is somewhat of a risk,
several core components were updated in `venndir()`, `render_venndir()`
and `venndir_label_style()`, which could impact a number of downstream
steps. Corresponding `testthis` entries have been added, hopefully to
prevent occurrence of new bugs.

* Added `vdiffr` to R package Suggests.
* `venndir()`, `render_venndir()`, `venndir_label_style()`

   * All three functions now recognize a new column in `Venndir` slot
   `jps` with column `"ref_polygon"`. This new column associates the set
   name with an overlap polygon, which points the set label to the
   appropriate part of a polygon. For example, by default "set_A" will
   point to the overlap_set "set_A" which has no overlaps with other sets.
   For proportional diagrams, sometimes "set_A" is fully encompassed by "set_B",
   in which case the label "set_A" will point to the overlap polygon
   which is (1) not empty of course, and (2) has the fewest overlaps.
   * Without this association, the set label was not displayed, which was
   not intended.
   * Further, the previous implementation was incorrect, causing some
   labels to appear in the wrong position. The line segment from label
   to overlap polygon still pointed to the correct position, but it was
   awkwardly placed outside the circle.
   * It turns out that moving the set label affected other assumptions
   of the data model, namely that the set name "set_A" may be associated
   with `overlap_set` by a different name, for example `"set_A&set_B"`.
   That change broke the label grouping logic, and needed to be resolved.

## Added test cases

* More `testthis` scenarios have been added, including the first examples
that test consistent graphical output. Mainly these tests focus on proportional
2-, 3-, and 4-way Venn and Euler diagrams, with empty sets, or nested
sets, or both.


## bug fixes

* `venn_meme()`

   * Partial fix for a bug that caused `item_cex` to be ignored.

# venndir 0.0.33.900

* Removed `matrixStats` from dependencies.

## changes to existing functions

* `venndir()`

   * New arguments: `setlist_labels` and `legend_labels` that use
   `names(setlist)` by default, but can be adjusted to be display-friendly.
   The `setlist_labels` are displayed on the Venn diagram itself.
   The `legend_labels` are displayed only in the Venn legend via
   `venndir_legender()`.

* `list2im_opt()` has a new default argument `do_sparse=FALSE`.

   * Previously all incidence matrix output used compressed `Matrix`
   class when available. In almost all cases, I immediately converted
   back to `matrix` or used `do_sparse=FALSE` anyway. Before wider
   use of the package, best to improve the default condition.

* Added examples to `rescale_coordinates()`.

# venndir 0.0.32.900

## Bug fixes

* `render_venndir()`

   * Fixed regression that did not recognize updated output from `venndir()`,
   addressing issues #7 and #8.

## Changes to existing functions

* `venndir()` and `render_venndir()`

   * Percent can now optionally be labeled for each overlap,
   with `show_labels` which includes the letter `"p"`.
   When `"c"` and `"p"` are both included, the two labels are included
   in the order they appear, separated using `percent_delimiter` which
   by default uses a newline.
   For now, percentage will always appear in the same location with
   counts `"c"`, unless counts are hidden.
   For example `show_labels="Ncp"` will place set names outside, and
   count and percent inside.
   Similarly `show_labels="Np"` will place set names outside, and only
   the percent will be displayed inside.
   Also, `show_labels="Npc"` will place the percentage first, then
   the overlap count.
   * New argument default `item_style="default"` will auto-detect Markdown
   and imposes `item_style="gridtext"` when found, otherwise uses the faster
   `item_style="text"`.
   * `show_items=NA` default is changed when items should be displayed,
   defined when `show_labels` contains `"i"`. In that case the `overlap_type`
   helps determine eithr `show_items="item"` or `show_items="sign item"`.

* `Venndir-class` - additional documentation.
* `venndir_label_style()`

   * recognizes option to include (or hide) percent, in the same location
   as the overlap count label.
   * Argument `percent_delim` controls the delimiter between count and
   percent, and takes effect only when both are visible. The default
   `"<br>"` places the second value on a new line.

* `venn_meme()` change via hotfix:

   * new argument `draw_legend=FALSE` to hide the Venn legend by default,
   a change from the default for `venndir()` and `render_venndir()`,
   since the legend is not relevant for `venn_meme()`.

## tests

* A series of small, simple unit tests were added for Venn diagrams,
nested-proportional Venn, Venn with an empty set, etc.

# venndir 0.0.31.900

* All `sp` and `rgeos` references were removed!
* More work to be done to improve the use of `Venndir` objects internally,
with "convenience functions" to help access and manipulate its data.

## changes to existing functions

* `add_orientation_JamPolygon()`, `area_JamPolygon()`, `plot.JamPolygon()`,
`union_JamPolygon()`

   * updated to improve detection of empty polygons

## REMOVED a bunch of sp/rgeos dependent functions

* Note: These functions were not, uh, functional anyway.
* Most functions were called internally, are were replaced with new
functions that call polyclip-based functions.
* Removed all references to `sp` and `rgeos` package prefix, even
in help text.

# venndir 0.0.30.900

**The polyclip update.** Removing all remnants of `sp` and `rgeos`.

Major change, all functions ported or rewritten not to rely upon `sp`,
`rgeos`, and `sf`, since the migration to `sf` would also incur heavy
burden of installing `sf` and all its map/geography-related system
libraries.

Backward compatibility is limited for pre-existing venndir output,
since `rgeos` was removed from CRAN (ugh). In principle a pre-existing
object could be re-run to produce the new object output.

Attempts were made for `venndir()` to produce output equivalent to previous
versions, but it is unclear how practical that may be.

All graphical output uses `grid`, no support for `base` or `ggplot2`.
The `grid` output was chosen since it can be conveniently combined
into panels using `patchwork` or `cowplot`, while offering more robust
features to position graphical components, also keeping 1:1 aspect ratio
so Venn circles remain circular.

New S4 objects:

* `JamPolygon` - makeshift polygon `data.frame` to hold one polygon per row
(where one "polygon" may be represented as a multi-part polygon with optional
holes or nested holes.) This object makes calling `polyclip` and `grid`
functions more reliable, since they recognize slightly different formats.
* `Venndir` - replacement for `list` output, with equivalent content.
Will transition to function accessors to obtain content, removing
need for direct list access.


## venndir package changes

* added `pracma`, `polyclip`, `vwline` to Depends.
* removed `sf`, `sp`, `rgeos` from Depends. (Ack)
* removed `ggtext` from Depends, all output uses `gridtext` or `grid`.
* replaced `venndir()` and `render_venndir()` with new versions,
very temporarily moving previous functions to `venndir_OLD()` and
`render_venndir_OLD()`.
* new function for item label fill, previously called `spsample()`.


## new `JamPolygon` object and functions

(It may eventually become its own R package.)

* `JamPolygon` is intended to provide a very basic mechanism to store
polygon coordinates with the following design:

   * One "polygon" is stored on one row of a `data.frame`.
   * One "polygon" can include multiple parts, for example including holes,
   nested polygons (polygon inside the hole of the parent polygon), and
   disjoint/separate polygon components. For example one "polygon"
   could represent multiple separate shapes, each with or without holes
   and enclosed polygons.
   The driving reason is to handle proportional Venn diagrams,
   which sometimes have a set fully enclosed inside another set
   which creates a hole in the larger set; sometimes two elliptical polygons
   overlap in the middle at an angle, so the unique portion of one ellipse
   is split into two parts.
   * Multi-part polygons are assumed to be simplified, with no overlapping
   solid regions. (Polygons are combined with `union_JamPolygon()`.)
   * Multi-part polygons always use `"evenodd"` logic, so any inner
   polygon is assumed to be a hole, and any polygon inside a hole is
   assumed to be solid. This logic is applied regardless the
   clockwise/counterclockwise orientation of points for each polygon
   (polyclip convention: solid is clockwise, holes are counter-clockwise).
   Calls to `polyclip` functions will adjust the polygon orientation
   as needed for the purpose of using `polyclip`, but these details
   are not important for `JamPolygon` itself.
   * Other properties can be associated with the polygon row, to help
   maintain and view these associations in a convenient way:
   name, label, fill, border, border.lwd, fontsize, family,
   innerborder, innerborder.lwd.
   * Optional properties can be added to describe multi-polygon rows:
   orientation, polygon_holes, polygon_clockwise, polygon_parent.
   They are mostly useful for internal functions.
   * `innerborder` is a new feature, my selfish desire to enable borders
   that are not overdrawn with adjacent polygons (which share the same border
   but on different sides). It requires a brilliant package `vwline`
   to draw variable-width lines (thanks Paul Murrell). It also requires
   knowledge of the direction lines are drawn, in order to define
   left/right border relative to the path taken when rendering the border.
   * All plot rendering uses `grid` graphics. All coordinates are
   encoded in the `JamPolygon` object, so they can be plotted separately,
   however `plot()` and `plot.JamPolygon()` use `grid`.

* Some supporting functions for `JamPolygon`:

   * `plot.JamPolygon()` - `grid` draw function for `JamPolygon` objects
   * `rbind2()` - generic function to combine multiple `JamPolygon` objects
   * `bbox_JamPolygon()` - bounding box enclosing all polygons
   * `area_JamPolygon()` - area for each row, accounting for holes and nested
   polygons.
   * `buffer_JamPolygon()` - expand or shrink polygon borders
   * `point_in_JamPolygon()` - test for point inside solid portion of polygons
   * `has_point_in_JamPolygon()` - logical presence/absence of point in any
   solid polygon
   * `add_orientation_JamPolygon()` - adds polygon/multipolygon orientation,
   including holes, clockwise, and parent enclosing polygon.
   * `labelr_JamPolygon()` - define best label position within each polygon
   * `minus_JamPolygon()` - first polygon, removing subsequent polygons
   * `union_JamPolygon()` - combine and simplify all polygons into one row
   * `intersect_JamPolygon()` - first polygon, intersecting subsequent polygons
   * `sample_JamPolygon()` - define sample points within solid polygon
   * `split_JamPolygon()` - split multi-part polygons onto separate rows.
   Note that holes become solid polygons during this step.
   * `polyclip_to_JamPolygon()` - convert `polyclip::polyclip()` output
   to `JamPolygon`.

## New object `Venndir` for venndir output

* Intended to replace previous `list` format, with data accessor functions
instead of direct access to list components.


# venndir 0.0.29.900

* bumped version dependency for colorjam (>= 0.0.26.900)
* added steps for plan to migrate from `sp` to `polyclip`, avoiding
heavy dependencies on GDAL, GEOS, LWGEOM software libraries which
are unrelated to venndir's use of polygon geometry functions.

## new functions

* `venndir_to_df()`

   * intended to convert Venn diagrams to `data.frame` or a user-friendly
   `kable` formatted table suitable in RMarkdown.
   * removes `"<br>"` from setlist names when relevant.

## changes to existing functions

* several functions have new argument `blend_preset` to enforce
`colorjam::blend_preset()` using the RYB color preset.

# venndir 0.0.28.900

## bug fixes

* `polygon_label_fill()` threw an error when supplied with an empty
polygon, now returns `NULL` so the upstream function can deal
with the lack of labels. The issue appears to arise from proportional
diagrams that use item labels, where the item label has no suitable
polygon to use. Presumably something from the updated `eulerr` package
that creates overlap circles/ellipses to use in these diagrams.


## new functions

* Not released: `reposition_venn_gridtext_labels()` intended to help adjust and
re-position gridtext labels relative to optional incoming line segments,
and with goal of enabling different layouts for combinations of labels.

# venndir 0.0.27.900

## changes to existing functions

* `curate_venn_labels()`

   * The pattern matching was improved so that it can handle replacement
   values that may contain patterns, without re-replacing resulting
   `character` strings.
   For example `from="1"` will no longer match the value `"-1"`
   when it is the first entry of `curate_df[,"from"]`.
   Similarly, an output string that contains `"1"` will no longer be
   re-replaced again by matching pattern `from="1"`.
   The use case was noticed when trying to specify a gridtext font size
   using `from=c("-1", "1")` and
   `sign=c("<span style='font-size=18pt'>Down</span>", "<span style='font-size=18pt'>Up</span>")`
   which caused the `"1"` to be re-replaced.

# venndir 0.0.26.900

## changes to existing functions

* `render_venndir()`

   * `plot_style="base"` now plots the `sf` form of data, rather than
   `SpatialPolygonsDataFrame`, the first step in transitioning from `sp`
   to `sf` prior to the deprecation of the `sp` package altogether.
   All plotting should be equivalent.
   * `lwd` is now honored when plotting Venn circles and polygons, and
   `lwd=0` is silently replaced with `lwd=1` and `col="#00000000"` so
   the borders are hidden.
   * `plot_style="gg"` honors `lwd` as `linewidth` using
   `ggplot2::scale_linewidth_identity()`, which unfortunately is slightly
   larger line width than corresponding values with base R plots. Ah well.
   * Default line width changed from `lwd=2` to `lwd=0.5`. The value is
   not configurable in function calls, but can be edited in the venndir
   output `list` object, in `venn_spdf`.

* `venndir_legender()`

   * now properly matches subset `setlist` and order of the `venndir_out`,
   in case using a subset of `setlist` in `venndir(..., sets=c(1, 3, 4))`
   * fixed issue with grid output when no `venndir_out` was supplied,
   now it defaults to white background with black text.
   * new argument `keep_newlines=FALSE` which by default removes newlines
   (line breaks) in the set labels. Use `keep_newlines=TRUE` to keep
   line breaks as-is.
   * new argument `set_colors` to specify optional colors to use in the table.
   * The `poly_alpha` value used to define transparency of Venn colors
   is honored in the color legend, using `venn_spdf$alpha`.
   * The border color `"border"` is now honored, as is `"lwd"`.

# venndir 0.0.25.900

## new functions

* `venndir_legend()`

   * option for grid or base style, works with base or ggplot2 venndir.
   * The recommended `style="grid"` as it offers a nicely organized style,
   with better positioning which works the same for `base` and `ggplot2`
   `venndir()` output.

## changes to dependencies

* added `data.table` to dependencies, since it is used by `shrink_df()`.
* added `gridExtra` to enable optional legend `gridExtra::tableGrob()`.

## changes to existing functions

* `venndir()`

   * argument default `overlap_type="concordance"` was changed to
   `overlap_type="detect"` to fix regression where all input data
   is assumed to be signed, thus showing up arrows even for non-directional
   input data. The function `signed_overlaps()` will detect an appropriate
   type unless given a specific `overlap_type` value.

* `list2im_opt()` and `list2im_value()`

   * argument default `do_sparse=TRUE` was changed to `do_sparse=FALSE`
   because in almost all cases the most appropriate class is `matrix`.
   Only for very rare cases with extremely large matrices should
   sparse matrix objects be used, which makes it most appropriate as an
   option and not a default.

## bug fixes

* `counts2setlist()` warnings regarding GEOS future retirement are silenced
in the examples.
* `list2im_opt()` uses the updated non-deprecated coersion of `matrix`
to `"ngCMatrix"`, although the end result is still an object with class
`"ngCMatrix"`. It now uses three `as()` coersions, as instructed by the
Matrix package. The Matrix `as()` is properly imported by using
`importFrom(methods, as)` and placing `Matrix` into the `Suggests` list.


# venndir 0.0.24.900

* `jamba` now requires version 0.0.90.900 for `coordPresets()` to
provide figure positioning rather than plot positioning. Used
for `plot_warning` with proportional Venn diagrams that cannot
display all possible overlaps.
* Signed labels should now have padding appropriate to their font size,
instead of inheriting padding used by other labels. The visual effect
is small, but should improve the packing of signed labels, making
the overall group of labels smaller and more cohesive.

## changes to existing functions

* `venndir()`

   * new argument `padding` to customize the padding around Venn labels.
   * new behavior, `padding` is scaled relative to `font_cex`, for smaller
   padding when the font size is scaled down.
   * `plot_warning` label is placed relative to the bottom of the
   plot figure, not the plot border, giving a bit more space below
   the typical proportional Venn diagram.

* `render_venndir()` calls `gridtext_richtext_grob()` instead of
`gridtext::richtext_grob()` to enable vectorized `padding`.

## new functions

Two temporary functions to customize gridtext. Both functions also
call internel `gridtext` functions, so they cannot be maintained when
`venndir` is prepared for CRAN. More likely these functions will become
the basis for `gridtext` issue and/or pull request (PR).

* `gridtext_richtext_grob()` - apply `padding` in vectorized form, to
enable distinct `padding` to be applied to each label. This function
is modified to extend `padding` and call custom `gridtext_make_outer_box()`.
* `gridtext_make_outer_box()` - apply `padding` in vectorized form, to
enable distinct `padding` to be applied to each label.



# venndir 0.0.23.900

## change in defaults

* The default `venndir_label_style()` position for set names is
now `"outside"`, changed from previous `"inside"`. There were
too many cases where the set name was long, then pushed
the numeric count labels too far away from the desired position at the
center of the Venn polygon. The new challenge is getting adequate
spacing around the Venn diagram to place labels that fit inside
the plot window.
* `make_color_contrast()` was adjusted to reduce the saturation/chroma
of light text colors, the end result reduces the "cyan" appearance of
"dodgerblue" when converted to a lighter contrasting color.

## changes to existing functions

* `venndir()` new argument `sign_count_delim` allows customization of
the delimiter between sign and numeric counts, for example `"^^: 12"`
uses the default delimiter `": "`. In some cases it may be preferred
to use `sign_count_delim=" "` to omit the colon `":"` character.

## bug fixes

* `venndir()` and related functions were failing in RStudio when
used under `devtools::load_all()`, apparently because the `rgeos`
package needs to be loaded with `@imports` before directly calling
functions from the `rgeos` package. This issue appears to occur only
when using `devtools`.

* Corrected some warnings caused by argument overloading:

   * `label_preset` is now recognized by `venndir()` and `render_venndir()`,
   and is now passed formally from `venndir()` to `render_venndir()` to
   prevent it being passed to other internal functions, such as the `plot()`
   function. The warning was: `"label_preset" is not a graphical parameter`
   * `display_counts` a developmental argument passed from `venndir()`
   to `render_venndir()`. The warning was:
   `"display_counts" is not a graphical parameter`

## new branch (future dev)

* A new Github branch was created
`"sp-to-sf"` to migrate away from the sp (Spatial Polygons) package,
and toward the sf (Simple Features) package. This migration will
remove dependencies on `sp` and `rgeos` which are being retired
in 2023 anyway. It should also make the overall dependency footprint
of `venndir` much smaller, since `sf` has few dependencies while
also covering the functions required from other packages.

# venndir 0.0.22.900

## bug fixes

* `textvenn()` was throwing an error with jamba version `0.0.83.900`
caused by updates to `jamba::printDebug()` used for printing colorized
output. Dependency was bumped to `0.0.84.900` where the bug was fixed.

# venndir 0.0.21.900

## updates to existing functions

* `venndir()` and `render_venndir()` new argument `item_buffer`

   * goal is to make this option easier to modify
   * It is passed to `polygon_label_fill()`
   as argument `scale_width` which was otherwise very hard to find.

* `render_venndir()` will now auto-calculate an appropriate `item_cex`

   * when `item_cex=NULL` or `item_cex` has only one `numeric` value.
   The scaling is based upon the total number of items inside
   each polygon, and the relative area of each polygon.
   * When `item_cex` is supplied as a single `numeric` value, it is multipled
   by the auto-scaled adjustment, so the `item_cex` acts as a
   relative adjustment to the auto-scale font sizing.
   * `item_cex_factor` helps control the magnitude of scaling for large
    numbers of items (which shouldn't matter much since they are already
    not very legible.).
   
* `render_venndir()` default change `segment_buffer=-0.05`

   * smaller than previous `segment_buffer=-0.2`
   * This change will cause
   most segments to end very near the outer border of the polygon.
   * For proportional euler diagrams, this change is also a
   stop-gap correction for having the segment line end too far inside
   the polygon, sometimes causing it to conflict with other polygons.
   That issue is caused by having the segment point inside the parent
   polygon (circle or ellipse) instead of an overlap polygon, and specifically
   the segment should point to the overlap polygon with the fewest overlaps,
   by default the polygon whose only overlap is to the set label itself.

* `venn_meme()` was updated to pass `item_cex` as a two-item `numeric`
vector in order to prevent the auto-scaling described above.


# venndir 0.0.20.900

## bug fixes

Proportional Venn diagrams have been a bit of a challenge for labeling.
Some edge cases were resolved finally, mostly relevant when certain
Venn overlap wedges were razor thin, or when an overlap was represented
by more than one polygon in the resulting geometry.

Also, label placement outside the Venn circle was inconsistent, some
of the options in `polygon_label_outside()` appear to work poorly together.
Its default values were adjusted to be more consistently "pleasing".

* `render_venndir()` had a bug that caused proportional Venn labels for
single-set overlaps to be assigned to the wrong set polygon when the label
was being positioned outside. The line segment would sometimes be visibly
ambiguous. It would point the setA line segment to the full setA instead of
the smaller non-overlap polygon representing unique setA items.
In most cases the workaround was to use `segment_buffer=0` so the label
arrow is drawn to the edge of the circle. When the overall Venn
diagram had reasonable proportions the visual effect was not that
obvious. With extreme Venn diagrams (small sliver of unique items
for some sets) the output was not as intended.
* `get_largest_polygon()` was updated to work with `SpatialPolygonsDataFrame`,
previously it did not subset the polygons, returning the full set of polygons.
It contributed to set labels being weirdly placed, specifically in rare cases
where an overlap set (for example `setA&setB`) was represented by two polygons
where setA and setB uniquely overlap, and where one of those polygons was
extremely small. In the weird test case, the smaller polygon had area 0.2,
while the larger polygon had area 150. The label is intended to point to
the largest available polygon for each overlap, however with objects
`SpatialPolygonsDataFrame` it was returning all polygons for each overlap,
and the first polygon was labeled. The issue showed up only when the first
of multiple polygons happened to be the smaller one, and substantially
smaller.

Lightweight change: `polygon_label_outside()` was moved into a separate .R
file. The `venndir-sp.R` file has too many functions, it will be split into
pieces eventually.

# venndir 0.0.19.900

## bug fixes

* `polygon_label_fill()`: At last fixed an elusive bug causing certain
item label scenarios to throw an error, turned out to be caused by lack
of rownames on a matrix sorted by `jamba::mixedSortDF()`. The error
occurred in an internal function `get_poly_points()` that iterates
different labeling conditions until at least `n` label coordinates
are returned. Apparently the function `spsample()` does not
guarantee returning the same number of coordinate points as requested.
This error must have bitten someone else somewhere in the R world,
requesting 15 sampled points, then receiving only 12.


## changes to existing functions

* `rescale_sp()` argument `rotate_degrees` will now recognized a named
vector.

   * names are expected to match polygon names stored in the
   `sp` object in the ID slotName of each individual `Polygons` object,
   for example the first polygon in a `SpatialPolygon` object would have
   name stored as: `sp@polygons[[1]]@ID`. When `rotate_degrees` contains names,
   only names that match the polygons will be applied, which allows
   rotating individual polygons within a `SpatialPolygons` object.
   * `share_polygon_center=TRUE` new argument intended for multi-polygon
   `Polygons` objects, which happens when there are two disconnected
   parts inside one `Polygons` element. When this occurs, by default
   the two parts are rotated as if they were one unit, around their own
   local center, by default. When `share_polygon_center=FALSE` each
   polygon is rotated about its own personal center.

* `venn_meme()`

   * new argument `item_style="gridtext"` is a different default than
   `venndir()` because memes are expected to have fewer labels, and are
   most likely to use fancy markdown text formatting. This argument is
   passed to `venndir()`. When `item_style="text"` it will autocorrect
   `"<br>"` into newline; when `item_style="gridtext"` it will autocorrect
   newline into `"<br>"`.

# venndir 0.0.18.900

## bug fixes

To resolve issue #2, the `colorjam` package was updated to
add prefix to call `jamba::tcount()` to function `colorjam::approx_degrees()`.
When the `colorjam` package is loaded, using `library(colorjam)` or
`require(colorjam)` it automatically also loads jamba so its functions
are available. However, when `colorjam::approx_degrees()` is called
directly, without formally loading the `colorjam` package, the
`jamba` functions are **not** loaded. This issue caused the error,
which unfortunately causes the `venndir()` function to fail unless
`jamba` or `colorjam` packages have been loaded previously.

All that said, the "fix" is to require the updated `colorjam(>= 0.0.22.950)`
package version that includes this fix.


# venndir 0.0.17.900

## updates to existing functions

* `venndir()` was updated to change the priority of label placement
for proportional Venn diagrams. It used to choose the full set circle
for the main set label, now it chooses the unique overlap polygon
for that set if it exists, then uses the full set circle only
as a fallback.
* `venndir_label_style()` was updated to fix longstanding weird glitch
with proportional Venn diagrams sometimes displaying counts when one
set is fully inside all other sets, but that set has non-zero items
unique to that set - so there is nowhere appropriate to display them.
Because the main set label was displayed, the count label was errantly
placed at the center of the Venn circle. Now the bug is fixed, the value
is not displayed - it is part of the optional warning message that
lists any overlap counts that could not be displayed.
* `render_venndir()` fixed type "bow_lwd" is now correctly "box_lwd",
only impacted box line width when the line width was changed from default.


# venndir 0.0.16.900

## bug fix

* `counts2setlist()` corrected an error when supplied with
empty overlaps, which previously included at least one
delimiter even for empty sets. Entries were added to the
tests to confirm this behavior.
* `polygon_label_fill()` small bug in how label placement used
rotation, the polygon center was not properly re-used when
un-rotating labels to the original orientation. The default was
-20 degrees, so the effect was subtle.


## changes to existing functions

* `venndir_label_style()` was updated to change the logic used
to place labels inside or outside each Venn overlap polygon,
specifically to allow item labels to be inside, or hidden,
and adjust the count label visibility accordingly.
* `venndir_label_style()`, and `venndir()` now properly hide item
labels using `max_items`, instead displaying the overlap count.
* `venndir_label_style()` arguments `count` and `signed` have new options:

   * `"ifneeded"` will display the overlap count when item labels are not
   being displayed, otherwise when item labels are displayed the count
   will be hidden.
   * `"detect"` will display the overlap count inside when item labels are not
   being displayed, otherwise the overlap count is outside when item labels
   are displayed.

* `polygon_label_fill()` new argument `ref_sp` used to know the relative
size of the polygon being labeled, to account for the adjustment
when `apply_n_scale=TRUE`:
the buffer polygon is generally smaller for fewer labels, but the new
adjustment prevents polygons from being shrunk when the polygon is
already very small.
* `signed_overlaps()` new default `overlap_type="default"` which
attempts to choose a reasonable option. When all values are `c(0, 1)`
it will choose `overlap_type="overlap"` which does not use directionality.
When the input contains `numeric` or `integer` values it will enforce
`sign()` so the comparison is done with the sign and not decimal values
by default.


# venndir 0.0.15.900

## bug fixes

* `signed_overlaps()` was not handling `factor` vectors as input,
they were getting coerced to `integer` values instead of `character`
values. This issue was caused by handling duplicate entries,
in this case the duplicate entries were not renamed to unique entries
by calling `c()` for the `makeNamesFunc` part of `jamba::makeNames()`.
The base R assignment `names(x) <- c(y)` coerces `factor` values in
`y` to `integer` values, which in this case breaks the expected behavior.
This seems like an edge case, but certainly one that `venndir`
should handle without issue - it is not always clear when values
are stored as factors instead of character, and the expected
behavior in all cases is for `venndir` to treat `factor` as
`character` values. (Thank you Dr. Ashley Brooks for bringing my
attention to this issue!)

## changes to existing functions

* `list2im_value()` was also updated to handle `factor` values by
coercing to `character` instead of allowing the base R `matrix`
coersion which by default coerces to `integer`.
* `list2im_value()` was also updated to handle `empty` which is
a user-defined value to use in the resulting incidence matrix for
empty entries. This argument is not used by `venndir` but is
available for general conversion of value list to value incidence
matrix.
* `list2im_value()` was also updated to use a consistent default
`empty` based upon whether the input `setlist` contains any
`character` or `factor` values. The previous behavior
handled each column independently.
The rule is that when any column contains `character`
or `factor` values, the default is `empty=""` across all columns;
if all values are `numeric` the default is `empty=0`.
The result is a consistently formatted value incidence matrix.
This is a rare edge case that is not expected for common use of
`venndir` but may arise from the conversion functions from value
list to value incidence matrix.

## new unit tests

* unit tests were added to confirm the behavior of `signed_overlaps()`
with named `factor` vectors as input.


# venndir 0.0.14.900

## new function

* `venn_meme()` is a wrapper function to help create "Venn Memes"
also known as "Concept Venns". These have text inside each overlap
but no other counts or labels. Input is expected to be an
overlap list, which is converted to setlist using `overlaplist2setlist()`.
See examples.

## updates

* `venndir()` argument `font_cex` was updated to accept length 3
which is applied to (1) main set labels, (2) main count labels, (3) directional
count labels. It is sometimes useful to have main set labels larger
than the main count labels, which prompted this change. Now
it is possible to place main set labels outside the diagram,
and have smaller count labels inside the diagram, for example to
fit inside proportional Euler diagram shapes. This change adds
`fontsize` to the `venn_spdf` output object, thus storing the
set label font size separate from the count label font size.
* `venndir_label_style()` new argument option `label_preset="custom"`
will not modify any label position settings. This option is intended
when labels are manually adjusted and should not be changed, and
where the user wants to apply `label_style` to affect the label
color, fill, and box outline.
* Updated dependency on `colorjam` to version `0.0.18.900` to
fix error with `rad2deg()` when blending colors.

## bug fixed

* `venndir_label_style()` now recognizes when items cannot be displayed
inside proportional diagrams, and sets `show_items="none"` for those
entries in `label_df`. This change prevents trying to place labels
inside a NULL overlap polygon.

## testthat

* Added more tests, covering `venndir converters` like `im2list_opt()`,
`list2im()`, `im_value2list()`, and `list2im_value()`.


# venndir 0.0.13.900

## updates to existing functions

* `label_polygon_fill()` new argument `layout_degrees` allows
rotating the label placement, relevant mostly for
`label_method="regular"` which produces square point grid,
and `label_method="hexagonal"` which produces a hexagonal
point grid. It can be useful to rotate those points so text
labels have less overlap. As a result `degrees=0` is the new
default, as `layout_degrees=-22` is also the new default.
See the new set of examples which includes visual representation
of methods to reduce text label overlaps.
* `render_venndir()` default is changed to `item_degrees=0`.
* `match_list()` was updated to require both list elements to
have the same length, to account for duplicate items.
* Added more tests, still many more to add.


# venndir 0.0.12.900

## updates to existing functions

* Added new function categories: "spatial", "label",
"conversion", "plotly".
* Several functions were updated with newer examples.
* Renamed function `label_polygon_fill()` to `polygon_label_fill()`
to be consistent with `polygon_label_outside()` and
`polygon_label_segment()` -- also before anyone happens to start
using this function!
* Fixed issue where `fontfamily` was not being used with item labels,
breaking display of unicode characters in some sessions.

## new functions (experimental for plotly)

* `to_basic.GeomRichText()` and `to_basic.GeomTextBox()` were added
for rudimentary compatibility when using `plotly::ggplotly()`
with `ggtext::geom_richtext()` and `ggtext::geom_textbox()`. These
functions do display text inside plotly figure, however: (1) they
do not cleanly use text positioning, even with `textposition`;
(2) plotly does not interpret markdown, so any markdown text is
displayed as-is. Ultimately plotly support will probably be added
to `render_venndir()` via `plot_style="plotly"` then using
`plotly::plot_ly()` commands in native form.

# venndir 0.0.11.900

## bug fixes

* FIXED: `show_zero=TRUE` was not working, zeros were always hidden.
* FIXED: `alpha_by_counts` appears to be mis-aligned.
* FIXED: Fix issue where directional label colors apply
`make_color_contrast()` without using the correct
`alpha` value. See with `venndir(make_venn_test(100, 3), poly_alpha=0)`


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
As "rgeos" `gBuffer()` has no ability to scale relative
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
`SpatialPolygons` compatible object, which must have
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
`polylabelr::poi()` for `SpatialPolygons` objects.

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

* `render_venndir()` added `sp` prefix to `plot()` for
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
* `label_polyogn_fill()` takes a `SpatialPolygons` object,
a vector of labels, and returns coordinates to place labels
inside the polygon. It has some adjustments to add buffer
around polygon boundaries, but does not directly detect
label overlaps. It either plots labels in base R graphics,
or returns coordinates suitable for ggplot2.

## new `rescale_*()` functions

The `rescale_*()` functions
are intended to allow manipulation of numeric coordinates in
each relevant object type, aimed at `SpatialPolygons`
and the various sub-types.

* `rescale_sp()` manipulates `SpatialPolygons` which
contains one or more `Polygons` objects, and therefore calls:
* `rescale_ps()` manipulates `Polygons` which contains
one or more `Polygon` objects, and therefore calls:
* `rescale_p()` manipulates `Polygon` which contains
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
