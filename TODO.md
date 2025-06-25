# TODO for venndir

## 12jun2025

* Add labels to `display_angles()` output?
* Consider options for item labels: outline text, fill/border around labels.
Older versions of venndir used "lite box" style item labels for clarity.
* DONE. Consider how to handle "Hidden overlaps"

   * DONE. Indicate with footnote-style icon? warning: "\u26A0", footnote
   dagger "\u2020", double-dagger "\u2021", info sign "\u24D8",
   reference mark "\u203B".
   * DONE. `has_footnotes()`, `footnotes()`

* Debug `venndir()` outside labels, specific edge case with proportional 4-way,
shape="ellipse", and labels pointing to polygons with multiple parts.
They sometimes point to the "far" part.

   * Outside labels are not associated with a segment point, but are
   defined one anyway. Maybe we should store and use the segment point?

* DONE.Debug `label_outside_JamPolygon()` angles again.
* DONE. Confirm `render_venndir()` is not placing item labels twice.
* New argument `marquee_style` to allow optional `marquee::style` object.
* DONE. Fix bug with `curate_df` not using custom signs in the legend.
* `highlight_venndir_overlap()` add easy way to update 'label.count'.
* `make_color_contrast()` - consider improving Luminance when flipping
white/black so the output is black/white and not darkgrey/lightgrey.
* Add `describe_venndir_overlap()` to complement `modify_venndir_overlap()`.

   * For example, it is not easy to see the `fontsize` already applied.
   * This function would return the values already defined in the `Venndir`.

* DONE. Need ability to move one label outside or inside.
* `render_venndir()` and `modify_venndir_overlap()`  review attributes
not being customized, low priority for now: label.padding, label.lwd,
label.lty, label.r, border.lty.
* Spot check `venndir()` how it populates `x_offset`,`y_offset` into
`label_df`. It might not be filling everything in as expected.
* PARTIAL. Add `svglite::svglite()` adaptation for `vdiffr` tests.
Interim tests show only subpixel differences.
* Seriously just refactor `venndir_label_style()`

   * It should essentially ignore everything but `show_labels` which should
   also become a trusted column in `label_df`.
   * The style preset could also become a column, attached to rows.
   * Consider option for inside/outside labels to have different styles.


## 02jun2025

* Add hexsticker.
* Consider adding `fontfamilies` to metadata.
* Consider some mechanism for `render_venndir()` to re-use its
own output instead of re-creating. E.g. if `gtree` exists, just render it?
* Improve default plot range to include outer label coordinates when defined.
It's embarrassing to have labels above the plot that are cropped by default.
* Consider a 'style' or 'venndir.theme' type object? Something to wrap up
all the different default options into one neat little object.
* DONE. Fix bug: segment pointing to blank label

   * `venndir(counts2setlist(c(B=5, "A&B"=3)), show_labels="NC", proportional=TRUE)`
   * DONE. Related to label grouping with set name pointing to inside overlap.
   The count label is also now displayed separately.
   It may become a user-defined option later, to control whether
   to combine set and count labels.

* Prep for Bioconductor

   * Minimize dependencies. Current heavy: marquee, data.table, colorjam
   Try: `pkgnet::CreatePackageReport("venndir")`
   * Consider pushing colorjam to CRAN for a short term release.
   * Consider remove or refactor `shrink_df()` the only need for data.table.
   * Run checks, remove all warnings

* Consider sorting sign based upon the order each direction appears in
`curate_df`.
* Consider new `show_labels` options

   * "T" for Total count per group, Dimitris for example used "Set A (1,234)",
   it would be required outside the Venn diagram by definition, and
   would only be displayed together with the set name. It may need to use
   some kind of glue template like '{n} items'.
   * "J" for Jaccard overlap?
   * "Z" for directional z-score? For example: `(agree - disagree)/sqrt(n)`
   * "Q" for Q-value (or P-value, but 'P' is already assigned to 'Percentage'
   so 'Q' is a reasonable alternative). How to define P-value is unclear.

* Consider integration with Upset plot capabilities

   * UpSetR, or ComplexHeatmap Upset functions for customizations.

* Future JamPolygon enhancements:

   * Add ability to draw points, lines. E.g. JamLines, JamPoints,
   or just a flag in jp@polygons that says "points" or "lines" so it
   would not render a filled path. Then optional columns 'pch', 'cex'.
   * easier method to "add grob" to an existing figure.
   Current method is clunky, requiring adjx()/adjy() and vp.
   * Consider: `get_grid_info()` to extract "all the things"?"


## 25may2025

* Consider temporary port of `blend_colors()` until colorjam is on CRAN.
* Consider showing optional overlap label for overlap sections,
e.g. assigning LETTERS to each overlap. Idea from DTheofilatos.
* DONE. Consider adding legend to `textvenn()` as a formal argument.
* DONE. Hide singlets as needed in `textvenn()`, consistent with `venndir()`.
* PARTIAL, made it darker. Consider custom segment color. Difficult to see.
* DONE. Consider reviewing polygon outside label placement.

   * DONE. Add "snap_y_fraction" to snap Y-values for labels within threshold.
   * DONE. Update the "adj" method to apply fractional adj for angular segments.

* Consider option in future to re-array outside labels.
* Consider method to customize item label positions.

   * Current idea: store item x,y coordinates in `label_df` and
   re-use when already pre-defined. It would allow manual adjustment
   as a stopgap option.

## Non-essential 25may2025

* Pie-in-the-sky: Prototype a ggplot2 implementation?

   * Major driver might be using ggrepel for improved label placement,
   except marquee cannot yet be used with ggrepel. (Might be WIP.)
   * See ggplot2 Layer Explorer:
   https://yjunechoe.github.io/ggplot2-layer-explorer/
   * Build prototype ggplot2 Geom/Stat methods for JamPolygon object
   * Motivates moving JamPolygons into a separate package

* Pie-in-the-sky: Consider venndir-shinyapp

   * Deploy live webR/shiny app to allow user intervention,
   Directly inspired by the "ggplot2-layer-explorer" shiny app.
   * Some type of data import - user-typed counts, or file, etc.
   * Consider deploying to shinyapps.io as live demo.

* Consider updating the plot layout so the legend is in a fixed region,
thus preventing overlap with the venndir polygons and labels.
* Consider making it easier to use patchwork with venndir plot output.
* Consider replacing `gridExtra::tableGrob()` with `gt`, then use marquee.
* Consider summarizing font usage techniques

   * `grDevices::embedGlyphs()` to embed font into a PDF file, see
   https://www.stat.auckland.ac.nz/~paul/Reports/Typography/glyphs/glyphs.html
   * Unicode characters, colored text, etc.

## 20may2025

* PARTIAL. Warn for overlaps not shown in proportional diagrams.

   * `warnings()` works for `Venndir` objects, printing missing overlaps.
   * `print()` for `Venndir` also includes overlaps not shown when present.
   * TODO: Consider method to add warning to a figure. Could be documented
   as a howto without providing a specific function.

## 19may2025

* DONE. (for now) Review item label use by marquee.

   * Consider how to use contrasting color for item labels.
   * Revisit how and when to use {.rel.1 text} and {.color text} to permit
   other markdown.

* Consider method to allow duplicate labels in different sections
with `venn_meme()`, which could be useful as blanks for example.

## 15may2025

* DONE. Debug warning on `assemble_venndir_label()` examples.
* DONE. Add more examples of gene expression data to the vignette,
at least one example using DESeq2.

## 01may2025

* PARTIAL. Fix issue with disappearing text when the font size is too small,
seems specific to "sans" font, perhaps systemfonts?

   * Consider changing default font back to "Arial"
   * Posted issue to marquee for review.

* DONE. Fix item labels converted to single-line with marquee.

   * DONE. However, now fix issue with huge whitespace padding between lines.

* Consider *item labeling* options:
   
      * `lineheight`, other marquee style attributes:
      `background`, `padding`, `border`, `border_size`, `border_radius`,
      `outline`, `outline_width` - interesting option similar to shadowText
      `align` - when showing bullets, left-align may be preferred
      * `wrapwidth`

* Consider changing how `blend_colors()` is called for color fill

   * currently ignores `poly_alpha` when blending colors, and
   could look better by using alpha during that step.
   * Also `blend_colors` itself may not be adding alpha aggressively
   enough with alpha > 0.5, it seems not to do much.

## 12apr2025

* DONE. Signed labeling: Hide concordance/agreement for singlets,
improve method to curate signs.
* DONE. Require marquee>=1.0.0 in order to avoid the rare R crash on MacOS when
using fonts whose font file path includes a space.
* MOSTLY COMPLETE. Consider transitioning all text grobs to use types:

   * `marquee::marquee_grob()`, `grid::textGrob()`, `gridtext::richtext_grob()`
   * Consider theming or argument to specify which type of grob to use.
   * Only marquee guarantees Unicode arrows, but lacks enhanced markdown
   in `gridtext::richtext_grob()` such as css styling, and inserted images.

* DONE. Consider wrapper for `marquee::marquee_grob()` as drop-in
replacement for `grid::textGrob()`, specifically to use with
`gridExtra::tableGrob()`
* PARTIAL. Migrate item text rendering to permit marquee, deprecating gridtext.

## 04apr2025

* DONE. Consider dependency `marquee(>=1.0.0)`

   * It requires `freetype2` (libfreetype) which may not be available
   conveniently on all systems.
   * Otherwise it could resolve "all" issues with Unicode characters,
   using internal font substitutions where necessary.
   * Potential drop-in replacement for: `grid::textGrob()`

## 28mar2025

* PARTIAL. Fix issue with `fontfamily` sometimes not displaying Unicode arrows.

   * Default `fontfamily="sans"` does not work for default MacOS on
   external device (non-ragg). "Helvetica" does not work on linux.
   * Consider dependency 'systemfonts>=1.1.0', test whether it automatically
   upgrades the Unicode glyph matching.
   * On MacOS R-4.3.3 using 'r-oldrel' the binary packages are marquee-0.1.0,
   systemfonts-1.1.0 which are old, before the crash fix (spaces in font
   filenames cause segfault and crash R session).
   Installing from source fails, it requires an updated build flag:
   `withr::with_makevars(c(OBJCXXFLAGS = "${CXX17STD}"), install.packages('marquee', type='source'))`
   However it still fails to find the conda-installed `libfreetype.6.dylib`,
   unknown reason.

* `venndir()`

   * DONE. Change defaults back to `innerborder.lwd=0.5`, `outerborder.lwd=0.5`,
   borders should be less visibly intrusive.
   * Consider global option "lwd" that effectively scales existing
   line widths, as convenient way to adjust lines.
   * DONE. Make `fontfamily` work by default for all common devices,
   meaning the upArrow and downArrow unicode should be properly
   displayed.
   Currently `fontfamily="sans"` MacOS displays [] empty boxes
   for arrows when not using ragg RStudio graphics device. Ugh.
   * `overlap_type="agreement"` should hide the signed counts for
   individual (non-overlapping) sets. It is not meaningful to say
   hits are in agreement when it only involves one set.
   * Small thing: Red + blue = purple. The purple needs improvement.
   * `show_labels` should be vectorized, to apply to each overlap in order.
   (This is not as intuitive as you'd think since the sets appear
   in label_df, then the overlap sets.)

* Improve `make_color_contrast()`

   * Fix `C_floor`. Figure out why output colors are desaturated.

* Improve `venndir::curate_venn_labels()`

   * Make default `curate_df` more visible. Perhaps a package object?

* Consider "Upset" export to UpSetR, and/or the ComplexHeatmap implementation.

   * Bonus points for figuring out if it's possible to include sign,
   for example in ComplexHeatmap can the bars be stacked, colorized by
   sign?

* Consider some type of "styling" object or system, with overall settings:

   * innerborder, innerborder.lwd, outerborder, outerborder.lwd,
   border, border.lwd,
   * template (tall/wide),
   * fontfamily/fontfamilies, fontsizes, fontfaces,
   fontcolors, label_spacing (list), label_padding (list),
   

## 10mar2025

* DONE. Make `venndir_legender()` title labels customizable.
* Partial. Add legend to `textvenn()`. Capability is there, not included
into the function yet. Want to add color.
* Consider `venndir_options()` as mechanism to adjust default values.
* Consider method to convert `signed_overlaps()` to setlist.
* Consider helper function `is_signed()` to review `list` input.
* Consider long-term approach to `label_fill()` that more accurately
fits/resizes/positions labels into the available space. Similar to
however wordcloud tools work.

## 05mar2025

* Consider ggplot2

   * perhaps new geoms (JamPolygon compatibility)
   * potential ability to use `ggrepel`

* Consider option for shadowText, `ggshadowtext` for clarity of text

* Consider small, "logo-size" Venn graphic, simplified to use in
smaller schematics, insets, or `gt` tables similar to sparklines.
See `gtExtras` package for example sparklines. Perhaps venndir there?
* Consider: Filling polygon by color to indicate % concordance?
Similar to using `show_items="sign"` except proper area fill.

## 24jan2025

* Fancy ideas for item labeling. Problem: Sometimes with gene symbols,
most genes are short "APOE", "MAP3K1", but others are "ENSMUSG00000141751".
Consider strategy to handle "long strings".

   1. Define "long string"
   
      * multiple words?
      * more than x-fold `nchar()` character length than median length?
      * more than fixed `nchar()` character length, and/or number of words?

   2. Strategies to handle

      * shrink the font size, proportional to string length.
      * For long item strings, use a narrow font. Might handle 2-fold effects.
      * Consider word wrap, only useful for multi-words. Or hyphenated?
      * Consider option to truncate after max `nchar()` or max words.

## 17jan2025

* Consider option not to merge count and outside set labels, particularly
when the count label is placed outside. Currently the counts for "A" are
placed underneath set label "A" when it appears outside. This grouping
makes sense sometimes, but may not make sense with proportional labels,
especially when the set label is manually nudged in the figure.

## 13jan2025

* Implement specific methods to handle DEG results as input

   * DESeq2 "hits"
   * limma-voom hits - either by `limma::topTable()`

## 10jan2025

* DONE. Further refine label spacing. Currently assembles multiple gtables,
however the outer buffers add, making the gap between label types too large.
Strategies to try:

   * DONE. `gtable::gtable_add_row_space()`, `gtable::gtable_add_padding()` -
   more clearly apply spacing between multi-rows of the same label type,
   spacing between rows of different label types, then padding around the
   grouped label.


## 09dec2024

* DONE. Consider "vertical" item label fill geometry, for "columnar arrangement".
* Make font label spacing scale up or down with font size.
* Consider multi-column legend, to make it wider and flatter.
* Debug outside label positioning with 4+ proportional sets.
* Debug error with empty proportional sets.
* Consider `bookdown` package docs with examples.
* Allow custom item fontface.
* Allow item coordinate nudging, and/or customization/persistence.
* Combine help docs for `venndir()` and `render_venndir()`.
* Consider `textvenn()` option for HTML output, via `print_colordf()`

## 05dec2024

* Improve outside label assignment, use nearest available outside point,
after labels are spread by `min_degrees`.

   * Iterate each outside label, find nearest point/distance.
   For labels sharing the same nearest point, assign to closest one. Repeat.
   * Can be added into `label_outside_JamPolygon()` as second step,
   after it calculates the label position outside (or simulates that).
   * Larger rework would be to assign outside labels only when needed,
   then reassign also only when adding or removing outside visibility?
   Might be unnecessary if the rework above is effective.

* Consider some visual default settings?

   * no set border
   * white set border

* Consider adding `outerborder`,`innerborder` optional `...` arguments,
into the Venndir metadata for persistence?
* Fix overlap labels shown outside for fully internalized sets, proportional.

   * An intersection overlap count should not be positioned outside together
   with an outside set label, since it means something different outside.
   * Overlap count label is not properly "grouped" with parent set label,
   though it is co-located. It should probably not be co-located, since
   it has different meaning than other unique set overlap labels.

* DONE. Debug multi-line overlap label being cropped when second line is wider
than the first line. Potential error with `"grobwidth"`.
* Legend:

   * DONE. Add some buffer space for left-aligned counts in "Size" column.
   * Consider option not to colorize the legend.

## 30nov2024

* Consider option to "flip/mirror" the coordinates, for example trying
to reproduce published layout, or optimize visual order of labels.
* Refactor label rendering

   * New functions `assembly_venndir_label()`, `repack_grobs()`.
   * Change to use `grid::frameGrob()` to build label parts.
   * Store font style in metadata, optionally per-overlap in `label_df`.
   Consider: main, count, signed, item
   * Store `template` in metadata, optionally per-overlap in `label_df`
   * Store `show_labels` in metadata, optionally per-overlap in `label_df`
   * Consider using `grid::textGrob()` as default.
   
      * Make `gridtext` and/or `marquee` optional?

* `render_venndir()`

   * call new label functions, drop `draw_gridtext_groups()`
   * drop `group_labels` argument

## 19nov2024

Residual todo items of note:

* Legend

   * DONE. Optional signed counts and percentage to legend.

* Item labels

   * DONE. Debug: 4-way Venn may not be applying `item_cex` properly.
   * Consider adding `item_fontfamily` to be distinct from other labels.
   * DONE. `cex` fontsize adjustment seems not quite right.
   * Revisit `item_buffer`, `width_buffer`, other ideas to position labels
   inside polygons.
   * Consider optional columns for item labels in `label_df`:
   coords, label cex/fontsize, border, fill, fontcolor.
   When `NA` use automated/defaults, otherwise they override.
   * Consider radically different approach to `label_fill_JamPolygon()`
   
      * Brute force placement of labels.
      * Create item label grobs, calculate absolute height/width bounding boxes
      * Divide into N rows, start top-left, move left-to-right, place
      labels inside the polygon, in order, prevent overlapping labels.
      Repeat on each row.
      * If not all labels fit, down-size labels and try again.
      Use adaptive sizing as in `sample_JamPolygon()`.
      * Final result: delete original item grobs, create new grobs with
      final font scaling, using coordinates as determined.
      * Consider allowing positive `buffer` to allow some spillover outside?

* Test `marquee` as drop-in replacement for `gridtext`. Goals:

   * Note it requires R-4.3.0 or higher, might be too restrictive.
   * Improve inconsistent font kerning
   * Enable custom styling of labels.
   * Test large number of item labels compared with grid and gridtext.

* Improve label kerning irregularities. Possible show-stopper for signed labels
and sign item labels. Arrows are rendered wider than internal font metrics.

   * Linux system font kerning is inconsistent, unclear whether it is
   specific to gridtext, or `grid::grid.text()` also.
   * Most labels could be created parts, then assembled.
   * `grid::grid.text()` could be used instead of `gridtext` for most labels.
   Test `grid.text()` as drop-in replacement for `gridtext::richtext_grob()`.
   Note it lacks `r`,`padding`.
   * `marquee` could also be drop-in replacement. If it works best, all
   `gridtext` should be replaced with `marquee`.

* Test PDF output with different font choices. Compare with `ragg_png`.

   * Need automated tests for PDF and PNG output. Not sure how yet.

* `textvenn()`

   * Enable multi-line headers in output
   * Add optional legend with counts

* `README.Rmd`

   * Add `venn_meme()`

* Lower priority ideas

   * Consider other directional metrics, similar to "percent" label
   
      * Jaccard overlap
      * Kruskal concordance: (agree - disagree)/(n)
      * Others: Cohen's Kappa; Holley and Guilford's G; Hubert's Gamma;
      Yule's coefficient of association Q
      * Ingenuity directional z-score (for a single set)

   * DONE. `sample_JamPolygon()`
   
      * DONE. Consider more organic/efficient algorithm: instead of linear
      testing across `n_seq`, do sparse testing, then fine-tune.
      E.g. requested: 100;
      try 100, 150, 200;
      150 failed, 200 worked;
      next try 175; if success try 162, if fail try 188;
      try 188; if success try 194

## 12nov2024

* item labeling

   * Consider storing item label coordinates in the `Venndir` object.
   Then `render_venndir()` must be able to re-use item labels.
   It would allow editing the coordinates for fine-tuning.
   * Consider alternatives to `label_fill_JamPolygon()` to fill labels
   inside a polygon. Ideally, it minimizes overlapping labels,
   and maximizes placement of labels inside each polygon.

      * Consider defining `adj` based upon label relative distance to
      left-right edges of the polygon. Doing so would allow labels to
      fit cleanly within the polygon.
      * Evaluate `wordcloud` type approaches which can fill text words
      into a polygon shape. Github `"Lchiffon/wordcloud2"` also on CRAN.
      Note: `wordcloud2` produces HTML/javascript, it also requires
      black-and-white mask for each shape, then places words inside
      the shape using active javascript. It often needs web page refresh.

   * Consider permitting item border and fill.

* Font and character handling

   * Consider `marquee` package for potentially more robust unicode character
   and font handling.
   
      * It would involve converting existing markdown/HTML
      to CommonMark format, which uses styles by name.
      * Named styles could enable more flexible control over labeling overall.
      E.g. `count` could be a `marquee::style()` with fontfamily, fontsize,
      color, etc.

* `textvenn()`

   * Consider optional text legend, counts per set, with total.
   * Consider optional multi-line headers, with one line per set name.

      * Goal: Prevent generating a very wide table.
      * Requires changes to the `data.frame` structure, very doable.
      * Consider supporting multi-line set names? Split them across rows using
      similar logic.

   * Consider option to display items beneath counts? Probably not practical,
   but could be combined with `max_items=20`, then abbreviate the item
   listing, something similar to: `"item1, item2, item3, ... (523 others)"`.
   Or: `cli::ansi_collapse(letters, style="head", trunc=20, ellipsis="...")`,
   but split across multiple lines.

* `venndir()`, `render_venndir()` - labeling placement options

   * Fix the `grobs_tile()` spacing, remove overlap with percent labels.
   
      * This issue might be solved with `ragg::agg_png()` font handling.
   
   * Find reproducible example of bad font kerning, post issue to `gridtext`.
   Consider `marquee` as alternative.
   * Consider adjustments to inner/outer labels. Examples:

      * Option to show counts/percentage only when the overlap label is shown.
      * Option to show overlap label for outside counts, e.g. `"setA&setB"`.
      Probably need to split across multiple lines.

   * Consider shorter default `segment_distance`.

* `JamPolygon` plotting

   * Need a method for adding components to venndir plot `grid` elements,
   e.g. text labels, points, any `grid` object.
   * Use case could be obtaining `grid` object, detecting the `gTree` elements
   by name, then adjusting item label positions?
   * Consider conversion to polypath-compatible coordinates, and `sf`.

* DONE. `Venndir` polygon manipulations

   * DONE. customize an overlap: fill color, innerborder/outerborder/border

* Add accessors to the `Venndir` object

   * DONE. setlist
   * DONE. `overlaplist()` - overlap item list
   * DONE. `signed_counts()` - return overlap count list
   * DONE. (`overlapdf()` and `venndir_to_df()`),
   alternatively: `as.data.frame()` - using the `setAs()` methodology.
   Convenient conversion to `data.frame` format.

* Summarize advice for fonts, plot resolution, anti-aliasing, etc.

   * `ragg::agg_png()` provides a notable improvement over `png()`,
   in RMarkdown put `dev="ragg_png"` in the kntir options, for example:
   `knitr::opts_chunk$set(dev="ragg_png")`

* Consider other S4 objects?

   * `Setlist` - consistent representation for setlist
   
      * When `signed_overlaps()` is run, it first "detects" input type,
      then converts data accordingly. It could be stored properly.
      * list, signed list, incidence matrix, signed incidence matrix.
      
   * `SignedOverlaps` - Structured info

## 08nov2024

* Based upon some excellent feedback:

   * DONE. Move default labels above, not beside the Venn diagram.
   * DONE. Add option for a plot title.
   * DONE. Related, make it "easy" to adjust outer labels.
   * DONE. Add option for signed counts below main counts in one column.
   * Add option to display signed counts in the legend.
   * Add option to display percentage in the legend.

## 01nov2024

* Item label enhancements

   * Debug `item_cex` and `item_cex_factor` not being applied as expected.

      * Consider assigning `item_cex` to `label_df` or `jps` for persistence.
      * Consider specific logic when `proportional=TRUE` to assume that
      all polygons are already proportional to the number of items.
   
   * Consider option to retain item factor order when provided,
   to control the order they are placed.
   * Consider some method to adjust the relative "height" of item labels,
   for example multi-line labels should take proportionally more height?
   * Allow custom `show_labels` so the set names can be displayed outside.
   * Store `item_cex` and `item_buffer` somehow for re-use, otherwise
   `plot(venndir_out)` does not retain these settings.
   * Improve the default adjustment to `item_buffer` based upon number
   of items, and relative size of the polygon. With fewer items (2 or 1),
   it should more sharply use the center position.
   
   * Consider option to justify item label based upon its left-right
   position in the polygon. Item labels on left edge would be placed so
   the left edge of the label aligns with the left edge of the polygon.
   It could help reduce labels overlapping the outside of a polygon.
   It might make labels more crowded inside the polygon.
   * Consider some option to reduce item label overlaps.
   * Consider some approach to place count label box inside the polygon,
   with item labels around it? Probably not feasible.
   
   * Useful test case, to reproduce a published figure.
   Note that `as_factor()` does not affect item sorting, only the numeric
   prefix seems to work.
   ```
   as_factor <- function(x){
      factor(x, levels=unique(x))
   }
   venndir::venn_meme(x=list(
      CLE=as_factor(c("1 CLE:",
         "2 IFNa>IFNb",
         "3 Anti-malarials effective\ntherapy for skin disease")),
      DM=as_factor(c("1 DM:",
         "2 IFNb>IFNa",
         "\n3 Triggered by immune\nstimulating herbal\nsupplements",
         "\n\n\n4 Anti-malarials effective\nin 25%, commonly causes\nmorbilliform rash",
         "\n\n\n\n5 Cannabinoid receptor\nagonist proising\ntreatment")),
      `CLE&DM`=as_factor(c(
         "1 Photosensitivity",
         "2 Triggered by viral and\nbacterial infections",
         "3 Increased\ntype I IFN"))),
      item_cex=c(1.5, 1.5, 1.2),
      xyratio=10,
      dither_cex=0)
   ```

## 13sep2024

* Adjust aesthetics based upon feedback.

   * The arrows could be larger relative to the signed counts.
   * Consider removing the colon `":"` between arrows and signed counts.
   * When adjusting arrow/signed count font sizes, the text is bottom-aligned,
   should probably be middle (top-middle-bottom) aligned.
   
      * Using `vjust=0.5` caused signed labels not to be grouped properly.

## 04sep2024

* Fix bug when `names(setlist)` contains parentheses, causing the set
name to appear inside the polygon. Removing the parentheses fixes
the problem, but unclear why this happens? Probably errant `grep()`
call somewhere.

## 28aug2024

* Need some way to move outside labels to the top (or bottom),
the default makes labels wide, which is difficult to use in figures.
* Need some way to nudge count labels, to adjust centering polygons.

* Consider using `cli` for messaging, and for unicode symbol support?
* Hide single-set signed labels with concordance and agreement
* Add generic convenience functions

   * `im()`, `im_value()` for `Venndir` objects: return incidence matrix,
   or signed-incidence matrix.
   * `setlist()` - accessor for `setlist`
   * `label_df()` - accessor for `label_df`
   * `overlaps()` or `get_overlaps()` - some accessors for set overlaps

* `nudge_JamPolygon()` - extend with `scale`, `rotation`.
* Consider `item_style` with similar presets as `label_style`.

   * Use case is to allow shading/border for item labels,
   `venndir()` and `venn_meme()`.

* Consider adding validation for `Venndir`

   * Probably only necessary when editing `jps` and `label_df`, which may be rare
   * Check that `label_df` and `jps` are compatible

* Consider `signed_overlaps()` S4 object?

   * Currently returns `data.frame` - usable but due to the fixed formatting.
   Could be useful to "lock it down" to add validation checks,
   and to provide convenience functions.
   * Probably not worth the effort yet.

## 22aug2024

* Enhance `venndir_to_df()`

   * Add two additional formats:
   
      1. Venn item format, one column for each overlap, with list of items
      in each column. This is "user-convenient" and easy way to find a list
      of items without having to filter the table.
      2. Hit format, with item rows, and one column for each `setlist` entry,
      with values `-1` or `1` indicating presence of that item.
      This format is programmatically easier to use, but requires some
      filtering of the table to find particular overlaps.

   * Consider changing `return_type="data.frame"`, with `"kable"` optional.

## 20aug2024

* Convert from `vwline` to `gridGeometry`, per pmur002 (author of both)

   * Initial testing confirms it should work.
   * Polygon offset is explained in detail here:
   https://www.stat.auckland.ac.nz/~paul/Reports/Geometry/offset/offset.html
   * gridGeometry explained here:
   https://www.stat.auckland.ac.nz/~paul/Reports/Geometry/gridgeometry/gridgeometry.html
   * Useful functions: `polyoffsetGrob()` - use `rule="evenodd"`

* Consider formally recognizing three types of border:

   * `outerborder` - only appears outside the polygon boundary
   * `innerborder` - only appears inside the polygon boundary
   * `border` - appears exactly on the polygon boundary, default `NA`
   * each use suffix parameters: `.lwd`, `.lty`
   * requires changing `venndir()` to use `outerborder` and not `border`.

* Consider customizable `digits` for percent overlap label.
* `textvenn()` quality of life

   * Accept optional input data in `...` for convenience
   * Add option to display percent overlap
   * Consider using something like `show_labels`

* Consider adding example test cases:

   * Venn diagram labeled by gene symbol.
   Cho et al 2019, https://doi.org/10.1073/pnas.1919528117
   Figure 5A: Venn diagram showing +/-rapamycin ER-mito proteome.
   Genes are also colorized and highlighted, it might be too much
   as a visual, but is possible to replicate.
   It shows item labels outside the Venn diagram which may
   be useful `venndir` feature in future.


## 01aug2024

* Consider adding argument `title` or `main` to add a title to the figure.

   * Define title as a `grob` that can be manipulated if needed.
   * The `grob` follows the figure, so multi-panel figures retain
   the title relative to each figure.
   * Consider options for placement: top, top-left, bottom, etc.;
   and text alignment options: `just`, `hjust`, `vjust`.
   * It may need its own simple wrapper function `venndir_title()` to
   hold these options, with useful defaults. It simply returns a `grob`.


## 29jul2024

* Consider `clisymbols` to handle logic of Unicode arrow symbols.
* Consider option not to sort item labels, which would allow user
control over the order when displayed in the Venn diagram.
* Document some "fancy" examples showing item labels:

   * One-column item layout.
   * Control over font sizes/colors via `gridtext` syntax.
   * Try examples using short text phrases as "items" in each overlap region.
   * Use of image instead of text label. It can be a mix of images and labels,
   using `gridtext` features.

## 24jul2024

* Customize item fill logic

   * Generally spread labels more to the center.
   * Note: `xyratio` argument to `label_fill_JamPolygon()` is passed, but
   high values `xyratio=10` cause labels to be placed too far at the top.
   * Consider new rules:
   
      * When more positions are produced than required, evenly distribute
      labels across that array of positions.

* Consider improving how percent overlap is handled

   * Currently appends to the `"text"` column.
   * Undesired outcome: percent overlap uses the same font size as the count.
   It could use slightly smaller font, or same font size as signed counts.
   * It does not permit custom styling, fontsize, fontface, and only currently
   permits percent overlap.
   * Examples for optional style:
      `"(24%)"`
      `"(24%) J:0.24"`
   * It could permit optional metrics: Jaccard overlap, concordance coefficient
   * Consider adding column to include the label.
   
      * Jaccard.overlap=0.7
      * Concordance.overlap=0.45
      * {Name}.overlap=#

## 18jul2024

* Add documentation. Here is a laundry list of tips:

   * Show examples with DESeq2 results; edgeR; limma.
   * Show how to import a list from a file.
   * Describe that the output is `grid::gTree` which can be drawn separately.
   * Show how to draw two Venn diagrams in one figure using `patchwork`,
   for example, do this twice, then add the results:
   `patchwork::wrap_elements(grid::grid.draw(attr(vo, "gtree")))`
   * Show how to change the setlist labels (as displayed in the figure),
   and legend labels (as displayed in the legend).
   * Show how to adjust the figure size to prevent overlap with legend.
   * Show how to adjust font sizes, legend font size, legend row padding.
   * Show how to supply custom colors.
   * Show how colors can be defined for 8 sets, then display only a subset,
   so the colors are consistent for each set displayed.
   * Show how to convert input from hit matrix to setlist.
   * Show how to convert input from overlap counts to setlist.
   * Show how to convert input from signed overlap counts to setlist,
   more complicated, can be useful when reproducing a figure.
   * Show how to nudge proportional circles during `venndir()` call.
   * Show how to display all labels inside, outside, or signed/count labels
   inside/outside - with and without item labels.
   * Show how to include percent overlap labels.
   * Show how to move an outside label manually.
   * Show how to supply a custom set of `JamPolygon` shapes instead of
   using pre-defined fixed shapes, or shapes from `eulerr::eulerr()`.
   * Show how to set `shape="ellipse"` with proportional diagrams.
   * Show effect of `set.seed(123)` for reproducibility.
   * Show how to customize `curate_venn_labels()` to use customized "signs".
   * Show how to extract items for each overlap.
   
      * Placeholder:
      ```R
      itemlist_by_overlap <- split(vns@label_df$items,
         factor(vns@label_df$overlap_set,
            levels=unique(vns@label_df$overlap_set)))
      ```

   * Show how to make an incidence matrix; or signed incidence matrix.

* Add R package integration functions

   * Probably most useful to integrate packages that produce setlist data,
   or to Upset plots for people who also prefer that functionality.
   Probably less useful to integrate with other Venn diagram packages.
   * `stats_to_hitlist()` - consider generic function to convert stats table
   to signed hits - generic function could be re-used for package-specific:
   
      * `DESeq_to_hitlist()` - consider converting DESeq2 results to hits
      * `topTable_to_hitlist()` - convert `limma::topTable()`
      * `edgeR_to_hitlist()`
   
   * `ChIPpeakAnno()` integration with `makeVennDiagram()`
   
      * It optionally includes labels with enrichment ratios
   
   * `to_VennDetail()`
   * `to_ggVennDiagram()`
   * `to_VennDiagram()`
   * `to_UpsetR()` - send data to create an Upset plot
   * `to_venn()` - send data to `venn` package
   * `to_RVenn()` - send data to `RVenn` package

* Add remaining small features

   * `venndir()` argument `rotate_degrees` is not yet functional.
   * `venndir()` add argument to control line width
   * Consider `launch_venndir_shiny()` with R-Shiny front-end.

* Consider JamPolygons using `outerborder` instead of `border`

   * Methods currently assume all borders are `outerborder`, the other option
   is `innerborder`. This was a design decision, might need to change.
   If there is need to display just a `border` (not inner, not outer) on
   the line itself, there is not a mechanism to do so currently.
   * Need changes to `find_venn_overlaps_JamPolygon()` and `venndir()`
   to use `outerborder` instead of `border`.
   * Need changes to `plot.JamPolygon()` to use `border` instead of thin border,
   and `outerborder` instead of `border`.

* Investigate R crash - probably specific to Quartz on MacOS Sonoma 14.4, 14.5

   * Reproducible workflow in RStudio:
   ```R
   library(venndir)
   vo <- venndir(make_venn_test(100, 3, do_signed=TRUE), show_labels="ncs")
   # open new device
   dev.new()
   # this time crashes
   vo <- venndir(make_venn_test(100, 3, do_signed=TRUE), show_labels="ncs")
   ```
   * The crash occurs inconsistently from separate R console, same workflow.
   The console prints this crash message:
   `"Fatal error: Duplicate keys of type 'DisplayList' were found in a Dictionary."`
   * Topic online suggests the error is linked with MacOS Sonoma 14.4, 14.5.
   https://stackoverflow.com/questions/78615561/fatal-error-duplicate-keys-of-type-displaylist-were-found-in-a-dictionary
   * The crash occurs with venndir-0.0.34.900, and venndir-0.0.36.900,
   suggesting the risk was not added by updates in 0.0.36.900.
   Therefore version 0.0.36.900 will be released once it passes other tests.

* `venndir_legender()`

   * Currently it is drawn using the plot viewport. But when drawn after
   the viewport is closed (`grid::popViewport()`) the legend appears at
   the edge of the figure region, which can be outside the venndir plot.
   Consider option for the legend to use the figure viewport and not the
   venndir plot viewport.

## 15jul2024

* Prepare for Bioconductor (or CRAN)

   * DONE. Remove any calls to internal package functions, using `:::`
   
      * `gridtext_richtext_grob()` is a custom function, must be removed.
      It calls internal functions in `gridtext` that can't be ported easily,
      for example `.Call()`.
      The custom function was solely intended to allow vectorized `padding`,
      currently the bottom,left,top,right is applied to all labels,
      and cannot be custom for each label.
      The driving goal was to have smaller margins between signed count labels,
      since they usually have smaller font size. Otherwise the distance
      between signed labels, line-by-line, was too far.
      First option, just call `gridtext::richtext_grob()` and remove the
      ability to use different padding.
      Second option, wrap the call inside `mapply()` which is what is done
      inside gridtext for multiple grobs anyway. It won't be any slower.
      Final option is to ask Dr. Claus Wilke to allow a list of vectors
      as input for argument `padding`.

* `plot.JamPolygon()`

   * DONE. Consider option to return grid grobs instead of drawing them to the
   graphical device.

* `render_venndir()`, and `venndir()`

   * DONE. Consider option to return the grid grobs to be rendered elsewhere.
   Return the `viewport` as defined by `plot.JamPolygon()`.
   * DONE. Consider something like post plot hook, to call a function after the
   plot is rendered, before calling `grid::pop.viewport()`.
   (Note: "DONE" means this functionality is possible, but because the `grid`
   grobs make it possible for the user to do whatever they want.)

## 09jul2024

* DONE. Fix bug where `sets` is properly subsetting `setlist` but is not
properly subsetting `set_colors`. Verify other components are
also properly subset as needed.

## 08jul2024

* Currently `venndir()` does not store `item_cex` and `item_buffer`
in the `Venndir` object, so calling `render_venndir()` does not
retain these values.
* Consider functions to adjust components of `Venndir` figure, to be
visualized by `render_venndir()`.

   * Nudge outer label position by set name, or `overlap_set`
   
      * When provided a value that matches `"label_df$ref_polygon"`, it should
      affect all rows with that value.
      * When provided a value that does not match `"label_df$ref_polygon"`,
      it should look for corresponding value in `"label_df$name"` or
      `"label_df$venn_name"`, then convert to `"label_df$ref_polygon"`.
      Then proceed to update all rows with that `ref_polygon` value.

   * Nudge inner label position, same syntax as above.
   * Nudge a set shape (This workflow is lower priority for now.)
   Best practice is to re-run `venndir()` using argument `circle_nudge`.
   
      * Second-best is a wrapper function that extracts data from `Venndir`
      and just calls `venndir()` with corresponding arguments. It should
      capture: `JamPolygons` for each set; `set_colors`; `setlist_labels`,
      `legend_labels`, `sets`, etc.

* Consider option to style item labels. Probably most useful for `venn_meme()`.
For example, allow shading, rounded rectangle border, as with set labels.

## 25jun2024

* Fix bugs with proportional diagrams:

   * One empty set in `setlist` causes an error.
   * One set fully encompassed inside another set causes the set label
   to be hidden.
   * The two bugs are related:
   
      * The "set label" is assigned to appear where there is a unique
      "overlap_set" - for example "set_A" should appear where there
      is "set_A" unique overlaps that do not overlap another set.
      * This condition is always valid for Venn diagrams.
      * For proportional diagrams, sometimes "set_A" is fully inside "set_B".
      We could call these "orphan set labels".
      * Previously the label "set_A" was associated with
      `overlap_set="set_A&set_B"`.
      * This set label also used its own outside label position, the goal
      was to allow potentially showing the set label "set_A", distinct
      from the overlap label "set_A&set_B", both pointing to "set_A&set_B".
      * Visually it could be confusing, when count labels were displayed
      outside - there were two labels pointing to the same polygon region.
      * The logic to assign an "orphan set label" to the best available
      overlap set also occurred in multiple functions, nothing was stored
      in the `Venndir` object to review this assignment.
      * Finally, the collection of labels which should appear together
      was complicated because the label position `hjust_inside`,
      `hjust_outside`, `vjust_inside`, `vjust_outside` relied upon knowing
      which labels were being placed together, and this logic failed.

   * The fix (fixes):
   
      * Associate set label to `"ref_polygon"` and store in `Venndir@jps`.
      * Update `render_venndir()` to recognize this association, instead
      of re-determining the association.
      * Update `venndir_label_style()` to recognize this association
      so it can properly assign `hjust` and `vjust` to affected labels.
      This function is one feasible place to make the changes.
      * Another alternative `draw_gridtext_groups()` which assembles the
      grouped labels into a "group" in order to draw one border around them.
      This step may be ideal to place labels together in more flexible ways.
      
         * Labels are laid out within active `grid` viewport context,
         then the observed dimensions in `grid` space are used to define
         the extent of the resulting `grid::roundrectGrob()`.
         * This step optionally re-centers the group of labels when
         `adjust_center=TRUE`, so the center of the group is at the label
         point, instead of the focal point of labels (normally the set name
         is "topleft", the overlap count is "bottomleft", and signed counts
         are "right").
         * This function could provide a convenient way to assemble labels
         without having to rely upon `hjust` and `vjust`.
         * This function could also enable different layouts, like set label
         at the top, counts at the bottom; set label at the top, then counts
         bottomleft and signed counts bottomright.


* Fix `venndir()` argument `rotate_degrees` which is currently not functioning.
* Consider minor adjustments to `venndir_legender()` labeling.

   * Option to hide the trailing colon `":"`
   * Option to left-justify set labels

* Consider returning the `viewport` from `render_venndir()`, and `venndir()`
when it calls `render_venndir()` due to `do_plot=TRUE`.

   * consider adding as an attribute of the `Venndir` object
   * the `grid` `viewport` is useful when adding to or adjusting an existing
   figure, otherwise the coordinate system is not usable.

* Write more intensive validation for `Venndir` objects.
* Consider option for black-and-white legend.
* Write vignette with larger variety of examples for `venndir()`
* Expand help docs for `JamPolygon`

* Write brief vignette for `JamPolygon` objects
* Tests for `JamPolygon` object functions

   * generate test shapes: circle, circle with hole (donut),
   two disjoin squares, square with hole and nested square inside the hole.
   * `point_in_JamPolygon()`, `has_point_in_JamPolygon()`
   points in the polygon, in the hole, in the nested internal solid area.
   * `labelr_JamPolygon()` to generate label position inside solid polygon,
   also test with `has_point_in_JamPolygon()`
   * `nearest_point_JamPolygon()`, `farthest_point_JamPolygon()` to confirm
   proper nearest/farthest points.
   * `intersect_JamPolygon()`, `union_JamPolygon()`, `minus_JamPolygon()`
   * `buffer_JamPolygon()` - confirm polygon with hole can become larger
   polygon without a hole. Probably check `area_JamPolygon()` before/after
   several use cases.

* Consider moving `JamPolygon` to a separate R package.

   * Painful but might make `JamPolygon` a lightweight standalone option,
   usable in other Jam packages (without guilt).
   * Dependencies: polyclip, polylabelr, vwline

* Consider adding `JamPoints`, `JamLines` objects? Ugh. What have I done?

   * There is currently no way to "Add a point" to a `JamPolygon` figure,
   since the figure is created in `grid` using a `viewport`.


## 22jun2024

* DONE. Consider changing default to `do_sparse=FALSE` so the default behavior
is to return a typical `matrix` class and not `Matrix` compressed form.
* DONE. Consider removing `matrixStats` from dependencies, it is only used once.
* DONE. Add examples to `rescale_coordinates()`.
* DONE. Fix `alpha_by_counts=TRUE.`
* DONE. Consider optional labels for `Venndir`

   * `setlist_labels` - optional user-customized labels for the Venn diagram,
   which may or may not match the name of the set.
   * `legend_labels` - optional user-customized labels for the Venn legend,
   again, these may be abbreviated or adjusted to be suitable for the legend.


## 18jun2024

* Outer label positions and line segments should be improved.

   * Sometimes the outer label is across the diagram from the count label,
   the line segment reaches to the far side. Test with nested proportional
   set Venn.
   * should be influenced by the position of the relevant count label.

* Consider whether to include percent in the Venndir legend.
* Consider an option to include percent in `textvenn()` output. It would
appear on the line below the overlap count.
* Consider allowing vectorized `show_labels` to be applied to each overlap
in order.

## 12jun2024

* DONE. `render_venndir()` - broken when provided with `venndir()` output separately.
* DONE. `venn_meme()` - some visual adjustments are needed

   * DONE. Fix regression in `gridtext`, item labels do not recognize `<br>`.
   * Center item labels in the polygon for few number of items (especially one).
   Current state: item label is placed near edges and not in the center.
   Current state: `item_buffer` is the best way to adjust manually.
   DONE for `venn_meme()` not for general use.

* DONE. Label segment for nested, nested sets, uses the full set as fallback.
Ideally, it would use the region with the fewest overlaps with other areas.

## 11jun2024

* DONE. `venndir()` error

   * Error is caused when two sets do not overlap, and `proportional=TRUE`. See
   `find_venn_overlaps_JamPolygon()` in venndir-base-polyclip.R#428
   `intersect_JamPolygon()` in venndir-polyclip.R#245.

## 02may2024

* `venndir_legender()`

   * Use "Set" and "Size" capitalized.
   * Left-align text in "Set" column; right-align numbers in "Size".
   * Consider option not to colorize the table rows.
   * Consider adding a column "Color" with filled cell. Bonus points
   for having the color be a perfect square (not rectangular).
   * Apply contrasting color, when rows are filled with `set_colors`,
   currently text is always black.

## 23apr2024

* `plot.JamPolygon()`, `plot()` (JamPolygon)

   * Allow and document how to create a multi-panel plot.
   For example, how can it be used with `patchwork` to assemble a
   Venn diagram beside a heatmap, or beside output from `plot_sedesign()`?

## 22apr2024

* `render_venndir()` throws font-related warnings when the output PNG or PDF
font library is not found, and sometimes substitutes Unicode characters
with `"."`. It seems to happen uniquely to PDF output, might be platform-
and device-dependent. Some suggested workarounds include:

   * Use `extrafonts` package to import the desired fonts. Not tested.
   * Use specific `fontfamily` which is recognized for the requested output,
   however in RMarkdown when creating PNG and PDF files for the same chunk,
   it appears that the PNG and PDF recognized fonts are not consistent.


## 22mar2024

* Remove all "sp",`sf`,"rgeos" functions
* Remove all portions of remaining functions that call "sp",`sf`,"rgeos"

   * `polygon_label_segment()` - still uses a hybrid approach,
   accepting sp and jp objects.

* Verify the new `venndir()` returns equivalent data as `venndir_OLD()`
even if it is returned in different manner.

   * Clean up output data format

* Verify `render_venndir_polyclip()` accepts equivalent input data.
* Consider making `venndir()` output equivalent to previous versions,
so that methods that expect specific output might still work?
* Consider S4 `Venndir` object, with slot names:

   * slots:

      * `jp` - set polygons, name, label, color
      * `jps` - set/overlap polygons, name, label, type, overlap
      * `label_df` - overlap count, signed count, count label, items (optional)
      * `setlist` - the input `setlist` for easy reference
   
   * methods:
   
      * `validate()` - check internal consistency, set names, overlaps, label xy
      
         * names(setlist) matches names(jp), are found in names(jps)
         * jps overlap names -> set names -> found in names(setlist), names(jp)
         * label_df overlap names all present in jps overlap names
         * label_df specific overlap count sum equals jps overlap count

      * `setlist()` - accessor for `setlist` slot
      * `im()` - calls `list2im_opt()`
      * `im_signed()` (or `im_value()`) - calls `list2im_value()`
      * `plot()` - calls `render_venndir()`

* label positioning shorthand: NOCPSI

   * Set _N_ame, _O_verlap Name, _C_ount, _P_ercentage, _S_igned, _I_tems
   * uppercase is "outside", lowercase is "inside" the relevant polygon
   * Default: Ncps: set _N_ame outside; _c_ount/_s_igned inside, others hidden
   * To show items: NCSi: _N_ame/_C_ount/_S_igned outside; items inside

* consider table item display inside/outside

   * `gridExtra::tableGrob()` with 1 to 3 columns, which could be positioned
   beneath other name/count labels. Seen in published Venn diagram figure.
   Probably only works well for a small number of items.

## 13mar2024

* Optionally display overall percentage below main count label.
* Consider `JamPolygon` which is a `data.frame`:

   * `data.frame` details:
   
      * Each row is strictly one polygon, multipart polygons are still one row.
      * `x`: `list` column with one or more `numeric` vectors.
      * `y`: `list` column with one or more `numeric` vectors.
      * `name` - unique entry, essentially `rownames()`
      * `label` -  by default uses `name` but can be anything including `NA`,
      also it can contain the same label as another polygon.
      
         * Unclear whether it can store `gridtext::richtext_grob()`?
      
      * `labels` - optional, label each multipart polygon - not implemented yet
      * `fill` - internal polygon fill color
      * `border`, `border.lwd`, `border.lty` - outer border
      
         * Consider R package `vwline` for variable width lines, part of
         which enables rendering the outer/inner portion of a line.
      
      * `innerborder`, `innerborder.lwd`, `innerborder.lty` - inner border
      * `family`, `fontsize` - `grid::gpar()` for the label.
      * `label_x`, `label_y` - to position the label inside the polygon
      * `outerlabel_x`, `outerlabel_y` - to position the label outside the polygon?

   * `plot.JamPolygon()`
   
      * Wrapper to render `JamPolygon` likely using `grid` methods.
   
   * `print.JamPolygon()`, `summary.JamPolygon()`
   
      * summary function to print contents, probably using `print.data.frame`


## 12feb2024

* Idea: Consider reporting "overlaps not shown" using `gridExtra::grid.table()`
* Related: Could `gridExtra::grid.table()` be used for Venn labels?

   * It would need to be rendered with `textGrob()` then replaced with
   `gridtext::richtext_grob()`

* Marry `gridtext` and `ggrepel` for non-overlapping labels.

   * Follow: https://github.com/wilkelab/gridtext/issues/33
   There is a working pull request in `gridtext`, pending changes in `ggrepel`.
   Just awaiting each package author approval.

* `venndir_legendir()`

   * Consider option to display "Total" unique items as the bottom row.

* Fix issue with Venn numeric label coloring when background color is dark.

   * The `venndir_legendir()` label shows light text correctly, the signed
   labels appear to be adjusted correctly, but the count label appears
   not to be adjusted correctly. Some cells are correct, others are not,
   suggesting this is a mismatched ordering issue.
   Observed with 4-way Venn, it appears only the overlap regions adjust
   text, not the unique-set regions.


## 27nov2023

* DONE. Urgent: Transition from "sp" and "rgeos" to `polyclip` since "rgeos"
has been removed from CRAN. Ugh.

   * Current status:
   
      * Most individual functions are ported with equivalent functions
      provided by `polyclip::polyclip()`.
      * There are still edge cases with polygons having holes, nested polygons
      (multi-part polygons) which are not handled consistently.

## 04oct2023

* debug issue when LOCALE is "C" the labels are not shown, and
error is generating saying `gridtext` does not recognize characters,
referencing some simple unicode characters (up/down arrows, etc.)

   * workaround is to change LOCALE to `"en_US.UTF-8"` with:
   `Sys.setlocale("LC_ALL", "en_US.UTF-8")` but unclear why it's a problem?
   This workaround does not seem portable for other actual locales.

* Optionally display overall percentage below main count label

## 21sep2023

* `venndir_legender()`

   * For signed data, include direction labels, colorized properly.
   `"size"` `"direction"`
   `"579"`  `"(↑: 123, ↓: 456)"`
   * It should call `curate_venn_labels()` which means it needs the arguments
   `curate_df`,`unicode` to allow customization.

* DONE. Migrate from "sp" to `polyclip`

   * "sp" is being retired October 2023. Decided not to use `sf` due to
   heavy dependencies focused on geographical map software libraries
   which are not relevant here.
   * `polyclip` provides the basic manipulations required:
   intersect, union, subtract, rotate, buffer.
   * Does `venndir` need to accept past object formats with "sp" objects?
   
      * Migration function to convert old "sp" to new `polyclip` object.
   
   * Replace `venn_spdf` with `data.frame` equivalent format.
   * Functions to port:
   
      * `find_vennpoly_overlaps()` - input `SpatialPolygons`,
      output `SpatialDataFrame`
      * `eulerr2polys()` - output `SpatialPolygons`
      * `intersect_polygons()` - input/output `SpatialPolygons`
      * `union_polygons()` - input/output `SpatialPolygons`
      * `get_largest_polygon()` - input/output `SpatialPolygons`
      * `sp_circles()`,`sp_ellipses()` - output `SpatialPolygons`
      * `nudge_sp()` - input/output `SpatialPolygons`
      * `rescale_sp()`,`rescale_ps()`,`rescale_p()`,`rescale_coordinates()` - ?
      * `polygon_label_segment()`
      * `sp_polylabelr()`
      * `sp_percent_area()`
      * `get_sp_buffer()`

## 07jul2023

* `render_venndir()`

   * label positioning
   
      * Count label placement should center the main counts at the target
      location, with signed counts offset to the right side.
      * When main label and count label are shown together, the x-center
      position should be the center of the combined set label, main count label.
      * The main goal is to have the main count positioned as well inside
      and centered within the Venn polygon as possible. The signed counts
      should be considered embellishments to the main count label, so
      they can be slightly outside the polygon as long as they are anchored
      to the main counts.
      * Currently, the whole label is centered in the polygon, which sometimes
      makes the main count label appear on the border between two polygons.
      * When set label and counts are shown together, consider placing the
      main label centered at the top, count label on the left, signed
      labels on the right, such that the signed labels are positioned at
      the middle relative to the count label on the left.
   
   * Consider option to display percentage beside counts for each section,
   as seen in several other Venn diagram tools. It's probably too much
   to include percentages with the signed counts.
   
   * Consider using `grid` coordinates for more rendering,
   specifically so labels might be positioned relative to other objects,
   seen when resizing a graphical window.
   Probably not a use case worth "solving" now that I write it out.
   * Line segments should more reliably point to "just inside" the
   polygon shape, where the overlap is more or less a fixed amount,
   and slightly more than the current defaults. Currently the line mostly
   points to the border, it's hard to see that the line ends just inside.

* Consider new function `reposition_venn_gridtext_labels()`

   * based upon `draw_gridtext_groups()`
   * Purpose would be to adjust label positions after grobs are defined,
   so the actual height/width is defined, but before rendering.
   * Currently there is a bug, when `group_labels=TRUE` for labels outside,
   the whole group of labels is adjusted to right/left side of the connecting
   line segment. When `group_labels=FALSE` this step does not occur.
   * This function should allow re-positioning labels within a label group,
   such as having the title at top-center, with count labels in one or
   two columns beneath it, properly aligned among themselves below the title.
   * This function could also therefore enable themes, with different types
   of layouts. Some themes could represent the style of popular Venn diagram
   packages, to facilitate using alongside those packages.


## 09may2023

* `venndir()` and `render_venndir()`

   * DONE: option to hide border around polygons/circles; or adjust line width

* Debug rare issue with incorrect text spacing within labels in Venn diagrams?

   * appears to be some rare `grid` state where signed labels have too much
   visible spacing between arrows and numbers, for example `"^^: 12"`
   appears like `"^^:          12"` for most labels, but not all.
   * a fresh R session seems to resolve the problem. Using the same
   R object in another new R session, the venndir output is rendered normally.
   * `dev.new()` does not resolve the problem in the new plot window.
   * the effect is exagerrated with `unicode=TRUE`, and diminished but still
   present with `unicode=FALSE`. Whitespace is still much wider than normal.

* Debug issue when `group_labels=FALSE`.

   * The main set labels are no longer positioned to left or right of the
   incoming segment, they are positioned over center of the segment endpoint.
   This code might be errantly inside a conditional block.

* consider "normalizing" proportional polygon coordinates so the `x,y` ranges
are roughly consistent with those used by fixed 3-way and 4-way Venns.
Potential benefit with other functions that may use absolute or relative
sizes for things like `segment_buffer` and `item_buffer`.
* consider `venndir_output` object class?

   * could be useful for re-use and editing
   * suggested slotNames
   
      * `venn_spdf`: soon to be using `sf` in form of `data.frame`
      
         * contains polygons for each main set (`spdf` will change to `sf`)
         * polygons for each individual Venn overlap
         * colors and alpha transparency assigned to main sets
         * `x_label,y_label` coordinates of label inside each polygon center,
         however not used? the `label_df[,c("x","y")]` are slightly different.
      
      * `label_df`: `data.frame`
      
         * contains label information to be rendered: `text`, `venn_counts`,
         `overlap_set`, `type`
         * `x,y` coordinates of polygon center
         * `x_offset,y_offset` to position label outside relative to `x,y`
         * `segment_buffer` used for segment depth into polygon
         * label positioning inside/outside; vjust/hjust/halign; padding
         * optionally contains items in each overlap
         * optional: `show_items`, `items`, `item_cex`, `item_degrees`
         
      * `rv_label_df`: (optional?) adjusted `data.frame` from `render_venndir()`
      * `setlist`: new `list` representing the input data, for convenience

* consider text output from `venndir_legender()` for use by `textvenn()`
* need convenient method to adjust set labels that are too wide

   * current option is to edit `names(setlist)`
   
      * not convenient when using `venndir()` as a one-liner
      * in theory only the display should be affected, not actual names
      * the returned object names would also be affected
   
   * option to "alias" the sets
   
      * use something like A, B, C
      * DONE: show both the alias and full name in `venndir_legender()` legend

   * option to adjust labels on the fly
   
      * custom adjustment function, rather than editing upfront in `setlist`
      * could provide a few common adjustment functions
      
         * word wrap: when label is too wide, it imposes word wrapping of
         whitespace
         * shortener: when label is too wide, it crops and makes labels unique
         * aliaser: assigns single values from `LETTERS` or
         `jamba::colNum2excelName()`

## 02may2023

* Overall: replace use of "sp" (SpatialPolygons) with `sf` (Simple Features)

   * requires changing `venn_spdf` into the `sf` equivalent, which could
   be nearly drop-in replacement. The `sf` default object is based upon
   `data.frame` already.
   * requires converting sp-specific functions: `nudge_sp()`, `rescale_ps()`,
   `rescale_sp()`, `rescale_p()`, `rescale_coordinates()`, `get_sp_buffer()`.
   Some functions like `get_sp_buffer()` may be built-in with `sf`.
   * remove dependencies "sp" and "rgeos".
   * replace "rgeos" functions with with built-in `sf` equivalents:
   
      * "rgeos" `gArea()` -> `sf::st_area()`
      * "rgeos" `gDifference()` -> `sf::st_difference()`
      * "rgeos" `gBuffer()` -> `sf::st_buffer()`
      * "rgeos" `gIntersects()` -> `sf::st_intersects()`
      * "rgeos" `gSimplify()` -> `sf::st_simplify()`
      * "rgeos" `gUnaryUnion()` -> `sf::st_union(x)`
      * "rgeos" `gContains()` -> `sf::st_contains()`
      * "rgeos" `gUnion()` -> `sf::st_union(x, y)`
      

* `venndir()`

   * accept incidence matrix input, by calling `im_value2list()`
   * properly convert newline `"\n"` characters to `<br>` internally,
   without having to encode this code at input.
   * properly detect when input data is not signed, to change
   `overlap_type="overlap"` and suppress the display of directional arrows.
   * consider calling `venndir_legend()`, perhaps as an option.

* `venn_meme()`

   * Examples for `venn_meme(wtah, proportional=TRUE)` causes an error
   in `polygon_label_fill()` potentiallly trying to fill labels into
   a polygon overlap that does not exist in the object. It previously
   skipped those sections, however an error is thrown.
   * Consider renaming as `venndir_meme()`

* For fun, consider renaming functions similar to `render_venndir()`.
Some ideas:

   * DONE: `venndir_legend()` -> `venndir_legender()`?
   * `find_vennpoly_overlaps()` -> `venndir_descender()`, it descends
   the polygon overlap combinations.
   * `get_venn_shapes()` -> `venndir_attender()`
   * `curate_venn_labels()` -> `venndir_amender()`
   * `make_venn_test()` -> `venndir_extender()` (not sure this one is valid)
   * `nudge_venndir_label()` -> `venndir_bender()` - general purpose function
   to adjust, rotate, re-position the polygons and labels.
   Alternate ideas: `venndir_mender()`, `venndir_blender()`
   * `make_venn_combn_df()` -> `venndir_blender()` since it combines the
   Venn sets into expected combinations of overlaps;
   or `venndir_sender()` since the function takes set names, "sends" them out
   to create a set of Venn overlap combinations.
   * `venndir_sender()` new function to return setlist for `venndir()` output

## 01may2023

* check build issues on R-4.2.3, dependency on `data.table`

* DONE: Add venndir legend, a simple list with the number of elements in each set.

   * NOT YET: automate the legend for `venndir()` and `render_venndir()`
   * NOT YET: optional breakdown by sign?
   * DONE: consider something like `grid.table()`, `grid::tableGrob()`; if that
   implementation is too slow, see custom `grid.ftable()` described here:
   https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html

* Add optional percentage to the Venn overlap counts.

   * calculation appears to use the overall percentage items.
   * The default layout would place percentage velow the main counts:

+--------------------+----------------------------+
|       set_name, overlap_name, or blank          |
+===================:+:===========================+
|    overlap_count   |  ^: up_counts              |
+--------------------+                            |
| [overlap_percent%] |  v: down_counts            |
+--------------------+----------------------------+

* Consider organizing labels using template styling, with commony used presets

   * See above for one example.

* When showing items, and there are more than `max_items`, use the appropriate
label instead of only showing the overlap counts. Honor the argument
`overlap_type` as if items were not enabled for this region.

* consider using equivalent `data.table` implementation for `list2im_value()`,
but only if this step appears to be too slow for routine use.
* Migrate previous items that are still active from `venndir_todo.md`.
