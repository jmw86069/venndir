# TODO for venndir

## 12jun2024

* `venn_meme()` - some visual adjustments are needed

   * Fix regression in `gridtext` for item labels, not recognizing `<br>`
   * When one item label is present, use proper center position instead
   of patterned fill. Or change the default fill pattern being used.
   Current state: item label is placed near edges and not in the center.

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
