# TODO for venndir

## 21sep2023

* `venndir_legender()`

   * For signed data, include direction labels, colorized properly.
   `"size"` `"direction"`
   `"579"`  `"(↑: 123, ↓: 456)"`
   * It should call `curate_venn_labels()` which means it needs the arguments
   `curate_df`,`unicode` to allow customization.

* Migrate from `sp` to `polyclip`

   * `sp` is being retired October 2023. Decided not to use `sf` due to
   heavy dependencies focused on geographical map software libraries
   which are not relevant here.
   * `polyclip` provides the basic manipulations required:
   intersect, union, subtract, rotate, buffer.
   * Does `venndir` need to accept past object formats with `sp` objects?
   
      * Migration function to convert old `sp` to new `polyclip` object.
   
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

* Overall: replace use of `sp` (SpatialPolygons) with `sf` (Simple Features)

   * requires changing `venn_spdf` into the `sf` equivalent, which could
   be nearly drop-in replacement. The `sf` default object is based upon
   `data.frame` already.
   * requires converting sp-specific functions: `nudge_sp()`, `rescale_ps()`,
   `rescale_sp()`, `rescale_p()`, `rescale_coordinates()`, `get_sp_buffer()`.
   Some functions like `get_sp_buffer()` may be built-in with `sf`.
   * remove dependencies `sp` and `rgeos`.
   * replace `rgeos` functions with with built-in `sf` equivalents:
   
      * `rgeos::gArea()` -> `sf::st_area()`
      * `rgeos::gDifference()` -> `sf::st_difference()`
      * `rgeos::gBuffer()` -> `sf::st_buffer()`
      * `rgeos::gIntersects()` -> `sf::st_intersects()`
      * `rgeos::gSimplify()` -> `sf::st_simplify()`
      * `rgeos::gUnaryUnion()` -> `sf::st_union(x)`
      * `rgeos::gContains()` -> `sf::st_contains()`
      * `rgeos::gUnion()` -> `sf::st_union(x, y)`
      

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
