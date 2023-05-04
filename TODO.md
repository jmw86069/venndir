# TODO for venndir

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
