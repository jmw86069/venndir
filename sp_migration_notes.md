
## Data design changes required to migrate away from sp

* venn_spdf: `SpatialDataFrame` must be converted to `data.frame`

   * coordinates should be converted to colnames `"x"` and `"y"`
   * each coordinate column should be `list` (`AsIs` using `I()`)
   and each element should also be a `list` with `numeric` vectors
   indicating each individual polygon.

* `rgeos` functions to migrate:

   * `rgeos::gArea()` with `polygon_areas()`
   * `rgeos::gContains()` with `polyclip::pointinpolygon()`
   * `rgeos::gDifference()`
   * `rgeos::gBuffer()` with `polyclip::polyoffset()`
   * `rgeos::gUnaryUnion()` or `rgeos::gUnion()` with `polyclip::polyclip()`
   * `rgeos::gSimplify()` with `polyclip::polysimplify()`
   * `rgeos::gIntersection()` with `polyclip::polyclip()`

* `sp` functions to migrate:

   * `sp::SpatialLines()`
   * `sp::bbox()`, `sp::geometry()`
   * `sp::SpatialPolygons()`, `sp::Polygons()`, `sp::Polygon()`
   * `sp::SpatialPolygonsDataFrame()`
   * `sp::spChFIDs()`
   * `sp::merge()`
   * `sp::plot()`

* `polylabelr` functions to confirm:

   * `polylabelr::poi()` - it should work without `sp` nor `sf`

* `venndir` functions to migrate:

   * `eulerr2polys()` to `eulerr_to_polygon_list()`
   * `find_vennpoly_overlaps()` to `find_venn_polygon_list_overlaps()`
   * `intersect_polygons()` to `intersect_polygon_list()`
   * `union_polygons()` to `union_polygon_list()`
