
# Polygon-dependent workflows


# Overview

* `polyclip` comments:

   * `polyclip` defines two formats for "a polygon":

      1. `list` with `x`,`y` elements. Simple polygon.
      2. `list` with `list` of simple polygons above. Used for multi-polygons,
      and/or holes within polygons.
   
   * Polygons are not, and should not be closed. (Start and end points differ.)
   * Output is therefore inconsistent:
   
      * sometimes `list` with `x`,`y`, for simple polygon
      * sometimes `list` with `list` of `x`,`y` for multi-polygon


* Sometimes Venn overlap polygons will be multi-part, most common for
4-way Venn using ellipses, sometimes both ends of the A ellipse represent
unique elements in A, not in any other set.
* Sometimes Venn overlap polygons will contain a hole, e.g. A is completely
inside B.

* When rescaling, rotating a polygon:

   * simple polygons are rotated about their center (or fixed point)
   * complet polygons are rotated about their collective center (or fixed point)

* When drawing:

   * `polypath()` recognizes `x` and `y` as long vectors.
   
      * Multi-part polygons are defined by inserting an `NA` between polygons,
      which recognizes holes, and multi-part polygons.
      * Polygons should be closed, which means the start and end points
      should be identical.
      * Different polygons should each be rendered independently, so that
      some polygons are not recognized as a "hole".

   * `ggplot2::geom_polygon()` uses `x`, `y`, `id`, `subid`
   
      * `id` is used to distinguish each distinct polygon.
      * `subid` is used to distinguish holes, multi-part polygons

   * `grid::grid.path()` recognizes `x`, `y`, `pathId`, `id`
   
      * `pathId` defines each polygon
      * `id` defines each sub-polygon, for example polygon holes or multi-part polygons

# Still todo:

* replace `venndir()` with polyclip equivalent function calls
* convert `venn_spdf` to `data.frame`



# Polygon formats

* polygon (list): `list` with x,y coordinates: single, simple polygon
* polygon (matrix): matrix with x,y columns: single, simple polygon

* polygon_list: `list` of named polygons

   * each named polygon may contain multiple polygons (rule="evenodd" to allow holes)
   * internal multiple polygons do not have names

* xy_list: `list` of elements `"x"`, `"y"`, each is a `list`

   * `"x"` contains `list` of `numeric` vectors for each polygon
   * `"x"` can contain nested list for multi-layer polygons

* vectors by id

   * `x` and `y` are continuous vectors, with `id` to define each polygon
   * used by `grid::grid.path(x, y, id)`
   * used by `ggplot2::geom_polygon(x, y, group=id)`

* vectors by NA (`"coord_list"`)

   * `x` and `y` are continuous vectors, each polygon separated by `NA`
   * used by `graphics::polypath(x, y, rule="evenodd")` - must be closed polygons (start-end use same coordinate)
   * used by `graphics::polygon()` but it does not recognize holes


## Design idea: New classes `jampolygon`, `jampolygons`

* (I don't want to do any of this work, new classes, methods, etc.
There should already be an R package here, without needing to install
huge map and GPS-aware geometry libraries.)

* goal is to use one format to store polygons

   * convert for tools only when needed
   * lossless format, fully describes polygons, multi-polygons, holes

* `jampolygon` represents one "polygon" with one or more internal polygons,

   * necessary to associate a hole with its proper parent
   * always `list` of polygons, even when only one
   * need some way to define attributes: `color`, `label`, etc
   * it will be stored in a `data.frame` field

* `jampolygons` is a `list` of `jampolygon` objects

   * can it contain another `polygons`? hopefully not, but maybe
   * it will be stored in a `data.frame` column

* could these be stored as `data.frame`?

   * polygon_id=`"name"`, `"name.1"`, `"name.2"` - requires `"name"` to be unique
   * label: could allow custom labeling without using the name
   * other attribute columns to associate

```R
jampolygon <- list(
   # first polygon
   name1=list(
      # first polygon for name1
      list(x, y),
      # second polygon for name1
      list(x, y)),
   name2=list(
      list(x, y),
      list(x, y))
    
```

## Design idea: `data.frame` format:

* column for coordinates

   * contains `list` of polygons - with one or more polygons
   * ultimately `list` with `numeric` elements: `"x"`, `"y"`

* use other data.frame columns for attributes
* aim to store inside `venn_spdf` as drop-in replacement
