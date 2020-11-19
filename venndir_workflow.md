
# venndir workflow summary

* recognize setlist input
* calculate `signed_overlaps()`
* make Venn shapes
* associate Venn overlaps with polygon intersections

   * returns venn_spdf

* combine venn_spdf with Venn shapes (venn_sp)
* define label positions outside the set polygons

   * optionally include all Venn overlap polygons

* build label_df

   * combine overlap counts, signed overlap counts

* call `curate_venn_labels()`

   * convert overlap signs to symbols
   * convert overlap signs to color


# design idea:

* one row per label in label_df
* "left" or "right" - already implied by main count (left), signed count (right)
* "inside" or "outside"
* coordinates are defined by polygon
