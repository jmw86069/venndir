
#' Reposition venn gridtext labels
#' 
#' Reposition venn gridtext labels
#' 
#' This is an internal function, based upon `draw_gridtext_groups()`
#' which takes grid grobs already defined in context of an active
#' graphics device, so the actual height and width is defined,
#' and repositions labels for each group of labels.
#' 
#' For example, when the group of labels is connected to a line
#' segment on the right side, the labels will be right-justified
#' so the line segment ends at the right edge of the labels.
#' 
#' Also, this function is intended to allow different "layouts",
#' such as having the set title at top-center, with counts in
#' two columns below the title on the left and right.
#' 
#' @family venndir label
#' 
#' @param g_labels `richtext_grob` class as returned by
#'    `gridtext::richtext_grob()`.
#' @param gdf `data.frame` with one row per `grob` in `g_labels`,
#'    with colnames `"x"`, `"y"`, `"overlap_set"`, `"r"`,
#'    `"r_unit"`, `"border_col"`, `"box_fill"`. The rows
#'    are grouped using `c("x", "y", "overlap_set")`.
#'    The `"r"` and `"r_unit"` values are used to define
#'    the radius and radius unit for the rounded corner.
#'    The row in `gdf` with the highest `"r"` value is the
#'    reference row, for the `"r"` radius, and the `"border_col"`
#'    and `"box_fill"` values.
#' @param segment_df `data.frame` with segment coordinates, optional.
#' @param do_draw `logical` indicating whether to draw the finished
#'    result in the context of the current open graphics device.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#' 
#' @export
reposition_venn_gridtext_labels <- function
(g_labels,
 gdf,
 segment_df=NULL,
 do_draw=TRUE,
 verbose=FALSE,
 ...)
{
   if (length(gdf) == 0) {
      if (verbose) {
         jamba::printDebug("reposition_venn_gridtext_labels(): ",
            "length(gdf) == 0");
         print(gdf);
      }
      return(gdf);
   }
   # IN PROGRESS
}
