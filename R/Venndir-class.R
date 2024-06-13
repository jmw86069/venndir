
#' Check Venndir object
#' 
#' Check Venndir object integrity
#' 
#' ## Todo:
#' 
#' * Write validation function.
#' * Consider whether to have `jp` (sets), `jps` (overlaps) in separate ojbects
#' 
#' @export
#' 
#' @family JamPolygon
#'
#' @param object `Venndir` object
#' 
#' @export
check_Venndir <- function
(object)
{
   # basic check to confirm all slot names exist
   # all(c("jp", "jps", "label_df", "setlist") %in% slotNames(object))
   is_valid <- (c("jps", "label_df", "setlist") %in% slotNames(object))
   # jamba::printDebug("is_valid:", is_valid);
   all(is_valid)
}

#' Venndir class
#' 
#' Venndir class
#' 
#' This object is intended to contain all the required data to produce
#' a venndir figure. The components in this object can be edited.
#' 
#' Slots:
#' 
#' * **jps**: `JamPolygon` containing the set polygons (usually the full circle
#'    or ellipse corresponding to each set in `setlist`), and the overlap
#'    polygons. Where an overlap does not exist, the polygon coordinates
#'    will be empty, or will have entirely `NA` values for "x" and "y".
#' * **label_df**: `data.frame` containing detailed information about
#'    where to place Venn labels in the figure, what font to use, color,
#'    and items (when items are necessary for the figure). The "x" and "y"
#'    coordinates define the location of each label inside the polygon,
#'    the "x_offset" and "y_offset" are added to those coordinates to
#'    define the label position outside the polygon.
#' * **setlist**: `list` with the `setlist` used to create the Venn overlaps.
#'    Previously this data could be inferred from `label_df` which was tedious.
#' 
#' @family JamPolygon
#' 
setClass("Venndir",
   slots=c(
      # jp="JamPolygon",
      jps="JamPolygon",
      label_df="data.frame",
      setlist="list"
   ),
   prototype=prototype(
      jps=NULL,
      label_df=data.frame(),
      setlist=list()
   ),
   validity=check_Venndir
);

# Todo:
# * print(), summary() functions
# * plot function as wrapper to render_venndir()
