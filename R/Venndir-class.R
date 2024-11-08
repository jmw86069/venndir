
#' Check Venndir object
#' 
#' Check Venndir object integrity
#' 
#' ## Todo:
#' 
#' * Write additional validation checks.
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
   
   # new practical validation
   vnames <- object@jps@polygons$venn_name;
   vsets <- unique(unlist(strsplit(vnames, "&")))
   onames <- object@label_df$overlap_set;
   osets <- unique(unlist(strsplit(onames, "&")))
   # validate venn contents
   is_valid_venn <- (
      all(vsets %in% names(object@setlist)) &&
      all(osets %in% names(object@setlist)) &&
      all(length(vnames) == 0 ||
         object@jps@polygons$venn_name %in% object@label_df$overlap_set)
   )
   all(is_valid) && all(is_valid_venn)
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
#' * **jps**: `JamPolygon` containing the set and overlap polygons.
#' 
#'    * Columns `"x"`,`"y"` contain polygon coordinates, see
#'    `JamPolygon-class` for details on multi-part polygons, and polygons
#'    with holes.
#'    * The polygon type is defined with column `type` and values
#'    `"set"` or `"overlap"`.
#'    * The "set" polygons are intended to be the full circle for each
#'    set, but may be `NA` or `NULL` to indicate the set is empty.
#'    * The "overlap" polygons are expected to be contained inside
#'    "set" polygons, and each overlap polygon may be multi-polygons
#'    with or without holes.
#'    For example, the "set" polygon for `set_A` which is a circle fully
#'    contained inside another set `set_B`.
#'    The "overlap" polygon for `set_A` would be empty, since there is no
#'    region unique to `set_A`.
#'    However the "overlap" polygon `set_A&set_B`
#'    would contain the full circle defined by "set" polygon `set_A`.
#'    Lastly the "overlap" polygon for `set_B` would be a circle with a hole
#'    located at "overlap" `set_A&set_B`.
#'    or ellipse. The overlap polygons are polygons, possibly multi-part
#'    with or without holes, or may be `NA` or `NULL` to indicate that
#'    this overlap does not have a corresponding polygon.
#'    * The `name` column contains set or overlap
#'    names, where multiple sets are concatenated with `"&"` by default.
#'    or ellipse corresponding to each set in `setlist`), and the overlap
#'    polygons. Where an overlap does not exist, the polygon coordinates
#'    will be empty, or will have entirely `NA` values for "x" and "y".
#' * **label_df**: `data.frame` which contains information about
#'    Venn labels placement and content.
#'    It should contain one row for each Venn overlap.
#'    When data contains signed overlaps, each overlap is sub-divided
#'    by the combination of overlap signs, with one row per overlap and
#'    overlap signs.
#'    Overlap signs are summarized based upon argument `overlap_type`,
#'    see `signed_overlaps()`.
#'    * `"overlap_sign"` represents the combination of overlap name,
#'    and the overlap sign across all sets.
#'    * `"text"` indicates the count label to be displayed.
#'    * `"venn_counts"` indicates the numeric number of Venn overlaps
#'    in each row.
#'    * `"item"` should be a `list` that contains `character` vectors
#'    of items in each overlap set. Column `"item"` is optional.
#'    * `"nsets"` indicates the number of sets involved in the overlap.
#'    * `"x"`, `"y"` represent the label coordinate to use inside
#'    each polygon overlap. When the label is displayed outside the polygon,
#'    the values `"x_offset"` and `"y_offset"` are added to `"x"` and `"y"`,
#'    respectively. In this way, a line segment can be drawn from the outer
#'    label back to the corresponding polygon.
#'    * `"color"` indicates the color for each label. Note the
#'    actual color may be modified based upon the background fill color
#'    to maximize the contrast and visibility of the color. For example
#'    blue on a dark background is displayed as light blue.
#'    * `"fontsize"` indicates the base font size, prior to any
#'    optional adjustment with `font_cex`.
#'    * `"fill"` indicates an optional color fill behind the count
#'    label. When `NULL` (default) there is no color fill. This value
#'    is usually populated by argument `label_style` which is passed to
#'    `venndir_label_preset()`.
#'    * `"border"` indicates an optional border around count labels,
#'    and is handled similar to `"fill"`.
#'    * `"padding"`, `"padding_unit"`, `"r"`, and `"r_unit"` are
#'    used with `gridtext::richtext_grob()` to define padding around count
#'    labels, and optional rounded corners when border is displayed.
#'    * Other columns are intended for internal use by `render_venndir()`
#'    and are not yet fully editable.
#' * **setlist**: `list` with the `setlist` used to create the Venn overlaps.
#'    Previously this data could be inferred from `label_df` which was
#'    tedious, and required column `"item"` which is optional.
#'    That said, `setlist` can be an empty `list()`.
#' * **metadata**: `list` with optional metadata, intended for future
#'    expansion, such as plot title.
#' 
#' @family JamPolygon
#' 
setClass("Venndir",
   slots=c(
      # jp="JamPolygon",
      jps="JamPolygon",
      label_df="data.frame",
      setlist="list",
      metadata="list"
   ),
   prototype=prototype(
      jps=NULL,
      label_df=data.frame(),
      setlist=list(),
      metadata=list()
   ),
   validity=check_Venndir
);


#' Plot Venndir object
#' 
#' Plot Venndir object
#' 
#' @returns `Venndir` object, invisibly.
#' 
#' @docType methods
#' @rdname `Venndir-methods`
#' 
#' @export
setMethod("plot",
   signature=c(x="Venndir", y="ANY"),
   definition=function(x, y, ...) {
      if (missing(y)) {
         y <- 0;
      }
      render_venndir(x, ...)
   })

# Todo:
# * print(), summary() functions

setMethod("length",
   signature=c(x="Venndir"),
   definition=function(x) {
      length(x@setlist);
   }
)

# if (!isGeneric("setlist")) {
setGeneric("setlist", function(x) standardGeneric("setlist"))
# }

#' Extract setlist from a Venndir object
#' 
#' @param x `Venndir` object
#' @docType methods
#' @rdname `Venndir-method`
#' @export
setMethod("setlist",
   signature(x="Venndir"),
   function(x) {
      x@setlist
   })

setMethod("names",
   signature=c(x="Venndir"),
   definition=function(x) {
      names(x@setlist);
   }
)
