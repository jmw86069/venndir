

#' Polygon area for simple or list of polygons
#' 
#' Polygon area for simple or list of polygons
#' 
#' @param x `numeric` input in one of the following formats:
#'    * `numeric` vector of coordinates for single polygon input, which
#'    also requires `y` is supplied as `numeric` vector with equal length.
#'    * `list` of `numeric` vectors representing multiple polygons,
#'    which also requires `y` is supplied as equivalent `list`.
#'    * `list` of polygons, each polygon contains elements `"x"` and `"y"`.
#'    * `list` of polygons, each polygon contains a `list` of polygon
#'    component parts which each contain elements `"x"` and `"y"`.
#' @param y `numeric` vector or `list` of numeric vectors, compatible
#'    with `x`, or `NULL` when `x` contains both coordinates.
#' @param simplify `logical` indicating whether area should be summed
#'    for each polygon, potentially containing nested component polygons.
#'    * `simplify=TRUE` returns `numeric` vector with one total area
#'    per polygon.
#'    * `simplify=FALSE` returns a `list` of `numeric` areas, using nested
#'    list to indicate component polygons.
#'    * Note that this step does not manipulate the polygons in any way,
#'    for example it does not call union over component polygons, therefore
#'    the component polygons may overlap.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#' 
#' @family venndir polygons
#' 
#' @returns `numeric` vector with polygon area for each individual polygon
#'    in the input `x`,`y`.
#'    * When `x` is a `list` that contains `"x"` and `"y"` elements, those
#'    elements are used.
#'    * When `x` and `y` both contain a `list` of `numeric` vectors, each
#'    vector is considered coordinates of a polygon, and the area is returned
#'    for each polygon.
#'    * When `x` and `y` are `numeric` vectors, it is considered a single
#'    polygon, and thus one area is returned.
#' 
#' @examples
#' D <- list(
#'    x=c(-3, 3, 3, 0, -3),
#'    y=c(-3, -3, 1.5, 4, 1.5))
#' polygon_areas(D)
#' 
#' E <- list(
#'    x=c(-3, 3, 3, -3),
#'    y=c(-3, -3, 3, 3))
#' polygon_areas(E)
#' 
#' DElist <- list(
#'    x=list(
#'       D=c(-3, 3, 3, 0, -3),
#'       E=c(-3, 3, 3, -3)),
#'    y=list(
#'       D=c(-3, -3, 1.5, 4, 1.5),
#'       E=c(-3, -3, 3, 3)))
#' polygon_areas(DElist)
#' 
#' # list of polygons
#' poly_list <- list(D=D, E=E)
#' polygon_areas(poly_list)
#'
#' # list of nested polygons
#' polygon_areas(list(DE=poly_list, D=D, E=E))
#' 
#' polygon_areas(list(DE=poly_list, D=D, E=E), simplify=TRUE)
#' 
#' @export
polygon_areas <- function
(x,
 y=NULL,
 simplify=FALSE,
 verbose=FALSE,
 ...)
{
   # simple wrapper around pracma::polyarea()
   if (is.list(x) && length(y) == 0) {
      if (all(c("x", "y") %in% names(x))) {
         if (verbose) {
            jamba::printDebug("polygon_areas(): ",
               "using \"x\" and \"y\" from input list ", "x")
         }
         y <- x[["y"]];
         x <- x[["x"]];
      } else {
         if (verbose) {
            jamba::printDebug("polygon_areas(): ",
               "splitting list ", "x ",
               "into component lists of \"x\" and \"y\"")
         }
         xy_list <- polygon_list_to_xy_list(x);
         x <- xy_list$x;
         y <- xy_list$y;
      }
   }
   if (is.atomic(x)) {
      if (length(y) == 0) {
         stop("x is atomic, y is empty. y must also be supplied.")
      }
      if (!is.atomic(y)) {
         stop("x is atomic, y is not atomic. y must also be atomic.")
      }
      parea <- abs(pracma::polyarea(
         x=x,
         y=y))
      return(parea);
   }
   pareas <- lapply(seq_along(x), function(i){
      if (is.list(x[[i]])) {
         polygon_areas(x=x[[i]],
            y=y[[i]],
            simplify=simplify)
      } else {
         abs(pracma::polyarea(
            x=x[[i]],
            y=y[[i]]))
      }
   })
   names(pareas) <- names(x);
   if (TRUE %in% simplify) {
      pareas <- sapply(pareas, sum)
   }
   return(pareas)
}
