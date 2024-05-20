

#' Convert polygon list of x,y coordinate into a list by x and y
#' 
#' Convert polygon list of x,y coordinate into a list by x and y
#' 
#' Input is a list of polygons, where each polygon contains a `list`
#' with elements `"x"` and `"y"`. Output is a list of `"x"` and `"y"`
#' split by each polygon.
#' 
#' @returns `list` with elements `"x"` and `"y"` which each contain a
#'    `list` with length `length(x)`.
#' 
#' @family venndir polygons
#'
#' @param x `list` of polygons
#'    * each polygon should contain a `list` with elements `"x"` and `"y"`.
#'    * each polygon can contain multiple component polygons as a
#'    nested list, in which case this function is called iteratively
#'    so that the component `"x"` and `"y"` are returned as equivalent
#'    nested `list` objects.
#'    * In all cases, `names(output$x)` and `names(output$y)` should equal
#'    `names(x)`.
#' @param flatten `logical` indicating whether all polygons should be
#'    flattened to the same level, without nested polygons.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' counts <- c(A=1, B=2, `A&B`=3)
#' x <- eulerr::euler(counts)
#' polygon_list <- eulerr_to_polygon_list(x)
#' jamba::ssdim(polygon_list)
#' jamba::ssdim(polygon_list_to_xy_list(polygon_list))
#' 
#' jamba::ssdim(polygon_list_to_xy_list(list(AB=polygon_list)))
#' 
#' @export
polygon_list_to_xy_list <- function
(x,
 flatten=FALSE,
 ...)
{
   #
   # if (!all(c("x", "y") %in% names(x[[1]]))) {
   #    stop("input must be a list containing \"x\" and \"y\"")
   # }
   list_y <- lapply(x, function(ix){
      if (is.list(ix) && !all(c("x", "y") %in% names(ix))) {
         polygon_list_to_xy_list(ix)$y
      } else {
         ix$y
      }
   })
   list_x <- lapply(x, function(ix){
      if (is.list(ix) && !all(c("x", "y") %in% names(ix))) {
         polygon_list_to_xy_list(ix)$x
      } else {
         ix$x
      }
   })
   
   # if input contains nested polygons (e.g. holes)
   # they must be unlisted to flatten them
   if (TRUE %in% flatten) {
      if (is.list(list_y[[1]])) {
         list_y <- unlist(list_y, recursive=FALSE);
      }
      if (is.list(list_x[[1]])) {
         list_x <- unlist(list_x, recursive=FALSE);
      }
   }
   return(list(
      x=list_x,
      y=list_y))
}


#' Convert coordinate list of x and y into polygon list of x,y coordinates
#' 
#' @examples
#' counts <- c(A=1, B=2, `A&B`=3)
#' x <- eulerr::euler(counts)
#' polygon_list <- eulerr_to_polygon_list(x)
#' xy_list <- polygon_list_to_xy_list(polygon_list)
#' polygon_list2 <- xy_list_to_polygon_list(xy_list)
#' identical(polygon_list, polygon_list2)
#' 
#' @export
xy_list_to_polygon_list <- function
(x,
 ...)
{
   #
   if (!all(c("x", "y") %in% names(x))) {
      stop("input must be a list containing \"x\" and \"y\"")
   }
   xy_seq <- jamba::nameVector(seq_along(x$x), names(x$x));
   polygon_list <- lapply(xy_seq, function(i){
      if (is.list(x$x[[i]])) {
         xy_list_to_polygon_list(x=list(x=x$x[[i]], y=x$y[[i]]))
      } else {
         list(x=x$x[[i]], y=x$y[[i]])
      }
   })
   return(polygon_list);
}


#' Convert xy_list to coordinate list format
#' 
#' @family venndir polygons
#' 
#' @param x `list` of coordinates with elements `"x"` and `"y"` which
#'    may themselves each be a `list` representing multiple polygons.
#' @param closed `logical` indicating whether the last point in each
#'    polygon should equal the first point in the polygon, to "close"
#'    the path so to speak.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' counts <- c(A=7, B=0, `A&B`=1)
#' x <- eulerr::euler(counts)
#' polygon_list <- eulerr_to_polygon_list(x)
#' plot_polygon_list(polygon_list, col=2:4)
#' 
#' A_only <- minus_polygon_list(polygon_list, new_name="A_only");
#' xy_list <- polygon_list_to_xy_list(A_only)
#' coord_list <- xy_list_to_coord_list(xy_list)
#' jamba::nullPlot(doBoxes=FALSE, xlim=c(-4, 4), ylim=c(-4, 4), asp=1)
#' polypath(x=coord_list$x, y=coord_list$y, rule="evenodd", col=c("red", "blue", "gold"))
#' 
#' counts <- c(A=7, B=0, C=0, `A&B`=1, `A&C`=2)
#' x <- eulerr::euler(counts)
#' polygon_list <- eulerr_to_polygon_list(x)
#' plot_polygon_list(polygon_list, col=2:4)
#' 
#' A_only <- minus_polygon_list(polygon_list, new_name="A_only");
#' xy_list <- polygon_list_to_xy_list(A_only)
#' coord_list <- xy_list_to_coord_list(xy_list, closed=TRUE)
#' jamba::nullPlot(doBoxes=FALSE, xlim=c(-4, 4), ylim=c(-4, 4), asp=1)
#' polypath(x=coord_list$x, y=coord_list$y, rule="evenodd", col=c("red", "blue", "gold"))
#' 
#' plot_polygon_list(c(polygon_list[1]),
#'    col=jamba::alpha2col(alpha=0.5, c("red", "blue", "gold")))
#' plot_polygon_list(c(A_only, polygon_list[2:3])[1],
#'    col=jamba::alpha2col(alpha=0.5, c("red", "blue", "gold")))
#' 
#' # show proper label avoiding the hole
#' points(polygon_list_labelr(c(A_only)), pch=20, col="red")
#' @export
xy_list_to_coord_list <- function
(x,
 closed=FALSE,
 verbose=TRUE,
 ...)
{
   # insert NA between subsequent polygons
   if (verbose) {
      jamba::printDebug("xy_list_to_coord_list(): ",
         "jamba::ssdim(x):");
      print(jamba::ssdim(x));
   }
   coord_list <- lapply(seq_along(x), function(j){
      if (verbose) {
         jamba::printDebug("xy_list_to_coord_list(): ",
            "iteration ", j, " (", names(x)[j], ")");
      }
      i <- x[[j]];
      if (length(i) == 1 && is.list(i)) {
         # convert i to a vector
         i <- i[[1]];
         if (verbose) {
            jamba::printDebug("xy_list_to_coord_list(): ",
               "i <- i[[1]]");
            jamba::printDebug("xy_list_to_coord_list(): ",
               "jamba::ssdim(i):");
            print(jamba::ssdim(i));
         }
      }
      if (length(i) == 1) {
         if (verbose) {
            jamba::printDebug("xy_list_to_coord_list(): ",
               "length(i) == 1");
         }
         i_coords <- unname(unlist(i))
         if (TRUE %in% closed &&
               length(i_coords) > 1 && head(i_coords, 1) != tail(i_coords, 1)) {
            i_coords <- c(i_coords, head(i_coords, 1));
            if (verbose) {
               jamba::printDebug("xy_list_to_coord_list(): ",
                  "closed", indent=6);
            }
         }
      } else {
         if (verbose) {
            jamba::printDebug("xy_list_to_coord_list(): ",
               "length(i) > 1");
         }
         # i <- rev(i);
         i_coords <- unname(unlist(
            lapply(i, function(ix){
               # check whether the polygon should be closed
               if (TRUE %in% closed &&
                     length(ix) > 1 && head(ix, 1) != tail(ix, 1)) {
                  ix <- c(ix, head(ix, 1));
               }
               c(ix, NA)
            })));
         i_coords <- head(i_coords, length(i_coords) - 1);
      }
      i_coords;
   })
   
}
