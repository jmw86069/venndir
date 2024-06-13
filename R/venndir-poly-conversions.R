

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
#' @family venndir polygons
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


