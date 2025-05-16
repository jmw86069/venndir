
#' Scale, rotate, and shift numeric coordinates
#' 
#' Scale, rotate, and shift numeric coordinates
#' 
#' This function takes a numeric matrix with two or more
#' numeric columns, and adjusts the coordinates in three
#' ways:
#' 
#' * scale: adjust coordinate range by a multiplier, relative to
#' a central point
#' * rotate: rotate coordinates around a central point in degrees
#' * shift: adjust coordinate range by adding a numeric value
#' 
#' The operations are performed in that order: rotate, scale, shift.
#' 
#' When `center` is not defined, the default behavior is to use
#' the mean of the range of each coordinate column. Using the mean
#' range is equivalent to using the mean of the bounding box.
#' 
#' @param x `matrix` with one or more columns containing `numeric`
#'    coordinates.
#' @param scale `numeric` vector whose values are expanded to length
#'    `ncol(x)`. After subtracting the `center`, the coordinates 
#'    in each column are multiplied by the `scale`.
#' @param rotate_degrees `numeric` value in degrees indicating
#'    rotation around the `center`, where positive values are
#'    clockwise rotation. This rotation is only applied to two
#'    columns in `x` defined by `rotation_axes`.
#' @param shift `numeric` vector whose values are expanded to length
#'    `ncol(x)`. The coordinates in each column are added to
#'    the `shift`, after applying `scale` and `rotate_degrees`
#' @param center `numeric` vector whose values are expanded to length
#'    `ncol(x)`, indicating the center point used for `scale` and
#'    `rotate_degrees` transformations. When `center=NULL` it
#'    is derived from the bounding box, which is the mean of the range
#'    for each column in `x`.
#' @param rotation_axes `integer` vector length 2, indicating which
#'    two columns in `x` are used for `rotate_degrees`.
#' @param plot_debug `logical` indicating whether to plot starting,
#'   intermediate, and final polygons during the processing.
#' @param ... additional arguments are ignored.
#' 
#' @return `matrix` with `numeric` values after processing.
#' 
#' @family venndir geometry
#' 
#' @examples
#' pts <- matrix(ncol=2, c(1:4, 6, 8, 5, 7));
#' rownames(pts) <- letters[1:4];
#' plot(pts, pch=rownames(pts), asp=1)
#' points(pts, pch=21, cex=3)
#' 
#' pts2 <- rescale_coordinates(pts, rotate_degrees=25);
#' arrows(x0=pts[,1], x1=pts2[,1], y0=pts[,2], y1=pts2[,2], col="red")
#' points(pts2, pch=21, cex=3, col="red", bg="white")
#' points(pts2, pch=rownames(pts), col="red")
#' 
#' pts3 <- rescale_coordinates(pts2, scale=0.5);
#' arrows(x0=pts2[,1], x1=pts3[,1], y0=pts2[,2], y1=pts3[,2], col="blue")
#' points(pts3, pch=21, cex=3, col="blue", bg="white")
#' points(pts3, pch=rownames(pts), col="blue")
#' 
#' pts4 <- rescale_coordinates(pts3, shift=c(0.5, 0.5));
#' arrows(x0=pts3[,1], x1=pts4[,1], y0=pts3[,2], y1=pts4[,2], col="gold")
#' points(pts4, pch=21, cex=3, col="gold", bg="white")
#' points(pts4, pch=rownames(pts), col="gold")
#' 
#' @export
rescale_coordinates <- function
(x,
 scale=c(1, 1),
 rotate_degrees=0,
 shift=c(0, 0),
 center=NULL,
 rotation_axes=c(1, 2),
 plot_debug=FALSE,
 ...)
{
   # Define default scale=1 if empty
   if (length(scale) == 0) {
      scale <- c(1, 1)
   }
   # apply scale to all coordinate columns
   scale <- rep(scale,
      length.out=ncol(x));
   
   # define default shift=0 if empty
   if (length(shift) == 0) {
      shift <- c(0, 0)
   }
   shift <- rep(shift,
      length.out=ncol(x));
   
   # define default center using colMeans of bounding box
   # which is the mean range
   if (length(center) == 0) {
      center <- colMeans(apply(x, 2, range, na.rm=TRUE));
   } else {
      center <- rep(center,
         length.out=ncol(x));
   }
   
   # determine whether we need to subtract the center
   # which is needed only when (rotate_degrees != 0) or (scale != 1)
   # and center is non-zero
   apply_center <- FALSE;
   if (any(center != 0) && (
      (length(rotate_degrees) == 1 & rotate_degrees != 0) ||
         any(scale != 1))) {
      apply_center <- TRUE;
   }
   
   # optionally plot polygons to visualize the process
   if (plot_debug) {
      polygon(x,
         col="#0000CDAA",
         border="#0000CD");
   }
   
   ## subtract center coordinate
   if (apply_center) {
      if (plot_debug) {
         points(rbind(center), pch="1", cex=1, col="#0000CD");
      }
      x <- x - rep(center, each=nrow(x));
   }
   
   ## apply scale to coordinates
   if (any(scale != 1)) {
      x <- x * rep(scale, each=nrow(x))
      # optionally plot polygons to visualize the process
      if (plot_debug) {
         polygon(x + rep(center, each=nrow(x)),
            col="#1874CDAA",
            lty="dashed",
            border="#1874CD");
      }
   }
   
   ## rotate coordinates
   if (length(rotate_degrees) == 1 && rotate_degrees != 0) {
      if (length(rotation_axes) != 2) {
         rotation_axes <- c(1, 2);
      }
      # prepare rotation matrix math
      co <- cos(-jamba::deg2rad(rotate_degrees));
      si <- sin(-jamba::deg2rad(rotate_degrees));
      # define axes of rotation
      axis1 <- rotation_axes[1];
      axis2 <- rotation_axes[2];
      # apply rotation matrix math
      axis12 <- cbind(
         co * x[,axis1] - si * x[,axis2],
         si * x[,axis1] + co * x[,axis2]);
      # optionally plot polygons to visualize the process
      if (plot_debug) {
         polygon(axis12 + rep(center, each=nrow(x)),
            col="#4F94CDAA",
            lty="dotted",
            lwd=2,
            border="#4F94CD");
      }
      # assign rotated coordinate column values to input matrix
      x[,c(axis1, axis2)] <- axis12;
   }
   
   ## apply shift to coordinates
   if (any(shift != 0)) {
      x <- x + rep(shift, each=nrow(x));
      # optionally plot polygons to visualize the process
      if (plot_debug) {
         polygon(x + rep(center, each=nrow(x)),
            col="#00BFFFAA",
            lty="dashed",
            lwd=2,
            border="#00BFFF");
      }
   }
   
   ## add center coordinate
   if (apply_center) {
      x <- x + rep(center, each=nrow(x));
      # optionally plot new center point and arrow to visualize
      # the change in the center point
      if (plot_debug) {
         arrows(x0=rbind(center)[,1],
            x1=rbind(center)[,1]+ rbind(shift)[,1],
            y0=rbind(center)[,2],
            y1=rbind(center)[,2]+ rbind(shift)[,2],
            lwd=2,
            col="#006FFF");
         points(rbind(center, rbind(center) + rbind(shift)),
            pch=c("1", "2"),
            cex=1.5,
            col=c("#0000CD", "#0000CD"));
      }
   }
   
   return(x)
}


#' Match list elements to another list
#' 
#' Match list elements to another list
#' 
#' This function takes two `list` objects, and matches
#' the first `list` elements to the second `list`.
#' 
#' Each list contains list elements, for example if `x`
#' is a `list`, then the element in position `i` is accessed
#' using `x[[i]]`.
#' 
#' A match between `x[[i]]` and `y[[j]]` is defined as follows:
#' 
#' * all elements in `x[[i]]` are contained in `y[[j]]`
#' * all elements in `y[[j]]` are contained in `x[[i]]`
#' 
#' For this function, item order and item duplication is
#' ignored.
#' 
#' This function uses logic in the form `all(x[[i]] %in% y[[j]])`,
#' so it will operate properly with input objects compatible
#' with that format. The function is intended to be used with
#' `list` that contains `atomic` `vectors`.
#' 
#' @return `integer` `vector` with the same length as `x`. The
#'    integer values give the position in `y` of the first match.
#' 
#' @param x,y `list` objects to be compared
#' @param ... additional arguments are ignored.
#'
#' @family venndir internal
#' 
#' @examples
#' x <- list(a=LETTERS[1],
#'    b=LETTERS[1:2],
#'    c=LETTERS[2:4]);
#' x;
#' y <- list(
#'    d=LETTERS[1:2],
#'    e=LETTERS[2],
#'    f=LETTERS[2:4]);
#' y;
#' match_list(x, y)
#' match_list(y, x)
#' 
#' @export
match_list <- function
(x,
 y,
 ...)
{
   sapply(x, function(i){
      for(j in seq_along(y)) {
         if (all(i %in% y[[j]]) &&
               all(y[[j]] %in% i) &&
               length(y[[j]]) == length(i)) {
            return(j)
         }
      }
      NA;
   })
}
