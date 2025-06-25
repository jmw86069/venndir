
#' Assign groups to a vector of degree angles
#' 
#' @param degrees `numeric` vector of degree angles, in any order.
#' @param min_degrees `numeric` value indicating the threshold for
#'    grouping values in `degrees` together.
#' @param ... additional arguments are ignored.
#' 
#' @returns `data.frame` with rows in the same order as input `degrees`,
#'    with column 'group' indicating the group assignment as an `integer`.
#' 
#' @family venndir internal
#'
#' @examples
#' assign_degree_groups(c(5, 10, 355, 350))
#' 
#' assign_degree_groups(c(5, 10, 355, 180, 350, 340, 174))
#' @export
assign_degree_groups <- function
(degrees,
 min_degrees=10,
 ...)
{
   
   ddf <- data.frame(degrees=degrees %% 360,
      order=seq_along(degrees));
   # order by increasing angle in degrees
   ddf <- ddf[order(ddf$degrees), , drop=FALSE]
   
   ddf$diff <- diff(c(tail(ddf$degrees, 1), ddf$degrees));
   ddf$diff_pre <- diff_degrees(c(tail(ddf$degrees, 1), ddf$degrees));
   ddf$diff_post <- diff_degrees(c(ddf$degrees, head(ddf$degrees, 1)));
   ddf$group <- seq_len(nrow(ddf))
   
   # iterate
   for (j in seq_len(nrow(ddf))) {
      for (i in seq_len(nrow(ddf))) {
         if (abs(ddf$diff_pre[i]) < min_degrees * 1.02) {
            if (i == 1) {
               k <- c(i, nrow(ddf));
               ddf$group[k] <- head(sort(ddf$group[k]), 1);
               # } else {
               #    k <- c(i - 1, i)
            }
         }
         if (abs(ddf$diff_post[i]) < min_degrees * 1.02) {
            if (i == nrow(ddf)) {
               k <- c(i, 1);
            } else {
               k <- c(i, i + 1);
            }
            ddf$group[k] <- head(sort(ddf$group[k]), 1);
         }
      }
   }
   ddf <- jamba::mixedSortDF(ddf, byCols="order")
   return(ddf)
}


#' Make degrees angles into clockwise arc
#' 
#' Make degrees angles into clockwise arc
#' 
#' The purpose is very specific, it takes a vector of degree angles,
#' determines the correct sequence of angles to constitute the
#' proper arc. A proper arc is defined as a series of points where
#' there is only one "largest gap" and this gap represents the
#' open space. When the arc crosses zero, angles are shifted
#' to negative values so that the numeric difference between
#' each angle is consistent.
#' 
#' For this purpose, "clockwise" is defined "increasing numeric order".
#' 
#' The assumption is that values in `x` only represent one contiguous arc,
#' that does not loop around itself, and therefore that values in `x`
#' may be provided in any order. Values returned will be sorted to start
#' with the first angle in the series, so that the last angle in the
#' series is positive and less than 360. If the arc crosses zero, the
#' first value in the series will be negative.
#' 
#' @param x `numeric` vector of degree angles.
#' 
#' @returns `data.frame` with three columns:
#'    * `'x'`: the `numeric` vector of degrees, sorted in increasing order,
#'    with values adjusted so that the difference between angle
#'    is positive, and has consistent numeric difference. When
#'    the arc crosses zero, the first angle will therefore be negative.
#'    * `'idx'`: the `integer` index position of input `degrees`,
#'    which may be used as the equivalent of `order()`.
#'    * `'xdiff'`: the numeric difference between each degree angle,
#'    and the next angle in the sequence. The last value should always
#'    be the largest, representing the gap between the end of the arc,
#'    and the start of the arc.
#' 
#' @family venndir internal
#' 
#' @examples
#' x <- c(355, 5, 10, 350, 15, 20)
#' make_degrees_clockwise(x)
#' 
#' # re-order the original angles
#' x[make_degrees_clockwise(x)$idx]
#' 
#' make_degrees_clockwise(c(100, 200))
#' make_degrees_clockwise(c(200, 100, 300) + 110)
#' @export
make_degrees_clockwise <- function
(x)
{
   # assume input is already sorted low to high
   # sort to be sure
   x <- (x %% 360);
   if (length(x) == 1) {
      return(data.frame(x=x, idx=1))
   }
   xdf <- jamba::mixedSortDF(data.frame(x=x, idx=seq_along(x)))
   xdf$xdiff <- diff(c(xdf$x, head(xdf$x, 1) + 360));
   xdiffmax <- which.max(xdf$xdiff);
   if (xdiffmax == length(x)) {
      return(xdf)
   }
   xseq <- seq(from=xdiffmax + 1, to=length(x));
   xdf$x[xseq] <- xdf$x[xseq] - 360;
   xdf <- jamba::mixedSortDF(xdf)
   return(xdf)
}
