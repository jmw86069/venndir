
#' Expand numeric range
#' 
#' Expand numeric range
#' 
#' This function takes a `numeric` range (or numeric vector
#' and calculates its range) and expands this range by
#' a fraction given by `expand_fraction`.
#' 
#' When the input range is zero, the minimum absolute range
#' can be given by `minimum_range`.
#' 
#' The input may be a `list` that contains `numeric` vectors,
#' in which case the `list` will be iterated to produce an
#' expanded range for each `numeric` vector. Each `numeric`
#' vector is expanded independently.
#' 
#' This function is intended to be a simple technique to expand
#' x-axis and y-axis ranges of a graphical plot.
#' 
#' @return `numeric` vector, or when input `x` is `list` the
#'    output will also be a `list` of `numeric` vectors.
#'    The numeric vector will contain the range after
#'    expansion.
#' 
#' @family venndir internal
#' 
#' @param x `numeric` vector, or `list` of `numeric` vectors.
#'    The input is converted to a range using `range(x, na.rm=TRUE)`
#'    to ensure no `NA` values are included. Note that this
#'    step will force the range to be in ascending order.
#' @param expand_fraction `numeric` value indicating the
#'    fraction of the range defined by `diff(range(x, na.rm=TRUE))`
#'    to add to the total range. When `expand_fraction` has
#'    only one value, it is applied across the whole range,
#'    which means each side is extended by half the `expand_fraction`.
#'    When `expand_fraction` contains two values, it is
#'    applied in order to the low, then high side of the
#'    numeric range, and each full `expand_range` value is
#'    applied.
#' @param minimum_range `numeric` value indicating the minimum
#'    range of the output, useful when the input has zero
#'    range, for example if `x=c(10, 10)`.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' x <- c(0, 10);
#' 
#' # expand the total range by 0.1
#' expand_range(x, 0.1);
#' 
#' # the original range is 10 units
#' diff(x);
#' 
#' # the expanded range is 11 units
#' diff(expand_range(x, 0.1));
#' 
#' # expand one side but not the other
#' expand_range(x, c(0.1, 0));
#' # this new range is 11 units
#' diff(expand_range(x, c(0.1, 0)))
#' 
#' # input with no range is extended to some minimum value
#' expand_range(1, minimum_range=1)
#' expand_range(1, minimum_range=c(1, 0))
#' 
#' # list input is iterated, for example xlim, ylim
#' xlim <- c(1, 10)
#' ylim <- c(1, 100)
#' expand_range(list(xlim=xlim, ylim=ylim))
#' 
#' @export
expand_range <- function
(x,
 expand_fraction=0.1,
 minimum_range=0.01,
 ...)
{
   if (is.list(x)) {
      x <- lapply(x, function(xi){
         expand_range(x=xi,
            expand_percent=expand_percent,
            minimum_range=minimum_range,
            ...)
      });
      return(x);
   }
   if (!is.numeric(x)) {
      stop("x must be a numeric vector, or a list of numeric vectors.");
   }
   x <- range(x, na.rm=TRUE);
   xdiff <- diff(x);
   xmean <- mean(x);
   if (length(expand_fraction) == 1) {
      expand_fraction <- rep(expand_fraction, 2) / 2;
   }
   if (length(minimum_range) == 1) {
      minimum_range <- rep(minimum_range, 2) / 2;
   }
   expand_fraction <- head(expand_fraction, 2);
   minimum_range <- head(minimum_range, 2);
   if (xdiff == 0) {
      expand_fraction <- minimum_range;
      xdiff <- 1;
   }
   xexpand <- xdiff * expand_fraction * c(-1, 1);
   x <- x + xexpand;
   return(x);
}
