
#' Get Venn shapes
#' 
#' Get Venn shapes
#' 
#' This function takes a Venn overlap counts
#' 
#' @family venndir utility
#' 
#' @export
get_venn_shapes <- function
(counts,
 proportional=FALSE,
 sep="&",
 circles_only=FALSE,
 x_nudge=NULL,
 y_nudge=NULL,
 circle_nudge=NULL,
 ...)
{
   # 
   setnames <- unique(unlist(strsplit(names(counts), split=sep)));
   n <- length(setnames);
   
   if (!proportional) {
      if (n < 1 || n > 5) {
         stop("get_venn_shapes() for non-proportional Venn circles requires 1, 2, 3, 4, or 5 sets.");
      }
      if (n == 1) {
         xcenter <- c(5);
         ycenter <- c(5);
         radius <- c(2);
         venn_sp <- sp_circles(xcenter, ycenter, setnames, radius);
      } else if (n == 2) {
         xcenter <- c(4, 6);
         ycenter <- c(5, 5);
         radius <- c(2, 2);
         venn_sp <- sp_circles(xcenter, ycenter, setnames, radius);
      } else if (n == 3) {
         xcenter <- c(4, 6, 5);
         ycenter <- c(6, 6, 4);
         radius <- c(2, 2, 2);
         venn_sp <- sp_circles(xcenter, ycenter, setnames, radius);
      } else if (n == 4 && circles_only) {
         #symbols(x=c(4, 5.5, 4, 5.5), y = c(6, 6, 4.5, 4.5), circles=c(2, 2, 2, 2),
         xcenter <- c(4, 5.5, 4, 5.5);
         ycenter <- c(6, 6, 4.5, 4.5);
         radius <- c(2, 2, 2, 2);
         venn_sp <- sp_circles(xcenter, ycenter, setnames, radius);
      } else if (n == 4) {
         xcenter <- c(3.4, 4.95, 5.3, 6.85);
         ycenter <- c(3.6, 4.5, 4.5, 3.6);
         xradius <- c(2, 2, 2, 2);
         yradius <- c(4, 4, 4, 4);
         rotation_degrees <- 39.2 * c(-1, -1, 1, 1);
         venn_sp <- sp_ellipses(xcenter, ycenter, setnames, xradius, yradius,
            rotation_degrees);
      } else if (n == 5) {
         xcenter <- c(4.83, 6.25, 6.10, 4.48, 3.70);
         ycenter <- c(6.20, 5.40, 3.50, 3.15, 4.80);
         xradius <- c(1.43, 1.7, 1.55, 1.55, 1.7);
         yradius <- c(4.11, 3.6, 3.9, 3.92, 3.6);
         rotation_degrees <- c(0, 66, 150, 210, 293.5);
         venn_sp <- sp_ellipses(xcenter, ycenter, setnames, xradius, yradius,
            rotation_degrees);
         
      }
   } else if (suppressPackageStartupMessages(require(eulerr))) {
      eu <- eulerr::euler(counts, ...);
      venn_sp <- eulerr2polys(eu);
   } else {
      stop("Proportional diagrams require the eulerr package.");
   }
   if (length(x_nudge) > 0 || length(y_nudge) > 0) {
      venn_sp <- nudge_sp(venn_sp,
         x_nudge=x_nudge,
         y_nudge=y_nudge);
   }
   return(invisible(venn_sp));
}
