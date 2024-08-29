

#' Get Venn shapes as polygon_list
#' 
#' Get Venn shapes as polygon_list
#' 
#' This function takes a Venn overlap counts and creates
#' corresponding circles or ellipses that represent
#' either a Venn diagram, or proportional Venn (Euler)
#' diagram.
#' 
#' For non-proportional Venn diagrams, this function accepts
#' up to 5 sets, although the 5-way Venn diagram is not
#' visually intuitive.
#' 
#' For proportional Euler diagrams, this function simply passes
#' the count vector to `eulerr::euler()` and returns the output.
#' That function accepts more sets, however not all overlaps may
#' be represented in the output.
#' 
#' @returns `list` in polygon_list format with `"x"` and `"y"` elements,
#'    or when `return_type="JamPolygon"` it returns `JamPolygon`.
#' 
#' @family venndir polygons
#' 
#' @param counts `integer` vector whose names represent set overlaps,
#'    where set names are separated by `sep` delimiter.
#' @param proportional `logical` indicating whether to create proportional
#'    circles, where `proportional=FALSE` creates standard Venn diagram,
#'    and `proportional=TRUE` creates a Euler diagram.
#' @param sep `character` delimiter used to separate set names in
#'    `names(counts)`.
#' @param circles_only `logical` indicating whether to force Venn
#'    4-way diagram to use only circles; or passed to `eulerr::euler()`
#'    to force it to return circles instead of allowing ellipse shapes.
#' @param circle_nudge `list` of `numeric` vectors each length 2, whose
#'    names match set names derived from `counts`. For example if
#'    `counts=c(set_A=5, set_B=10, "setA&set_B"=3)`, then to nudge
#'    the `set_A` circle, use `circle_nudge=list(set_A=c(1, 0))`.
#'    This argument is intended to allow manipulation of specific
#'    circle or ellipse positions for aesthetic effects. Particularly
#'    for proportional Euler diagrams, sometimes the algorithm places
#'    circles in non-ideal locations
#' @param rotate_degrees `numeric` value indicating rotation in degrees
#'    for the entire set of shapes. This argument is intended to
#'    change the overall orientation, for example so that certain
#'    sets are at the top.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' counts <- c(A=1, B=2, `A&B`=3, C=4)
#' venn_colors <- colorjam::rainbowJam(3, alpha=0.5);
#' 
#' vjp <- get_venn_polygon_shapes(counts, return_type="JamPolygon")
#' plot(vjp, fill=venn_colors)
#' 
#' vjp <- get_venn_polygon_shapes(counts,
#'    return_type="JamPolygon",
#'    proportional=TRUE)
#' plot(vjp, fill=venn_colors)
#' 
#' @export
get_venn_polygon_shapes <- function
(counts,
 proportional=FALSE,
 sep="&",
 circles_only=FALSE,
 circle_nudge=NULL,
 rotate_degrees=0,
 return_type=c("JamPolygon"),
 ...)
{
   # validate return_type
   return_type <- match.arg(return_type);
   #
   setnames <- unique(unlist(strsplit(names(counts),
      fixed=TRUE,
      split=sep)));
   n <- length(setnames);
   
   if (!proportional) {
      if (n < 1 || n > 5) {
         stop("get_venn_shapes() for non-proportional Venn circles requires 1, 2, 3, 4, or 5 sets.");
      }
      if (n == 1) {
         xcenter <- c(5);
         ycenter <- c(5);
         radius <- c(2);
         venn_jp <- polygon_circles(xcenter=xcenter,
            ycenter=ycenter,
            setnames=setnames,
            radius=radius,
            ...);
      } else if (n == 2) {
         xcenter <- c(4, 6);
         ycenter <- c(5, 5);
         radius <- c(2, 2);
         venn_jp <- polygon_circles(xcenter=xcenter,
            ycenter=ycenter,
            setnames=setnames,
            radius=radius,
            ...);
      } else if (n == 3) {
         xcenter <- c(4, 6, 5);
         ycenter <- c(6, 6, 4);
         radius <- c(2, 2, 2);
         venn_jp <- polygon_circles(xcenter=xcenter,
            ycenter=ycenter,
            setnames=setnames,
            radius=radius,
            ...);
      } else if (n == 4 && circles_only) {
         #symbols(x=c(4, 5.5, 4, 5.5), y = c(6, 6, 4.5, 4.5), circles=c(2, 2, 2, 2),
         xcenter <- c(4, 5.5, 4, 5.5);
         ycenter <- c(6, 6, 4.5, 4.5);
         radius <- c(2, 2, 2, 2);
         venn_jp <- polygon_circles(xcenter=xcenter,
            ycenter=ycenter,
            setnames=setnames,
            radius=radius,
            ...);
      } else if (n == 4) {
         xcenter <- c(3.4, 4.95+0.035, 5.30-0.035, 6.85);
         ycenter <- c(3.6, 4.5, 4.5, 3.6);
         xradius <- c(2, 2, 2, 2);
         yradius <- c(4, 4, 4, 4);
         rotation_degrees <- 39.2 * c(-1, -1, 1, 1);
         venn_jp <- polygon_ellipses(xcenter=xcenter,
            ycenter=ycenter,
            setnames=setnames,
            xradius=xradius,
            yradius=yradius,
            rotation_degrees=rotation_degrees,
            ...);
      } else if (n == 5) {
         xcenter <- c(4.83, 6.25, 6.10, 4.48, 3.70);
         ycenter <- c(6.20, 5.40, 3.50, 3.15, 4.80);
         xradius <- c(1.43, 1.7, 1.55, 1.55, 1.7);
         yradius <- c(4.11, 3.6, 3.9, 3.92, 3.6);
         rotation_degrees <- c(0, 66, 150, 210, 293.5);
         venn_jp <- polygon_ellipses(xcenter=xcenter,
            ycenter=ycenter,
            setnames=setnames,
            xradius=xradius,
            yradius=yradius,
            rotation_degrees=rotation_degrees,
            ...);
      }
   } else if (jamba::check_pkg_installed("eulerr")) {
      eu <- eulerr::euler(counts,
         ...);
      # test for any identical circles/ellipses, potential methods:
      # * test identical polygons, after rounding coordinates
      # * test identical coordinate ranges, bbox(sp), after rounding
      #test_repeat_polygons()
      venn_jp <- eulerr_to_JamPolygon(eu)
   } else {
      stop("Proportional diagrams require the eulerr package.");
   }
   if (length(circle_nudge) > 0 ||
         (length(rotate_degrees) > 0 && any(rotate_degrees != 0))) {
      # jamba::printDebug("get_venn_polygon_shapes(): ", "before circle_nudge:");print(venn_polygon_list);# debug
      # jamba::printDebug("get_venn_polygon_shapes(): ", "circle_nudge:");print(circle_nudge);# debug
      venn_jp <- nudge_JamPolygon(jp=venn_jp,
         nudge=circle_nudge,
         rotate_degrees=NULL,
         ...)
   }
   return(venn_jp);
   # JamPolygon
   df <- data.frame(name=names(venn_polygon_list),
      x=I(lapply(venn_polygon_list, function(i){i$x})),
      y=I(lapply(venn_polygon_list, function(i){i$y})));
   jpdf <- new("JamPolygon",
      polygons=df);
   return(jpdf);
}
