
# Create S4 object JamPolygon
# - extends data.frame
# - special plot() function to plot polygons

## Method Todo:
# - consider method to split multi-part polygons into individual parts.
#    - bonus points for keeping "holes" with their parent polygon
#    - nested polygons would be separated (perhaps optional) but
#      interior holes would be removed.
# - consider how to define distinct entities:
#    - orientation: 1=clockwise, -1=counter-clockwise
#    - holes: 1=solid, -1=hole
#    - parent polygon part


#' Check JamPolygon object
#'
#' Check whether a JamPolygon object is valid.
#' 
#' It requires:
#' 
#' * `slotName(object)` to contain `"polygons"`
#' * `object@polygons` to inherit `"data.frame"`
#' * `colnames(object@polygons)` to contain `c("name", "x", "y")`
#' 
#' General guidance for JamPolygon objects:
#' 
#' * Empty polygons can be represented as one row in the `data.frame`
#' which contains `NULL` or `numeric(0)` x,y coordinates.
#' 
#'    * Empty polygons can be the result of `intersect_JamPolygon()`
#'    that finds no suitable intersect.
#'    * Note that downstream processing, and plotting, must account for
#'    empty polygons and handle or ignore them accordingly.
#' 
#' @param object `JamPolygon` object
#' 
#' @family JamPolygon
#' 
#' @export
check_JamPolygon <- function
(object)
{
   # check for valid content
   "polygons" %in% slotNames(object) &&
      inherits(object@polygons, "data.frame") &&
      length(colnames(object@polygons)) > 0 && 
      all(c("x", "y", "name") %in% colnames(object@polygons))
   # Todo:
   # check that x,y are list columns with numeric vectors
}

#' JamPolygon class
#' 
#' JamPolygon class contains one slot `"polygons"` which is a `data.frame`
#' with one polygon per row. An individual polygon can 
#' 
#' @family JamPolygon
#'
#' @examples
#' df <- data.frame(name=c("polygon1", "polygon2"),
#'    x=I(list(
#'       list(c(1, 4, 4, 1),
#'          c(2, 3, 3, 2)),
#'       c(5, 6, 6, 5))),
#'    y=I(list(
#'       list(c(1, 1, 4, 4),
#'          c(2, 2, 3, 3)),
#'       c(1, 1, 2, 2))),
#'    fill=c("gold", "firebrick"))
#' jpdf <- new("JamPolygon", polygons=df);
#' 
setClass("JamPolygon",
   slots=c(
      polygons="data.frame"
   ),
   prototype=prototype(
      polygons=data.frame(name=character(0),
         x=I(list()),
         y=I(list()))
   ),
   validity=check_JamPolygon
);

#' Subset JamPolygon object
#' 
#' @docType methods
#' @rdname JamPolygon-methods
#' @export
setMethod("[",
   signature=c(x="JamPolygon",
      i="ANY",
      j="ANY"),
   definition=function(x, i=NULL, j=NULL, ...) {
      if (missing(i)) {
         i <- NULL;
      }
      if (missing(j)) {
         j <- NULL;
      }
      if (length(i) > 0) {
         if (is.numeric(i)) {
            x@polygons <- x@polygons[i, , drop=FALSE]
         } else if (is.character(i)) {
            imatch <- match(i, x@polygons$name);
            x@polygons <- x@polygons[imatch, , drop=FALSE]
         }
      }
      # validate_JamObject(x,
      #    rows=i,
      #    columns=j)
      x;
   }
)

# Custom plot function
# - it is recommended not to use curly braces around the function
# setGeneric("plot", function(object) standardGeneric("plot"))
if (!isGeneric("plot")) {
   setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
}

#' Plot JamPolygon object
#' 
#' Plot JamPolygon object
#' 
#' @returns `JamPolygon` object, invisibly.
#' 
#' @family JamPolygon
#' 
#' @examples
#' dfx <- data.frame(name=c("polygon1", "polygon2"),
#'    x=I(list(
#'       list(c(1, 4, 4, 1),
#'          c(2, 3, 3, 2)),
#'       c(5, 6, 6, 5))),
#'    y=I(list(
#'       list(c(1, 1, 4, 4),
#'          c(2, 2, 3, 3)),
#'       c(1, 1, 2, 2))),
#'    fill=c("gold", "firebrick"))
#' jpx <- new("JamPolygon", polygons=dfx);
#' plot(jpx);
#' 
#' dfz <- data.frame(name=c("polygon1", "polygon2", "polygon3"),
#'    x=I(list(
#'       list(c(1, 4, 4, 1),
#'          c(2, 3, 3, 2)),
#'       list(c(4.5, 6.5, 6.5, 4.5),
#'          c(5, 6, 6, 5)),
#'       list(c(1, 4, 4, 1),
#'          c(2, 3, 3, 2),
#'          c(5, 6, 6, 5)))),
#'    y=I(list(
#'       list(c(1, 1, 4, 4),
#'          c(2, 2, 3, 3)),
#'       list(c(1, 1, 3, 3),
#'          c(3, 3, 4, 4)+0.5),
#'       list(c(5, 5, 8, 8),
#'          c(6, 6, 7, 7),
#'          c(6, 6, 7, 7)))),
#'    fill=c("gold", "firebrick", "dodgerblue"));
#' jpz <- new("JamPolygon", polygons=dfz);
#' jpz@polygons[, c("label_x", "label_y")] <- as.data.frame(labelr_JamPolygon(jpz))
#' jpz@polygons$border <- c("orange", "gold", "purple");
#' jpz@polygons$border.lwd <- c(3, 4, 5);
#' jpz <- add_orientation_JamPolygon(jpz);
#' plot(jpz);
#' 
#' @docType methods
#' @rdname JamPolygon-methods
#' @export
setMethod("plot",
   signature=c(x="JamPolygon", y="missing"),
   definition=function(x, y, ...) {
      plot.JamPolygon(x, y, ...)
   })

#' Plot JamPolygon object
#' 
#' Plot JamPolygon object
#' 
#' This function is a general purpose function to plot `JamPolygon`
#' objects using `grid` graphics.
#' 
#' It currently calls direct rendering functions, for example
#' `grid::pathGrob()`, `grid::grid.lines()`, `gridGeometry::polyoffsetGrob()`
#' to render complex, potentially nested polygons with or without
#' holes, and with inner, outer, and/or direct border.
#' 
#' Rendering guidelines used by this function:
#' 
#' * Each polygon is rendered in order, and in series, with no
#' parallelization.
#' * All polygon labels are rendered afterward, so that labels
#' are not covered by subsequent polygons. Labels are intended
#' to appear "on top" of all rendered polygons.
#' * Borders are drawn after each polygon "fill" is performed, so
#' the border can modify the look of the fill. Borders are rendered
#' with inner, outer, then direct border, in that order. The polygon
#' area "fill" is also shrunk by the width defined by `innerborder.lwd`,
#' unless `innerborder` is NA, so that the inner border does not
#' affect the same region represented by the area fill.
#' * Since polygons are drawn in the order they are provided by the
#' `JamPolygon` object, one polygon may be drawn on top of another
#' polygon, using default R methods to overlay one color onto another,
#' for example with optional alpha transparency.
#' 
#' ## Rendering options recognized in `jp@polygons`:
#' 
#' * `name`, `label` - name and display label. A `label` of `NA` or `""`
#' will not be rendered.
#' * `label_color` - color used to render each polygon label.
#' * `family`, `fontsize` - font family, and font point size used to render
#' each polygon label, passed to `grid::gpar()` by default. Note that
#' `venndir()` and `render_venndir()` may internally call
#' `marquee::marquee_grob()` which does not use `grid::gpar()`.
#' * `x`, `y` - x- and y-coordinates to define each polygon or multipolygon.
#' These columns are required for a `JamPolygon` object.
#' * `fill` - polygon fill color, or `NA` for no fill color.
#' * `innerborder`, `innerborder.lwd` - inner border and line width.
#' The inner border is drawn inside the polygon absolute boundary.
#' * `outerborder`, `outerborder.lwd` - inner border and line width
#' The inner border is drawn outside the polygon absolute boundary.
#' * `border`, `border.lwd` - border color and line width (outer border).
#' The border is drawn on top of the polygon absolute boundary. Note
#' that when two polygons share the same border, one border will
#' necessarily overwrite the other. For this reason, the innerborder
#' may be useful in order to preserve the color of each polygon border
#' without overlap.
#' 
#' ## Return grobs
#' 
#' * To return the list of grobs to be drawn without drawing them,
#' use `do_draw=FALSE`, which also does not call `grid::grid.newpage()`.
#' 
#' ## Todo
#' 
#' 1. Enable arguments in `...` to override equivalent values in columns of
#' `jp@polygons`.
#' Partially complete.
#' 2. Convert `grid` rendering to generate graphical objects (grobs)
#' which can be optionally rendered, or returned as a `gTree`.
#' Mostly complete.
#' 3. Continue debugging the `vwline` graphical glitches which are
#' apparent when rendering outer borders.
#' Complete.
#' See [https://github.com/pmur002/vwline/issues/2].
#' 
#'    * Current recommendation is to render outer border after the inner
#'    border, and with outer border at least the same or larger width
#'    as the inner border. Otherwise, for acute angles, inner border may
#'    exceed the outer border because of its line width. However, if the
#'    outer border is drawn afterward, it will fully cover the inner border.
#'    With sufficiently small inner border width, the graphical glitch may
#'    not be apparent.
#' 
#' 4. Consider allowing labels for each multi-part polygon.
#' Low priotity.
#' 5. Consider drawing optional x- and y-axis, although both could be added
#' using `grid` functions.
#' Low priority.
#' 6. Consider using different approach than `"snpc"` to enforce aspect
#' ratio, for example ggplot2 uses `respect=TRUE` then leaves the x/y axis
#' ranges intact. Making that change would affect other venndir functions
#' that may assume scaled units are between 0 and 1.
#' 
#' @family JamPolygon
#' 
#' @returns `JamPolygon` object, invisibly. Some `grid` attributes are
#'    added to the returned object:
#'    * `"adjx"`,`"adjy"`: functions to adjust native x/y values to
#'    the corresponding `grid` units in `"snpc"`.
#'    * `"viewport"`: the `grid::viewport()` object suitable to push
#'    the same viewport in order to add features to an existing plot.
#'    * `"xrange"`,`"yrange"`: x- and y-axis ranges used to determine
#'    the viewport to be used.
#'    * `"grob_tree"`: a `grid::gTree` object suitable to call
#'    `grid::grid.draw()`. It includes the same `viewport`, so it
#'    does not need to have the viewport defined.
#' 
#' @examples
#' dfx <- data.frame(name=c("polygon1", "polygon2"),
#'    x=I(list(
#'       list(c(1, 4, 4, 1),
#'          c(2, 3, 3, 2)),
#'       c(5, 6, 6, 5))),
#'    y=I(list(
#'       list(c(1, 1, 4, 4),
#'          c(2, 2, 3, 3)),
#'       c(1, 1, 2, 2))),
#'    fill=c("gold", "firebrick"))
#' jpx <- new("JamPolygon", polygons=dfx);
#' plot(jpx);
#' 
#' # if you want to add to the plot, you must capture output
#' # to use the viewport
#' jpxout <- plot(jpx);
#' vp <- attr(jpxout, "viewport");
#' adjx <- attr(jpxout, "adjx");
#' adjy <- attr(jpxout, "adjy");
#' 
#' # grob inside the hole of polygon1
#' grid::grid.path(x=adjx(c(2.1, 2.9, 2.9, 2.1)),
#'    y=adjy(c(2.1, 2.1, 2.9, 2.9)),
#'    vp=vp,
#'    gp=grid::gpar(fill="purple", col="red1", lwd=2),
#'    default.units="snpc")
#' grid::grid.text(x=adjx(2.5), y=adjy(2.5),
#'    label="new\ngrob",
#'    vp=vp,
#'    gp=grid::gpar(col="yellow", fontsize=20),
#'    default.units="snpc")
#'
#' dfz <- data.frame(name=c("polygon1", "polygon2", "polygon3"),
#'    x=I(list(
#'       list(c(1, 4, 4, 1),
#'          c(2, 3, 3, 2)),
#'       list(c(4.5, 6.5, 6.5, 4.5),
#'          c(5, 6, 6, 5)),
#'       list(c(1, 4, 4, 1),
#'          c(2, 3, 3, 2),
#'          c(5, 6, 6, 5)))),
#'    y=I(list(
#'       list(c(1, 1, 4, 4),
#'          c(2, 2, 3, 3)),
#'       list(c(1, 1, 3, 3),
#'          c(3, 3, 4, 4)+0.5),
#'       list(c(5, 5, 8, 8),
#'          c(6, 6, 7, 7),
#'          c(6, 6, 7, 7)))),
#'    fill=c("gold", "firebrick", "dodgerblue"));
#' jpz <- new("JamPolygon", polygons=dfz);
#' jpz@polygons[, c("label_x", "label_y")] <- as.data.frame(labelr_JamPolygon(jpz))
#' jpz@polygons$outerborder <- c("orange", "gold", "purple");
#' jpz@polygons$outerborder.lwd <- 0;
#' jpz@polygons$outerborder.lwd <- c(3, 4, 5);
#' jpz@polygons$innerborder <- c("orange4", "gold3", "purple4");
#' jpz@polygons$innerborder.lwd <- c(3, 4, 5);
#' jpz@polygons$border.lwd <- 1;
#' jpz@polygons$border.lty <- 2;
#' #jpz <- add_orientation_JamPolygon(jpz);
#' plot(jpz);
#' 
#' @param x `JamPolygon` object
#' @param y not used.
#' @param xlim,ylim `numeric` optionally used to define the x- and y-axis
#'    range to be rendered. When `NULL` they are defined using the
#'    observed range of values.
#' @param flip_sign `logical` indicating whether to flip the polygon
#'    orientation, or `numeric` where the sign is multiplied by the
#'    polygon orientation.
#'    The polygon orientation is used to define inner/outer border, relative
#'    to whether the border represents a solid inner polygon, or the hole
#'    inside a solid polygon. In most cases, the orientation is automatically
#'    recognized and applied appropriately.
#'    Specifically:
#'    * `TRUE` or `-1` reverses the polygon orientation of
#'    inner/outer border
#'    * `FALSE` or `1` keeps the polygon orientation unchanged.
#' @param render_vectorized `logical` indicating whether to render all
#'    polygons in one call, thereby ignoring `innerborder` values. All
#'    `border` values are rendered as regular polygon borders. This option
#'    may be substantially faster for large collections of polygons.
#' @param render_thin_border `logical` indicating whether to render a thin
#'    border on the border itself, default `TRUE` renders a thin grey line.
#' @param linejoin `character` string (default `"bevel"`) passed to
#'    `grid::grid.path()` and `vwline::grid.vwline()` when rendering
#'    polygons, and inner/outer polygon borders, respectively.
#'    Note that vwline version `0.2.2` displayed some graphical glitches
#'    when used with `"mitre"` and `"round"`, so `"bevel"` is the new default.
#' @param mitrelimit `numeric` passed to `vwline::grid.vwline()` to adjust
#'    the maximum extension at a line join caused by `linejoin="mitre"`.
#' @param show_labels `logical` indicating whether to render labels for
#'    each polygon. Note that labels are rendered after all polygons are
#'    drawn, so they will not be covered by other polygons.
#' @param buffer `numeric` used to expand the x- and y-axis range beyond
#'    the polygons to be drawn.
#' @param do_newpage `logical` (default TRUE) indicating whether to call
#'    `grid::grid.newpage()` to open a new graphical output page.
#' @param do_viewport `logical` (default TRUE) indicating whether to define
#'    and push a new `grid` viewport with `grid::grid.pushViewport()`.
#' @param do_pop_viewport `logical` (default TRUE) indicating whether to
#'    close/pop the `grid` viewport with `grid::popViewport()`.
#'    This action is only performed when `do_viewport=TRUE`.
#'    This option is intended to allow layering multiple calls to this
#'    or other `grid` functions.
#' @param do_draw `logical` (default TRUE) indicating whether to call
#'    `grid::grid.draw()` for each graphical object.
#'    When `do_draw=FALSE`, it also forces `do_newpage=FALSE`,
#'    `do_viewport=FALSE`, and `do_pop_viewport=FALSE`.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param debug `logical` (default FALSE) indicating whether to enable
#'    debug operations. When `debug=TRUE` it is also passed to `grid`
#'    functions such as `vwline::grid.vwline()` to display internal
#'    calculations in the graphical output.
#' @param ... additional arguments are recognized to customize plot features.
#' 
#' 
#' @export
plot.JamPolygon <- function
(x,
 y,
 xlim=NULL,
 ylim=NULL,
 flip_sign=1,
 render_vectorized=FALSE,
 render_thin_border=TRUE,
 linejoin=c("bevel",
    "mitre",
    "round"),
 mitrelimit=-20,
 show_labels=TRUE,
 buffer=0.05,
 do_newpage=TRUE,
 do_viewport=TRUE,
 do_pop_viewport=TRUE,
 do_draw=TRUE,
 verbose=FALSE,
 debug=FALSE,
 ...)
{
   # custom plot function here
   #
   # handle user-defined variables
   dots <- list(...);
   linejoin <- match.arg(linejoin);
   # flip_sign
   if (length(flip_sign) == 0) {
      flip_sign <- 1;
   } else {
      if (is.logical(flip_sign) && TRUE %in% flip_sign) {
         flip_sign <- -1;
      } else if (is.numeric(flip_sign)) {
         flip_sign <- head(flip_sign, 1);
         flip_sign <- ifelse(flip_sign >= 0, 1, -1);
      }
      if (flip_sign < 0 && TRUE %in% verbose) {
         jamba::printDebug("plot.JamPolygon(): ",
            "Handling flip_sign:", flip_sign);
      }
   }
   # grid options
   # do_newpage: by default calls grid::grid.newpage()
   if (length(do_draw) == 0) {
      do_draw <- TRUE;
   } else {
      do_draw <- head(as.logical(do_draw), 1);
   }
   if (FALSE %in% do_draw) {
      do_newpage <- FALSE;
      do_viewport <- FALSE;
      do_pop_viewport <- FALSE;
   }
   # do_newpage: by default calls grid::grid.newpage()
   if (length(do_newpage) == 0) {
      do_newpage <- TRUE;
   } else {
      do_newpage <- head(as.logical(do_newpage), 1);
   }
   # do_viewport: by default calls grid::pushViewport()
   if (length(do_viewport) == 0) {
      do_viewport <- TRUE;
   } else {
      do_viewport <- head(as.logical(do_viewport), 1);
   }
   # do_pop_viewport: by default calls grid::popViewport()
   if (FALSE %in% do_viewport) {
      do_pop_viewport <- FALSE
   }
   if (length(do_pop_viewport) == 0) {
      do_pop_viewport <- TRUE;
   } else {
      do_pop_viewport <- head(as.logical(do_pop_viewport), 1);
   }
   # render_vectorized: whether to draw polygons altogether, then borders later
   render_vectorized <- FALSE;
   if (length(render_vectorized) == 0 || FALSE %in% render_vectorized) {
      render_vectorized <- FALSE;
   } else {
      render_vectorized <- head(as.logical(render_vectorized), 1);
      if (TRUE %in% verbose) {
         jamba::printDebug("plot.JamPolygon(): ",
            "Overriding render_vectorized:", render_vectorized);
      }
   }
   # linejoin: the inner/outer border method
   if (length(mitrelimit) == 0) {
      mitrelimit <- -20;
   } else {
      mitrelimit <- as.numeric(dots$mitrelimit);
   }
   # show_labels: whether to render label for each polygon
   if (length(show_labels) == 0) {
      show_labels <- TRUE;
   } else {
      show_labels <- head(as.logical(show_labels), 1)
   }
   if (TRUE %in% show_labels) {
      # add label coordinates when missing
      if (!all(c("label_x", "label_y") %in% colnames(x@polygons))) {
         x <- labelr_JamPolygon(x, add_to_jp=TRUE);
      }
   }

   ################################################
   # Todo:
   # - permit dots values to override other graphical parameters
   # - for now we assume all are defined via colnames(jp@polygons)
   opt_args <- c("fill",
      "label",
      "outerborder",
      "outerborder.lwd",
      "outerborder.lty",
      "border",
      "border.lwd",
      "border.lty",
      "innerborder",
      "innerborder.lwd",
      "innerborder.lty",
      "family",
      "fontsize");
   use_opt_args <- intersect(names(dots), opt_args);
   for (opt_arg in use_opt_args) {
      if (verbose) {
         jamba::printDebug("plot.JamPolygon(): ",
            "Overriding arg: ", opt_arg);
      }
      x@polygons[, opt_arg] <- unlist(dots[[opt_arg]]);
   }
   # print(x);
   
   # add polygon orientations
   x <- add_orientation_JamPolygon(x,
      flip_sign=flip_sign);
   # jamba::printDebug("plot.JamPolygon(): ", "x:");print(x);# debug
   
   # fill in some default values where missing
   if (!"border.lwd" %in% colnames(x@polygons)) {
      x@polygons$border.lwd <- 1;
   } else {
      x@polygons$border.lwd <- jamba::rmNA(x@polygons$border.lwd,
         naValue=0)
   }
   if (!"border.lty" %in% colnames(x@polygons)) {
      x@polygons$border.lty <- 1;
   }
   if (!"border" %in% colnames(x@polygons)) {
      if (verbose) {
         jamba::printDebug("plot.JamPolygon(): ",
            "Defining default border");
      }
      x@polygons$border <- "black";
   }
   if (!"fill" %in% colnames(x@polygons)) {
      x@polygons$fill <- NA;
   }
   
   # assemble tall format for coordinates
   df <- x@polygons;

   ## Reverse orientation of points where warranted
   # (Experimental)
   if (any(unlist(df$orientation) %in% c(1, -1))) {
      for (i in seq_len(nrow(df))) {
         idirs <- df$orientation[[i]];
         for (ipart in seq_along(idirs)) {
            idir <- idirs[[ipart]];
            if (-1 %in% idir) {
               # reverse the orientation for convenience later on
               df$x[[i]][[ipart]] <- rev(df$x[[i]][[ipart]]);
               df$y[[i]][[ipart]] <- rev(df$y[[i]][[ipart]]);
            }
         }
      }
   }
   # JamPolygon_to_grid_coords()
   row_lengths <- lapply(df$x, function(i){
      if (is.list(i)) {
         lengths(i)
      } else {
         length(i)
      }
   });
   polygon_lengths <- sapply(row_lengths, sum);
   polygon_defined <- (polygon_lengths > 0) * 1;
   coords_df <- data.frame(check.names=FALSE,
      x=unlist(df$x),
      y=unlist(df$y),
      pathId=rep(seq_along(polygon_lengths), polygon_lengths),
      id=rep(seq_along(unlist(row_lengths)), unlist(row_lengths))
   );

   if (length(xlim) > 0) {
      xrange <- range(xlim, na.rm=TRUE)
   } else {
      xrange <- range(coords_df$x, na.rm=TRUE);
   }
   xmid <- mean(xrange);
   xspan <- diff(xrange);
   if (length(ylim) > 0) {
      yrange <- range(ylim, na.rm=TRUE)
   } else {
      yrange <- range(coords_df$y, na.rm=TRUE);
   }
   if (verbose) {
      jamba::printDebug("plot.JamPolygon(): ",
         "xrange:", xrange, ", yrange:", yrange);
   }
   ymid <- mean(yrange);
   yspan <- diff(yrange);
   
   ## expand using buffer
   # maxspan <- max(c(xspan, yspan)) * (1 + buffer);
   #
   ## Todo: accept buffer on all four sides
   if (length(buffer) == 0) {
      buffer <- 0;
   }
   if (TRUE) {
      ## somewhat experimental method to use four-sided buffer
      buffer <- rep(buffer, length.out=4);
      xspan1 <- xspan * (1 + buffer[2] + buffer[4]);
      yspan1 <- yspan * (1 + buffer[1] + buffer[3]);
      maxspan <- max(c(xspan1, yspan1));
      adjx <- function(x){
         (x - xmid) / (maxspan) + 0.5 + (buffer[2] - buffer[4]) / 2;
      }
      adjy <- function(y){
         (y - ymid) / (maxspan) + 0.5 + (buffer[1] - buffer[3]) / 2
      }
   } else {
      ## fix to buffer to use only one value
      buffer <- max(buffer, na.rm=TRUE);
      maxspan <- max(c(xspan, yspan)) * (1 + buffer);
      adjx <- function(x){
         (x - xmid) / maxspan / (1 + buffer) + 0.5;
      }
      adjy <- function(y){
         (y - ymid) / maxspan / (1 + buffer) + 0.5;
      }
   }
   
   # assign plot features as attributes
   attr(x, "adjx") <- adjx;
   attr(x, "adjy") <- adjy;
   attr(x, "xrange") <- xrange;
   attr(x, "yrange") <- yrange;
   
   # grid rendering
   if (TRUE %in% do_newpage) {
      grid::grid.newpage();
   }
   
   use_vp <- grid::viewport(
      width=grid::unit(1, "snpc"),
      height=grid::unit(1, "snpc"),
      xscale=c(0, 1),
      yscale=c(0, 1));
   
   ## Note the viewport is included even when not used here
   attr(x, "viewport") <- use_vp;
   if (TRUE %in% do_viewport) {
      grid::pushViewport(use_vp);
   }
   
   grob_list <- list();
   # polygon fill - all polygons rendered at once
   if (TRUE %in% render_vectorized) {
      # note the border is not rendered here
      grobname <- paste0("vectorized", ":", "pathGrob"); # rowname:grob
      path_grob <- grid::pathGrob(
         rule="evenodd",
         x=adjx(coords_df$x),
         y=adjy(coords_df$y),
         pathId=coords_df$pathId,
         id=coords_df$id,
         name=grobname,
         vp=use_vp,
         gp=grid::gpar(
            fill=rep(df$fill, polygon_defined),
            lwd=rep(df$border.lwd, polygon_defined),
            lty=rep(df$border.lty, polygon_defined),
            col=NA)
            # col=rep(df$border, polygon_defined))
      )
      if (TRUE %in% do_draw) {
         grid::grid.draw(path_grob)
      }
      grob_list <- c(grob_list,
         setNames(list(path_grob), grobname));
   }
   
   # Split calls to render each polygon in series
   if (TRUE) {
      # use orientation for each polygon
      # and draw only the border on the appropriate side
      #
      # iterate each row
      if (debug) {
         jamba::printDebug("df:");print(df);# debug
      }
      for (irow in seq_len(length(x))) {
         irowname <- rownames(x@polygons)[irow];
         # jamba::printDebug("irow:", irow, ", irowname:", irowname);# debug
         # render each polygon
         if (TRUE) {
            if (verbose) {
               jamba::printDebug("Rendering polygon with gridGeometry: ", irow);
            }
            # first create the polygon path grob
            use_coords_df <- subset(coords_df, pathId %in% irow);
            if (nrow(use_coords_df) == 0 || all(is.na(use_coords_df$x))) {
               next;
            }
            # jamba::printDebug("use_coords_df:");print(use_coords_df);# debug
            
            bc <- df[["border"]][[irow]];
            bw <- jamba::rmNA(naValue=0,
               df[["border.lwd"]][[irow]])
            bt <- jamba::rmNA(naValue=0,
               df[["border.lty"]][[irow]]);
            # jamba::printDebug("bc:", bc, ", bw:", bw, ", bt:", bt);# debug
            fc <- df$fill[[irow]];
            has_border <- (length(bc) > 0 &&
                  length(bw) > 0 &&
                  !any(bc %in% c(NA, "")) &&
                  all(!is.na(bw) & bw > 0));
            # jamba::printDebug("df[irow,]:");print(df[irow, , drop=FALSE]);# debug
            # jamba::printDebug("irow: ", irow, ", fc:", fc, ", bc:", bc, ", bw:", bw);jamba::printDebugI(fc);# debug
            if (length(bt) == 0) {
               bt <- 1;
            }
            if (length(bc) == 0 || any(bc %in% c(NA, "")) ||
                  length(bw) == 0 || any(bw %in% c(NA) | bw <= 0)) {
               bw <- 1;
               bc <- NA;
            }
            shrunken <- NULL;
            obc <- df[["outerborder"]][[irow]];
            obw <- jamba::rmNA(naValue=0,
               df[["outerborder.lwd"]][[irow]] / 2);
            obt <- jamba::rmNA(naValue=0,
               df[["outerborder.lwd"]][[irow]]);
            has_outer <- (length(obc) > 0 &&
                  length(obw) > 0 &&
                  !any(obc %in% c(NA, "")) &&
                  all(obw > 0));
            ibc <- df[["innerborder"]][[irow]];
            ibw <- jamba::rmNA(naValue=0,
               df[["innerborder.lwd"]][[irow]] / 2);
            ibt <- jamba::rmNA(naValue=0,
               df[["innerborder.lwd"]][[irow]]);
            # jamba::printDebug("bw:", bw, ", ibw:", ibw, ", obw:", obw);# debug
            has_inner <- (length(ibc) > 0 &&
                  length(ibw) > 0 &&
                  !any(ibc %in% c(NA, "")) &&
                  all(ibw > 0));
            if (debug) {
               jamba::printDebug("ibc:", ibc, ", ibw:", ibw, ", ibt:", ibt, ", has_inner:", has_inner);# debug
            }
            if (!has_border) {
               if (has_inner) {
                  bw <- 0.5;
                  bc <- ibc;
               } else if (has_outer) {
                  bw <- 0.5;
                  bc <- obc;
               }
            }
            
            if (has_inner) {
               grobname <- paste0(irowname, ":", "pathGrob");
               path_grob <- grid::pathGrob(
                  rule="evenodd",
                  x=adjx(use_coords_df$x),
                  y=adjy(use_coords_df$y),
                  pathId=use_coords_df$pathId,
                  id=use_coords_df$id,
                  name=grobname,
                  vp=use_vp,
                  gp=grid::gpar(
                     fill=NA,
                     lwd=bw,
                     lty=bt,
                     col=bc))
                     # col=bc))
            } else {
               grobname <- paste0(irowname, ":", "pathGrob");
               path_grob <- grid::pathGrob(
                  rule="evenodd",
                  x=adjx(use_coords_df$x),
                  y=adjy(use_coords_df$y),
                  pathId=use_coords_df$pathId,
                  id=use_coords_df$id,
                  name=grobname,
                  vp=use_vp,
                  gp=grid::gpar(
                     fill=fc,
                     lwd=bw,
                     lty=bt,
                     col=bc))
                     # col=bc))
            }
            # check for inner border
            # - if so, shrink polygon, use difference as inner border
            if (has_inner) {
               # jamba::printDebug("ibc:", ibc, ", ibw:", ibw, ", ibt:", ibt, fgText=ibc);# debug
               shrunken_grob <- gridGeometry::polyoffsetGrob(
                  A=path_grob,
                  rule="evenodd",
                  delta=grid::unit(-(ibw), "mm"),
                  name=paste0(grobname, ":inner"),
                  gp=grid::gpar(
                     fill=fc,
                     lwd=bw,
                     lty=bt,
                     col=NA));
               # add the shrunken path_grob
               if (TRUE %in% do_draw) {
                  grid::grid.draw(shrunken_grob)
               }
               grob_list <- c(grob_list,
                  setNames(list(shrunken_grob), paste0(grobname, ":inner:0")));
               innerborder_grob <- gridGeometry::polyclipGrob(
                  A=path_grob,
                  B=shrunken_grob,
                  op="minus",
                  name=paste0(grobname, ":inner:1"),
                  gp=grid::gpar(
                     col=NA,
                     fill=ibc))
               # add the innerborder_grob
               if (TRUE %in% do_draw) {
                  grid::grid.draw(innerborder_grob)
               }
               grob_list <- c(grob_list,
                  setNames(list(innerborder_grob), paste0(grobname, ":inner:1")));
            }
            if (length(obc) > 0 &&
                  length(obw) > 0 &&
                  !any(obc %in% c(NA, "")) &&
                  all(obw > 0)) {
               # jamba::printDebug("obc:", obc, ", obw:", obw, ", obt:", obt, fgText=obc);# debug
               expanded_grob <- gridGeometry::polyoffsetGrob(
                  A=path_grob,
                  rule="evenodd",
                  delta=grid::unit(obw, "mm"),
                  name=paste0(grobname, ":outer:0"),
                  gp=grid::gpar(
                     fill="#FF0000",
                     col=NA))
               outerborder_grob <- gridGeometry::polyclipGrob(
                  A=expanded_grob,
                  B=path_grob,
                  op="minus",
                  name=paste0(grobname, ":outer:1"),
                  gp=grid::gpar(
                     col=NA,
                     fill=obc))
               # add the outerborder_grob
               if (TRUE %in% do_draw) {
                  grid::grid.draw(outerborder_grob)
               }
               grob_list <- c(grob_list,
                  setNames(list(outerborder_grob), paste0(grobname, ":outer:1")));
            }
            # add the original path_grob
            if (TRUE %in% do_draw) {
               grid::grid.draw(path_grob)
            }
            grob_list <- c(grob_list,
               setNames(list(path_grob), paste0(grobname)));
            # check for outer border
            # - if so, expand polygon, use difference as outer border
            # check for border (drawn last?)
            # - if so render only the edge with no color fill
            # assemble gTree?
         } else if (FALSE %in% render_vectorized) {
            if (verbose) {
               jamba::printDebug("Rendering polygon: ", irow);
            }
            use_coords_df <- subset(coords_df, pathId %in% irow);
            # 0.0.31.900 - when polygon is empty, skip
            if (all(is.na(use_coords_df$x))) {
               next;
            }
            if (nrow(use_coords_df) > 0) {
               # render one polygon without border
               # jamba::printDebug("grid.path data:");print(data.frame(x=adjx(use_coords_df$x),
               #    y=adjy(use_coords_df$y),
               #    pathId=use_coords_df$pathId,
               #    id=use_coords_df$id));# debug
               # grobname <- paste0("path.", irow); # grob:rownum
               grobname <- paste0(irowname, ":", "pathGrob"); # rowname:grob
               path_grob <- grid::pathGrob(
                  rule="evenodd",
                  x=adjx(use_coords_df$x),
                  y=adjy(use_coords_df$y),
                  pathId=use_coords_df$pathId,
                  id=use_coords_df$id,
                  name=grobname,
                  vp=use_vp,
                  gp=grid::gpar(
                     fill=df$fill[irow],
                     lwd=df$border.lwd[irow],
                     lty=df$border.lty[irow],
                     col=NA))
               # jamba::printDebugI("grobname:", grobname, ", df$fill[irow]", fgText=df$fill[irow]);# debug
               if (TRUE %in% do_draw) {
                  grid::grid.draw(path_grob)
               }
               grob_list <- c(grob_list,
                  setNames(list(path_grob), grobname));
            }
         }
         
      }
   }
   # Debug print grob_list
   # if (debug > 1) {
   #    jamba::printDebug("sdim(grob_list):");print(jamba::sdim(grob_list));# debug
   # }
   
   # optionally print labels
   # Todo: Determine whether labels should be rendered "per polygon"
   # as with borders above. Unclear if we want labels to be obscured
   # by overlapping polygons? For now, we decide not to obscure labels.
   if (TRUE %in% show_labels) {
      if (all(c("label_x", "label_y") %in% colnames(df))) {
         if (!"label" %in% colnames(df)) {
            df$label <- df$name;
         }
         # rows to label
         which_label <- which(!is.na(df$label_x) &
               !is.na(df$label) &
               nchar(df$label) > 0);
         if (length(which_label) > 0) {
            # label gpar
            label_gp <- c("fontsize",
               "family",
               "fontfamily",
               "label_color");
            if (any(label_gp %in% colnames(df))) {
               use_label_gp <- jamba::nameVector(
                  intersect(label_gp, colnames(df)));
               gp_values <- lapply(use_label_gp, function(igp){
                  ivalues <- df[[igp]][which_label];
                  ivalues;
               });
               if ("family" %in% names(gp_values) &&
                     !"fontfamily" %in% names(gp_values)) {
                  matchf <- match("family", names(gp_values));
                  names(gp_values)[matchf] <- "fontfamily";
               }
               names(gp_values) <- gsub("color", "col",
                  gsub("^label_", "",
                     names(gp_values)));
               ## if any duplicates, use the first occurrence (for now)
               if (any(duplicated(names(gp_values)))) {
                  matchgn <- match(unique(names(gp_values)),
                     names(gp_values));
                  gp_values <- gp_values[matchgn];
               }
               ## assemble into gpar object
               use_gp <- do.call(grid::gpar, gp_values);
               # print(use_gp);
            } else {
               use_gp <- grid::gpar();
            }
            # grobname <- "label";# old style
            grobname <- paste0(irowname, ":textGrob:",
               "labels"); # rowname:grob:labels
            text_grob <- grid::textGrob(
               x=adjx(unlist(df$label_x)[which_label]),
               y=adjy(unlist(df$label_y)[which_label]),
               label=df$label[which_label],
               name=grobname,
               vp=use_vp,
               gp=use_gp)
            if (TRUE %in% do_draw) {
               grid::grid.draw(text_grob);
            }
            grob_list <- c(grob_list,
               setNames(list(text_grob), grobname));
         }
      }
   }
   ## Convert to gTree
   attr(x, "grob_list") <- grob_list;
   grob_tree <- do.call(grid::grobTree,
      c(grob_list,
         alist(vp=use_vp,
            name="jps")));
   attr(x, "grob_tree") <- grob_tree;
   
   if (TRUE %in% do_pop_viewport) {
      grid::popViewport();
   }
   return(invisible(x));
}



# Todo:
# - Enhance rbind2 to keep all columns and not only keep the shared columns.
#   Useful and important to keep things like fill/border color.

#' Combine multiple JamPolygon objects
#' 
#' Combine multiple JamPolygon objects, given two JamPolygon or multiple
#' objects in a list.
#' 
#' This function is intended to support input as `rbind2(list(JamPolygons))`
#' or `do.call(rbind2.JamPolygon, list(JamPolygons))` with any
#' combination of one or more `JamPolygon` objects.
#' 
#' @family JamPolygon
#'
#' @docType methods
#' @rdname JamPolygon-methods
#' 
#' @param x,y `JamPolygon` object
#' @param ... additional `JamPolygon` objects if present
#' @export
rbind2.JamPolygon <- function
(x,
 y,
 ...)
{
   # convert everything to a list of dots
   dots <- list(...);
   if (!missing(y)) {
      if (is.list(y)) {
         dots <- c(y, dots);
      } else {
         dots <- c(list(y), dots);
      }
      y <- NULL;
   }
   if (!missing(x)) {
      if (is.list(x)) {
         dots <- c(x, dots);
      } else {
         dots <- c(list(x), dots);
      }
      x <- NULL;
   }
   use_dots <- sapply(dots, function(i){
      # ("JamPolygon" %in% class(i))
      inherits(i, "JamPolygon")
   })
   if (!any(use_dots)) {
      return(NULL)
   }
   use_dots <- which(use_dots)
   # Option to keep columns already in all JamPolygon objects
   if (FALSE) {
      use_colnames <- Reduce("intersect",
         lapply(dots[use_dots], function(i){
            colnames(i@polygons)}))
   } else {
      # Option to keep every column across all JamPolygon objects
      use_colnames <- Reduce("union",
         lapply(dots[use_dots], function(i){
            colnames(i@polygons)}))
   }
   # jamba::printDebug("use_colnames: ", use_colnames);# debug
   
   new_polygons <- dots[[head(use_dots, 1)]]@polygons;
   new_names <- names(dots[use_dots]);
   new_names[new_names %in% c("", NA)] <- "new";
   new_names <- jamba::makeNames(new_names,
      renameFirst=FALSE);

   if (any(use_dots)) {
      for (i in tail(use_dots, -1)) {
         match1 <- match(use_colnames, colnames(new_polygons));
         match2 <- match(use_colnames, colnames(dots[[i]]@polygons));
         df1 <- as.data.frame(check.names=FALSE,
            row.names=jamba::makeNames(new_polygons$name,
               renameFirst=FALSE),
            jamba::rmNULL(nullValue=NA,
               as.list(new_polygons)[match1]));
         colnames(df1) <- use_colnames;
         df2list <- jamba::rmNULL(nullValue=NA,
            as.list(dots[[i]]@polygons)[match2]);
         names(df2list) <- use_colnames;
         if ("name" %in% use_colnames) {
            df2 <- as.data.frame(check.names=FALSE,
               row.names=jamba::makeNames(df2list$name,
                  renameFirst=FALSE),
               df2list);
            # jamba::printDebug("df2:");print(df2);# debug
         } else {
            df2 <- as.data.frame(check.names=FALSE,
               row.names=as.character(seq_along(df2list[[1]])),
               df2list);
         }
         # if ("name" %in% colnames(df2)) {
         #    rownames(df2) <- df2[, "name"];
         # }
         colnames(df2) <- use_colnames;
         new_polygons <- rbind(df1, df2);
      }
   }
   x <- new("JamPolygon", polygons=new_polygons);
   # ensure names are unique
   names(x) <- jamba::makeNames(names(x),
      renameFirst=FALSE)
   rownames(x@polygons) <- names(x);
   
   # one step of validation to make sure certain columns are not empty
   # if ("label" %in% colnames(x@polygons)) {
   #    na_label <- is.na(x@polygons$label);
   #    if (any(na_label)) {
   #       x@polygons$label[na_label] <- x@polygons$label[na_label]
   #    }
   # }
   
   # jamba::printDebug("x:");print(x);# debug
   return(x)
}

#' Combine multiple JamPolygon objects
#'
#' @docType methods
#' @rdname JamPolygon-methods
#' @export
setMethod("rbind2",
   signature=c(x="JamPolygon", y="ANY"),
   definition=function(x, y, ...) {
      rbind2.JamPolygon(x, y, ...)
   }
)

## accessors
#
# names()
setMethod("names",
   signature=c(x="JamPolygon"),
   definition=function(x) {
      x@polygons$name;
   }
)
setMethod("names<-",
   signature=c(x="JamPolygon"),
   definition=function(x, value) {
      x@polygons$name <- value;
      # keep rownames in sync with names
      rownames(x@polygons) <- value;
      check_JamPolygon(x)
      x
   }
)

## descriptors
#
# nrow()
setGeneric("nrow", function(x) standardGeneric("nrow"))
setMethod("nrow",
   signature=c(x="JamPolygon"),
   definition=function(x) {
      nrow(x@polygons);
   }
)

# length()
# length.JamPolygon <- function
# (x)
# {
#    #
#    nrow(x@polygons)
# }
if (!isGeneric("length")) {
   setGeneric("length")
}
setMethod("length",
   signature=c(x="JamPolygon"),
   definition=function(x) {
      nrow(x@polygons);
   }
)
# lengths()
# - describes the number of internal polygons per polygon
if (!isGeneric("lengths")) {
   setGeneric("lengths")
}
setMethod("lengths",
   signature=c(x="JamPolygon", use.names="logical"),
   definition=function(x, use.names) {
      ilengths <- lapply(x@polygons$x, function(i) {
         if (is.list(i)) {
            length(i)
         } else {
            (length(i) > 0) * 1
         }
      })
      if (TRUE %in% use.names) {
         names(ilengths) <- names(x);
      }
      ilengths;
   }
)

setGeneric("ncol", function(x) standardGeneric("ncol"))
setMethod("ncol",
   signature=c(x="JamPolygon"),
   definition=function(x) {
      ncol(x@polygons);
   }
)

# Todo:
# - validate_JamPolygon()
#     - enable subsetting by row
#     - validate internal structure and content
# - plot.JamPolygon()
# - cbind.JamPolygon()

#' Calculate angle between three consecutive points
#' 
#' Calculate angle between three consecutive points
#' 
#' @family venndir geometry
#' 
#' @returns `numeric` angle defined by the three points, where
#'    the second point is considered the vertex.
#' 
#' @param x `matrix` with first two columns assumed to contain `x`, `y`
#'    coordinates.
#' 
#' @examples
#' x <- cbind(x=c(1, 1, 3), y=c(2, 1, 1))
#' three_point_angle(x)
#' 
#' @export
three_point_angle <- function
(x)
{
   # See:
   # https://stackoverflow.com/questions/1211212/how-to-calculate-an-angle-from-three-points
   if (nrow(x) < 3) {
      stop("x must contain 3 rows")
   }
   degrees <- jamba::rad2deg(
      atan2(
         y=x[3, 2] - x[2, 2],
         x=x[3, 1] - x[2, 1]) -
      atan2(
         y=x[2, 2] - x[1, 2],
         x=x[2, 1] - x[1, 1]));
   if (degrees > 180) {
      degrees <- degrees - 360;
   }
   return(degrees);
}

#' Define orientation for each polygon, clockwise or counterclockwise
#' 
#' Define orientation for each polygon, clockwise or counterclockwise
#' 
#' Todo:
#' * When polygons are nested, which is intended to create holes inside
#' one larger polygon, the hole should have its orientation flipped
#' so that the border is drawn on the opposite side per its orientation.
#' So the task is to determine whether a polygon is the hole of
#' another polygon.
#' 
#' @returns `JamPolygon` with column `"orientation"` added to
#'    slot "polygons".
#' 
#' @family JamPolygon
#' 
#' @param jp `JamPolygon`
#' @param flip_sign `integer` to indicate whether the orientation should
#'    be flipped, inverting the meaning of all returned values.
#'    For example, clockwise polygons by default are considered
#'    "solid", and counter-clockwise are considered "holes", for
#'    the purpose of interpreting nested polygons. When `flip_sign=-1`
#'    it reverses these assumptions.
#'    This option is experimental and intended mainly for internal
#'    testing.
#' @param include_holes,include_clockwise,include_parent `logical`
#'    indicating whether to include additional columns to describe
#'    multi-part polygons. The clockwise direction is used to define
#'    the inner and outer edge of the border, usually defined as
#'    left or right, in relation to the direction the line is drawn.
#'    Nested multi-part polygons define interior holes, and are distinguished
#'    from adjacent polygons.
#'    * `"polygon_clockwise"`: 1 clockwise, -1 counter-clockwise
#'    * `"polygon_holes"`: 1 is a solid polygon, -1 is a hole inside
#'    another polygon indicated with `"polygon_parent"`.
#'    * `"polygon_parent"`: integer index used to indicate the parent
#'    polygon for nested multi-part polygons. The index will indicate
#'    itself, when the polygon is not nested inside another polygon.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored
#' 
#' @examples
#' df3 <- data.frame(name=c("polygon1", "polygon2"),
#'    label=c("polygon1", "polygon2"),
#'    x=I(list(
#'       list(c(1, 6, 6, 1),
#'          c(2, 5, 5, 2),
#'          c(3, 4, 4, 3)),
#'       list(#c(11, 16, 16, 11),
#'          c(12, 15, 15, 12),
#'          c(13, 14, 14, 13))
#'       )),
#'    y=I(list(
#'       list(c(1, 1, 6, 6),
#'          c(2, 2, 5, 5),
#'          c(3, 3, 4, 4)),
#'       list(#c(1, 1, 6, 6),
#'          c(2, 2, 5, 5),
#'          c(3, 3, 4, 4))
#'       )),
#'    fill=c("gold", "firebrick"))
#' jp3 <- new("JamPolygon", polygons=df3);
#' plot(jp3);
#' 
#' add_orientation_JamPolygon(jp3,
#'    include_holes=TRUE,
#'    include_clockwise=TRUE,
#'    include_parent=TRUE)
#' 
#' @export
add_orientation_JamPolygon <- function
(jp,
 flip_sign=1,
 include_holes=FALSE,
 include_clockwise=FALSE,
 include_parent=FALSE,
 verbose=FALSE,
 ...)
{
   # validate flip_sign
   if (length(flip_sign) == 0) {
      flip_sign <- 1;
   }
   flip_sign <- head(flip_sign, 1);
   if (flip_sign >= 0) {
      flip_sign <- 1;
   } else {
      flip_sign <- -1;
   }
   # iterate each row
   orientations <- jamba::rbindList(lapply(seq_len(length(jp)), function(i){
      # jamba::printDebug("orientations jp:");print(jp);# debug
      unlistx <- unlist(jp@polygons$x[[i]]);
      if (length(jamba::rmNA(unlistx)) == 0) {
         # jamba::printDebug("orientations, empty polygon i:", i);# debug
         return(data.frame(polygon_clockwise=NA,
            orientation=NA,
            holes=NA,
            polygon_parent=NA))
      }
      use_x <- jp@polygons$x[[i]];
      use_y <- jp@polygons$y[[i]];
      if (!is.list(use_x)) {
         use_x <- list(use_x);
         use_y <- list(use_y);
      }
      # if (is.atomic(jp@polygons$x[[i]])) {
      #    use_x <- list(jp@polygons$x[[i]]);
      #    use_y <- list(jp@polygons$y[[i]]);
      # } else {
      # }
      
      if (verbose > 1) {
         jamba::printDebug("irow: ", i, " (", length(use_x), " parts)");
      }
      
      ## Determine orientation defined by clockwise/counterclockwise
      os <- sapply(seq_along(use_x), function(j){
         cdf <- data.frame(x=use_x[[j]],
            y=use_y[[j]],
            num=seq_along(use_x[[j]]))
         # First idea (not used currently):
         # # calculate area, positive is clockwise, negative is counterclockwise
         # c_area <- pracma::polyarea(cdf$x, cdf$y)
         #
         keynum <- head(jamba::mixedSortDF(cdf, byCols=c("y", "-x")), 1)$num;
         if (keynum == 1) {
            keynums <- c(nrow(cdf), keynum, keynum + 1);
         } else if (keynum == nrow(cdf)) {
            keynums <- c(keynum - 1, keynum, 1);
         } else {
            keynums <- c(keynum - 1, keynum, keynum + 1);
         }
         key_angle <- three_point_angle(cdf[keynums, 1:2]);
         # ifelse(key_angle > 0, "clockwise", "counterclockwise")
         ifelse(key_angle > 0, 1, -1)
      })
      if (verbose > 1) {
         jamba::printDebug("os:");
         print(os);
      }
      ## Bonus points: encode all polygons clockwise?
      #
      # Todo:
      # - verify what happens when polygon sub-parts are encoded counterclockwise
      # - for now, leave data as-is, which means we need to keep sign
      #   on pracma::polyarea() calculations
      #
      # if (any(os < 0)) {
      #    for (ik in which(os < 0)) {
      #       use_x[[ik]] <- rev(use_x[[ik]]);
      #       use_y[[ik]] <- rev(use_y[[ik]]);
      #       os[ik] <- os[ik] * -1;
      #    }
      # }

      # for this polygon row, iterate multipolygons to detect holes
      os_parent <- seq_along(use_x);
      if (length(use_x) == 1) {
         os_signs <- 1;
      } else {
         os_signs <- rep(1, length(use_x));
         for (isub in seq_along(use_x)) {
            if (verbose > 1) {
               jamba::printDebug("isub: ", isub, indent=3);
            }
            if (isub == 1) {
               os_signs[isub] <- 1;
            } else {
               for (iup in seq(from=isub - 1, to=1, by=-1)) {
                  if (verbose > 1) {
                     jamba::printDebug("iup: ", iup, indent=6);
                  }
                  # test whether polygon is fully inside the other polygon
                  area_sub <- abs(pracma::polyarea(x=use_x[[isub]],
                     y=use_y[[isub]]))
                  area_up <- abs(pracma::polyarea(x=use_x[[iup]],
                     y=use_y[[iup]]))
                  if (verbose > 1) {
                     jamba::printDebug("area_sub: ", area_sub, indent=9);
                     jamba::printDebug("area_up: ", area_up, indent=9);
                  }
                  testdf <- data.frame(name=c("sub", "up"),
                     x=I(list(use_x[[isub]], use_x[[iup]])),
                     y=I(list(use_y[[isub]], use_y[[iup]])))
                  # Todo: handle multiple distinct polygons returned
                  testjp <- union_JamPolygon(new("JamPolygon", polygons=testdf))
                  test_x <- testjp@polygons$x[[1]];
                  test_y <- testjp@polygons$y[[1]];
                  if (!is.list(test_x)) {
                     test_x <- list(test_x);
                     test_y <- list(test_y);
                  }
                  # sum the area of multipart non-overlapping polygons
                  area_unions <- abs(sapply(seq_along(test_x), function(ti){
                     pracma::polyarea(x=test_x[[ti]], y=test_y[[ti]])
                  }))
                  area_union <- sum(area_unions);
                  if (verbose) {
                     jamba::printDebug("area_unions: ", area_unions, indent=9);
                     jamba::printDebug("area_union: ", area_union, indent=9);
                  }
                  area_diff <- abs(area_up - area_union);
                  area_diff_pct <- area_diff / min(c(area_up, area_union)) * 100;
                  if (area_up == area_union || area_diff_pct < 1e-5) {
                     if (verbose > 1) {
                        jamba::printDebug("area_up == area_union",
                           indent=12, fgText="green");
                     }
                     # sub polygon is fully inside the parent polygon
                     os_signs[isub] <- os_signs[iup] * -1;
                     if (verbose > 1) {
                        jamba::printDebug("isub:", isub,
                           " is member of iup:", iup,
                           " with parent os_parent[iup]:", os_parent[iup]);
                     }
                     os_parent[isub] <- os_parent[iup];
                     if (verbose > 1) {
                        jamba::printDebug("os_signs[iup]:", os_signs[iup], indent=12)
                        jamba::printDebug("os_signs[isub]:", os_signs[isub], indent=12)
                     }
                     break;
                  } else {
                     if (verbose > 1) {
                        jamba::printDebug("", "area_up != area_union, diff:",
                           (area_up - area_union),
                           ", area_diff_pct:", area_diff_pct,
                           indent=12, fgText="firebrick");
                     }
                     os_signs[isub] <- 1;
                  }
               }
            }
         }
      }
      if (verbose) {
         jamba::printDebug("os_signs: ", os_signs);
         jamba::printDebug("os: ", os);
         jamba::printDebug("os_parent: ", os_parent);
      }
      use_os <- lapply(seq_along(os), function(k){
         os[[k]] * os_signs[[k]] * flip_sign;
      })
      osdf <- data.frame(
         polygon_clockwise=I(list(os)),
         orientation=I(list(use_os)),
         holes=I(list(os_signs)),
         polygon_parent=I(list(os_parent)));
      if (verbose) {
         print(osdf);
      }
      osdf;
   }));
   # combine into one sign
   jp@polygons[, "orientation"] <- I(list(orientations$orientation));
   if (TRUE %in% include_holes) {
      jp@polygons[, "polygon_holes"] <- I(list(orientations$holes));
   }
   if (TRUE %in% include_clockwise) {
      jp@polygons[, "polygon_clockwise"] <- I(list(orientations$polygon_clockwise));
   }
   if (TRUE %in% include_parent) {
      jp@polygons[, "polygon_parent"] <- I(list(orientations$polygon_parent));
   }
   jp;
}


#' Bounding box for JamPolygon
#' 
#' @param jp `JamPolygon` object
#' @param ... additional arguments are ignored.
#' 
#' @family JamPolygon
#' 
#' @export
bbox_JamPolygon <- function
(jp,
 ...)
{
   #
   n <- length(jp);
   xvals <- unlist(jp@polygons[, "x"]);
   yvals <- unlist(jp@polygons[, "y"]);
   xrange <- range(xvals, na.rm=TRUE);
   yrange <- range(yvals, na.rm=TRUE);
   bbox_m <- rbind(xrange, yrange);
   rownames(bbox_m) <- c("x", "y");
   colnames(bbox_m) <- c("min", "max");
   return(bbox_m);
}

#' Area for each polygon in JamPolygon
#' 
#' Area for each polygon in JamPolygon
#' 
#' This function calculates the area of each individual polygon (row)
#' represented in a `JamPolygon` object. It calculates total area
#' of multi-part polygons, which by default calculates the positive area
#' of polygons, and subtracts area for any holes. The orientation defines
#' the presence of holes using `add_orientation_JamPolygon()`.
#' 
#' For example, it is possible to have three concentric circles as a
#' three-part polygon. By convention, the first polygon defines the
#' outer border, the second polygon defines an inner border hole), and
#' the third polygon defines a nested internal polygon.
#' 
#' Todo:
#' * Consider simplifying polygons beforehand, to guarantee that no
#' multi-part polygons contain overlapping sections. Currently this
#' function assumes the input polygons are already simplified, such
#' that multi-part polygons on one row of `jp@polygons` do not contain
#' partially overlapping polygons, instead polygons are either adjacent,
#' fully overlapping (holes), or fully disconnected.
#'
#' @family JamPolygon
#'
#' @returns `numeric` vector with the area of each polygon, one value
#'    per row in `jp@polygons`.
#'    Or when `return_list=TRUE` a `list` where each element is produced
#'    per row of `jp@polygons`, containing a `numeric` vector with the area
#'    of each polygon part. Note that holes are represented with negative
#'    area multiplied by the orientation of the parent encompassing polygon.
#' 
#' @param jp `JamPolygon`
#' @param apply_abs `logical` (default FALSE) indicating whether to use the
#'    absolute value of the area prior to applying the orientation.
#'    * When FALSE, it uses the orientation of the points, where clockwise
#'    indicates positive, and counter-clockwise indicates negative.
#'    * When TRUE it calculates absolute area, and flips the sign based
#'    upon the orientation from `add_orientation_JamPolygon()`, which
#'    itself defines orientation based upon (1) the clockwise nature of points,
#'    and (2) whether the polygon is inside a parent polygon.
#' @param flip_sign `integer` indicating whether to flip the sign
#'    of the orientation, and is passed to `add_orientation_JamPolygon()`.
#' @param return_list `logical` indicating whether to return a `list`
#'    with area of each polygon part, one `numeric` vector per row
#'    in `jp@polygons`. Note that this step does not indicate which
#'    polygon hole is encompassed inside another larger polygon, so it
#'    cannot directly be used to determine the largest polygon in complex
#'    nested polygon structures.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' df3 <- data.frame(name=c("polygon1", "polygon2"),
#'    label=c("polygon1", "polygon2"),
#'    x=I(list(
#'       list(c(1, 6, 6, 1),
#'          c(2, 5, 5, 2),
#'          c(3, 4, 4, 3)),
#'       list(#c(11, 16, 16, 11),
#'          c(12, 15, 15, 12),
#'          c(13, 14, 14, 13))
#'       )),
#'    y=I(list(
#'       list(c(1, 1, 6, 6),
#'          c(2, 2, 5, 5),
#'          c(3, 3, 4, 4)),
#'       list(#c(1, 1, 6, 6),
#'          c(2, 2, 5, 5),
#'          c(3, 3, 4, 4))
#'       )),
#'    fill=c("gold", "firebrick"))
#' jp3 <- new("JamPolygon", polygons=df3);
#' plot(jp3);
#' 
#' area_JamPolygon(jp3)
#'
#' jp3@polygons$label <- paste0(names(jp3),
#'    "\narea=", area_JamPolygon(jp3));
#' plot(jp3)
#' 
#' area_JamPolygon(jp3, return_list=TRUE)
#' 
#' @export
area_JamPolygon <- function
(jp,
 apply_abs=FALSE,
 flip_sign=1,
 return_list=FALSE,
 verbose=FALSE,
 ...)
{
   # Logic is to iterate each row (polygon or multipolygon)
   # calculate area of each polygon, multiply by orientation
   # (polygon or hole)
   
   # add orientation (sign) to each polygon part
   # jamba::printDebug("area_JamPolygon(): ", "jp");print(jp);# debug
   jp <- add_orientation_JamPolygon(jp,
      flip_sign=flip_sign,
      ...)
   if (verbose) {
      jamba::printDebug("area_JamPolygon(): (with orientation)");
      print(jp);
   }

   # iterate each row (polygon)   
   jp_areas <- lapply(seq_len(length(jp)), function(irow){
      use_x <- jp@polygons$x[[irow]];
      use_y <- jp@polygons$y[[irow]];
      # jamba::printDebug("area_JamPolygon(): ", "use_x");print(use_x);# debug
      if (length(jamba::rmNA(unlist(use_x))) == 0) {
         return(0);
      }
      use_orientations <- jp@polygons$orientation[[irow]]
      if (verbose) {
         jamba::printDebug("area_JamPolygon(): ",
            "use_orientations: ");
         print(use_orientations);
      }
      if (!is.list(use_x)) {
         use_x <- list(use_x);
         use_y <- list(use_y);
      }
      iareas <- unlist(lapply(seq_along(use_x), function(ipart){
         ix <- use_x[[ipart]];
         iy <- use_y[[ipart]];
         if (length(jamba::rmNA(unlist(ix))) == 0) {
            return(0)
         }
         iarea <- pracma::polyarea(x=ix, y=iy)
         if (TRUE %in% apply_abs) {
            iarea <- abs(iarea);
         }
         if (verbose) {
            jamba::printDebug("area_JamPolygon(): ",
               "use_orientations[[ipart]]: ");
            print(use_orientations[[ipart]]);
         }
         iarea * use_orientations[[ipart]]
      }))
      # jamba::printDebug("polygon areas: ", iareas, sep=" + ");
      if (TRUE %in% return_list) {
         iareas
      } else {
         sum(iareas)
      }
   });
   if (!TRUE %in% return_list) {
      jp_areas <- unlist(jp_areas);
   }
   return(jp_areas);
}

#' Determine if a point is inside a JamPolygon
#' 
#' @family JamPolygon
#' 
#' @returns `list` with `integer` index values representing any
#'    polygons in `jp` which contain each point in `x`.
#'    When there is no overlapping polygon, `nullValue` is returned.
#' 
#' @param x `list` with `"x"`,`"y"` with `numeric` values, representing
#'    one or more points.
#' @param jp `JamPolygon`
#' @param apply_holes `logical` (default TRUE) whether internal polygon
#'    holes are applied, which requires a point in a solid portion of
#'    the polygon.
#'    When `apply_holes=FALSE` a point must only be contained within the
#'    outer boundary of the polygon.
#' @param nullValue default `NA` to define the return value when there
#'    is no overlapping polygon for a given point.
#'    Another alternative is `nullValue=NULL` which has the benefit of
#'    returning a vector with length 0, convenient to test for presence
#'    of any overlaps. In fact, this option is used by
#'    `has_point_in_JamPolygon()` for convenience.
#' @param ... additional arguments are ignored.
#' 
#' @export
point_in_JamPolygon <- function
(x,
 jp,
 apply_holes=TRUE,
 nullValue=NA,
 verbose=FALSE,
 ...)
{
   #
   if (is.list(x)) {
      pt_x <- x[["x"]];
      pt_y <- x[["y"]];
   }
   if (length(names(pt_x)) == 0) {
      names(pt_x) <- paste0("point", as.character(seq_along(pt_x)));
   }

   # get jp hole information
   jp <- add_orientation_JamPolygon(jp, include_holes=TRUE);
   if (FALSE %in% apply_holes) {
      jp@polygons$polygon_holes <- I(lapply(jp@polygons$polygon_holes, function(i){
         abs(i)
      }));
   }
   # jamba::printDebug("point_in_JamPolygon(): ", "jp:");print(jp);# debug
   
   # iterate each row in jp@polygons
   olm1 <- do.call(cbind, lapply(jamba::nameVector(seq_len(length(jp))), function(irow){
      # iterate each sub-polygon
      use_x <- jp@polygons$x[[irow]];
      use_y <- jp@polygons$y[[irow]];
      if (!is.list(use_x)) {
         use_x <- list(use_x);
         use_y <- list(use_y);
      }
      # jamba::printDebug("irow (", irow, ") holes:");print(jp[irow, ]@polygons$polygon_holes);
      # iterate each polygon component part
      olm <- do.call(cbind, lapply(jamba::nameVector(seq_along(use_x)), function(ipart){
         pip <- polyclip::pointinpolygon(P=list(x=pt_x, y=pt_y),
            A=list(x=use_x[[ipart]], y=use_y[[ipart]]))
         pip * jp@polygons$polygon_holes[[irow]][[ipart]];
      }))
      colnames(olm) <- jamba::makeNames(rep(names(jp)[irow], ncol(olm)))
      (rowSums(olm) > 0) * 1
   }))
   colnames(olm1) <- names(jp);
   rownames(olm1) <- names(pt_x);
   names(dimnames(olm1)) <- c("points", "polygons");
   
   if (verbose) {
      jamba::printDebug("olm1:");
      print(olm1);
   }
   ol <- lapply(seq_len(nrow(olm1)), function(i){
      iwhich <- jamba::rmNULL(nullValue=nullValue,
         which(olm1[i, ] > 0))
   })
   # ol <- apply(olm1, 1, function(i){
   #    iwhich <- jamba::rmNULL(nullValue=NA,
   #       which(i > 0))
   # })
   return(ol)
}

#' Determine if a point is inside any JamPolygon
#' 
#' @family JamPolygon
#' 
#' @returns `logical` vector with one result per point `x`, where
#'    * `TRUE` indicates the point overlaps at least one polygon,
#'    * `FALSE` indicates the point does not overlap any polygon.
#'    * Note that a point contained in a polygon "hole" is expected
#'    to return `FALSE` when `apply_holes=TRUE` (default).
#' 
#' @param x `list` with `"x"`,`"y"` with `numeric` values, representing
#'    one or more points.
#' @param jp `JamPolygon`
#' @param apply_holes `logical` (default TRUE) whether internal polygon
#'    holes are applied, which requires a point in a solid portion of
#'    the polygon.
#'    When `apply_holes=FALSE` a point must only be contained within the
#'    outer boundary of the polygon.
#' @param ... additional arguments are passed to `point_in_JamPolygon()`
#' 
#' @export
has_point_in_JamPolygon <- function
(x,
 jp,
 apply_holes=TRUE,
 ...)
{
   #
   # jamba::printDebug("has_point_in_JamPolygon(): ", "jp:");print(jp);# debug
   lengths(point_in_JamPolygon(x=x,
      jp=jp,
      apply_holes=apply_holes,
      nullValue=NULL,
      ...)) > 0
}

#' Split JamPolygon multipart polygons
#' 
#' Split JamPolygon multipart polygons into separate polygons, one
#' polygon per row in `jp@polygons`.
#' 
#' @family JamPolygon
#' 
#' @returns `JamPolygon` with one row per distinct polygon part in the
#'    input `jp` object.
#' 
#' @param jp `JamPolygon`
#' @param suffix `character` passed to `jamba::makeNames()` when creating
#'    new unique `names()` for the resulting polygons. The default `"_sub"`
#'    will create sub-parts with `"_sub1"`, `"_sub2"`, `"_sub3"`, and so on.
#' @param ... additional arguments are ignored.
#' 
#' @export
split_JamPolygon <- function
(jp,
 suffix="_sub",
 ...)
{
   #
   jpseq <- seq_len(length(jp))
   rowseq <- rep(jpseq, lengths(jp));
   new_coord_list <- unlist(recursive=FALSE, lapply(jpseq, function(irow){
      use_x <- jp@polygons$x[[irow]];
      use_y <- jp@polygons$y[[irow]];
      if (!is.list(use_x)) {
         use_x <- list(use_x);
         use_y <- list(use_y);
      }
      lapply(seq_along(use_x), function(ipart){
         list(x=use_x[[ipart]], y=use_y[[ipart]])
      });
   }))
   newseq <- seq_along(new_coord_list);
   new_x <- lapply(newseq, function(i){
      new_coord_list[[i]]$x;
   })
   new_y <- lapply(newseq, function(i){
      new_coord_list[[i]]$y;
   })
   
   new_jp <- jp;
   new_jp@polygons <- jp@polygons[rowseq, , drop=FALSE];
   names(new_jp) <- jamba::makeNames(names(new_jp), ...);
   # rownames(new_jp@polygons) <- names(new_jp);
   new_jp@polygons$x <- I(new_x);
   new_jp@polygons$y <- I(new_y);

   # optionally update other attributes
   new_jp <- update_JamPolygon(new_jp);
   
   return(new_jp);
}

#' Update attributes for a JamPolygon object
#' 
#' Update attributes for a JamPolygon object, including default label
#' position, and polygon orientations.
#' 
#' The default label position is defined with `"label_x","label_y"`
#' if these columns do not already exist in `jp@polygons`.
#' The coordinates are defined by `labelr_JamPolygon()` with
#' argument `add_to_jp=TRUE`.
#' 
#' If the column `"orientation"` does not exist in `jp@polygons`
#' it is added by calling `add_orientation_JamPolygon()`. It
#' subsequently adds columns `"holes"`, `"polygon_clockwise"`, and
#' `"polygon_parent"` when each is not already present.
#' 
#' @family JamPolygon
#'
#' @param jp `JamPolygon`
#' @param ... additional arguments are ignored.
#' 
#' @export
update_JamPolygon <- function
(jp,
 ...)
{
   #
   jpcols <- colnames(jp@polygons);
   if ("orientation" %in% jpcols) {
      jp <- add_orientation_JamPolygon(jp=jp,
         include_holes="holes" %in% jpcols,
         include_clockwise="polygon_clockwise" %in% jpcols,
         include_parent="polygon_parent" %in% jpcols)
   }
   # optionally update label_x,label_y
   if (all(c("label_x", "label_y") %in% colnames(jp@polygons))) {
      jp <- labelr_JamPolygon(jp=jp,
         add_to_jp=TRUE)
   }
   return(jp);
}

#' Convert polyclip polygon to JamPolygon
#' 
#' @family JamPolygon
#' 
#' @param A output from `polyclip` functions.
#' 
#' @examples
#' df <- data.frame(name=c("polygon1", "polygon2"),
#'    x=I(list(
#'       list(c(1, 4, 4, 1),
#'          c(2, 3, 3, 2)),
#'       c(5, 6, 6, 5))),
#'    y=I(list(
#'       list(c(1, 1, 4, 4),
#'          c(2, 2, 3, 3)),
#'       c(1, 1, 2, 2))),
#'    fill=c("gold", "firebrick"))
#' jpdf <- new("JamPolygon", polygons=df);
#' 
#' @export
polyclip_to_JamPolygon <- function
(A,
 ...)
{
   #
   if (all(c("x", "y") %in% names(A[[1]]))) {
      A <- list(A);
   }
   new_list_x <- lapply(seq_along(A), function(i){
      Ai <- A[[i]];
      new_x <- lapply(seq_along(Ai), function(j){
         Ai[[j]]$x
      })
   })
   new_list_y <- lapply(seq_along(A), function(i){
      Ai <- A[[i]];
      new_y <- lapply(seq_along(Ai), function(j){
         Ai[[j]]$y
      })
   })

   A_names <- names(A);
   if (length(A_names) == 0) {
      A_names <- as.character(seq_along(A));
   }
   df <- data.frame(name="new",
      x=I(new_list_x),
      y=I(new_list_y))
   new("JamPolygon", polygons=df);
}

#' Sample points within JamPolygon
#' 
#' Sample points within JamPolygon
#' 
#' This function arrays points across solid portions of polygons
#' provided in `jp`.
#' 
#' ## Todo:
#' 
#' 1. Enable polygon buffer to guarantee minimum spacing from borders.
#' 2. Allow different spatial patterns, currently square or rectangular.
#' In future, consider hexagonal, diamond, or diagonal.
#' 
#' @family JamPolygon
#' 
#' @examples
#' df3 <- data.frame(name=c("polygon1", "polygon2"),
#'    label=c("polygon1", "polygon2"),
#'    x=I(list(
#'       list(c(1, 6, 6, 1),
#'          c(2, 5, 5, 2),
#'          c(3, 4, 4, 3)),
#'       list(#c(11, 16, 16, 11),
#'          c(12, 15, 15, 12),
#'          c(13, 14, 14, 13))
#'       )),
#'    y=I(list(
#'       list(c(1, 1, 6, 6),
#'          c(2, 2, 5, 5),
#'          c(3, 3, 4, 4)),
#'       list(#c(1, 1, 6, 6),
#'          c(2, 2, 5, 5),
#'          c(3, 3, 4, 4))
#'       )),
#'    fill=c("gold", "firebrick"))
#' jp3 <- new("JamPolygon", polygons=df3);
#' 
#' sample_JamPolygon(jp3[1,], n=40, do_plot=TRUE)
#' sample_JamPolygon(jp3[1,], n=40, do_plot=TRUE, spread=TRUE)
#' sample_JamPolygon(jp3[1,], n=40, do_plot=TRUE, algorithm="seq")
#' sample_JamPolygon(jp3[1,], n=40, do_plot=TRUE, algorithm="seq", spread=TRUE)
#' 
#' sample_JamPolygon(jp3[1,], n=60, buffer=-0.3, spread=FALSE, do_plot=TRUE, xyratio=0.6)
#' sample_JamPolygon(jp3[1,], n=60, buffer=-0.3, spread=FALSE, do_plot=TRUE, xyratio=0.6, algorithm="seq")
#' 
#' sample_JamPolygon(jp3[1,], n=40, do_plot=TRUE, pattern="columns")
#' 
#' @param jp `JamPolygon`
#' @param n `integer` number of points required
#' @param xyratio `numeric` adjustment for the x/y ratio, numbers larger than
#'    1 make the x-axis spacing larger than the y-axis spacing.
#' @param spread `logical` (default `FALSE`) when more then `n` points are
#'    defined than the target number of points, `spread` indicates whether
#'    the subset of `n` points returned is defined using the first `n`
#'    points (spread=FALSE) or an even spread from start to end (spread=TRUE),
#'    * `spread=TRUE` can produce unusual distributions, with potential
#'    improvementwhen filling an irregular polygon.
#'    * `spread=FALSE` produces more "regular" placement of labels, also
#'    without gaps.
#' @param n_ratio `numeric` ratio which must be `1` or higher, default 1,
#'    how many total valid points should be defined before choosing
#'    a subset of points to use.
#'    * `n_ratio=1` - defines `n` points as closely as possible.
#'    * `n_ratio=2` - defines twice the points, then takes a subset
#'    to use, based upon argument `spread`. It may be beneficial when
#'    trying to fill an irregularly shaped polygon to use a higher
#'    `n_ratio`, thereby forcing the discovery of many more possible
#'    points. That said, the subset of points may not be "ideally"
#'    distributed relative to other labels, and relative to the polygon
#'    shape.
#' @param pattern `character` string indicating how to array the points:
#'    * `"offset"` (default) uses a rectangular grid where alternating
#'    points on each row are offset slightly on the y-axis.
#'    * `"columns"` uses a rectangular grid with points on each row
#'    that share the same y-axis value. Essentially the same as "offset"
#'    using offset=0.
#' @param buffer `numeric` optional buffer used to adjust the `jp` polygon
#'    size overall, where negative values will slightly shrink the
#'    polygon border. Points are sampled after this adjustment.
#' @param byCols `character` passed to `jamba::mixedSortDF()` to determine
#'    how to sort the resulting coordinates. Default `byCols=c("-y", "x")`
#'    sorts top-to-bottom, then left-to-right.
#' @param algorithm `character` string, default "split"
#'    * `"split"`: newer approach that starts with large step increases in `n`,
#'    then subdivides between failure/success to find the optimal final `n`.
#'    During testing it was substantially faster and more accurate than
#'    the previous algorithm `"seq"`.
#'    * `"seq"`: attempts a linear sequence of `n` values with gradual
#'    increases. It may be slightly more accurate, always finding the lowest
#'    value, at the expense of brute force speed.
#' @param do_plot `logical` indicating whether to create a plot to illustrate
#'    the process.
#' @param n_seq `numeric`, default NULL, used to provide a custom sequence
#'    of `n` values to attempt, used for internal testing but may be useful
#'    to provide exact values determined by an external approach.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#' 
#' @export
sample_JamPolygon <- function
(jp,
 n=100,
 xyratio=1.2,
 spread=FALSE,
 n_ratio=1,
 pattern=c("offset",
    "columns"),
 buffer=-0.2,
 width_buffer=0.1,
 max_width_buffer=10,
 byCols=c("-y", "x"),
 algorithm=c("split",
    "seq"),
 do_plot=FALSE,
 n_seq=NULL,
 verbose=FALSE,
 ...)
{
   # validate arguments
   pattern <- match.arg(pattern);
   algorithm <- match.arg(algorithm);
   
   # optional buffer
   if (length(buffer) == 0) {
      buffer <- 0;
   }
   buffer <- head(buffer, 1);
   max_buffer <- NULL;
   if (buffer != 0) {
      if (verbose) {
         jamba::printDebug("sample_JamPolygon(): ",
            "Applying buffer:", buffer);# debug
      }
      use_jp <- buffer_JamPolygon(jp,
         relative=TRUE,
         buffer=buffer,
         ...);
      max_buffer <- attr(use_jp, "max_buffer");
      # experimental width_buffer
      # slides the polygon right and left
      if (length(width_buffer) == 1 && width_buffer > 0 && width_buffer < 1) {
         if (verbose) {
            jamba::printDebug("sample_JamPolygon(): ",
               "detected max_buffer:", max_buffer);# debug
         }
         width_nudge <- jamba::noiseFloor(max_buffer * width_buffer,
            ceiling=max_width_buffer)
         use_jpr <- nudge_JamPolygon(use_jp,
            nudge=c(width_nudge, 0))
         use_jpl <- nudge_JamPolygon(use_jp,
            nudge=c(-width_nudge, 0))
         use_jpi <- intersect_JamPolygon(rbind2(use_jpr, use_jpl));
         area_ratio <- (area_JamPolygon(use_jpi) / area_JamPolygon(use_jp));
         area_ratio_threshold <- 0.3;
         if (verbose) {
            jamba::printDebug("sample_JamPolygon(): ",
               "detected max_buffer:", max_buffer,
               " * width_buffer:", width_buffer,
               ", ceiling uses max_width_buffer:", max_width_buffer)
            jamba::printDebug("sample_JamPolygon(): ",
               "resulting width_nudge:", width_nudge,
               ", area_ratio:", area_ratio,
               " (threshold > ", area_ratio_threshold, ")");# debug
         }
         if (area_JamPolygon(use_jpi) / area_JamPolygon(use_jp) >
               area_ratio_threshold) {
            use_jp <- use_jpi;
         }
      }
   } else {
      use_jp <- jp;
   }
   ## Todo: Fix error with use_jp has zero x,y coordinates
   # - bbox_JamPolygon() creates c(-Inf,Inf) bbox values
   # - jp has empty x,y coordinates
   # - buffer_JamPolygon() may create use_jp with empty x,y coordinates
   # jamba::printDebug("jp:");print(jp);# debug
   # jamba::printDebug("use_jp:");print(use_jp);# debug
   if (length(jamba::rmNA(unlist(jp@polygons$x))) == 0) {
      return(list(x=numeric(0),
         y=numeric(0)));
   }
   
   # bounding box
   xyrange <- bbox_JamPolygon(use_jp);
   
   # custom function to take n points from a list
   # limit_n_points(A, pip, n)
   limit_n_points <- function(A, pip=TRUE, n=1, spread=FALSE,
      verbose=FALSE, ...) {
      #
      Adf <- data.frame(x=A$x[pip],
         y=A$y[pip]);
      rowrank <- jamba::nameVector(rank(-unique(Adf$y)), unique(Adf$y));
      colrank <- jamba::nameVector(rank(unique(Adf$x)), unique(Adf$x));
      Adf$colnum <- colrank[as.character(Adf$x)]
      Adf$rownum <- rowrank[as.character(Adf$y)]
      if (TRUE %in% spread) {
         Adf <- jamba::mixedSortDF(Adf, byCols=c("colnum", "rownum"));
         row_seq <- round(seq(from=1, to=nrow(Adf), length.out=n))
         Adf$final_keep <- FALSE;
         Adf$final_keep[row_seq] <- TRUE;
         Adf_use <- subset(Adf, final_keep %in% TRUE);
         Adf_use <- jamba::mixedSortDF(Adf_use, byCols=c("rownum", "colnum"));
         A <- list(x=Adf_use$x, y=Adf_use$y)
         return(A)
         
      }
      Adf <- jamba::mixedSortDF(Adf, byCols=c("rownum", "colnum"));
      
      if (n >= nrow(Adf)) {
         A <- list(x=Adf$x, y=Adf$y)
         return(A)
      }
      
      # take the middle points
      hval <- (nrow(Adf) - n) / 2;
      hval1 <- floor(hval);
      hval2 <- ceiling(hval);
      pip_set <- seq_len(nrow(Adf));
      pip_seq <- head(pip_set, -hval2)
      if (hval1 > 0) {
         pip_seq <- tail(pip_seq, -hval1)
      }
      Adf$keep <- FALSE;
      Adf$keep[pip_seq] <- TRUE;
      
      # optionally take first rownum after evenly spacing labels
      Adf_sub <- subset(Adf, keep %in% TRUE);
      Adf_minrow <- min(Adf_sub$rownum)
      start_row <- match(Adf_minrow, Adf$rownum)
      row_seq <- seq(from=start_row, length=n);
      Adf$final_keep <- FALSE;
      Adf$final_keep[row_seq] <- TRUE;
      Adf_use <- subset(Adf, final_keep %in% TRUE);
      if (TRUE %in% verbose) {
         jamba::printDebug("Adf_minrow: ", Adf_minrow);# debug
         jamba::printDebug("Adf: ");print(Adf);# debug
      }

      A <- list(x=Adf_use$x, y=Adf_use$y)
      return(A)
   }
   
   # custom function to wrap some re-usable logic
   #
   # array_points() defines a grid of points covering the bounding box
   array_points <- function
   (xyrange,
    n=100,
    pattern="columns",
    verbose=FALSE,
    do_plot=FALSE,
    ...)
   {
      # calculate span
      xspan <- diff(xyrange[1, 1:2]);
      yspan <- diff(xyrange[2, 1:2]);
      # determine number of steps on each axis
      istep <- sqrt((xspan * yspan) / (n))
      # bonus points: allow different aspect ratio for x/y spacing
      xstep <- istep * xyratio;
      ystep <- istep / xyratio;
      if (verbose) {
         jamba::printDebug("sample_JamPolygon(): ",
            "array_points(): ",
            "n:", n,
            ", xstep:", xstep, ", ystep:", ystep);
      }
      # define x sequence
      xseq <- seq(from=xyrange[1, 1], to=xyrange[1, 2], by=xstep);
      xseq <- xseq - (tail(xseq, 1) - xyrange[1, 2]) / 2;
      
      # define y sequence
      yseq <- seq(from=xyrange[2, 1], to=xyrange[2, 2], by=ystep);
      yseq <- yseq - (tail(yseq, 1) - xyrange[2, 2]) / 2;
      yseq <- rev(yseq);
      
      # define point sequence with regular rectangular grid pattern
      xuse <- rep(xseq, length(yseq));
      yuse <- rep(yseq, each=length(xseq));
      
      # optional offset strategies:
      # - "offset"
      #    defines up/down placement, every other entry half-height up
      #
      # - "angled" (to be implemented)
      #    applies offset so each entry is progressively higher/lower
      #   
      if (length(xseq) > 1) {
         if (any(c("offset") %in% pattern)) {
            if (length(yseq) == 1) {
               ystep <- yspan;
            } else {
               ystep <- diff(head(yseq, 2));
            }
            if ("offset" %in% pattern) {
               # offset half-height between rows
               offset_height <- ystep / 2;
               # adjust by half this offset height
               yadjust <- offset_height / 2;
               offset_row <- rep(c(0, offset_height),
                  length.out=length(xseq));
            }
            # adjust point sequence
            yuse <- yuse - yadjust + offset_row;
            isna <- is.na(yuse);
            yuse <- yuse[!isna];
            xuse <- xuse[!isna];
         }
      }
      
      if (verbose) {
         jamba::printDebug("sample_JamPolygon(): ",
            "array_points(): ",
         "xrange:", range(xuse), ", yrange:", range(yuse));
      }
      list(x=xuse, y=yuse);
   }
   
   # range of values for n to attempt
   target_n <- (n * n_ratio);
   if ("seq" %in% algorithm) {
      if (verbose) {
         jamba::printDebug("sample_JamPolygon(): ",
            "Algorithm ", "'seq'");
      }
      if (length(n_seq) > 0 && any(n_seq >= n)) {
         # use n_seq as provided
      } else {
         n_seq <- unique(round(seq(from=n * 1, to=n * 50, by=ceiling(n / 50))));
      }
      if (verbose) {
         nseq_vals <- c(head(n_seq, 3), "...", tail(n_seq, 2))
         jamba::printDebug("sample_JamPolygon(): ",
            "n_seq:", nseq_vals, "(length ", length(n_seq), ")");
      }
      
      # define n points inside the polygon
      n_ratio <- head(n_ratio, 1);
      if (length(n_ratio) == 0 || any(n_ratio < 1)) {
         n_ratio <- 1;
      }
      if (!TRUE %in% spread) {
         n_ratio <- 1;
      }
      for (try_n in n_seq) {
         # jamba::printDebug("sample_JamPolygon(): ", "try_n:", try_n);# debug
         # jamba::printDebug("use_jp:");print(use_jp);# debug
         if (verbose) {
            jamba::printDebug("sample_JamPolygon(): ",
               "Iteration ", match(try_n, n_seq), ", try_n:", try_n);
         }
         A <- array_points(xyrange,
            n=try_n,
            pattern=pattern,
            verbose=verbose > 2);
         if (verbose) {
            jamba::printDebug("sample_JamPolygon(): ",
               "array_points() complete.");
         }
         pip <- has_point_in_JamPolygon(x=A,
            jp=use_jp);
         if (verbose > 1) {
            jamba::printDebug("sample_JamPolygon(): ",
               "has_point_in_JamPolygon() complete. ",
               "required n:", n,
               ", target_n:", target_n,
               ", requested points:", try_n,
               ", returned points:", length(A$x),
               ", usable points:", sum(pip));
         }
         # print(table(pip))
         if (sum(pip) >= (target_n)) {
            use_A <- limit_n_points(A=A,
               pip=pip,
               n=n,
               spread=spread,
               ...)
            break;
         }
      }
   } else if ("split" %in% algorithm) {
      if (verbose) {
         jamba::printDebug("sample_JamPolygon(): ",
            "Algorithm ", "'split'");
      }
      # 
      n_tolerance <- 2;
      try_n <- target_n;
      last_fail <- target_n - 1;
      last_success <- NA;
      last_piplist <- NULL;
      iteration <- 0;
      # iterate n values
      while (TRUE) {
         iteration <- iteration + 1;
         if (iteration > 100) {
            if (verbose) {
               jamba::printDebug("sample_JamPolygon(): ",
                  "Max Iteration ", iteration,
                  ", breaking loop.");
            }
            if (is.na(last_success)) {
               stop("No solution after 100 iterations.")
            }
            break;
         }
         if (verbose) {
            jamba::printDebug("sample_JamPolygon(): ",
               "Iteration ", iteration,
               # ", last_fail:", last_fail,
               # ", last_success:", last_success,
               ", try_n:", try_n)
         }
         A <- array_points(xyrange,
            n=try_n,
            pattern=pattern,
            verbose=verbose > 2);
         pip <- has_point_in_JamPolygon(x=A,
            jp=use_jp);
         n_pip <- sum(pip);
         if (verbose > 1) {
            jamba::printDebug("sample_JamPolygon(): ",
               indent=5,
               "has_point_in_JamPolygon() complete. ",
               "required n:", n,
               ", target_n:", target_n,
               ", requested points:", try_n,
               ", returned points:", length(A$x),
               ", usable points:", n_pip);
         }
         # check for success
         if (n_pip >= target_n) {
            # success, check difference from last fail
            if (abs(try_n - last_fail) > n_tolerance) {
               # split the difference and try again
               last_success <- try_n;
               last_piplist <- list(A=A, pip=pip);
               try_n <- floor(mean(c(last_success, last_fail)));
               if (verbose) {
                  jamba::printDebug("-> ",
                     indent=5, fgText=c("darkorange1", "royalblue"),
                     "Success, next try_n:", try_n);
               }
               next;
            }
            # success!
            if (verbose) {
               jamba::printDebug("-> ",
                  indent=5, fgText=c("darkorange1", "green"),
                  "Success, decision! try_n:", try_n);
            }
            last_piplist <- list(A=A, pip=pip);
            break;
         } else {
            last_fail <- try_n;
            if (is.na(last_success)) {
               try_n <- try_n + (target_n / 2)
            } else {
               if (abs(try_n - last_success) > n_tolerance) {
                  try_n <- floor(mean(c(last_success, last_fail)));
               } else {
                  # use last_success
                  try_n <- last_success;
                  if (verbose) {
                     jamba::printDebug("-> ",
                        indent=5, fgText=c("darkorange1", "green"),
                        "Fail, decision!    try_n:", try_n);
                  }
                  break;
               }
            }
            if (verbose) {
               jamba::printDebug("-> ",
                  indent=5, fgText=c("darkorange1", "red"),
                  "Fail,    next try_n:", try_n);
            }
            next;
         }
         #
      }
      # end while()
      A <- last_piplist$A;
      pip <- last_piplist$pip;
      # assume success
      if (sum(pip) >= n) {
         use_A <- limit_n_points(A=A,
            pip=pip,
            n=n,
            spread=spread,
            ...)
# 
#          pip_set <- which(pip);
#          if (TRUE %in% spread) {
#             
#             # evenly spaced by order
#             pip_seq <- pip_set[seq(from=1, to=length(pip_set), length.out=n)]
# 
#             pip[] <- FALSE;
#             pip[pip_seq] <- TRUE;
#          } else {
#             # first n points
#             pip_seq <- head(pip_set, n);
#             
#             # points in the middle
#             hval <- (length(pip_set) - n) / 2;
#             hval1 <- floor(hval);
#             hval2 <- ceiling(hval);
#             pip_seq <- tail(head(pip_set, -hval2), -hval1)
#             if (hval1 > 0) {
#                pip_seq <- tail(head(pip_set, -hval2), -hval1)
#             } else {
#                pip_seq <- head(pip_set, -hval2)
#             }
# 
#             pip[] <- FALSE;
#             pip[pip_seq] <- TRUE;
#          }
      }
   } else {
      stop(paste0("algorithm '", algorithm, "' is not yet implemented."))
   }
   
   # Adf <- data.frame(x=A$x[pip],
   #    y=A$y[pip]);
   # colrank <- jamba::nameVector(rank(unique(Adf$x)), unique(Adf$x));
   # rowrank <- jamba::nameVector(rank(unique(Adf$y)), unique(Adf$y));
   # Adf$colnum <- colrank[as.character(Adf$x)]
   # Adf$rownum <- rowrank[as.character(Adf$y)]
   # Adf <- jamba::mixedSortDF(Adf, byCols=c("rownum", "colnum"));
   # jamba::printDebug("head(Adf):");print(head(Adf));# debug
   
   # ptcol <- ifelse(pip, "gold", "grey")
   ptcol <- "gold";
   # ptcol[seq_along(ptcol) > n] <- "grey85";
   # ptpch <- ifelse(seq_along(ptcol) <= n, 20, 4);
   ptpch <- 20;
   
   # Todo: define label adjustment relative to leftmost/rightmost point
   # in contiguous sets.
   
   # optional plot
   if (2 %in% do_plot) {
      xyrange <- bbox_JamPolygon(jp);
      plot(NULL, type="n", xlim=xyrange[1,], ylim=xyrange[2,]);
      text(x=use_A$x,
         y=use_A$y,
         col=ptcol,
         label=seq_along(use_A$x));
   } else if (1 %in% do_plot) {
      xuse <- use_A$x;
      yuse <- use_A$y;
      # pip <- TRUE;
      #
      use_fill <- "dodgerblue";
      if (!identical(jp, use_jp)) {
         jp <- rbind2(jp, use_jp);
         use_fill <- c("dodgerblue", "#FFD70044")
      }
      jpo <- plot(jp,
         fill=use_fill,
         do_pop_viewport=FALSE);
      adjx <- attr(jpo, "adjx");
      adjy <- attr(jpo, "adjy");
      grid::grid.points(x=adjx(xuse),
         y=adjy(yuse),
         pch=ptpch,
         size=grid::unit(TRUE*4 + 2, "mm"),
         gp=grid::gpar(col=ptcol),
         default.units="snpc")
      grid::grid.text(x=0.5, y=0.45,
         label=paste0("n=", n,
            "\ntotal pts=", length(A$x),
            "\nusable n=", sum(pip),
            "\nfinal n=", length(use_A$x)),
         default.units="snpc")
      grid::popViewport();
   }
   Adf <- jamba::mixedSortDF(
      data.frame(x=use_A$x,
         y=use_A$y),
      byCols=byCols);
   # A <- list(x=A$x[pip], y=A$y[pip]);
   A <- list(x=Adf$x,
      y=Adf$y);
   return(invisible(A));
}

#' Apply buffer outside or inside JamPolygon
#' 
#' Apply buffer outside or inside JamPolygon
#' 
#' @family JamPolygon
#' 
#' @returns `JamPolygon` with one polygon, although the polygon could
#'    contain multiple disconnected parts.
#' 
#' @param jp `JamPolygon` with one or more polygons. When multiple polygons
#'    are provided, they are combined with `union_JamPolygon()` so that
#'    one overall buffer can be provided.
#' @param buffer `numeric` buffer, where negative values cause the polygon
#'    to be reduced in size.
#' @param steps `numeric` number of steps, default 200, used to
#'    determine relative unit sizes when `relative=TRUE` (which is default).
#' @param relative `logical` default `TRUE`, indicating whether to resize
#'    polygons using relative dimensions. Relative units are defined by
#'    the minimum negative buffer that results in non-zero area, where
#'    relative unit -1 would result in zero area.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' DEdf <- data.frame(check.names=FALSE,
#'    name=c("D", "E"),
#'    x=I(list(
#'       c(-3, 3, 3, 0, -3),
#'       c(-4, 2, 2, -4))),
#'    y=I(list(
#'    c(-3, -3, 1.5, 4, 1.5),
#'    c(-2, -2, 4, 4))),
#' fill=c("#FFD70055", "#B2222255"))
#' jp <- new("JamPolygon", polygons=DEdf)
#' plot(jp)
#'
#' jp2 <- nudge_JamPolygon(jp, nudge=list(D=c(10, 0)));
#' jp_jp2 <- rbind2(jp2, buffer_JamPolygon(jp2));
#' plot(jp_jp2,
#'    border.lty=c(1, 1, 2),
#'    fill=c(NA, NA, "gold"));
#' 
#' @export
buffer_JamPolygon <- function
(jp,
 buffer=-0.5,
 steps=20,
 relative=TRUE,
 verbose=FALSE,
 ...)
{
   #
   if (buffer == 0) {
      # return buffer=0 jp unchanged
      return(jp)
   }
   if (length(jamba::rmNA(unlist(jp@polygons$x))) == 0) {
      # return empty jp unchanged
      return(jp)
   }

   # combine multi-part polygons before calculating buffer
   if (length(jp) > 1) {
      jp <- union_JamPolygon(jp)
   }
   
   # add orientation so we can adjust clockwise direction if needed
   jp <- add_orientation_JamPolygon(jp,
      include_holes=TRUE,
      include_clockwise=TRUE);
   
   poly_list <- lapply(seq_len(length(jp)), function(irow){
      use_x <- jp@polygons$x[[irow]];
      use_y <- jp@polygons$y[[irow]];
      if (!is.list(use_x)) {
         use_x <- list(use_x);
         use_y <- list(use_y);
      }
      poly_list1 <- lapply(seq_along(use_x), function(i){
         is_hole <- jp@polygons$polygon_holes[[irow]][[i]];
         is_clockwise <- jp@polygons$polygon_clockwise[[irow]][[i]];
         j <- list(x=use_x[[i]], y=use_y[[i]])
         if (is_hole != is_clockwise) {
            # reverse direction so hole matches clockwise
            j[[1]] <- rev(j[[1]]);
            j[[2]] <- rev(j[[2]]);
         }
         j
      })
   })
   names(poly_list) <- names(jp);
   
   # apply buffer
   apply_buffer_poly_list <- function
   (poly_list,
    buffer,
    k=1,
    ...)
   {
      #
      buffer_polygon_list <- polyclip::polyoffset(
         poly_list[[k]],
         buffer,
         jointype="round")
      if (length(jamba::rmNA(unlist(buffer_polygon_list))) == 0) {
         return(NULL);
      }
      return(polyclip_to_JamPolygon(buffer_polygon_list));
   }
      
   # relative size
   bbox_jp <- bbox_JamPolygon(jp);
   bbox_max <- max(apply(bbox_jp, 1, diff))
   max_buffer <- NULL;
   if (TRUE %in% relative) {
      buffer_seq <- tail(seq(from=bbox_max,
         to=0,
         length.out=steps), -1)
      # jamba::printDebug("buffer_seq:", round(digits=2, buffer_seq));
      
      # iterate buffer widths to determine complete removal
      for (max_buffer in buffer_seq) {
         # jamba::printDebug("max_buffer:", max_buffer);
         new_jp <- apply_buffer_poly_list(poly_list,
            buffer=-max_buffer,
            k=1);
         if (length(new_jp) == 0) {
            next;
         }
         break;
      }
      buffer <- buffer * max_buffer;
      if (verbose) {
         jamba::printDebug("buffer_JamPolygon(): ",
            "max_buffer:", max_buffer);
         jamba::printDebug("buffer_JamPolygon(): ",
            "calculated buffer:",
            format(buffer, digits=3, big.mark=","));
      }
   }
   
   # apply this buffer
   if (verbose) {
      jamba::printDebug("buffer_JamPolygon(): ",
         "buffer:",
         format(buffer, digits=3, big.mark=","));
   }
   new_jp <- apply_buffer_poly_list(poly_list,
      buffer=buffer,
      k=1);
   attr(new_jp, "max_buffer") <- max_buffer;
   return(new_jp);
}


setReplaceMethod("[", "JamPolygon",
   function(x, i, j, ..., value) {
      if (missing(i))
         stop("subscript i is missing")
      if (!is.character(i) && !is.numeric(i))
         stop("invalid subscript type")
      if (length(i) < 1L)
         stop("attempt to select less than one element")
      if (!inherits(value, c("JamPolygon", "data.frame"))) {
         stop("replacement must be a JamPolygon or data.frame")
      }
      if (!length(value)) {
         names(value) <- NULL # instead of character()
      }
      if (inherits(value, "data.frame")) {
         x@polygons[i, ] <- value
      } else {
         x@polygons[i, ] <- value@polygons
      }
      x
   })
