
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
#' JamPolygon class
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
#' Todo:
#' * Consider re-factoring so that inner/outer borders are rendered
#' in proper order, immediately after each polygon is drawn.
#' This change means the polygons can no longer be rendered in
#' vectorized fashion, since each polygon should have the opportunity
#' to overlap existing polygons and their borders.
#' * Implement method to render inner and outer borders where defined.
#' Currently only the outer border is rendered.
#' * Consider disabling the thin black border by default.
#' 
#' @returns `JamPolygon` object, invisibly.
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
#' `grid::grid.path()`, `grid::grid.lines()`.
#' 
#' Rendering guidelines used by this function:
#' 
#' * Each polygon is rendered in order, and in series.
#' * All polygon labels are rendered afterward, so that labels
#' are not covered by subsequent polygons.
#' 
#' ## Rendering options recognized in `jp@polygons`:
#' 
#' * `name`, `label` - name and display label. A `label` of `NA` or `""`
#' will not be rendered.
#' * `label_color` - color used to render each polygon label.
#' * `family`, `fontsize` - font family, and font point size used to render
#' each polygon label.
#' * `x`, `y` - x- and y-coordinates to define each polygon or multipolygon.
#' * `fill` - polygon fill color, or `NA` for no fill color.
#' * `border`, `border.lwd` - border color and line width (outer border)
#' * `innerborder`, `innerborder.lwd` - inner border and line width
#' 
#' Todo:
#' 
#' 1. Enable arguments in `...` to override equivalent values in columns of
#' `jp@polygons`.
#' 2. Convert `grid` rendering to generate graphical objects (grobs)
#' which can be optionally rendered, or returned as a `gTree`.
#' 3. Continue debugging the `vwline` graphical glitches which are
#' apparent when rendering outer borders.
#' See [https://github.com/pmur002/vwline/issues/2].
#' 
#'    * Current recommendation is to render outer border after the inner
#'    border, and with outer border at least the same or larger width
#'    as the inner border. Otherwise, for acute angles, inner border may
#'    exceed the outer border because of its line width. However, if the
#'    outer border is drawn afterward, it will fully cover the inner border.
#'    With sufficiently small inner border width, the graphical glitch may
#'    not be apparent.
#' 4. Consider allowing labels for each multi-part polygon.
#' 5. Consider drawing optional x- and y-axis, although both could be added
#' using `grid` functions.
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
#' #jpz <- add_orientation_JamPolygon(jpz);
#' plot(jpz);
#' 
#' @param x `JamPolygon` object
#' @param y not used.
#' @param xlim,ylim `numeric` optionally used to define the x- and y-axis
#'    range to be rendered. When `NULL` they are defined using the
#'    observed range of values.
#' @param flip_sign `numeric` where `-1` or `TRUE` reverses the orientation of
#'    inner/outer border; or `1` keeps the orientation unchanged.
#' @param render_vectorized `logical` indicating whether to render all
#'    polygons in one call, thereby ignoring `innerborder` values. All
#'    `border` values are rendered as regular polygon borders. This option
#'    may be substantially faster for large collections of polygons.
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
#'    This option is only relevant when `do_viewport=TRUE`.
#'    This option is intended to allow layering multiple calls to this
#'    or other `grid` functions.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param debug `logical` (default FALSE) indicating whether to enable
#'    debug operations. When `debug=TRUE` it is also passed to `grid`
#'    functions such as `vwline::grid.vwline()` to display internal
#'    calculations in the graphical output.
#' @param ... additional arguments are recognized to customize plot features.
#' 
#' @export
plot.JamPolygon <- function
(x,
 y,
 xlim=NULL,
 ylim=NULL,
 flip_sign=1,
 render_vectorized=FALSE,
 linejoin=c("bevel",
    "mitre",
    "round"),
 mitrelimit=-20,
 show_labels=TRUE,
 buffer=0.05,
 do_newpage=TRUE,
 do_viewport=TRUE,
 do_pop_viewport=TRUE,
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
      if (flip_sign < 0) {
         jamba::printDebug("Handling flip_sign:", flip_sign);
      }
   }
   # grid options
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
   if (length(render_vectorized) == 0 || FALSE %in% render_vectorized) {
      render_vectorized <- FALSE;
   } else {
      render_vectorized <- head(as.logical(render_vectorized), 1);
      jamba::printDebug("Overriding render_vectorized:", render_vectorized);
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
      "border", "border.lwd", "border.lty",
      "innerborder", "innerborder.lwd", "innerborder.lty",
      "family", "fontsize");
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
   
   # fill in some default values where missing
   if (!"border.lwd" %in% colnames(x@polygons)) {
      x@polygons$border.lwd <- 1;
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
      jamba::printDebug("xrange:", xrange, ", yrange:", yrange);
   }
   ymid <- mean(yrange);
   yspan <- diff(yrange);
   
   ## expand using buffer
   # maxspan <- max(c(xspan, yspan)) * (1 + buffer);
   #
   ## fix to buffer to use only one value
   ## Todo: accept buffer on all four sides
   buffer <- max(buffer, na.rm=TRUE);
   maxspan <- max(c(xspan, yspan)) * (1 + buffer);
   # jamba::printDebug("xspan:", xspan, ", yspan:", yspan, ", maxspan:", maxspan, ", buffer:", buffer);# debug
      # (x - xrange[1]) / maxspan / 1.05 + 0.025;
      # (x - xmid) / maxspan / (1 + buffer) + (buffer / 4) + 0.5;
   adjx <- function(x){
      (x - xmid) / maxspan / (1 + buffer) + 0.5;
   }
      # (y - yrange[1]) / maxspan / 1.05 + 0.025;
      # (y - ymid) / maxspan / (1 + buffer) + (buffer / 4) + 0.5;
   adjy <- function(y){
      (y - ymid) / maxspan / (1 + buffer) + 0.5;
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
   if (TRUE %in% do_viewport) {
      use_vp <- grid::viewport(
         width=grid::unit(1, "snpc"),
         height=grid::unit(1, "snpc"),
         xscale=c(0, 1),
         yscale=c(0, 1));
      attr(x, "viewport") <- use_vp;
      grid::pushViewport(use_vp);
   }
   # polygon fill - all polygons rendered at once
   if (TRUE %in% render_vectorized) {
      # note the border is not rendered here
      grid::grid.path(rule="evenodd",
         x=adjx(coords_df$x),
         y=adjy(coords_df$y),
         pathId=coords_df$pathId,
         id=coords_df$id,
         gp=grid::gpar(
            fill=rep(df$fill, polygon_defined),
            lwd=rep(df$border.lwd, polygon_defined),
            lty=rep(df$border.lty, polygon_defined),
            col=NA)
            # col=rep(df$border, polygon_defined))
      )
   }
   
   # polygon inner border (experimental)
   # lwd_pts <- 5;
   # npts <- length(coords_df$x);
   # use_w <- vwline::widthSpec(list(
   #    left=grid::unit(rep(lwd_pts, npts), "pt"),
   #    right=grid::unit(rep(0, npts), "pt")), default.units="pt");

   # Split calls to render each polygon in series
   if (TRUE) {
      # use orientation for each polygon
      # and draw only the border on the appropriate side
      #
      # iterate each row
      for (irow in seq_len(length(x))) {
         # render each polygon
         if (FALSE %in% render_vectorized) {
            if (verbose) {
               jamba::printDebug("Rendering polygon: ", irow);
            }
            use_coords_df <- subset(coords_df, pathId %in% irow);
            if (nrow(use_coords_df) > 0) {
               # render one polygon without border
               grid::grid.path(rule="evenodd",
                  x=adjx(use_coords_df$x),
                  y=adjy(use_coords_df$y),
                  pathId=use_coords_df$pathId,
                  id=use_coords_df$id,
                  gp=grid::gpar(
                     fill=df$fill[irow],
                     lwd=df$border.lwd[irow],
                     lty=df$border.lty[irow],
                     col=NA))
            }
         }
         
         ########################################
         # render inner and outer borders
         for (border_type in c("inner", "outer")) {
            if ("outer" %in% border_type) {
               use_border <- x@polygons$border[[irow]];
               osign <- 1;
            } else {
               use_border <- x@polygons$innerborder[[irow]];
               osign <- -1;
            }
            if (length(use_border) == 0 || any(is.na(use_border))) {
               # skip rows with no border color
               next;
            }
            if (verbose) {
               jamba::printDebug("Rendering ", border_type, " border: ",
                  use_border, fgText=list("darkorange", use_border));
            }
            use_x <- x@polygons$x[[irow]];
            use_y <- x@polygons$y[[irow]];
            if (!is.list(use_x)) {
               use_x <- list(use_x);
               use_y <- list(use_y);
            }
            # iterate each polygon part
            for (ipart in seq_along(use_x)) {
               part_x <- use_x[[ipart]];
               part_y <- use_y[[ipart]];
               part_x <- c(tail(part_x, 1), part_x, head(part_x, 0));
               part_y <- c(tail(part_y, 1), part_y, head(part_y, 0));
               npts <- length(part_x);
               if (npts == 0) {
                  next;
               }
               part_orientation <- x@polygons$orientation[[irow]][[ipart]];
               if ("inner" %in% border_type) {
                  lwd_pts <- jamba::rmNULL(x@polygons$innerborder.lwd[[irow]],
                     nullValue=2);
                  lty_pts <- jamba::rmNULL(x@polygons$innerborder.lty[[irow]],
                     nullValue=1);
               } else {
                  lwd_pts <- jamba::rmNULL(x@polygons$border.lwd[[irow]],
                     nullValue=2);
                  lty_pts <- jamba::rmNULL(x@polygons$border.lty[[irow]],
                     nullValue=1);
               }
               # define line width at each point
               use_w <- vwline::widthSpec(list(
                  right=grid::unit(rep(
                     lwd_pts * ((osign * part_orientation) > 0),
                     # lwd_pts * (part_orientation != 0),
                     npts), "pt"),
                  left=grid::unit(rep(
                     lwd_pts * ((osign * part_orientation) < 0),
                     # lwd_pts * (part_orientation != 0),
                     npts), "pt")));
               # use_w <- grid::unit(rep(lwd_pts * (part_orientation != 0), npts), "pt");
               # render the border
               # vwline::grid.vwXspline(
               vwline::grid.vwline(
                  x=adjx(part_x),
                  y=adjy(part_y),
                  w=use_w,
                  open=FALSE,
                  stepWidth=TRUE,
                  mitrelimit=mitrelimit,
                  linejoin=linejoin,
                  lineend="butt",
                  debug=debug,
                  gp=grid::gpar(
                     fill=use_border,
                     col=NA,
                     lwd=1))
               
               if (TRUE) {
                  # consider whether to draw a thin border as below
                  grid::grid.path(x=adjx(part_x),
                     y=adjy(part_y),
                     rule="evenodd",
                     pathId=rep(1, length(part_x)),
                     id=rep(1, length(part_x)),
                     gp=grid::gpar(
                        fill=NA,
                        col="#00000066",
                        lty=lty_pts,
                        lwd=0.5));
               }
               if (FALSE) {
                  # for debug only, print "1" at first point in polygon
                  grid::grid.text(
                     x=head(adjx(part_x), 1),
                     y=head(adjy(part_y), 1),
                     label="1",
                     gp=grid::gpar(fontsize=8, color="black"))
               }
            }
         }
         # optional debug, return the polygon
         if (debug) {
            return(list(
               x=(adjx(part_x)),
               y=(adjy(part_y)),
               w=use_w,
               open=FALSE,
               linejoin=linejoin,
               gp=grid::gpar(
                  fill=use_border,
                  col=use_border,
                  lwd=1)))
         }
      }
   }
   
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
            label_gp <- c("fontsize", "family", "label_color");
            if (any(label_gp %in% colnames(df))) {
               use_label_gp <- jamba::nameVector(
                  intersect(label_gp, colnames(df)));
               gp_values <- lapply(use_label_gp, function(igp){
                  ivalues <- df[[igp]][which_label];
                  ivalues;
               })
               names(gp_values) <- gsub("color", "col",
                  gsub("^label_", "",
                     names(gp_values)));
               use_gp <- do.call(grid::gpar, gp_values);
               # print(use_gp);
            } else {
               use_gp <- grid::gpar();
            }
            grid::grid.text(
               x=adjx(unlist(df$label_x)[which_label]),
               y=adjy(unlist(df$label_y)[which_label]),
               label=df$label[which_label],
               gp=use_gp)
         }
      }
   }
   
   if (TRUE %in% do_pop_viewport) {
      grid::popViewport();
   }
   return(invisible(x));
}



# Todo:
# - Enhance rbind2 to keep all columns and not only keep the shared columns.
#   Useful and important to keep things like fill/border color.
setMethod("rbind2",
   signature=c(x="JamPolygon", y="JamPolygon"),
   definition=function(x, y, ...) {
      if (!all(colnames(x@polygons) == colnames(y@polygons))) {
         use_colnames <- intersect(colnames(x@polygons),
            colnames(y@polygons));
         new_polygons <- rbind(x@polygons[, use_colnames, drop=FALSE],
            y@polygons[, use_colnames, drop=FALSE]);
      } else {
         new_polygons <- rbind(x@polygons, y@polygons);
      }
      dots <- list(...);
      for (i in seq_along(dots)) {
         if (!all(colnames(new_polygons) == colnames(dots[[i]]@polygons))) {
            new_polygons <- rbind(new_polygons[, use_colnames, drop=FALSE],
               dots[[i]]@polygons[, use_colnames, drop=FALSE]);
         } else {
            new_polygons <- rbind(new_polygons, dots[[i]]@polygons);
         }
      }
      x@polygons <- new_polygons;
      x;
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
      check_JamPolygon(x)
      x
   }
)

## descriptors
#
# nrow()
if (!isGeneric("nrow")) {
   setGeneric("nrow", function(x) standardGeneric("nrow"))
}
setMethod("nrow",
   signature=c(x="JamPolygon"),
   definition=function(x) {
      nrow(x@polygons);
   }
)

# length()
length.JamPolygon <- function
(x)
{
   #
   nrow(x@polygons)
}
if (!isGeneric("length")) {
   # setGeneric("length", function(x) standardGeneric("length"))
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
   # setGeneric("lengths", function(x, use.names) standardGeneric("lengths"))
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
# ncol()
if (!isGeneric("ncol")) {
   # setGeneric("ncol", function(x) standardGeneric("ncol"))
   setGeneric("ncol")
}
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
#' @param x `matrix` with first two columns assumed to contain `x`, `y`
#'    coordinates.
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
#' @param jp `JamPolygon`
#' @param flip_sign `integer` to indicate whether the orientation should
#'    be flipped, inverting the meaning of all returned values.
#'    This argument is intended to be used when defining inner and outer
#'    borders alongside something like `vwline::grid.vwline()` which
#'    defines border on the left and right, as it travels from point
#'    to point. By using `flip_sign=-1` it will flip the right and
#'    left sides accordingly.
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
      if (length(unlist(jp@polygons$x[[i]])) == 0) {
         return(NA)
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
      if (length(unlist(use_x)) == 0) {
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
         if (length(unlist(ix)) == 0) {
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
   # jamba::printDebug("jp:");print(jp);
   
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
#'    `TRUE` indicates the point overlaps at least one polygon, and
#'    `FALSE` indicates the point does not overlap any polygon.
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
   lengths(point_in_JamPolygon(x=x,
      jp=jp,
      apply_holes=apply_holes,
      nullValue=NULL,
      ...)) > 0
}

#' Split JamPolygon multipart polygons
#' 
#' @family JamPolygon
#'
#' @param jp `JamPolygon`
#' @param suffix `character` passed to `jamba::makeNames()` when creating
#'    new unique `names()` for the resulting polygons.
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
   new_jp@polygons$x <- I(new_x);
   new_jp@polygons$y <- I(new_y);

   # optionally update other attributes
   new_jp <- update_JamPolygon(new_jp);
   
   return(new_jp);
}

#' Update attributes for a JamPolygon object
#' 
#' @family JamPolygon
#'
#' @params jp `JamPolygon`
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
#' sample_JamPolygon(jp3[1,], n=80, do_plot=TRUE)
#' 
#' sample_JamPolygon(jp3[1,], n=40, xyratio=1.5, do_plot=TRUE)
#' 
#' sample_JamPolygon(jp3[1,], n=40, xyratio=1/1.5, do_plot=TRUE)
#' 
#' @param jp `JamPolygon`
#' @param n `integer` number of points required
#' @param xyratio `numeric` adjustment for the x/y ratio, numbers larger than
#'    1 make the x-axis spacing larger than the y-axis spacing.
#' @param spread `logical` when more then `n` points can be fit inside
#'    `jp`, `spread=TRUE` spreads the points evenly across the available
#'    points, while `spread=FALSE` only takes the first `n` points.
#' @pattern `character` string indicating how to array the points:
#'    * `"offset"` (default) uses a rectangular grid where alternating
#'    points on each row are offset slightly on the y-axis.
#'    * `"rectangle"` uses a rectangular grid with points on each row
#'    that share the same y-axis value.
#' @pattern buffer `numeric` optional buffer used to adjust the `jp` polygon
#'    size overall, where negative values will slightly shrink the
#'    polygon border. Points are sampled after this adjustment.
#' @param byCols `character` passed to `jamba::mixedSortDF()` to determine
#'    how to sort the resulting coordinates. Default `byCols=c("-y", "x")`
#'    sorts top-to-bottom, then left-to-right.
#' @param do_plot `logical` indicating whether to create a plot to illustrate
#'    the process.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#' 
#' @export
sample_JamPolygon <- function
(jp,
 n=100,
 xyratio=1.1,
 spread=FALSE,
 pattern=c("offset",
    "rectangle"),
 buffer=0,
 byCols=c("-y", "x"),
 do_plot=FALSE,
 verbose=FALSE,
 ...)
{
   # validate arguments
   pattern <- match.arg(pattern);
   
   # optional buffer
   if (length(buffer) == 0) {
      buffer <- 0;
   }
   buffer <- head(buffer, 1);
   if (buffer != 0) {
      use_jp <- buffer_JamPolygon(jp,
         buffer=buffer);
   } else {
      use_jp <- jp;
   }
   
   # bounding box
   xyrange <- bbox_JamPolygon(use_jp);

   # custom function to wrap some re-usable logic
   array_points <- function
   (xyrange,
    n=100,
    pattern="rectangle",
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
      if ("triangle" %in% pattern) {
         ystep <- ystep * 2;
      }
      if (verbose) {
         jamba::printDebug("xstep:", xstep, ", ystep:", ystep);
      }
      # define x sequence
      xseq <- seq(from=xyrange[1, 1], to=xyrange[1, 2], by=xstep);
      xseq <- xseq - (tail(xseq, 1) - xyrange[1, 2]) / 2;
      # define y sequence
      yseq <- seq(from=xyrange[2, 1], to=xyrange[2, 2], by=ystep);
      yseq <- yseq - (tail(yseq, 1) - xyrange[2, 2]) / 2;
      yseq <- rev(yseq);
      
      # define point sequence
      xuse <- rep(xseq, length(yseq));
      yuse <- rep(yseq, each=length(xseq));
      
      # optional offsets
      if (length(xseq) > 1) {
         if (any(c("offset") %in% pattern)) {
            if (length(yseq) == 1) {
               ystep <- yspan;
            } else {
               ystep <- diff(head(yseq, 2));
            }
            if ("offset" %in% pattern) {
               # offset
               offset_height <- ystep / 2;
               yadjust <- offset_height / 2;
               offset_row <- rep(c(0, offset_height),
                  length.out=length(xseq));
            }
            # hexagonal (skip for now)
            offset_row2 <- rep(c(0, NA), length.out=length(xseq));
            yuse <- yuse - yadjust + offset_row;
            isna <- is.na(yuse);
            yuse <- yuse[!isna];
            xuse <- xuse[!isna];
         }
      }
      
      if (verbose) {
         jamba::printDebug("xrange:", range(xuse), ", yrange:", range(yuse));
      }
      list(x=xuse, y=yuse);
   }
   
   # range of values for n to attempt
   n_seq <- unique(round(seq(from=n * 1, to=n * 50, by=ceiling(n / 50))));
   # jamba::printDebug("sample_JamPolygon(): ",
   #    "n_seq: ", n_seq);
   
   # define n points inside the polygon
   for (try_n in n_seq) {
      A <- array_points(xyrange,
         n=try_n,
         pattern=pattern,
         verbose=verbose);
      pip <- has_point_in_JamPolygon(x=A,
         jp=use_jp);
      if (verbose > 1) {
         jamba::printDebug("try_n: ", try_n,
            ", returned_n:", length(A$x),
            ", usable n:", sum(pip));
      }
      # print(table(pip))
      if (sum(pip) >= n) {
         if (TRUE %in% spread) {
            pip_set <- which(pip);
            pip_seq <- pip_set[seq(from=1, to=length(pip_set), length.out=n)]
            pip[] <- FALSE;
            pip[pip_seq] <- TRUE;
         } else {
            pip_seq <- head(which(pip), n);
            pip[] <- FALSE;
            pip[pip_seq] <- TRUE;
         }
         break;
      }
   }
   
   ptcol <- ifelse(pip, "gold", "grey")
   # ptcol[seq_along(ptcol) > n] <- "grey85";
   # ptpch <- ifelse(seq_along(ptcol) <= n, 20, 4);
   ptpch <- 20;
   
   # optional plot
   if (2 %in% do_plot) {
      xyrange <- bbox_JamPolygon(jp);
      plot(NULL, type="n", xlim=xyrange[1,], ylim=xyrange[2,]);
      text(x=A$x, y=A$y, col=ptcol, label=seq_along(A$x));
   } else if (1 %in% do_plot) {
      xuse <- A$x;
      yuse <- A$y;
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
         size=grid::unit(pip*4 + 2, "mm"),
         gp=grid::gpar(col=ptcol),
         default.units="snpc")
      grid::grid.text(x=0.5, y=0.45,
         label=paste0("n=", n,
            "\ntotal pts=", length(xuse),
            "\nusable n=", sum(pip)),
         default.units="snpc")
      grid::popViewport();
   }
   Adf <- jamba::mixedSortDF(
      data.frame(x=A$x[pip],
         y=A$y[pip]),
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
#' @param jp `JamPolygon` with one or more polygons. When multiple polygons
#'    are provided, they are combined with `union_JamPolygon()` so that
#'    one overall buffer can be provided.
#' @param buffer `numeric` buffer, where negative values cause the polygon
#'    to be reduced in size.
#' 
#' @export
buffer_JamPolygon <- function
(jp,
 buffer=-0.5,
 steps=50,
 relative=TRUE,
 verbose=FALSE,
 ...)
{
   #
   if (buffer == 0) {
      # return buffer=0 jp unchanged
      return(jp)
   }
   if (length(unlist(jp@polygons$x)) == 0) {
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
         # union_polygon_list(polygon_list),
         buffer,
         jointype="round")
      if (length(unlist(buffer_polygon_list)) == 0) {
         return(NULL);
      }
      return(polyclip_to_JamPolygon(buffer_polygon_list));
   }
      
   # relative size
   bbox_jp <- bbox_JamPolygon(jp);
   bbox_max <- max(apply(bbox_jp, 1, diff))
   if (relative) {
      buffer_seq <- tail(seq(from=bbox_max,
         to=0,
         length.out=100), -1)
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
         # buffer_polygon_list2 <- polyclip::polyoffset(
         #    poly_list[[1]],
         #    # union_polygon_list(polygon_list),
         #    # -max_buffer,
         #    -0.05,
         #    jointype="round")
         # new_jp2 <- polyclip_to_JamPolygon(buffer_polygon_list2);
         if (FALSE) {
            plot(do.call(rbind2, list(jp, new_jp, new_jp2)),
               border=c("grey", "gold", "blue"), border.lwd=c(1, 3, 3),
               fill=c("firebrick", NA, "red1"))
         }
         if (length(buffer_polygon_list) > 0 &&
               sum(polygon_areas(buffer_polygon_list, simplify=TRUE)) > 0) {
            break;
         }
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
   return(new_jp);
}
