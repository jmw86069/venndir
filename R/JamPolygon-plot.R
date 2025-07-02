

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
#' 3. Consider allowing labels for each multi-part polygon.
#' Low priotity.
#' 4. Consider drawing optional x- and y-axis, although both could be added
#' using `grid` functions. Key option would be to transform `adjx()`,
#' `adjy()` to represent actual numeric values instead of `'snpc'`
#' coordinates.
#' Low priority.
#' 5. Consider using different approach than `'snpc'` to enforce aspect
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
#' @param do_plot_scale `logical` (default FALSE) whether to apply scaling
#'    so the effective x,y ranges are (0, 1). The default changed 0.0.57.900
#'    for more intuitive use of grid coordinates.
#'    
#'    Use TRUE for grobs to use 'snpc' units, at the expense
#'    of always keeping a square viewport open even when data
#'    may have a non-square 1:1 aspect ratio.
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
 do_plot_scale=FALSE,
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
      "label_color",
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
      if (TRUE %in% do_plot_scale) {
         adjx <- function(x){
            (x - xmid) / (maxspan) + 0.5 + (buffer[2] - buffer[4]) / 2;
         }
         adjy <- function(y){
            (y - ymid) / (maxspan) + 0.5 + (buffer[1] - buffer[3]) / 2
         }
      } else {
         # aspect1 <- xspan1 / yspan1;
         xspan1a <- xspan * (buffer[2]);
         xspan1b <- xspan * (buffer[4]);
         yspan1a <- yspan * (buffer[1]);
         yspan1b <- yspan * (buffer[3]);
         adjx <- function(x)x;
         adjy <- function(x)x;
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
   
   use_units <- "npc";
   if (TRUE %in% do_plot_scale) {
      use_vp <- grid::viewport(
         width=grid::unit(1, "snpc"),
         height=grid::unit(1, "snpc"),
         xscale=c(0, 1),
         yscale=c(0, 1));
   } else {
      # experimental option using respect=TRUE to maintain aspect ratio
      new_xscale <- c(-1, 1) * (xspan1 / 2) + mean(xrange);
      new_yscale <- c(-1, 1) * (yspan1 / 2) + mean(yrange);
      new_xscale <- xrange + c(-xspan1a, xspan1b);
      new_yscale <- yrange + c(-yspan1a, yspan1b);
      vl1 <- grid::grid.layout(nrow=1, ncol=1,
         just="center",
         widths=diff(new_xscale),
         heights=diff(new_yscale),
         respect=TRUE);
      vp1 <- grid::viewport(layout=vl1)
      vp2 <- grid::viewport(
         layout=vl1,
         layout.pos.row=1,
         layout.pos.col=1,
         width=1, height=1,
         xscale=new_xscale,
         yscale=new_yscale)
      use_vp <- grid::vpTree(parent=vp1, children=grid::vpList(vp2))
      use_units <- "native";
   }
   
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
         default.units=use_units, # 0.0.57.900
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
                  default.units=use_units, # 0.0.57.900
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
                  default.units=use_units, # 0.0.57.900
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
                  default.units=use_units, # 0.0.57.900
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
            # jamba::printDebug("gp_values:");print(gp_values);# debug
            text_grob <- grid::textGrob(
               x=adjx(unlist(df$label_x)[which_label]),
               y=adjy(unlist(df$label_y)[which_label]),
               label=df$label[which_label],
               name=grobname,
               vp=use_vp,
               default.units=use_units, # 0.0.57.900
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
