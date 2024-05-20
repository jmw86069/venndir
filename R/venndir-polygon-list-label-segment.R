

#' Define a polygon label line segment
#' 
#' Define a polygon label line segment
#' 
#' This function takes a line segment and polygon, and
#' returns the point where a line segment starting
#' at the source point touches the outer boundary of
#' the polygon.
#' 
#' This function is intended to be used with polygon
#' labels, where the original label position
#' is inside the polygon, but is manually adjusted
#' and may be placed somewhere outside the polygon.
#' This function finds a suitable point to draw a line
#' segment from the new label position to the polygon.
#' 
#' Some different situations are handled as follows,
#' use `verbose=TRUE` to see which situation occurred.
#' 
#' * When the starting point is outside the polygon,
#' and end point is inside the polygon, this function
#' finds the point where this line segment first
#' intersects the polygon boundary. This is the
#' driving reason for this function.
#' * When the starting point is outside the polygon,
#' and end point also outside the polygon, this function
#' finds the nearest the nearest point along
#' the polygon boundary.
#' * When the starting point is inside the polygon,
#' this function returns NULL.
#' 
#' Note that this function currently does not adjust for
#' the size or position of a label.
#' 
#' @return `numeric` matrix with one row, representing the
#'    point of intersection with the polygon boundary,
#'    see comments above for exceptions. When
#'    `return_class="matrix"` then the `matrix` contains
#'    two rows, where the second row contains the
#'    point of intersection. When `return_class="SpatialLines"`
#'    the object is `sp::SpatialLines` and represents the
#'    line from the first point and the point of intersection.
#'    Note that when there is no second point, the
#'    `sp::SpatialLines` object will only have one point.
#' 
#' @family venndir label
#' 
#' @examples
#' x0 <- 0;x1 <- 1;y0 <- 0; y1 <- 1;
#' sp <- polygon_ellipses(xcenter=1, ycenter=1, xradius=0.5, yradius=1)
#' par("xpd"=TRUE)
#' sl2 <- polygon_list_label_segment(x0, x1, y0, y1, sp,
#'    return_class="matrix",
#'    plot_debug=TRUE,
#'    main="segment drawn to boundary intersection");
#' text(x=sl2[1, 1], y=sl2[1, 2],
#'    labels="adjusted label",
#'    cex=2,
#'    adj=sl2[1, c("adjx", "adjy")])
#' 
#' # example of making a debug plot
#' lxy <- cbind(c(x0, x1), c(y0, y1));
#' sp <- polygon_ellipses(xcenter=1, ycenter=1, xradius=0.5, yradius=1);
#' plot_polygon_list(sp, col="#00007777", border="blue3", lwd=2,
#'    xlim=c(0, 2), ylim=c(0, 2), asp=1,
#'    main="segment drawn to boundary intersection");
#' plot_polygon_list(list(list(x=sl2[, "x"], y=sl2[, "y"])), border="red3", lwd=4, add=TRUE)
#' 
#' # example where the line intersects multiple boundaries
#' x0 <- 0;x1 <- 2;y0 <- 0; y1 <- 2;
#' sl2 <- polygon_list_label_segment(x0, x1, y0, y1, sp,
#'    return_class="matrix",
#'    plot_debug=TRUE,
#'    verbose=TRUE,
#'    main="segment drawn to first boundary intersection");
#' 
#' # example where starting line does not intersect
#' x0 <- 0;x1 <- 1;y0 <- 0; y1 <- 1;
#' sp <- polygon_ellipses(xcenter=2, ycenter=1, xradius=0.5, yradius=1);
#' sl2 <- polygon_list_label_segment(x0, x1, y0, y1, sp,
#'    return_class="matrix",
#'    plot_debug=TRUE,
#'    xlim=c(0, 3),
#'    main="segment drawn to nearest boundary point");
#' 
#' # example showing line fully inside the polygon
#' x0 <- 0;x1 <- 2;y0 <- 0; y1 <- 2;
#' sp <- polygon_ellipses(xcenter=1, ycenter=1, xradius=1.5, yradius=2.2);
#' sl2 <- polygon_list_label_segment(x0, x1, y0, y1, sp,
#'    return_class="matrix",
#'    plot_debug=TRUE,
#'    main="segment contained inside returns one point");
#' 
#' # example showing polygon with a hole inside
#' # TODO: Fix polyclip generating a polygon with hole inside
#' sp <- polygon_ellipses(xcenter=1, ycenter=1, xradius=0.5, yradius=1);
#' sp_hole <- polygon_ellipses(xcenter=1, ycenter=1, xradius=0.2, yradius=0.5);
#' plot_polygon_list(list(sp, sp_hole), col=c("red", "blue"))
#' sp_donut <- minus_polygon_list(c(sp, sp_hole));
#' plot_polygon_list(sp_donut, col=jamba::alpha2col("gold", 0.5))
#' sl2 <- polygon_list_label_segment(x0, x1, y0, y1, sp_donut,
#'    return_class="matrix",
#'    plot_debug=TRUE,
#'    main="segment drawn to first outer boundary intersection");
#' 
#' # example with line inside the polygon hole
#' x0 <- 0.9;x1 <- 1.1;y0 <- 0.9; y1 <- 1.1;
#' sp <- polygon_ellipses(xcenter=1, ycenter=1, xradius=0.5, yradius=1);
#' sp_hole <- polygon_ellipses(xcenter=1, ycenter=1, xradius=0.2, yradius=0.5);
#' sp_donut <- minus_polygon_list(sp, sp_hole);
#' sl2 <- polygon_list_label_segment(x0, x1, y0, y1, sp_donut,
#'    return_class="matrix",
#'    plot_debug=TRUE,
#'    main="segment drawn to nearest boundary");
#' 
#' # line crosses inside the polygon hole
#' x0 <- 1;x1 <- 1.4;y0 <- 1; y1 <- 1.4;
#' sp <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.5, yradius=1);
#' sp_hole <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.2, yradius=0.5);
#' sp_donut <- rgeos::gDifference(sp, sp_hole);
#' sl2 <- polygon_list_label_segment(x0, x1, y0, y1, sp_donut,
#'    return_class="matrix",
#'    plot_debug=TRUE,
#'    main="segment drawn to boundary intersection");
#' 
#' x0 <- 0.6;x1 <- 1;y0 <- 0.6; y1 <- 1;
#' sp <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.5, yradius=1);
#' sp_hole <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.2, yradius=0.5);
#' sp_donut <- rgeos::gDifference(sp, sp_hole);
#' sl2 <- polygon_list_label_segment(x0, x1, y0, y1, sp_donut,
#'    return_class="matrix",
#'    plot_debug=TRUE,
#'    main="first point inside the polygon, returns one point");
#' 
#' # example showing multiple input points
#' x0 <- c(0.6, 1.1);
#' x1 <- c(1, 1.4);
#' y0 <- c(0.6, 1.1);
#' y1 <- c(1, 1.4);
#' sl2 <- polygon_list_label_segment(x0, x1, y0, y1, sp=sp_donut,
#'    return_class="point", verbose=TRUE, plot_debug=TRUE); 
#' 
#' # example showing sp_buffer
#' sl2 <- polygon_list_label_segment(x0, x1, y0, y1, sp=sp_donut,
#'    sp_buffer=-0.1,
#'    return_class="point", verbose=TRUE, plot_debug=TRUE); 
#' 
#' @param x0 `numeric` x-axis source position
#' @param x1 `numeric` x-axis target position
#' @param y0 `numeric` y-axis source position
#' @param y1 `numeric` y-axis target position
#' @param polygon_list `list` with `"x"` and `"y"` elements
#' @param buffer `numeric` indicating an optional buffer to
#'    use for the `sp` polygon. By default `sp_buffer=0` uses no
#'    buffer, but a suggested buffer `sp_buffer=-0.01` would make
#'    the polygon `1%` smaller, therefore the line segment would be
#'    slightly inside the polygon border.
#' @param return_class `character` string
#'    * `"point"` returns a `matrix` with one row containing the new
#'    target point;
#'    * `"matrix"` contains two rows with source and new target points
#' @param relative `logical` indicating whether `sp_buffer` is scaled
#'    relative to the size of the polygon, see `get_sp_buffer()`
#'    for more details.
#' @param verbose `logical` indicating whether to print verbose output,
#'    specifically describing which situation occurred.
#' @param ... additional arguments are passed to `get_sp_buffer()`.
#' 
#' @export
polygon_list_label_segment <- function
(x0,
 x1,
 y0,
 y1,
 polygon_list,
 # sp,
 return_class=c("point", "matrix"),
 buffer=0,
 plot_debug=FALSE,
 relative=TRUE,
 verbose=FALSE,
 ...)
{
   return_class <- match.arg(return_class);
   if (length(x0) > 1) {
      i <- seq_along(x0);
      x1 <- rep(x1, length.out=length(x0));
      y0 <- rep(y0, length.out=length(x0));
      y1 <- rep(y1, length.out=length(x0));
      buffer <- rep(buffer, length.out=length(x0));
      polygon_listi <- ((i - 1) %% length(polygon_list)) + 1;
      listout <- lapply(i, function(j){
         if (verbose) {
            jamba::printDebug("polygon_label_segment(): ",
               "j:", j, ", polygon_listi[j]:", polygon_listi[j]);
         }
         polygon_list_use <- polygon_list[[polygon_listi[j]]];
         polygon_label_segment(x0=x0[j],
            x1=x1[j],
            y0=y0[j],
            y1=y1[j],
            polygon_list=polygon_list_use,
            buffer=buffer[[j]],
            return_class=return_class,
            plot_debug=plot_debug,
            verbose=verbose,
            add=(j > 1),
            ...);
      });
      if ("point" %in% return_class) {
         listout <- jamba::rbindList(listout);
      }
      return(listout);
   }
   
   lxy <- cbind(c(x0, x1), c(y0, y1));
   colnames(lxy) <- c("x", "y")
   line_xy <- list(x=c(x0, x1), y=c(y0, y1))
   # sl <- sp::SpatialLines(list(sp::Lines(list(sp::Line(lxy)), ID="a")))
   # spt <- sp::SpatialPoints(lxy);
   
   # optional debug plot
   if (TRUE %in% plot_debug) {
      # xlim <- range(c(sp::bbox(sp)[1,],
      #    lxy[,1]), 
      #    na.rm=TRUE);
      # ylim <- range(c(sp::bbox(sp)[2,], 
      #    lxy[,2]), 
      #    na.rm=TRUE);
      plot_polygon_list(polygon_list,
         col=jamba::alpha2col("blue2", alpha=0.2),
         border="blue2",
         lwd=3,
         # xlim=xlim,
         # ylim=ylim,
         lty="solid",
         ...)
      segments(x0=x0,
         x1=x1,
         y0=y0,
         y1=y1,
         col="blue4",
         lwd=2,
         lty="dotted",
         add=TRUE)
   }
   
   # optional polygon buffer
   buffer <- head(buffer, 1);
   # if (is.list(sp)) {
   #    sp <- sp[[1]];
   # }
   if (is.numeric(buffer) && !buffer %in% 0) {
      # buffer_polygon_list <- polyclip::polyoffset(
      #    union_polygon_list(polygon_list),
      #    buffer,
      #    jointype="round")

      buffer_polygon_list <- get_polygon_buffer(polygon_list,
         buffer=buffer,
         relative=relative,
         ...);
      # optional debug plot
      if (verbose) {
         jamba::printDebug("polygon_label_segment(): ",
            "Applied buffer:", buffer);
      }
      if (TRUE %in% plot_debug) {
         plot_polygon_list(buffer_polygon_list,
            col=jamba::alpha2col("gold", alpha=0.2),
            border=jamba::alpha2col("gold", alpha=0.4),
            lwd=2,
            lty="dashed",
            add=TRUE)
      }
   }
   
   # obtain a line from polygon border to external point
   line_in_poly <- polyclip::pointinpolygon(line_xy,
      polygon_list[[1]])
   if (all(line_in_poly %in% c(-1, 1))) {
      # return if line_xy is fully contained in (or on the line) polygon_list
      if (verbose) {
         jamba::printDebug("polygon_label_segment(): ",
            "The line is fully contained by the polygon.");
      }
      lxy[2,] <- rep(NA, 2);
      line_xy$x[2] <- NA;
      line_xy$y[2] <- NA;
   } else if (all(line_in_poly %in% 0)) {
      # the line does not intersect polygon_list,
      # so it uses the nearest point
      if (verbose) {
         jamba::printDebug("polygon_label_segment(): ",
            "The line does not intersect the polygon, using nearest polygon point.");
      }
      # TODO: find nearest points on the polygon to the line
      # Euclidean distance of line points to polygon points
      point_m <- cbind(
         x=c(line_xy$x[1],
            polygon_list[[1]]$x),
         y=c(line_xy$y[1],
            polygon_list[[1]]$y));
      point_dist <- dist(point_m);
      line_poly_dist <- as.matrix(dist(point_m))[-1, 1];
      line_point_nearest <- unname(which.min(line_poly_dist));
      if (verbose) {
         jamba::printDebug("Finding nearest point to the polygon.");
         # print(line_poly_dist);
         # jamba::printDebug("line_point_nearest:", line_point_nearest);
      }
      # points(x=polygon_list[[1]]$x[line_point_nearest], y=polygon_list[[1]]$y[line_point_nearest])
      lxy[2, ] <- c(polygon_list[[1]]$x[line_point_nearest],
         polygon_list[[1]]$y[line_point_nearest])
      line_xy$x[2] <- polygon_list[[1]]$x[line_point_nearest];
      line_xy$y[2] <- polygon_list[[1]]$y[line_point_nearest];
      if (plot_debug) {
         jamba::printDebug("line_xy:")
         print(line_xy)
         # plot_polygon_list(polygon_list)
         plot_polygon_list(list(line_xy),
            border="darkorange",
            lty="dashed",
            lwd=3,
            add=TRUE)
      }
      
      # spt_new <- rgeos::gNearestPoints(spt[1], sp);
      # lxy[2,] <- spt_new@coords[2,];
   } else if (line_in_poly[1] %in% c(-1, 1)) {
      # first point inside (or on the line), second point outside
      if (verbose) {
         jamba::printDebug("polygon_label_segment(): ",
            "The first point is inside the polygon.");
      }
      lxy[2,] <- rep(NA, 2);
      line_xy$x[2] <- NA;
      line_xy$y[2] <- NA;
   } else {
      # all other cases should involve the line crossing the polygon boundary
      # TODO: subtract the polygon from the line
      # polyclip::polyclip(A, B, closed=FALSE)
      # where A represents the line
      line_xy_diff <- polyclip::polyclip(A=line_xy,
         B=polygon_list[[1]],
         op="minus",
         closed=FALSE)
      # sl_diff <- rgeos::gDifference(sl, sp);
      if (plot_debug) {
         # plot_polygon_list(polygon_list)
         plot_polygon_list(line_xy_diff,
            border="darkorange",
            lty="dashed",
            lwd=3,
            add=TRUE)
      }
      
      # If the new line has multiple segments,
      # use the first segment that includes the first input point
      if (verbose) {
         jamba::printDebug("polygon_label_segment(): ",
            "The first point is outside the polygon.");
      }
      if (length(line_xy_diff) > 1) {
         # detect which segment intersects the first point
         keep_segment <- sapply(line_xy_diff, function(ixy){
            test_xy <- rbind(
               signif(digits=4, do.call(cbind, line_xy)[1, , drop=FALSE]),
               signif(digits=4, do.call(cbind, ixy)))
            (nrow(unique(test_xy)) < 3)
         })
         # TODO: If no segment perfectly includes the point,
         # consider using line closest to the first point
         if (any(keep_segment)) {
            line_xy_diff <- line_xy_diff[head(which(keep_segment), 1)]
         } else {
            min_dist_segment <- sapply(line_xy_diff, function(ixy){
               min(as.matrix(dist(rbind(
                  do.call(cbind, line_xy)[1, , drop=FALSE],
                  do.call(cbind, ixy)
               )))[-1, 1])
            })
            keep_segment <- which.min(min_dist_segment)
            line_xy_diff <- line_xy_diff[keep_segment]
         }
         # sl_diff_int <- sapply(sl_diff@lines[[1]]@Lines, function(L){
         #    sl1 <- sp::SpatialLines(list(sp::Lines(list(L), ID="b")))
         #    rgeos::gIntersects(sl1, spt[1])
         # });
      }
   
      # sl_diff_which <- head(which(sl_diff_int), 1);
      # if (verbose) {
      #    jamba::printDebug("polygon_label_segment(): ",
      #       "Using first outer line segment ", sl_diff_which);
      # }
      # lxy <- sl_diff@lines[[1]]@Lines[[sl_diff_which]]@coords;
      lxy <- do.call(cbind, line_xy_diff[[1]])
   }
   
   # optional debug plot
   if (plot_debug) {
      if (is.na(lxy[2,1])) {
         points(x=lxy[1,c(1, 1, 1)],
            y=lxy[1,c(2, 2, 2)],
            cex=c(1, 3, 5),
            pch=c(20, 1, 1),
            col="red3",
            add=TRUE);
      } else {
         # sl@lines[[1]]@Lines[[1]]@coords <- lxy;
         use_xy <- list(x=lxy[, 1],
            y=lxy[, 2])
         # sp::plot(sl,
         plot_polygon_list(use_xy,
            col="darkorchid4",
            lwd=4,
            lty="solid",
            add=TRUE);
         points(x=lxy[1:2,c(1, 1, 1)],
            y=lxy[1:2,c(2, 2, 2)],
            cex=c(1, 3, 5),
            pch=c(20, 1, 1),
            col=rep(c("blue2", "red3"), 3),
            add=TRUE);
      }
   }
   
   # optional convenience step, add text adj
   if (any(is.na(lxy[,1]))) {
      adjx <- 0.5;
      adjy <- 0.5;
   } else {
      degrees <- jamba::rad2deg(
         atan2(y=diff(-lxy[,2]), x=diff(-lxy[,1])) + 
            rnorm(1) * 1e-6) %% 360;
      adjdf <- degrees_to_adj(degrees,
         top=90,
         clockwise=FALSE);
      adjx <- adjdf[,"adjx"];
      adjy <- adjdf[,"adjy"];
   }
   lxy <- cbind(lxy, adjx=adjx, adjy=adjy);
   
   if ("matrix" %in% return_class) {
      return(lxy);
   } else if ("point" %in% return_class) {
      return(lxy[2,,drop=FALSE]);
   }
}


#' Get polygon_list after applying buffer width
#' 
#' Get polygon_list after applying buffer width
#' 
#' This function is intended to help apply negative
#' buffer width, where the intended buffer width may
#' result in a zero-size polygon, in which case a
#' progressively smaller buffer width is applied
#' until a polygon can be returned with non-zero area.
#' 
#' An added bonus, when `relative=TRUE` it will determine
#' the actual buffer value that would barely result in non-zero
#' area, and define that as 1x buffer, and the `buffer`
#' will be scaled to that value. So `buffer=-0.5` will
#' return an interior polygon at half the width it would take
#' to reduce the polygon to zero size. See examples.
#' 
#' Ultimately if no buffer can be applied, then the input `sp`
#' object is returned without change.
#' 
#' This function is useful to draw a line segment from outside
#' into a polygon, without drawing it to the center point of
#' the polygon, where a text label may already be positioned.
#' 
#' @returns `list` polygon_list object with `"x"` and `"y"` elements.
#' 
#' @family venndir polygons
#' 
#' @param polygon_list `list` with `"x"` and `"y"` elements.
#' @param buffer `numeric` value, usually negative, which
#'    will create a buffer inside the `polygon_list` polygon.
#' @param steps `integer` number of steps to iterate between
#'    `buffer` and `0`.
#' @param relative `logical` indicating whether `buffer` is
#'    a fraction of the buffer width required to reduce the
#'    `polygon_list` polygon to zero area.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' polygon_list <- get_venn_polygon_shapes(counts=c(A=1))
#' plot_polygon_list(polygon_list, col="red")
#' polygon_list_inside <- get_polygon_buffer(polygon_list, -0.5, relative=FALSE)
#' plot_polygon_list(polygon_list_inside, col="cornsilk", lty="dotted", add=TRUE);
#' 
#' polygon_list <- get_venn_polygon_shapes(counts=c(A=1))
#' plot_polygon_list(polygon_list, col="red")
#' polygon_list_inside <- get_polygon_buffer(polygon_list, -0.2, relative=TRUE)
#' plot_polygon_list(polygon_list_inside, col="cornsilk", lty="dotted", add=TRUE);
#' 
#' # by default relative=TRUE
#' sp1 <- polygon_ellipses(c(3.5, 2), c(2, 3), xradius=c(2, 3), yradius=c(3, 1))
#' sp <- minus_polygon_list(sp1[1:2]);
#' opar <- par("mfrow"=c(2,2));
#' on.exit(par(opar));
#' for (i in -c(0.9, 0.75, 0.5, 0.25)) {
#'    plot_polygon_list(sp, col="red",
#'       main=paste0("buffer=", i));
#'    plot_polygon_list(get_polygon_buffer(sp, i, relative=TRUE),
#'       col="cornsilk", lty="dotted", add=TRUE);
#' }
#' par(opar);
#' 
#' @export
get_polygon_buffer <- function
(polygon_list,
 buffer=-0.5,
 steps=50,
 relative=TRUE,
 verbose=FALSE,
 ...)
{
   if (buffer == 0) {
      return(polygon_list)
   }
   # relative size
   bbox_max <- max(sapply(bbox_polygon_list(polygon_list), diff));
   if (relative) {
      buffer_seq <- seq(from=bbox_max,
         to=0,
         length.out=100);
      for (max_buffer in buffer_seq) {
         buffer_polygon_list <- polyclip::polyoffset(
            union_polygon_list(polygon_list),
            -max_buffer,
            jointype="round")
         if (length(buffer_polygon_list) > 0 &&
               sum(polygon_areas(buffer_polygon_list, simplify=TRUE)) > 0) {
            break;
         }
      }
      buffer <- buffer * max_buffer;
      if (verbose) {
         jamba::printDebug("get_sp_buffer(): ",
            "max_buffer:", max_buffer);
         jamba::printDebug("get_sp_buffer(): ",
            "calculated buffer:",
            format(buffer, digits=3, big.mark=","));
      }
   } else if (buffer < 0 && abs(buffer) > bbox_max) {
      buffer <- -bbox_max;
   }
   
   if (buffer > 0) {
      buffer_seq <- buffer;
   } else {
      buffer_seq <- seq(from=buffer,
         to=0,
         length.out=steps);
   }
   for (buffer in buffer_seq) {
      if (verbose) {
         jamba::printDebug("get_sp_buffer(): ",
            "buffer:", buffer);
      }
      buffer_polygon_list <- polyclip::polyoffset(
         union_polygon_list(polygon_list),
         buffer,
         jointype="round")
      if (length(buffer_polygon_list) == 0 ||
            sum(polygon_areas(buffer_polygon_list, simplify=TRUE)) == 0) {
         next;
      }
      return(buffer_polygon_list);
   }
   return(polygon_list);
}

