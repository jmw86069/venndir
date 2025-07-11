
#' Position labels outside JamPolygon
#' 
#' Position labels outside JamPolygon
#' 
#' The purpose is to arrange labels outside a `JamPolygon` that
#' contains one or more parts. In general, it works best to supply
#' the entire `JamPolygon` even when labeling a subset of parts,
#' using `which_jp` to select the parts to label,
#' since it defines many coordinates relative to the overall
#' geometry.
#' 
#' There are several strategies used, from experiences trying
#' to label Euler diagrams in automated way. The general steps:
#' 
#' 1. Define **center**.
#' 
#'    * The default uses the center of the bounding box.
#'    * An alternative is the mean position of each polygon internal
#'    label, which is effective when most labels are skewed to one side.
#' 
#' 2. Draw lines from center, through the polygon to label,
#' outside some distance.
#' 
#'    * The default chooses the farthest point from center for each
#'    polygon.
#'    * An alternative is to use the polygon internal label position.
#' 
#' 3. Define a line segment from the outside point back to the polygon,
#' inside by some distance.
#' 
#'    * The default uses the nearest point on the polygon to the outside point.
#'    * An alternative directs the line segment toward the internal
#'    label for each polygon.
#' 
#' The defaults are quite effective, however some unusual arrangements or
#' shapes may warrant trying the other options.
#' 
#' When calling `venndir()`, the ellipses `'...'` are passed through to
#' this function `label_outside_JamPolygon()` to customize these options.
#' 
#' @family JamPolygon
#'
#' @param jp `JamPolygon`
#' @param which_jp `integer` or `NULL`; when `which_jp` contains one
#'    or more `integer` values, they refer to rows in `jp`, and
#'    each will be analyzed in sequence. When `which_jp=NULL` then
#'    all the polygons in `jp` will be analyzed in sequence.
#' @param center `numeric` vector or matrix with two values indicating
#'    the center position.
#'    * In all cases, the overall center is defined using
#'    `center_method`, then it is adjusted by `center` when defined.
#'    * When `center=NULL` the `center_method` value is used.
#'    * When `center` is provided and `relative=TRUE`
#'    (default), the overall center position is adjusted using
#'    `center` interpreted as relative values.
#'    * When `relative=FALSE` the values `center` are added to the
#'    overall center position. To use `center` as absolute coordinates,
#'    select `center_method="none"`.
#' @param buffer `numeric` buffer, default -0.1, inside the polygon
#'    used to draw a line segment connecting the label to the appropriate
#'    polygon.
#' @param distance `numeric` value, default 0.1, indicating the
#'    distance from the perimiter of `jp` to place labels.
#'    This value is the `buffer` for `buffer_JamPolygon()`.
#' @param center_method `character` string indicating the method to
#'    determine the `center`:
#'    * `"label"` uses the mean x,y coordinate
#'    of all the polygon label positions;
#'    * `"bbox"` uses the mean x,y
#'    coordinate of the bounding box that encompasses the polygons.
#'    * `"none"` uses the origin 0,0 and is intended mainly to allow
#'    `center` to be used as absolute coordinates.
#'    
#'    The effect
#'    is to extend outer labels radially around this center point.
#'    Using the mean label position with `center_method="label"`
#'    is helpful because it ensures labels are extended in all directions
#'    even when most labels are in one upper or lower section of
#'    the polygons.
#' @param vector_method `character` string indicating the vector from
#'    `center` outward, to define the position outside the polygons.
#'    * `'farthest'` (default) aims to the farthest point from center.
#'    * `'label'` aims through the default label position in each polygon.
#' @param segment_method `character` string indicating how to connect
#'    a line segment from the outside label, back to the polygon.
#'    The line segment ends inside the polygon, defined by `buffer`.
#'    * `'nearest'` (default) points to the nearest border.
#'    * `'label'` points toward the default label position inside the
#'    polygon.
#' @param min_degrees `numeric`, default 15, minimum degrees spacing
#'    to impose between label positions, oriented around the center.
#'    When there are more labels than can be divided, the threshold
#'    is automatically lowered proportionally.
#' @param relative `logical` whether distance and buffer are relative
#'    to plot dimensions, default TRUE.
#' @param y_snap_percent `numeric` percent of the plot dimensions
#'    used to decide whether to "snap" two labels to the same y-axis value.
#'    Default 5 means any labels within 5 percent of the plot dimensions
#'    of one or more other labels will use the mean y-axis value,
#'    thereby helping align labels by height where appropriate.
#' @param seed `numeric` used to set the random seed for reproducibility,
#'    via `set.seed()`. Default is 123.
#' @param debug `logical` whether to print detailed debug information.
#' @param do_plot `logical` whether to create a plot with the input `jp`
#'    and corresponding labels and line segments assigned to `which_jp`.
#'    Default is FALSE.
#' @param verbose `logical` whether to print verbose output.
#' @param ... additional arguments are ignored.
#' 
#' 
#' @examples
#' v <- venndir(make_venn_test(n_sets=3), do_plot=FALSE)
#' jps <- v@jps
#' 
#' # plot visual summary
#' label_outside_JamPolygon(jps, do_plot=TRUE)
#' 
#' @export
label_outside_JamPolygon <- function
(jp,
 which_jp=NULL,
 center=NULL,
 buffer=-0.1,
 distance=0.1,
 center_method=c("bbox",
    "label",
    "none"),
 vector_method=c(
    "farthest",
    "label"),
 segment_method=c(
    "nearest",
    "vector"),
 min_degrees=15,
 relative=TRUE,
 y_snap_percent=5,
 seed=123,
 debug=FALSE,
 do_plot=FALSE,
 verbose=FALSE,
 ...)
{
   # validate input
   center_method <- match.arg(center_method);
   vector_method <- match.arg(vector_method);
   segment_method <- match.arg(segment_method);
   if (length(which_jp) == 0) {
      ## use only those entries with polygon coordinates
      # which_jp <- which(sapply(seq_len(nrow(jp@polygons)), function(ijp){
      #    length(jamba::rmNA(unlist(jp@polygons$x[[ijp]]))) > 0
      # }))
      which_jp <- seq_len(length(jp));
      # jamba::printDebug("which_jp: ", which_jp);# debug
   }
   
   ## check for empty polygons among which_jp
   which_jp_is_empty <- sapply(which_jp, function(ijp){
      length(jamba::rmNA(unlist(jp@polygons$x[[ijp]]))) == 0
   })
   ## Todo: Check for all empty polygons, and return NA, NA if true
   
   # buffer
   if (length(buffer) == 0) {
      buffer <- 0;
   }
   buffer <- rep(buffer,
      length.out=length(which_jp));
   names(buffer) <- names(jp)[which_jp];

   # get bbox for the whole polygon
   jpbox <- bbox_JamPolygon(jp);
   
   # added tiny buffer around polygons before union
   # otherwise tiny slivers of holes remain, apparently
   # due to rounding errors in the polygon coordinates,
   # which cause adjacent polygons to be not-quite-adjacent.
   # Anyway, the tiny holes caused label_segment_JamPolygon()
   # to take 10-15 seconds per line, instead of being instant.
   jpb <- max(apply(jpbox, 1, diff)) / 100;
   jpall <- buffer_JamPolygon(
      union_JamPolygon(
         buffer_JamPolygon(jp,
            buffer=jpb,
            relative=FALSE)),
      buffer=jpb,
      relative=FALSE);
   # plot(jpall);# debug

   # expand the bounding box
   jpbox_ex <- jpbox;
   jpbox_ex[] <- t(apply(jpbox, 1, expand_range, 0.2));

   # distance (as fraction of bbox)
   bbox_max <- max(apply(jpbox, 1, diff));
   distance <- bbox_max * distance;
   distance <- rep(distance,
      length.out=length(which_jp));
   names(distance) <- names(jp)[which_jp];
   
   # get center
   set.seed(seed);
   # calculate plot center
   new_center <- cbind(x=0, y=0);
   if ("bbox" %in% center_method || length(which_jp) == 1) {
      new_center <- matrix(ncol=2,
         rowMeans(jpbox)) + (rnorm(2) / 1000);
   } else if ("label" %in% center_method) {
      # use mean of label positions
      if (all(c("label_x", "label_y") %in% colnames(jp))) {
         new_center <- cbind(
            x=mean(range(jp@polygons$label_x[which_jp], na.rm=TRUE)),
            y=mean(range(jp@polygons$label_y[which_jp], na.rm=TRUE)))
      } else {
         label_xy <- labelr_JamPolygon(jp[which_jp, ]);
         # average from range of labels
         # center <- cbind(
         #    x=mean(range(label_xy[,1], na.rm=TRUE)),
         #    y=mean(range(label_xy[,2], na.rm=TRUE)))
         # average from all actual labels
         new_center <- cbind(
            x=mean(label_xy[,1], na.rm=TRUE),
            y=mean(label_xy[,2], na.rm=TRUE))
      }
   }
   if (length(center) == 2) {
      # relative adjustment
      if (TRUE %in% relative) {
         center[1] <- diff(jpbox["x", ]) * center[1];
         center[2] <- diff(jpbox["x", ]) * center[2];
      }
      center <- center + new_center;
   } else {
      center <- new_center
   }
   colnames(center) <- c("x", "y");
   # jamba::printDebug("center:");print(center);# debug
   # jamba::printDebug("which_jp:");print(which_jp);# debug
   # jamba::printDebug("jp:");print(jp);# debug
   
   # reference coordinate for each polygon
   polyref_xy <- jamba::rbindList(lapply(which_jp, function(iwhich){
      # get sub-polygon
      ijp <- jp[iwhich, ];
      # jamba::printDebug("ijp:");print(ijp);# debug
      # jamba::printDebug("ijp@polygons$x:");print(ijp@polygons$x);# debug
      if (length(jamba::rmNA(unlist(ijp@polygons$x))) == 0) {
         # no polygon present
         return(cbind(x=NA_integer_, y=NA_integer_))
      }
      # get point farthest from center
      if ("farthest" %in% vector_method) {
         # get farthest point from xy
         # print(center);
         xymax <- farthest_point_JamPolygon(center, ijp);
      } else if ("label" %in% vector_method) {
         # from center to the polygon label position
         if (all(c("label_x", "label_y") %in% colnames(ijp)) &&
               !is.na(ijp@polygons$label_x) &&
               !is.na(ijp@polygons$label_y) ) {
            # re-use label position stored in jp
            xymax <- matrix(ncol=2,
               c(ijp@polygons$label_x, ijp@polygons$label_y));
            colnames(xymax) <- c("x", "y");
            rownames(xymax) <- rownames(ijp);
         } else {
            # calculate label position
            xymax <- labelr_JamPolygon(ijp);
         }
      }
      # Todo: Consider check if xymax == center
      xymax;
   }));
   rownames(polyref_xy) <- names(jp)[which_jp];
   if (verbose > 1) {
      jamba::printDebug("polyref_xy:");print(polyref_xy);# debug
   }

   # iterate multiple polygons to find angles
   angles_df <- jamba::rbindList(lapply(which_jp, function(iwhich){
      iname <- names(jp)[iwhich];
      xymax <- polyref_xy[iname, , drop=FALSE];
      # get reference position
      x1 <- c(center[1, "x"], xymax[1, "x"]);
      y1 <- c(center[1, "y"], xymax[1, "y"]);
      # clever trick to sort set/overlap names top-bottom not clockwise
      if (grepl("[|]set$", iname[1])) {
         y1[1] <- y1[1] - 1e-4;
         # y1[1] <- y1[1] + 1e-4;
      } else {
         # jamba::printDebug("iname:", iname);# debug
      }

      # add a small random value to prevent identical angles
      angle <- jamba::rad2deg(
         atan2(y=diff(y1), x=diff(x1)) + 
            rnorm(1) * 1e-6) %% 360;
      angle_df <- data.frame(check.names=FALSE,
         row.names=iname,
         iname=iname,
         xdiff=diff(x1),
         ydiff=diff(y1),
         angle=angle)
      return(angle_df);
   }));
   angles_df <- jamba::mixedSortDF(angles_df,
      byCols=c("-ydiff", "-angle", "iname"))
   if (verbose > 1) {
      jamba::printDebug("angles_df:");print(angles_df);# debug
   }
   angles1 <- jamba::nameVector(angles_df[, c("angle", "iname"), drop=FALSE]);

   # logic here to spread out angles too close to each other
   angles <- angles1;
   not_na <- !is.na(angles1);

   angles[not_na] <- spread_degrees(angles1[not_na],
      min_degrees=min_degrees);
   
   # names(angles) <- names(jp)[which_jp];
   angles_df$new_angle <- angles;
   # jamba::printDebug("angles_df:");print(angles_df);# debug
   # jamba::printDebug("angles:");# debug
   # print(data.frame(angles_in=round(angles1), angles_out=round(angles)));# debug
   
   if (verbose > 1) {   
      jamba::printDebug("label_outside_JamPolygon(): ",
         "angles:");
      print(head(data.frame(angles1, angles), 20));
   }
   
   # expand to twice the bbox size
   # max_radius <- max(rowDiffs(jpbox)) * 2;
   max_radius <- max(apply(jpbox, 1, diff)) * 2;

   ## for each angle find the line segment
   segmentxy_list <- lapply(which_jp, function(iwhich){
      # get sub-polygon
      if (verbose > 1) {
         jamba::printDebug("label_outside_JamPolygon(): ",
            "iwhich: ", match(iwhich, which_jp), " of ", length(which_jp));
      }
      ijp <- jp[iwhich, ];
      iname <- names(ijp);
      
      # define point outside using angle and max_radius
      idistance <- distance[[iname]];
      angle <- angles[[iname]];
      xedge <- cos(jamba::deg2rad(angle)) * 
         (max_radius + idistance) + center[1,1];
      yedge <- sin(jamba::deg2rad(angle)) * 
         (max_radius + idistance) + center[1,2];
      
      # Find the point at the encompassing polygon outer edge.
      # This point is the outside label position.
      # polygon_label_segment()
      if (verbose > 1) {
         jamba::printDebug("label_outside_JamPolygon(): ",
            "started first label_segment_JamPolygon.")
      }
      plsxy1 <- label_segment_JamPolygon(
         jp=jpall,
         buffer=idistance * 1,
         relative=FALSE,
         x0=xedge,
         y0=yedge,
         x1=center[1, 1],
         y1=center[1, 2],
         verbose=verbose,
         ...);
      if (verbose > 1) {
         jamba::printDebug("label_outside_JamPolygon(): ",
            "completed first label_segment_JamPolygon");
      }
      
      # find the point at the specific polygon outer edge
      if ("vector" %in% segment_method) {
         # use the boundary point on the vector to the inside label position
         # not the closest point on the inside polygon
         x1use <- polyref_xy[iname, 1];
         y1use <- polyref_xy[iname, 2];
      } else {
         # use closest point on the inside polygon
         # not along the vector to the inside label position
         x1use <- plsxy1[1, 1];
         y1use <- plsxy1[1, 2];
      }

      # polygon_label_segment()
      if (verbose > 1) {
         jamba::printDebug("label_segment_JamPolygon call:");
         print(list(
            jp=ijp,
            buffer=buffer[[iname]],
            relative=relative,
            x0=unlist(plsxy1[1, 1]),
            y0=unlist(plsxy1[1, 2]),
            x1=x1use,
            y1=y1use
         ))
      }
      plsxy <- label_segment_JamPolygon(
         jp=ijp,
         buffer=buffer[[iname]],
         relative=relative,
         x0=unlist(plsxy1[1, 1]),
         y0=unlist(plsxy1[1, 2]),
         x1=x1use,
         y1=y1use,
         verbose=verbose,
         ...);
      if (verbose > 1) {
         jamba::printDebug("label_outside_JamPolygon(): ",
            "completed second label_segment_JamPolygon");
      }
      
      # line segment
      segmentxy <- cbind(
         x=unname(c(plsxy[1, 1], plsxy1[1, 1])),
         y=unname(c(plsxy[1, 2], plsxy1[1, 2])),
         degrees=unname(angle));
      rownames(segmentxy) <- c("border", "label");
      adjdf <- degrees_to_adj(angle,
         top=90,
         clockwise=FALSE);
      segmentxy <- cbind(segmentxy,
         adjx=adjdf[, 1])
      segmentxy <- cbind(segmentxy,
         adjy=adjdf[, 2])
      
      return(segmentxy);
   });
   if (verbose > 1) {   
      jamba::printDebug("label_outside_JamPolygon(): ",
         "completed segmentxy_list");
   }
   
   names(segmentxy_list) <- names(jp)[which_jp];
   
   # optional "snap" y-label to mean value
   label_ys <- jamba::rbindList(lapply(segmentxy_list, function(segmentxy){
      segmentxy["label", , drop=FALSE]
   }))
   rownames(label_ys) <- names(segmentxy_list);
   row_set <- jamba::nameVectorN(segmentxy_list);
   for (i in seq_len(nrow(label_ys))) {
      idiff <- abs(label_ys[i, "y"] - label_ys[, "y"]);
      idiff_low <- which((idiff / bbox_max * 100) <= y_snap_percent);
      row_set[idiff_low] <- row_set[head(idiff_low, 1)];
   }
   if (any(duplicated(row_set))) {
      row_set_dupes <- names(jamba::tcount(row_set, 2));
      for (row_set_dupe in row_set_dupes) {
         ivals <- names(row_set)[row_set %in% row_set_dupe];
         new_y <- mean(label_ys[ivals, "y"], na.rm=TRUE);
         for (ival in ivals) {
            segmentxy_list[[ival]]["label", "y"] <- new_y;
         }
      }
   }

   # optional plot
   if (TRUE %in% do_plot) {
      # basic plot of polygons
      j6 <- plot(jp,
         xlim=expand_range(jpbox["x", ], expand_fraction=0.5),
         ylim=expand_range(jpbox["y", ], expand_fraction=0.5));
      vp <- attr(j6, "viewport");
      adjx <- attr(j6, "adjx");
      adjy <- attr(j6, "adjy");
      default.units <- "snpc";
      if (inherits(vp, "vpTree")) {
         default.units <- "native";
      }
      
      for (i in seq_along(segmentxy_list)) {
         xy <- segmentxy_list[[i]];
         pchv <- c(border=20, label=4)
         if (all(!is.na(xy[, "x"]))) {
            gp <- grid::pointsGrob(
               x=adjx(xy[,"x"]),
               y=adjy(xy[,"y"]),
               default.units=default.units, vp=vp,
               pch=pchv[rownames(xy)],
               gp=grid::gpar(col="red", cex=0.3))
            grid::grid.draw(gp)
            gl <- grid::linesGrob(
               x=adjx(xy[,"x"]),
               y=adjy(xy[,"y"]),
               default.units=default.units, vp=vp,
               gp=grid::gpar(col="indianred1"))
            grid::grid.draw(gl)
            gt <- grid::textGrob(
               x=adjx(xy["label", "x"]),
               y=adjy(xy["label", "y"]),
               label=names(segmentxy_list)[i],
               default.units=default.units, vp=vp,
               gp=grid::gpar(col="red3"))
            grid::grid.draw(gt)
         }
      }
      # Add the center
      gc <- grid::pointsGrob(
         x=adjx(center[1]),
         y=adjy(center[2]),
         default.units=default.units, vp=vp,
         pch=5,
         gp=grid::gpar(col="red", cex=0.3))
      grid::grid.draw(gc)
      
   }
   
   return(invisible(segmentxy_list));
}

#' Get the farthest polygon point from a reference point
#'
#' @family JamPolygon
#'
#' @returns `numeric` matrix with columns `"x"` and `"y"`
#' 
#' @param x `numeric` matrix with columns `"x"` and `"y"`
#' @param jp `JamPolygon`
#' @param ... additional arguments are ignored.
#'
#' @export
farthest_point_JamPolygon <- function
(x,
 jp,
 ...)
{
   # get polygon points
   jpx <- unlist(jp@polygons[, "x"])
   jpy <- unlist(jp@polygons[, "y"])
   jpxy <- cbind(x=jpx, y=jpy);

   # validate x
   if (!all(c("x", "y") %in% colnames(x)) || !is.numeric(x) || length(x) == 0) {
      stop("x must be a numeric matrix with colnames 'x' and 'y'");
   }
   xy <- x[, c("x", "y"), drop=FALSE];

   xydist <- as.matrix(dist(rbind(xy, jpxy)))[-1, 1];
   xymax <- jpxy[which.max(xydist), , drop=FALSE];
   colnames(xymax) <- c("x", "y");
   return(xymax);
}

#' Get the nearest polygon point to a reference point
#' 
#' @family JamPolygon
#'
#' @returns `numeric` matrix with columns `"x"` and `"y"`
#' 
#' @param x `numeric` matrix with columns `"x"` and `"y"`
#' @param jp `JamPolygon`
#' @param ... additional arguments are ignored.
#'
#' @export
nearest_point_JamPolygon <- function
(x,
 jp,
 ...)
{
   # get polygon points
   jpx <- unlist(jp@polygons[, "x"])
   jpy <- unlist(jp@polygons[, "y"])
   jpxy <- cbind(x=jpx, y=jpy);
   
   # validate x
   if (!all(c("x", "y") %in% colnames(x)) || !is.numeric(x) || length(x) == 0) {
      stop("x must be a numeric matrix with colnames 'x' and 'y'");
   }
   xy <- x[, c("x", "y"), drop=FALSE];
   
   xydist <- as.matrix(dist(rbind(xy, jpxy)))[-1, 1];
   xymax <- jpxy[which.min(xydist), , drop=FALSE];
   colnames(xymax) <- c("x", "y");
   return(xymax);
}

#' Define a label segment for JamPolygon
#' 
#' @family JamPolygon
#' 
#' @export
label_segment_JamPolygon <- function
(x0,
 x1,
 y0,
 y1,
 jp,
 return_class=c("point",
    "matrix"),
 buffer=0,
 plot_debug=FALSE,
 relative=TRUE,
 verbose=FALSE,
 ...)
{
   return_class <- match.arg(return_class);
   
   if (length(x0) > 1) {
      if (verbose) {
         jamba::printDebug("label_segment_JamPolygon(): ",
            "iterating ", length(x0), " input entries.");
      }
      i <- seq_along(x0);
      x1 <- rep(x1, length.out=length(x0));
      y0 <- rep(y0, length.out=length(x0));
      y1 <- rep(y1, length.out=length(x0));
      buffer <- rep(buffer, length.out=length(x0));
      jpi <- ((i - 1) %% length(jp)) + 1;
      # jamba::printDebug("i:", i);# debug
      # jamba::printDebug("jpi:", jpi);# debug
      listout <- lapply(i, function(j){
         # jamba::printDebug("j:", j);# debug
         # jamba::printDebug("class(jp):", class(jp));# debug
         jp_use <- jp[[jpi[j]]];
         lsjp <- label_segment_JamPolygon(x0=x0[j],
            x1=x1[j],
            y0=y0[j],
            y1=y1[j],
            jp=jp_use,
            buffer=buffer[[j]],
            return_class=return_class,
            plot_debug=plot_debug,
            verbose=verbose,
            add=(j > 1),
            ...);
         # jamba::printDebug("lsjp:");print(lsjp);@ debug
         lsjp;
      });
      if ("point" %in% return_class) {
         listout <- jamba::rbindList(listout);
      }
      return(listout);
   }

   lxy <- cbind(x=c(x0, x1),
      y=c(y0, y1));

   # check for empty jp
   if ("list" %in% class(jp)) {
      if (length(jp) == 1) {
         jp <- jp[[1]];
      } else {
         jp <- do.call(rbind2, jp);
      }
      # jamba::printDebug("label_segment_JamPolygon(): ", "use_jp:");print(use_jp);# debug
      if (length(jamba::rmNA(unlist(jp@polygons$x))) == 0) {
         # jamba::printDebug("label_segment_JamPolygon(): ", "empty jp");# debug
         return(lxy);
      }
   } else {
      # jamba::printDebug("label_segment_JamPolygon(): ", "jp@polygons:");print(jp@polygons);# debug
      if (length(jamba::rmNA(unlist(jp@polygons$x))) == 0) {
         return(lxy);
      }
   }
   
   # optional polygon buffer
   buffer <- head(buffer, 1);

   # grow polygon in size using buffer
   if (!0 %in% buffer && !"list" %in% class(jp)) {
      jp <- buffer_JamPolygon(jp,
         buffer=buffer,
         relative=relative,
         ...);
   }
   
   # check line (points) are inside polygon
   P <- list(x=lxy[, 1], y=lxy[, 2]);
   points_inside <- has_point_in_JamPolygon(x=P, jp=jp);

   # jamba::printDebug("lxy (input):");print(lxy);# debug
   if (all(points_inside)) {
      # return if sl is fully contained inside sp
      lxy[2, ] <- c(NA, NA);
   } else if (!any(points_inside)) {
      # the line does not intersect the polygon
      # use nearest point on the polygon boundary
      nearestP <- nearest_point_JamPolygon(do.call(cbind, P)[1, , drop=FALSE], jp);
      # jamba::printDebug("nearestP:");print(nearestP);# debug
      lxy[2, c("x", "y")] <- c(nearestP[1, "x"], nearestP[1, "y"]);
   } else {
      # subtract the polygon from the line
      line_jp <- new("JamPolygon",
         polygons=data.frame(name="line",
            x=I(list(lxy[, 1])),
            y=I(list(lxy[, 2]))))
      # plot(rbind2(line_jp, jp))
      line_diff <- minus_JamPolygon(rbind2(line_jp, jp), closed=FALSE);
      # check for multiple line segments, take only the first
      # plot(rbind2(line_diff, jp))
      line_x <- line_diff@polygons$x[[1]];
      line_y <- line_diff@polygons$y[[1]];
      if (is.list(line_x)) {
         line_x <- line_x[[1]];
         line_y <- line_y[[1]];
      }
      line_diff@polygons$x <- I(list(line_x));
      line_diff@polygons$y <- I(list(line_y));
      lxy <- cbind(x=line_x, y=line_y)
   }

   # jamba::printDebug("lxy (after):");print(lxy);# debug
   # optional convenience step, add text adj
   if (any(is.na(lxy[, 1]))) {
      adjx <- 0.5;
      adjy <- 0.5;
   } else {
      degrees <- jamba::rad2deg(
         atan2(y=diff(-lxy[, 2]),
            x=diff(-lxy[, 1])) + 
            rnorm(1) * 1e-6) %% 360;
      adjdf <- degrees_to_adj(degrees,
         top=90,
         clockwise=FALSE);
      adjx <- adjdf[, "adjx"];
      adjy <- adjdf[, "adjy"];
   }
   lxy <- cbind(lxy, adjx=adjx, adjy=adjy);
   if ("matrix" %in% return_class) {
      return(lxy);
   } else if ("point" %in% return_class) {
      return(lxy[2, , drop=FALSE]);
   }
   #
}
