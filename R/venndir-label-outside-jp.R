
#' Position labels outside JamPolygon
#' 
#' Position labels outside JamPolygon
#' 
#' @family JamPolygon
#'
#' @param jp `JamPolygon`
#' @param which_jp `integer` or `NULL`; when `which_jp` contains one
#'    or more `integer` values, they refer to rows in `jp`, and
#'    each will be analyzed in sequence. When `which_jp=NULL` then
#'    all the polygons in `jp` will be analyzed in sequence.
#' @param center `numeric` vector or matrix with two values indicating
#'    the center position. When `center=NULL` then the center is
#'    determined using a method defined by `center_method`.
#' @param distance `numeric` value indicating the absolute coordinate
#'    distance away from the perimiter of `jp` to place labels.
#'    This value is the `buffer` for `buffer_JamPolygon()`.
#' @param center_method `character` string indicating the method to
#'    determine the `center`:
#'    * `"label"` uses the mean x,y coordinate
#'    of all the polygon label positions;
#'    * `"bbox"` uses the mean x,y
#'    coordinate of the bounding box that encompasses the polygons.
#'    
#'    The effect
#'    is to extend outer labels radially around this center point.
#'    Using the mean label position with `center_method="label"`
#'    is helpful because it ensures labels are extended in all directions
#'    even when most labels are in one upper or lower section of
#'    the `sp` polygons.
#' 
#' @export
label_outside_JamPolygon <- function
(jp,
 which_jp=NULL,
 center=NULL,
 buffer=0.1,
 center_method=c("bbox",
    "label"),
 vector_method=c("label",
    "farthest"),
 segment_method=c("nearest",
    "vector"),
 distance=0.1,
 min_degrees=15,
 relative=TRUE,
 seed=123,
 debug=FALSE,
 verbose=FALSE,
 ...)
{
   # validate input
   center_method <- match.arg(center_method);
   if (length(which_jp) == 0) {
      ## use only those entries with polygon coordinates
      # which_jp <- which(sapply(seq_len(nrow(jp@polygons)), function(ijp){
      #    length(jamba::rmNA(unlist(jp@polygons$x[[ijp]]))) > 0
      # }))
      which_jp <- seq_len(length(jp));
      # jamba::printDebug("which_jp: ", which_jp);# debug
   } else {
      # use only those entries with polygon coordinates
      ## 0.0.34.900 - do not subset which_jp so the function can always return
      ## coordinates in the order requested, and for all entries provided,
      ## even when an entry is empty
      # which_jp_sub <- sapply(which_jp, function(ijp){
      #    length(jamba::rmNA(unlist(jp@polygons$x[[ijp]]))) > 0
      # })
      # jamba::printDebug("table(which_jp_sub): ");print(table(which_jp_sub));# debug
      # which_jp <- which_jp[which_jp_sub];
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
         xymax <- labelr_JamPolygon(ijp);
      }
      # Todo: Consider check if xymax == center
      xymax;
   }));
   # jamba::printDebug("polyref_xy:");print(polyref_xy);# debug
   rownames(polyref_xy) <- names(jp)[which_jp];

   # iterate multiple polygons to find angles
   angles1 <- sapply(which_jp, function(iwhich){
      iname <- names(jp)[iwhich];
      xymax <- polyref_xy[iname, , drop=FALSE];
      # get reference position
      x1 <- c(center[1, 1], xymax[1, 1]);
      y1 <- c(center[1, 2], xymax[1, 2]);
      
      # add a small random value to prevent identical angles
      angle <- jamba::rad2deg(
         atan2(y=diff(y1), x=diff(x1)) + 
            rnorm(1) * 1e-6) %% 360;
      return(angle);
   });
   names(angles1) <- names(jp)[which_jp];
   # jamba::printDebug("angles1:");print(angles1);# debug
   
   # logic here to spread out angles too close to each other
   angles <- angles1;
   not_na <- !is.na(angles1);
   angles[not_na] <- spread_degrees(angles1[not_na],
      min_degrees=min_degrees);
   names(angles) <- names(jp)[which_jp];
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
