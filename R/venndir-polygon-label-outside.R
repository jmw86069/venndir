
#' Polygon label outside
#' 
#' Polygon label outside
#' 
#' This function takes a `sp::SpatialPolygons` object and
#' determines an appropriate position for one or more labels
#' outside the full polygon. For each label two coordinates
#' are returned, one for the label and one for a line segment
#' that is drawn to its relevant polygon border.
#' 
#' The expected input is `sp::SpatialPolygonsDataFrame` which
#' contains multiple polygons, and one of the polygons should
#' be labeled. However, the function should also work with
#' `sp::SpatialPolygons`.
#' 
#' This function differs from some other functions in that
#' it places multiple labels in context of each other,
#' to minimize overlapping labels. It also positions
#' labels outside the full set of polygons, even when
#' labeling one of the polygons in the input `sp`.
#' For example `polygon_label_outside(sp, which_sp=2)`
#' will generate a label position for the second polygon
#' in `sp` and position it outside the full polygon set.
#' 
#' To combine multiple `sp::SpatialPolygons` objects,
#' use `sp::rbind(..., makeUniqueIDs=TRUE)`. To combine
#' `sp::SpatialPolygonsDataFrame` with another spatial
#' object, convert to generic `sp::SpatialPolygons` first
#' using `sp::geometry()` then apply `sp::rbind()`.
#' 
#' The basic workflow:
#' 
#' 1. A `center` point is either provided, or determined
#' using the polygons in `sp` using the `center_method`.
#' 2. For each polygon in `which_sp` a directional vector
#' is defined using `vector_method`, which effectively draws
#' a line from the `center` through the chosen point
#' for each polygon. The end result is an angle for each
#' label.
#' 
#'    * `vector_method="farthest"` chooses the farthest
#'    point from the `center` for each polygon.
#'    * `vector_method="label"` chooses the best polygon
#'    label position for each polygon, using
#'    `sp_polylabelr()` which calls `polylabelr::poi()`.
#' 
#' 3. The chosen label angles are spread using `spread_degrees()`
#' to ensure the angles are at least `min_degrees` apart
#' from each other. Groups of labels are spaced around the
#' mean angle for each set of labels that are too close together.
#' This step defines the label position outside the full `sp`
#' polygon.
#' 4. The last step is to define a line segment from the outer
#' label position back to the corresponding polygon boundary,
#' using `polygon_label_segment()`. This step has two approaches
#' with subtle differences:
#' 
#'    * `segment_method="vector"` draws the line segment toward
#'    the reference coordinate for each polygon, as defined
#'    by `vector_method`. This method visually draws the line
#'    segment toward where a label might be expected inside
#'    the polygon, which may be visually most intuitive.
#'    However, if the label angles have been changed
#'    by `spread_degrees()` then sometimes this point is not
#'    the closest or most intuitive for the label.
#'    * `segment_method="nearest"` draws the line segment
#'    toward the nearest polygon coordinate to the label
#'    position. Depending upon the polygon shape, the
#'    nearest point may or may not be preferred.
#' 
#' Limitations:
#' * This method does not
#' limit labels within a plot boundary, therefore a plot
#' may need to be re-drawn such that labels will fit inside
#' the plot x-axis and y-axis ranges. When using `venndir()`
#' the argument `expand_fraction` is intended for this purpose,
#' for example try `venndir(..., expand_fraction=0.2)`.
#' * This method does not detect label size, nor label overlaps,
#' when positioning labels around the plot. For example, one
#' may ideally want larger `min_degrees` angles at the top
#' and bottom, but smaller `min_degrees` on the left and right,
#' to accommodate the flow of text. A workaround may be to use
#' a vector of `distance` or `distance_fraction` values to alter
#' the distance of certain labels after visual review.
#' 
#' @family venndir label
#' 
#' @examples
#' setlist <- list(A=letters, B=sample(letters, 4));
#' C <- sample(setdiff(letters, setlist$B), 4);
#' setlist$C <- C;
#' 
#' vo <- venndir(setlist, proportional=FALSE, expand_fraction=0.2);
#' segment1 <- polygon_label_outside(sp=vo$venn_spdf, sp_buffer=-0.1, debug=TRUE)
#' 
#' # example drawing labels manually
#' vo <- venndir(setlist, proportional=FALSE, expand_fraction=0.2);
#' segment1 <- polygon_label_outside(sp=vo$venn_spdf, sp_buffer=-0.1)
#' for (i in seq_along(segment1)) {
#' lines(segment1[[i]], col=vo$venn_spdf$border[i], lwd=2)
#' text(
#'    x=segment1[[i]][2,1],
#'    y=segment1[[i]][2,2],
#'    labels=names(segment1)[i],
#'    adj=c(segment1[[i]][1,"adjx"],
#'       segment1[[i]][1,"adjy"])
#'    )
#' }
#' # an alternative is to use jamba::drawLabels()
#' jamba::drawLabels(txt=vo$venn_spdf$label[i],
#'    boxColor=vo$venn_spdf$color[i],
#'    labelCex=1.5,
#'    adjX=segment1[[i]][,"adjx"],
#'    adjY=segment1[[i]][,"adjy"],
#'    x=segment1[[i]][2,1], y=segment1[[i]][2,2])
#' 
#' D <- sample(letters, 4);
#' setlist$D <- D;
#' vo4 <- venndir(setlist, sets=1:4);
#' polygon_label_outside(vo4$venn_spdf, debug=TRUE, sp_buffer=-0.1)
#' 
#' par("xpd"=TRUE);
#' par("mfrow"=c(4,2));
#' st <- "nearest";
#' vo <- venndir(setlist, proportional=TRUE, seed=1,
#'    expand_fraction=0.2, do_plot=FALSE);
#' for (vm in c("farthest", "label")) {
#' for (cm in c("label", "bbox")) {
#'    for (st in c("vector", "nearest")) {
#'       plot(vo$venn_spdf, xlim=c(-5, 5))
#'       title(cex.main=0.8,
#'          line=1.6,
#'          main=paste0("vector_method:", vm,
#'             "\ncenter_method:", cm,
#'             "\nsegment_method:", st));
#'       segmentxy <- polygon_label_outside(sp=vo$venn_spdf,
#'          which_sp=2:length(vo$venn_spdf),
#'          sp_buffer=-0.1,
#'          center_method=cm,
#'          vector_method=vm,
#'          debug=TRUE)
#'    }
#' }
#' }
#' par("mfrow"=c(1,1))
#' 
#' par("mfrow"=c(1, 2));
#' st <- "nearest";
#' for (cm in c("label", "bbox")) {
#' for (vm in c("farthest", "label")) {
#'    for (st in c("vector", "nearest")) {
#'       set.seed(12);
#'       vo <- venndir(setlist, proportional=TRUE,
#'          expand_fraction=0.2);
#'       title(cex.main=1,
#'          main=paste0("vector_method:", vm,
#'             "\ncenter_method:", cm,
#'             "\nsegment_method:", st));
#'       segmentxy <- polygon_label_outside(sp=vo$venn_spdf,
#'          center_method=cm,
#'          vector_method=vm,
#'          debug=2)
#'    }
#' }
#' }
#' 
#' vo <- venndir(setlist, proportional=TRUE);
#' segmentxy <- polygon_label_outside(sp=vo$venn_spdf, debug=2)
#' 
#' set.seed(1)
#' vo <- venndir(setlist, proportional=TRUE, expand_fraction=0.3);
#' segmentxy <- polygon_label_outside(sp=vo$venn_spdf,
#'    debug=TRUE, segment_method="nearest")
#' segmentxy <- polygon_label_outside(sp=vo$venn_spdf, 
#'    debug=TRUE, segment_method="vector")
#' 
#' @param sp `sp::SpatialPolygonsDataFrame` or `sp::SpatialPolygons`
#'    object, which may contain multiple polygons.
#' @param which_sp `integer` or `NULL`; when `which_sp` contains one
#'    or more `integer` values, they refer to polygons in `sp`, and
#'    each will be analyzed in sequence. When `which_sp=NULL` then
#'    all the polygons in `sp` will be analyzed in sequence.
#' @param center `numeric` vector or matrix with two values indicating
#'    the center position. When `center=NULL` then the center is
#'    determined using a method defined by `center_method`.
#' @param distance `numeric` value indicating the absolute coordinate
#'    distance away from the perimiter of `sp` to place labels.
#'    This value is effectively the same as buffer used in
#'    `rgeos::gBuffer()`. A vector of values can be provided,
#'    to apply to each polygon in `which_sp`. When `distance=NULL`
#'    the `distance_fraction` is used.
#' @param distance_fraction `numeric` which defines `distance` as
#'    a fraction of the x-axis and y-axis range. Note that
#'    `distance_fraction` is only used when `distance=NULL`.
#' @param center_method `character` string indicating the method to
#'    determine the `center`:
#'    * `"label"` uses the mean x,y coordinate
#'    of all the polygon label positions;
#'    * `"bbox"` uses the mean x,y
#'    coordinate of the bounding box that encompasses `sp`.
#'    
#'    The effect
#'    is to extend outer labels radially around this center point.
#'    Using the mean label position with `center_method="label"`
#'    is helpful because it ensures labels are extended in all directions
#'    even when most labels are in one upper or lower section of
#'    the `sp` polygons.
#' @param vector_method `character` string indicating the point to draw
#'    a vector from the `center` position for each polygon:
#'    * `"farthest"` chooses the farthest point in each polygon from
#'    the `center` point;
#'    * `"label"` uses the position of the label.
#'    
#'    Using `vector_method="farthest"` works well to label the outer
#'    edge of each polygon. Using `vector_method="label"` works well
#'    when the polygon label is set based upon the interior open area
#'    of the polygon.
#' @param segment_method `character` string indicating how to draw the
#'    line segment from the outside label back to the polygon:
#'    * `"vector"` defines the line segment by extending the line
#'    used by `vector_method` along the same direction;
#'    * `"nearest"` defines a new line to the nearest point on the polygon.
#' @param min_degrees `numeric` value passed to `spread_degrees()`
#'    with the minimum angle in degrees for labels to be placed
#'    around the outside of a polygon.
#' @param sp_buffer `numeric` value passed to `polygon_label_segment()`
#'    which defines a buffer when determining the line segment
#'    position at each polygon in `which_sp`. A negative value
#'    is recommended, which will create a buffer inside the polygon,
#'    causing the line segment to end slightly inside each polygon.
#'    This effect may be preferred when more than one polygon
#'    boundary is overlapping. An example value `sp_buffer=0.1`.
#'    Note that `sp_buffer` is applied to each polygon in `which_sp`
#'    and a vector of values can be provided.
#' @param seed `numeric` value passed to `set.seed()` to ensure
#'    reproducible results, or `seed=NULL` to enable random
#'    results.
#' @param debug `logical` indicating whether to plot the results
#'    using base R graphics. When `debug=2` it will also add some
#'    visual decorations showing the center point, the points
#'    determined by polygon boundaries.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#' 
#' @export
polygon_label_outside <- function
(sp,
   which_sp=NULL,
   center=NULL,
   distance=NULL,
   distance_fraction=0.05,
   center_method=c("label", "bbox"),
   vector_method=c("label", "farthest"),
   segment_method=c("nearest", "vector"),
   offset_fraction=c(0, -0.05),
   min_degrees=15,
   sp_buffer=0,
   relative=TRUE,
   font_cex=1,
   seed=1,
   debug=FALSE,
   verbose=FALSE,
   ...)
{
   #
   # logical workflow:
   # - define center point for the full source polygon
   # -
   #
   if (length(seed) == 1) {
      set.seed(seed);
   }
   center_method <- match.arg(center_method);
   vector_method <- match.arg(vector_method);
   segment_method <- match.arg(segment_method);
   
   # default which_sp uses every polygon
   if (length(which_sp) == 0) {
      which_sp <- seq_len(length(sp));
   }
   if (verbose) {
      jamba::printDebug("polygon_label_outside(): ",
         "which_sp:",
         which_sp);
   }
   
   # sp_buffer
   if (length(sp_buffer) == 0) {
      sp_buffer <- 0;
   }
   sp_buffer <- rep(sp_buffer,
      length.out=length(which_sp));
   names(sp_buffer) <- which_sp;
   
   # get bbox for the whole polygon
   if ("JamPolygon" %in% class(sp)) {
      spall <- union_JamPolygon(sp)
      spbox <- bbox_JamPolygon(sp);
   } else {
      spall <- rgeos::gUnaryUnion(sp);
      spbox <- sp::bbox(sp);
   }
   
   # expand the bounding box
   spbox_ex <- spbox;
   spbox_ex[] <- t(apply(spbox, 1, expand_range, 0.2));
   
   # verify distance
   if (length(distance) == 0) {
      if (length(distance_fraction) == 0) {
         stop("distance or distance_fraction must be supplied");
      }
      #distance <- diff(par("usr")[1:2]) * distance_fraction;
      distance <- diff(spbox[1,c(1,2)]) * distance_fraction;
   }
   distance <- rep(distance,
      length.out=length(which_sp));
   names(distance) <- which_sp;
   if (verbose) {
      jamba::printDebug("polygon_label_outside(): ",
         "distance:",
         format(distance, digits=2, trim=TRUE));
   }
   
   # verify offset
   if (length(offset_fraction) == 0) {
      offset <- c(0, 0);
   } else {
      offset_fraction <- rep(offset_fraction, length.out=2);
      offset <- c(diff(spbox[1,c(1,2)]) * offset_fraction[1],
         diff(spbox[2,c(1,2)]) * offset_fraction[2]);
   }
   offset <- offset + rnorm(2) * 1e-10;
   
   # get center
   if (length(center) != 2) {
      if ("bbox" %in% center_method) {
         # use middle of bounding box
         center <- matrix(ncol=2, rowMeans(spbox)) + (rnorm(2) / 1000);
      } else {
         # use mean of label positions
         if ("JamPolygon" %in% class(sp)) {
            center <- labelr_JamPolygon(sp);
         } else {
            center <- matrix(ncol=2,
               colMeans(jamba::rbindList(lapply(seq_len(length(sp)), function(iwhich){
                  unlist(
                     sp_polylabelr(sp::geometry(sp)[iwhich]))
               }))[,c("x", "y")]));
         }
      }
   } else {
      center <- matrix(ncol=2, center);
   }
   center <- center + offset;
   if (verbose) {
      jamba::printDebug("polygon_label_outside(): ",
         "center:",
         format(center, digits=2, trim=TRUE));
   }
   if (debug > 1) {
      points(center, col="navy", pch=20, cex=0.5);
      points(center, col="navy", pch="c", cex=2);
   }
   
   # utility function to get farthest polygon point from reference point
   farthest_point_from_polygon <- function
   (sp,
      xy)
   {
      # get polygon points
      if ("JamPolygon" %in% class(sp)) {
         spx <- unlist(sp@polygons[, "x"])
         spy <- unlist(sp@polygons[, "y"])
         spxy <- cbind(x=spx, y=spy);
      } else {
         spi <- get_largest_polygon(sp);
         #plot(spi, col="green", border="green3", add=TRUE)
         spxy <- spi@polygons[[1]]@Polygons[[1]]@coords;
      }
      xy <- matrix(ncol=ncol(spxy), rep(xy, length.out=ncol(spxy)));
      xydist <- as.matrix(dist(rbind(xy, spxy)))[-1, 1];
      xymax <- spxy[which.max(xydist), , drop=FALSE];
      colnames(xymin) <- c("x", "y");
      return(xymax);
   }
   
   # utility function to get farthest polygon point from reference point
   nearest_point_to_polygon <- function
   (sp,
      xy)
   {
      # get polygon points
      if ("JamPolygon" %in% class(sp)) {
         spx <- unlist(sp@polygons[, "x"])
         spy <- unlist(sp@polygons[, "y"])
         spxy <- cbind(x=spx, y=spy);
      } else {
         spi <- get_largest_polygon(sp);
         spxy <- spi@polygons[[1]]@Polygons[[1]]@coords;
      }
      xy <- matrix(ncol=ncol(spxy), rep(xy, length.out=ncol(spxy)));
      xydist <- as.matrix(dist(rbind(xy, spxy)))[-1, 1];
      xymin <- spxy[which.min(xydist), , drop=FALSE];
      colnames(xymin) <- c("x", "y");
      cbind(xymin, dist=xydist);
   }
   
   # reference coordinate for each polygon
   polyref_xy <- jamba::rbindList(lapply(which_sp, function(iwhich){
      # get sub-polygon
      if ("JamPolygons" %in% class(sp)) {
         isp <- sp[iwhich, ];
      } else if ("SpatialPolygonsDataFrame" %in% class(sp)) {
         isp <- sp[iwhich, ];
      } else {
         isp <- sp[iwhich];
      }
      if (debug > 1) {
         if ("JamPolygons" %in% class(sp)) {
            plot(isp, do_newpage=FALSE, do_pop_viewport=FALSE)
         } else {
            sp::plot(isp, col="#44000011", add=TRUE)
         }
      }
      
      # get point farthest from center
      if ("farthest" %in% vector_method) {
         # get farthest point from xy
         xymax <- farthest_point_from_polygon(isp, center)[1,,drop=FALSE];
      } else {
         # possible check to see if xymax equals center or
         # within a small fraction compared to total bbox size,
         # if so then consider using farthest point
         if ("JamPolygon" %in% class(sp)) {
            xymax <- labelr_JamPolygon(isp);
         } else {
            xymax <- matrix(ncol=2,
               unlist(sp_polylabelr(isp))[c("x", "y")]);
         }
         if (all(xymax == center)) {
            if (verbose) {
               jamba::printDebug("polygon_label_outside(): ",
                  "The polygon xy for which_sp=", iwhich,
                  " equals the center. Using farthest_point_from_polygon() as a workaround.");
            }
            xymax <- farthest_point_from_polygon(isp, center)[1,,drop=FALSE];
         }
      }
      if (debug > 1) {
         if ("JamPolygon" %in% class(sp)) {
            # grid::grid.points(x=xymax[,1], y=xymax[,2])
         } else {
            points(xymax, col="purple3", pch=20, cex=3);
         }
      }
      xymax;
   }));
   rownames(polyref_xy) <- which_sp;
   
   # iterate multiple polygons to find angles
   angles1 <- sapply(which_sp, function(iwhich){
      xymax <- polyref_xy[as.character(iwhich), , drop=FALSE];
      # get reference position
      x1 <- c(center[1, 1], xymax[1, 1]);
      y1 <- c(center[1, 2], xymax[1, 2]);
      
      # add a small random value to prevent identical angles
      angle <- jamba::rad2deg(
         atan2(y=diff(y1), x=diff(x1)) + 
            rnorm(1) * 1e-6) %% 360;
      return(angle);
   });
   if (verbose) {
      jamba::printDebug("polygon_label_outside(): ",
         "degree angles from initial calculation:",
         format(angles1, digits=2, trim=TRUE));
   }
   
   # logic here to spread out angles too close to each other
   angles <- spread_degrees(angles1,
      min_degrees=min_degrees);
   names(angles) <- which_sp;
   if (verbose) {
      jamba::printDebug("polygon_label_outside(): ",
         "degree angles after spread_degrees():",
         format(angles, digits=2, trim=TRUE));
      if ("JamPolygon" %in% class(sp)) {
         inames <- names(sp);
      } else if ("SpatialPolygonsDataFrame" %in% class(sp)) {
         inames <- rownames(data.frame(sp));
      } else {
         inames <- names(sp);
      }
      inames <- inames[which_sp];
      adf <- data.frame(angles_in=angles1,
         angles_spread=angles,
         angle_diff=angles-angles1,
         name=inames);
   }
   
   # expand to twice the bbox size
   max_radius <- max(rowDiffs(spbox)) * 2;
   
   if (debug > 1) {
      for (idistance in unique(distance)) {
         if (!"JamPolygon" %in% class(sp)) {
            sp::plot(rgeos::gBuffer(spall, width=idistance),
               col=NA, border="grey60", lty=2, add=TRUE);
         }
      }
   }
   
   # for each angle find the line segment
   segmentxy_list <- lapply(which_sp, function(iwhich){
      # get sub-polygon
      if ("SpatialPolygonsDataFrame" %in% class(sp)) {
         isp <- sp[iwhich,];
         iname <- rownames(data.frame(isp));
      } else {
         isp <- sp[iwhich];
         iname <- names(isp);
      }
      # define point outside using angle and max_radius
      idistance <- distance[as.character(iwhich)];
      angle <- angles[[as.character(iwhich)]];
      xedge <- cos(jamba::deg2rad(angle)) * 
         (max_radius + idistance) + center[1,1];
      yedge <- sin(jamba::deg2rad(angle)) * 
         (max_radius + idistance) + center[1,2];
      
      # Find the point at the encompassing polygon outer edge.
      # This point is the outside label position.
      plsxy1 <- polygon_label_segment(
         sp=spall,
         sp_buffer=idistance,
         relative=FALSE,
         x0=xedge,
         y0=yedge,
         x1=center[1,1],
         y1=center[1,2]);
      if (debug > 1) {
         points(plsxy1, col="darkorange", pch=2, cex=1);
      }
      
      # find the point at the specific polygon outer edge
      if ("vector" %in% segment_method) {
         # use the boundary point on the vector to the inside label position
         # not the closest point on the inside polygon
         x1use <- polyref_xy[as.character(iwhich),1];
         y1use <- polyref_xy[as.character(iwhich),2];
      } else {
         # use closest point on the inside polygon
         # not along the vector to the inside label position
         x1use <- plsxy1[1,1];
         y1use <- plsxy1[1,2];
      }
      # Todo: adapt polygon_label_segment() for JamPolygon input.
      plsxy <- polygon_label_segment(
         sp=isp,
         sp_buffer=sp_buffer[[as.character(iwhich)]],
         relative=relative,
         x0=plsxy1[1,1],
         y0=plsxy1[1,2],
         x1=x1use,
         y1=y1use);
      if (debug > 1) {
         points(plsxy, col="red", pch=2, cex=2);
      }
      
      # line segment
      segmentxy <- cbind(
         x=unname(c(plsxy[1,1], plsxy1[1,1])),
         y=unname(c(plsxy[1,2], plsxy1[1,2])),
         degrees=unname(angle));
      rownames(segmentxy) <- c("border", "label");
      adjdf <- degrees_to_adj(angle,
         top=90,
         clockwise=FALSE);
      segmentxy <- cbind(segmentxy,
         adjx=adjdf[,1])
      segmentxy <- cbind(segmentxy,
         adjy=adjdf[,2])
      if (debug) {
         lines(segmentxy,
            col="black",
            lwd=2);
         jamba::drawLabels(
            txt=iname,
            drawBox=FALSE,
            labelCex=1 * font_cex,
            adjX=adjdf[,1],
            adjY=adjdf[,2],
            x=segmentxy[2,1],
            y=segmentxy[2,2])
      }
      return(segmentxy);
   });
   # Todo: Adapt below to handle class "JamPolygon"
   if ("SpatialPolygonsDataFrame" %in% class(sp)) {
      if (length(rownames(data.frame(sp))) == 0) {
         names(segmentxy_list) <- which_sp;
      } else {
         names(segmentxy_list) <- rownames(data.frame(sp))[which_sp];
      }
   } else {
      if (length(names(sp)) == 0) {
         names(segmentxy_list) <- which_sp;
      } else {
         names(segmentxy_list) <- names(sp)[which_sp];
      }
   }
   return(invisible(segmentxy_list));
}
