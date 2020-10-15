
#' Convert eulerr() output to polygons
#' 
#' # @import sp
#' 
#' @family venndir utility
#' 
#' @examples
#' if (require(eulerr)) {
#' }
#' 
#' @export
eulerr2polys <- function
(x)
{
   # x <- va
   ellipses1 <- eulerr:::ellipse(h=x$ellipses$h,
      k=x$ellipses$k,
      a=x$ellipses$a,
      b=x$ellipses$b,
      phi=x$ellipses$phi);
   names(ellipses1) <- rownames(x$ellipses);
   ellipses <- lapply(ellipses1, function(i){
      do.call(cbind, i)
   });
   
   ## list of sp::Polygon objects
   ellipses_P <- lapply(ellipses, function(i){
      sp::Polygon(i)
   })
   #jamba::sdim(ellipses_P);
   
   ## sp::SpatialPolygon object (numbered)
   ellipses_SP1 <- sp::SpatialPolygons(
      lapply(seq_len(length(ellipses_P)), function(i2){
         iName <- as.character(i2);
         ell1 <- ellipses_P[[i2]];
         sp::Polygons(list(ell1),
            iName);
      }),
      pO=seq_len(length(ellipses_P)));
   
   ## sp::SpatialPolygon object (named)
   ellipses_SP <- sp::SpatialPolygons(
      lapply(names(ellipses_P), function(i2){
         iName <- as.character(i2);
         ell1 <- ellipses_P[[i2]];
         sp::Polygons(list(ell1), iName);
      }),
      pO=seq_len(length(ellipses_P)));
   return(ellipses_SP);
}

#' Find Venn polygon overlaps
#' 
#' Find Venn polygon overlaps
#' 
#' This function takes a named list of polygons
#' and returns the combination of polygon overlaps
#' as used in a Venn diagram.
#' 
#' When a vector of Venn counts is supplied, the
#' counts are associated with the respective polygon,
#' and any counts not represented by a polygon
#' are returned as an attribute `"venn_missing"`.
#' 
#' @return `sp::SpatialPolygonsDataFrame` object, with
#'    additional columns:
#'    * `"label"`
#'    * `"venn_counts"`
#'    * `"venn_color"`
#'    * `"x_label"`
#'    * `"y_label"`
#' 
#' @family venndir utility
#' 
#' @param sp `sp::SpatialPolygons` object that contains one polygon
#'    per set. Therefore for a three-way Venn diagram, the
#'    `sp` will contain three polygons. The `sp` object is expected
#'    to be named using set names.
#' @param venn_counts `vector` with `integer` values, whose names
#'    represent each Venn overlap set combination, using
#'    `sep` as delimiter between set names.
#' @param venn_items `list` or `NULL` that contains items in each
#'    overlap set.
#' @param sep `character` string used as a delimiter between set names.
#' @param preset,blend_preset `character` string passed to
#'    `colorjam::rainbowJam()` and `colorjam::blend_colors()`,
#'    respectively, to define the color hue wheel used for categorical
#'    colors, and for color blending. The default `preset="dichromat"`
#'    chooses color-blindness-friendly categorical colors, and
#'    `blend_preset="ryb"` blends multiple colors using a red-yellow-blue
#'    color wheel, consistent with paint-type color combinations.
#' @param sp_nudge,rotate_degrees passed to `nudge_sp()` to allow manual
#'    adjustment of `sp` objects.
#' @param do_plot `logical` indicating whether to plot the output
#'    `SpatialPolygonsDataFrame` object.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to supporting functions
#'    `colorjam::group2colors()`, `colorjam::blend_colors()`, `nudge_sp()`.
#' 
#' @examples
#' # simple Venn circles
#' test_counts <- c(A=5, B=10, C=3, `B&C`=2)
#' sp <- get_venn_shapes(counts=test_counts, proportional=TRUE)
#' 
#' # Venn overlap polygons
#' spdf <- find_vennpoly_overlaps(sp, venn_counts=test_counts)
#' 
#' # behold the data.frame annotations
#' data.frame(spdf)
#' 
#' # simple Venn plot
#' plot(spdf, col=spdf$color)
#' text(x=spdf$x_label,
#'    y=spdf$y_label,
#'    labels=paste0(spdf$label,
#'       "\n",
#'       spdf$venn_counts))
#' 
#' @export
find_vennpoly_overlaps <- function
(sp,
 venn_counts=NULL,
 venn_items=NULL,
 venn_colors=NULL,
 sep="&",
 preset="dichromat",
 blend_preset="ryb",
 sp_nudge=NULL,
 rotate_degrees=0,
 do_plot=FALSE,
 verbose=FALSE,
 ...)
{
   numSets <- length(sp);
   if (length(venn_colors) == 0) {
      venn_colors <- colorjam::group2colors(names(sp),
         preset=preset,
         ...);
   }
   if (length(names(venn_colors)) == 0) {
      names(venn_colors) <- names(sp);
   }
   
   ## define incidence matrix of overlaps
   el1 <- make_venn_combn_df(names(sp),
      sep=sep);
   if (verbose) {
      jamba::printDebug("find_vennpoly_overlaps(): ",
         "head(el1):");
      print(head(el1, 20));
   }
   
   ## Optionally nudge individual polygon coordinates
   if (length(sp_nudge) > 0 && any(unlist(sp_nudge) != 0)) {
      sp <- nudge_sp(sp,
         sp_nudge=sp_nudge,
         ...);
   }
   
   ## Optionally rotate all polygon coordinates
   if (length(rotate_degrees) > 0 && any(rotate_degrees != 0)) {
      sp <- rescale_sp(sp,
         rotate_degrees=rotate_degrees,
         ...);
   }
   
   if (length(venn_counts) > 0) {
      venn_counts_names <- strsplit(names(venn_counts), "&");
   }
   if (length(venn_items) > 0) {
      venn_items_names <- strsplit(names(venn_items), "&");
   }
   
   ## calculate venn overlap polygons
   #vennCoords <- lapply(1:nrow(el1), function(j){
   venn_poly_coords <- lapply(1:nrow(el1), function(j){
      i <- el1[j,];
      if (verbose) {
         jamba::printDebug("find_vennpoly_overlaps(): ",
            rownames(el1)[j]);
      }
      whichYes <- which(i %in% 1);
      whichNo <- which(i %in% 0);
      i_names <- colnames(el1)[whichYes];
      venn_color <- colorjam::blend_colors(venn_colors[i_names],
         preset=blend_preset,
         ...);
      poly_name <- paste(
         jamba::mixedSort(colnames(el1)[whichYes]),
         collapse="&");
      if (length(venn_counts) > 0) {
         venn_match <- match_list(list(i_names), venn_counts_names);
         if (is.na(venn_match)) {
            venn_poly_count <- 0;
         } else {
            venn_poly_count <- venn_counts[[venn_match]];
         }
      } else {
         venn_poly_count <- 0;
      }
      if (length(venn_items) > 0) {
         match_list(list(i_names), venn_counts_names)
         venn_match <- match_list(list(i_names), venn_items_names);
         if (is.na(venn_match)) {
            venn_poly_items <- character(0);
         } else {
            venn_poly_items <- venn_items[[venn_match]];
         }
      } else {
         venn_poly_items <- character(0);
      }
      ## Intersection of relevant sets
      if (length(whichYes) >= 1) {
         ellYes <- intersect_polygons(sp[whichYes]);
      }
      if (length(ellYes) == 0) {
         ellUse <- list();
      } else {
         if (length(whichNo) >= 1) {
            ellNo <- union_polygons(sp[whichNo]);
            ellUse <- rgeos::gDifference(ellYes, ellNo);
         } else {
            ellNo <- NULL;
            ellUse <- ellYes;
         }
         if (length(ellUse) > 0) {
            if (length(ellUse@polygons) == 1) {
               ellUse@polygons[[1]]@ID <- poly_name;
            } else {
               for (j1 in seq_along(ellUse@polygons)) {
                  new_id <- as.character((j-1)*10 + j1);
                  ellUse@polygons[[j1]]@ID <- new_id;
               }
            }
         } else {
            ellUse <- list();
         }
      }
      attr(ellUse, "venn_name") <- poly_name;
      attr(ellUse, "venn_count") <- venn_poly_count;
      attr(ellUse, "venn_items") <- venn_poly_items;
      attr(ellUse, "venn_color") <- venn_color;
      ellUse;
   })
   names(venn_poly_coords) <- rownames(el1);
   
   venn_poly_colors <- sapply(venn_poly_coords, function(i){
      attr(i, "venn_color");
   });
   venn_poly_counts <- sapply(venn_poly_coords, function(i){
      attr(i, "venn_count");
   });
   venn_poly_items <- sapply(venn_poly_coords, function(i){
      attr(i, "venn_items");
   });
   
   ## sp::SpatialPolygon object (named)
   vennUse <- which(lengths(venn_poly_coords) > 0);
   vennMissing <- which(lengths(venn_poly_coords) == 0);
   venn_SP <- do.call(sp:::rbind.SpatialPolygons, venn_poly_coords[vennUse]);
   venn_spdf <- sp::SpatialPolygonsDataFrame(venn_SP,
      data.frame(color=venn_poly_colors[vennUse],
         label=names(venn_SP),
         stringsAsFactors=FALSE));
   
   #jamba::printDebug("names(venn_SP):", names(venn_SP));
   #jamba::printDebug("names(venn_poly_counts):", names(venn_poly_counts));
   #jamba::printDebug("vennUse:", vennUse);
   venn_spdf$venn_counts <- venn_poly_counts[vennUse];
   venn_spdf$venn_color <- venn_poly_colors[vennUse];
   
   if (do_plot) {
      plot(venn_SP,
         col=venn_poly_colors[vennUse],
         border=jamba::makeColorDarker(venn_poly_colors[vennUse]))
      plot(sp,
         col="transparent",
         border=alpha2col(alpha=0.5, venn_colors),
         lwd=2,
         add=TRUE)
   }
   
   ## Get central label from each polygon
   ## Optional: allow using sp::coordinates()
   venn_labels_xys <- lapply(venn_poly_coords[vennUse], function(i){
      i_sp <- get_largest_polygon(i);
      ixy <- i_sp@polygons[[1]]@Polygons[[1]]@coords;
      polylabelr::poi(ixy, precision=0.01)
   });
   venn_labels_xy <- jamba::rbindList(venn_labels_xys);
   venn_spdf$x_label <- unname(unlist(venn_labels_xy[,1]));
   venn_spdf$y_label <- unname(unlist(venn_labels_xy[,2]));
   if (do_plot) {
      points(venn_labels_xy, pch=20,
         col=jamba::makeColorDarker(venn_poly_colors));
   }
   
   ## TODO: handle labels not represented
   venn_missing <- venn_poly_coords[vennMissing];
   attr(venn_spdf, "venn_missing") <- venn_missing;
   
   return(invisible(venn_spdf));
}

#' Intersect one or more polygons
#' 
#' Intersect one or more polygons
#' 
#' This function takes a `list` of `sp::SpatialPolygons`
#' objects, and performs iterative `rgeos::gIntersection()` on
#' the full list, since `rgeos::gIntersection()` only operates on
#' two polygons at a time.
#' 
#' @return object `sp::SpatialPolygons`
#' 
#' @family venndir utility
#' 
#' @param sp `list` object that contains one or more
#'    `sp::SpatialPolygons`.
#' @param ... additional arguments are ignored.
#' 
#' @export
intersect_polygons <- function
(sp,
 ...)
{
   ## Purpose is to use rgeos gIntersection() on more than 2 Polygons objects,
   ## but allowing 1, 2, or more than 2.
   if (!suppressPackageStartupMessages(require(rgeos))) {
      stop("The rgeos (and thus the sp) package is required for this function.");
   }
   if (length(sp) <= 1) {
      return(sp);
   } else {
      spatPolyInt <- sp[1];
      for (i in 2:length(sp)) {
         spatPolyInt <- rgeos::gIntersection(spatPolyInt,
            sp[i]);
         if (length(spatPolyInt) == 0) {
            return(spatPolyInt);
         }
      }
      return(spatPolyInt);
   }
}

#' Union one or more polygons
#' 
#' Union one or more polygons
#' 
#' This function takes a `list` of `sp::SpatialPolygons`
#' objects, and performs iterative `rgeos::gUnion()` on
#' the full list, since `rgeos::gUnion()` only operates on
#' two polygons at a time.
#' 
#' @return object `sp::SpatialPolygons`
#' 
#' @family venndir utility
#' 
#' @param sp `list` object that contains one or more
#'    `sp::SpatialPolygons`.
#' @param ... additional arguments are ignored.
#' 
#' @export
union_polygons <- function
(sp,
 ...)
{
   ## Purpose is to use rgeos gUnion() on more than 2 Polygons objects,
   ## but allowing 1, 2, or more than 2.
   if (!suppressPackageStartupMessages(require(rgeos))) {
      stop("The rgeos (and thus the sp) package is required for this function.");
   }
   if (length(sp) <= 1) {
      return(sp);
   } else {
      spatPolyInt <- sp[1];
      for (i in 2:length(sp)) {
         spatPolyInt <- rgeos::gUnion(spatPolyInt, sp[i]);
      }
      return(spatPolyInt);
   }
}

#' Match list elements to another list
#' 
#' Match list elements to another list
#' 
#' This function takes two `list` objects, and matches
#' the first `list` elements to the second `list`.
#' 
#' Each list contains list elements, for example if `x`
#' is a `list`, then the element in position `i` is accessed
#' using `x[[i]]`.
#' 
#' A match between `x[[i]]` and `y[[j]]` is defined as follows:
#' 
#' * all elements in `x[[i]]` are contained in `y[[j]]`
#' * all elements in `y[[j]]` are contained in `x[[i]]`
#' 
#' For this function, item order and item duplication is
#' ignored.
#' 
#' This function uses logic in the form `all(x[[i]] %in% y[[j]])`,
#' so it will operate properly with input objects compatible
#' with that format. The function is intended to be used with
#' `list` that contains `atomic` `vectors`.
#' 
#' @return `integer` `vector` with the same length as `x`. The
#'    integer values give the position in `y` of the first match.
#' 
#' @family venndir utility
#' 
#' @examples
#' x <- list(a=LETTERS[1],
#'    b=LETTERS[1:2],
#'    c=LETTERS[2:4]);
#' x;
#' y <- list(
#'    d=LETTERS[1:2],
#'    e=LETTERS[2],
#'    f=LETTERS[2:4]);
#' y;
#' match_list(x, y)
#' match_list(y, x)
#' 
#' @export
match_list <- function
(x,
 y,
 ...)
{
   sapply(x, function(i){
      for(j in seq_along(y)) {
         if (all(i %in% y[[j]]) && all(y[[j]] %in% i)) {
            return(j)
         }
      }
      NA;
   })
}

#' Largest polygon in a SpatialPolygons object
#' 
#' Largest polygon in a SpatialPolygons object
#' 
#' This function returns the largest polygon in a
#' `sp::SpatialPolygons` object, notably when there
#' are multiple polygons contained in one object.
#' The function calculates the largest area using
#' `rgeos:gArea()`.
#' 
#' If two polygons have identical area, the first
#' polygon is returned.
#' 
#' This function calls a helper function `sp::disaggregate()`
#' which creates one `sp::SpatialPolygons` object
#' for each individual polygon in the source object.
#' 
#' @family venndir utility
#' 
#' @param sp object with class `sp::SpatialPolygons` that may
#'    contain one or more closed polygons.
#' @param ... additional arguments are ignored.
#' 
#' @export
get_largest_polygon <- function
(sp,
 ...)
{
   ## Subset multi-polygon entries to use the largest polygon
   sp_dis <- sp::disaggregate(sp);
   if (length(sp_dis) > 1) {
      sp_area <- sapply(1:length(sp_dis), function(i){
         rgeos::gArea(sp_dis[i]);
      });
      sp_out <- sp_dis[which.max(sp_area)];
   } else {
      sp_out <- sp;
   }
   return(sp_out);
}

#' Make SpatialPolygons circles
#' 
#' Make SpatialPolygons circles
#' 
#' This function creates one or more circles as
#' `sp::SpatialPolygons` objects.
#' 
#' @family venndir utility
#' 
#' @return object `sp::SpatialPolygons` with a number of circles
#'    defined by `length(xcenter)`.
#' 
#' @param xcenter,ycenter `numeric` vector that defines the x and y
#'    coordinate position of the center of each circle.
#' @param setnames `vector` that contains names for each circle, stored
#'    in the `sp::SpatialPolygons` object as the polygon name. When
#'    `setnames` is `NULL` then integer index positions are used.
#' @param radius `numeric` vector that defines the radius of each circle.
#'    This `vector` is recycled to `length(xcenter)`.
#' @param n `integer` value indicating the number of subdivisions to
#'    use in the circle.
#' @param ... additional arguments are ignored.
#' 
#' @export
sp_circles <- function
(xcenter,
 ycenter,
 setnames=NULL,
 radius=1,
 n=60,
 ...)
{
   angle_seq <- head(
      seq(from=0,
         to=pi*2,
         length.out=n+1),
      n);
   if (length(setnames) == 0) {
      setnames <- seq_along(x);
   }
   xvals <- sin(angle_seq);
   yvals <- cos(angle_seq);
   if (length(radius) == 0) {
      radius <- 1;
   }
   radius <- rep(radius,
      length.out=length(xcenter));
   
   ell_sp <- sp::SpatialPolygons(lapply(seq_along(xcenter), function(i){
      sp::Polygons(list(
         sp::Polygon(cbind(xvals * radius[i] + xcenter[i],
            yvals * radius[i] + ycenter[i]))),
         setnames[i])
   }), pO=seq_along(xcenter));
   invisible(ell_sp);
}

#' Make SpatialPolygons ellipses
#' 
#' Make SpatialPolygons ellipses
#' 
#' This function creates one or more ellipses as
#' `sp::SpatialPolygons` objects.
#' 
#' @family venndir utility
#' 
#' @return object `sp::SpatialPolygons` with a number of ellipses
#'    defined by `length(xcenter)`.
#' 
#' @param xcenter,ycenter `numeric` vector that defines the x and y
#'    coordinate position of the center of each ellipse.
#' @param setnames `vector` that contains names for each ellipse, stored
#'    in the `sp::SpatialPolygons` object as the polygon name. When
#'    `setnames` is `NULL` then integer index positions are used.
#' @param xradius,yradius `numeric` vector that defines the radius
#'    of each ellipse along the x-axis and y-axis, respectively.
#'    Each `vector` is recycled to `length(xcenter)`.
#' @param rotation_degree `numeric` vector representing degrees to
#'    rotate each ellipse after it is created, where values are
#'    conformed to between `0` and `360`, rotating clockwise.
#' @param n `integer` value indicating the number of subdivisions to
#'    use in the circle.
#' @param ... additional arguments are ignored.
#' 
#' @export
sp_ellipses <- function
(xcenter,
 ycenter,
 setnames=NULL,
 xradius=1,
 yradius=2,
 rotation_degrees=c(0),
 n=60,
 ...)
{
   angle_seq <- head(
      seq(from=0,
         to=pi*2,
         length.out=n+1),
      n);
   if (length(setnames) == 0) {
      setnames <- seq_along(xcenter);
   }
   
   xvals <- sin(angle_seq);
   yvals <- cos(angle_seq);

   #ellipse <- cbind(radius[1] * cos(angles), radius[2] * sin(angles));
   #ellipse <- cbind(ellipse[,1]*cos(rotate) + ellipse[,2]*sin(rotate), ellipse[,2]*cos(rotate) - ellipse[,1]*sin(rotate) );
   #ellipse <- cbind(center[1]+ellipse[,1], center[2]+ellipse[,2]);
   
   xvals <- sin(angle_seq);
   yvals <- cos(angle_seq);
   if (length(xradius) == 0) {
      xradius <- 1;
   }
   if (length(yradius) == 0) {
      yradius <- 2;
   }
   xradius <- rep(xradius,
      length.out=length(xcenter));
   yradius <- rep(yradius,
      length.out=length(xcenter));
   if (length(rotation_degrees) == 0) {
      rotation_degrees <- 0;
   }
   rotation_degrees <- rep(rotation_degrees,
      length.out=length(xcenter));
   rotation_rad <- jamba::deg2rad(rotation_degrees);
   
   ell_sp <- sp::SpatialPolygons(lapply(seq_along(xcenter), function(i){
      i_xvals <- (xvals * xradius[i]);
      i_yvals <- (yvals * yradius[i]);
      e_xvals <- (i_xvals * cos(rotation_rad[i]) + i_yvals * sin(rotation_rad[i]));
      e_yvals <- (i_yvals * cos(rotation_rad[i]) - i_xvals * sin(rotation_rad[i]));
      sp::Polygons(list(
         sp::Polygon(cbind(e_xvals + xcenter[i],
            e_yvals + ycenter[i]))),
         setnames[i])
   }), pO=seq_along(xcenter));
   invisible(ell_sp);
}

#' Nudge SpatialPolygons
#' 
#' Nudge SpatialPolygons
#' 
#' This helper function is intended to take `sp::SpatialPolygons`
#' and "nudge" (move by adding a scalar value to each coordinate)
#' only a subset of polygons identified by name.
#' 
#' It differs from `rescale_sp()` because `rescale_sp()` manipulates
#' all contained polygons together.
#' 
#' @family venndir utility
#' 
#' @return object `sp::SpatialPolygons`.
#' 
#' @param sp `sp::SpatialPolygons` object
#' @param sp_nudge `list` whose names are found in `names(sp)`,
#'    and whose values are `x` and `y` coordinates to be moved.
#' @param rotate_degrees `numeric` value in degrees (0, 360) to
#'    rotate the `sp` object and all contained polygons. (Not yet
#'    implemented.)
#' @param ... additional arguments are ignored.
#' 
#' @export
nudge_sp <- function
(sp=NULL,
 sp_nudge=NULL,
 ...)
{
   ## Optionally nudge the polygon coordinates
   if (length(sp) == 0) {
      return(sp);
   }
   if (!inherits(sp, "SpatialPolygons")) {
      stop("sp must inherit from 'SpatialPolygons'");
   }
   if (inherits(sp, "SpatialPolygonsDataFrame")) {
      sp_names <- rownames(data.frame(sp));
   } else {
      sp_names <- names(sp);
   }

   ## shift sp by coordinates
   if (length(sp_nudge) > 0 &&
         is.list(sp_nudge) &&
         any(names(sp_nudge) %in% sp_names)) {
      for (i in names(sp_nudge)[names(sp_nudge) %in% sp_names]) {
         j <- match(i, sp_names);
         i_nudge <- sp_nudge[[i]];
         sp@polygons[[j]] <- rescale_ps(sp@polygons[[j]],
            shift=i_nudge);
      }
   }

   return(invisible(sp));
}


#' Arrange text labels inside a polygon
#' 
#' Arrange text labels inside a polygon
#' 
#' This function takes a vector of text labels, and will
#' arrange those labels to fill the inside of a polygon.
#' Currently the method uses `sp::spsample()` which
#' has a few algorithms to generate point positions
#' inside a polygon, and these positions are used to
#' anchor text labels. Currently this method does no
#' overlap detection.
#' 
#' The primary method to avoid overlap is to use
#' `label_method="hexagon"` and `angle=20`, since hexagonal
#' layout tends to have points at roughly 0 and 60 degrees
#' from one another, and 20 degree rotation tends to allow
#' text labels to nestle beside each other without much
#' overlap.
#' 
#' @param sp object `sp::SpatialPolygons`
#' @param labels `character` vector of labels, the length defines
#'    the number of coordinate positions to return.
#' @param color `vector` of R compatible colors which defines the
#'    color of each label.
#' @param border `vector` or `NA` with colors to define the border around
#'    each label.
#' @param cex `numeric` value used to resize all text labels by
#'    multiplying the font size.
#' @param fontsize `numeric` value indicating the font size in points.
#' @param angle `numeric` `vector` indicating the angle in degrees
#'    to rotate each text label, where positive values rotate
#'    in clockwise direction.
#' @param scale_width `numeric` value or `NULL`, where a numeric
#'    value indicates the relative size of the polygon to use as
#'    a buffer around the polygon, and should be given as
#'    negative values. For example `scale_width=-0.1` will create
#'    a buffer at 10% the size of the polygon.
#' @param apply_n_scale `logical` indicating whether to adjust the
#'    polygon buffer based upon the number of labels, specifically
#'    so that few labels (1, 2, or 3 labels) have much higher buffer
#'    and therefore are positioned more central inside the polygon.
#' @param buffer_w,buffer_h `numeric` width and height, respectively,
#'    used for additional buffer inside each polygon. These values
#'    are appropriate when the width of text label is known. The
#'    buffer polygon derived from `scale_width` and `apply_n_scale`
#'    is moved left, right, up, down, and the intersection of these
#'    operations is used with `sp::spsample()` to define label
#'    positions. In this way, labels should fit inside the original
#'    polygon without overlapping the boundary. This function does
#'    not define default values, because the actual text label width
#'    is dependent upon the diplay device properties at the time
#'    the plot is drawn, and this device may not even be open when
#'    this function is called.
#' @param label_method `character` string indicating the layout type
#'    used by `sp::spsample()`.
#' @param draw_buffer `logical` indicating whether the buffer polygon
#'    should be drawn, intended for visual review of the processing.
#' @param buffer_fill,buffer_border color values used when `draw_buffer=TRUE`.
#' @param draw_points `logical` indicating whether to draw points at
#'    each coordinate position, intended for visual review of the processing.
#' @param draw_labels `logical` indicating whether to draw text labels
#'    which is performed using `gridtext::richtext_grob()` inside
#'    a base R plot.
#' @param verbose `logical` indicating whether to print verbose output.
#' 
#' @return `list` that contains: `items_df` as a `data.frame` of item
#'    label coordinates; and `g_labels` as output from
#'    `gridtext::richtext_grob()` whose coordinates are defined
#'    as `"native"`.
#' 
#' @family venndir utility
#' 
#' @examples
#' setlist <- make_venn_test(100, 3);
#' vo <- venndir(setlist, return_items=TRUE, font_cex=0.01, proportional=FALSE);
#' 
#' # labels inside each venn overlap polygon
#' venn_spdf <- vo$venn_spdf;
#' label_df <- vo$label_df;
#' for (i in seq_len(nrow(venn_spdf))) {
#'    j <- match(venn_spdf$label[i], label_df$overlap_set);
#'    if (length(unlist(label_df[j,"items"])) > 0) {
#'    label_polygon_fill(sp=venn_spdf[i,],
#'       ref_sp=venn_spdf,
#'       color=venn_spdf$border[i],
#'       scale_width=-0.1,
#'       draw_buffer=TRUE,
#'       labels=unlist(label_df[j,"items"]));
#'    }
#' }
#' 
#' # same example as above but using proportional circles
#' vo <- venndir(setlist, return_items=TRUE, font_cex=0.01, proportional=TRUE);
#' 
#' # labels inside each venn overlap polygon
#' venn_spdf <- vo$venn_spdf;
#' label_df <- vo$label_df;
#' for (i in seq_len(nrow(venn_spdf))) {
#'    j <- match(venn_spdf$label[i], label_df$overlap_set);
#'    if (length(unlist(label_df[j,"items"])) > 0) {
#'    label_polygon_fill(sp=venn_spdf[i,],
#'       ref_sp=venn_spdf,
#'       color=venn_spdf$border[i],
#'       scale_width=-0.1,
#'       draw_buffer=TRUE,
#'       labels=unlist(label_df[j,"items"]));
#'    }
#' }
#' 
#' 
#' @export
label_polygon_fill <- function
(sp,
 labels,
 color="black",
 border=NA,
 cex=1,
 fontsize=10,
 angle=-20,
 scale_width=-0.1,
 apply_n_scale=TRUE,
 buffer_w=0,
 buffer_h=0,
 label_method=c("hexagonal",
    "nonaligned",
    "regular",
    "random",
    "stratified",
    "clustered"),
 draw_buffer=FALSE,
 buffer_fill="#FFFFFF77",
 buffer_border="red",
 draw_points=FALSE,
 draw_labels=TRUE,
 verbose=FALSE,
 ...)
{
   ##
   label_method <- match.arg(label_method);
   n <- length(labels);
   if (n == 0) {
      return(NULL);
   }

   ## expand vectors to the number of labels
   color <- rep(color, length.out=n);
   border <- rep(border, length.out=n);
   cex <- rep(cex, length.out=n);
   angle <- rep(angle, length.out=n);
   
   ## resize polygon before applying labels
   #ellYesDiffsmall11 <- shrinkPolygon(ellYesDiffsmall11,
   #   scale=c(labelPolyScale[j]*parPinRatio, labelPolyScale[j]));
   if (length(scale_width) == 0) {
      scale_width <- -0.001;
   }
   
   ## apply additional scaling based upon n
   if (length(apply_n_scale) > 0 && apply_n_scale) {
      n_scale <- 1 - 1 / (n*2);
      if (verbose) {
         jamba::printDebug("label_polygon_fill(): ",
            "n_scale:",
            format(digits=2, n_scale));
         jamba::printDebug("label_polygon_fill(): ",
            "scale_width (before):",
            format(digits=2, scale_width));
      }
      scale_width <- (scale_width + 1) * (1 - (1 - n_scale) * 1.1) - 1;
   }
   
   ## Apply polygon buffer
   if (verbose) {
      jamba::printDebug("label_polygon_fill(): ",
         "scale_width:",
         format(digits=2, scale_width));
   }
   if (scale_width < 0) {
      for (sw in unique(seq(from=scale_width, to=0, length.out=5))) {
         if (sw == 0) {
            sp_buffer <- sp;
         } else {
            sp_buffer <- rgeos::gBuffer(sp,
               width=sw);
         }
         if (length(sp_buffer) > 0 && rgeos::gArea(sp_buffer) > 0) {
            break;
         }
      }
      scale_width <- sw;
   } else {
      sp_buffer <- sp;
   }

   buffer_ws <- unique(c(0, buffer_w * c(-1, -0.5, 0.5, 1)));
   buffer_hs <- unique(c(0, buffer_h * c(-1, -0.5, 0.5, 1)));
   if (verbose) {
      jamba::printDebug("label_polygon_fill(): ",
         "buffer_ws:",
         format(digits=2, buffer_ws));
      jamba::printDebug("label_polygon_fill(): ",
         "buffer_hs:",
         format(digits=2, buffer_hs));
   }
   for (w1 in unique(c(0, buffer_w * c(-1, -0.5, 0.5, 1)))) {
      for (h1 in buffer_hs) {
         if (h1 != 0 || w1 != 0) {
            sp_buffer_x <- rescale_sp(sp, shift=c(w1, h1));
            sp_buffer <- rgeos::gIntersection(sp_buffer, sp_buffer_x);
         }
      }
   }
   
   if (draw_buffer) {
      try(
         sp::plot(sp_buffer,
            add=TRUE,
            col=buffer_fill,
            border=buffer_border,
            lwd=2,
            lty="dotted")
      )
   }
   ## gArea(ellYesDiffDis[i]);
   
   ## item coordinates
   ## note that it sometimes requires iterations with increasing
   ## number of labels for the procedure to return at least
   ## that many label positions
   get_poly_points <- function
   (sp,
    n,
    type,
    iter,
    offset=c(0.5, 0.5),
    tries=50,
    ...)
   {
      if (n == 0) {
         return(NULL);
      }
      try_step <- ceiling(n/100);
      for (k in (seq_len(tries)-1)*try_step) {
         if (n + k == 1 && "hexagonal" %in% type) {
            type <- "regular";
         }
         label_xy <- tryCatch({
            sp::coordinates(
               sp::spsample(sp,
                  n=n + k,
                  type=type,
                  offset=offset,
                  iter=iter));
         }, error=function(e){
            NULL;
         });
         if (length(label_xy) > 0 && nrow(label_xy) >= n) {
            # sort coordinates top to bottom
            label_xy <- jamba::mixedSortDF(label_xy,
               byCols=c(-2, 1));
            label_xy <- head(label_xy, n);
            return(label_xy);
         }
      }
      stop(paste0("spsample failed to return ", n, " points, ", nrow(label_xy),
         ", k:", k));
   }
   label_xy <- get_poly_points(sp_buffer,
      n,
      type=label_method,
      iter=50,
      ...);
   
   if (draw_points) {
      points(label_xy,
         pch=20,
         col=color,
         cex=cex/2,
         ...);
   }
   
   ## prepare data.frame for re-use
   items_df <- data.frame(
      x=label_xy[,1],
      y=label_xy[,2],
      text=labels,
      rot=angle,
      col=color,
      fontsize=fontsize * cex,
      border=border,
      stringsAsFactors=FALSE);

   # define text label grob   
   g_labels <- gridtext::richtext_grob(
      x=label_xy[,1],
      y=label_xy[,2],
      text=labels,
      rot=-angle,
      default.units="native",
      padding=grid::unit(2, "pt"),
      r=grid::unit(2, "pt"),
      vjust=0.5,
      hjust=0.5,
      halign=0.5,
      gp=grid::gpar(
         col=color,
         fontsize=fontsize * cex
      ),
      box_gp=grid::gpar(
         col=border
      )
   );
   
   if (draw_labels) {
      ## Draw labels
      # to draw using grid we have to use a custom viewport
      if (length(dev.list()) > 0) {
         vps <- gridBase::baseViewports();
         grid::pushViewport(vps$inner, vps$figure, vps$plot);
         grid::grid.draw(g_labels);
         grid::popViewport(3);
      }
   }
   return(invisible(
      list(
         items_df=items_df,
         g_labels=g_labels)));
}

#' Rescale a SpatialPolygons object
#' 
#' Rescale a SpatialPolygons object
#' 
#' This function simply applies `rescale_coordinates()` to an
#' object `sp::SpatialPolygons`, and it does so by calling
#' `rescale_ps` on each `sp::Polygons` object contained in
#' the `sp` input.
#' 
#' @family venndir utility
#' 
#' @return object `sp::SpatialPolygons`
#' 
#' @inheritParams rescale_coordinates
#' 
#' @param p object `sp::SpatialPolygons`
#' @param share_center `logical` indicating whether all polygons
#'    should share the same center, where `share_center=TRUE` will
#'    adjust everything collectively, and `share_center=FALSE` will
#'    adjust each polygon independently relative to its own center
#'    coordinate.
#' 
#' @export
rescale_sp <- function
(sp,
 rotate_degrees=0,
 scale=c(1, 1),
 shift=c(0, 0),
 center=NULL,
 share_center=FALSE,
 update_bbox=TRUE,
 ...)
{
   ## SpatialPolygons
   if (length(center) == 0 && share_center) {
      center <- rowMeans(bbox(sp));
   }
   sp@polygons <- lapply(sp@polygons, function(ps){
      rescale_ps(ps,
         scale=scale,
         shift=shift,
         rotate_degrees=rotate_degrees,
         center=center,
         ...)
   });
   
   if (update_bbox) {
      sp@bbox[] <- t(apply(do.call(cbind, lapply(sp@polygons, sp::bbox)), 1, range));
   }
   return(sp);
}

#' Rescale a Polygons object
#' 
#' Rescale a Polygons object
#' 
#' This function simply applies `rescale_coordinates()` to an
#' object `sp::Polygons`, and it does so by calling
#' `rescale_p` on each `sp::Polygon` object contained
#' in the `ps` input.
#' 
#' @family venndir utility
#' 
#' @return object `sp::Polygons`
#' 
#' @inheritParams rescale_coordinates
#' 
#' @param p object `sp::Polygons`
#' 
#' @export
rescale_ps <- function
(ps,
 rotate_degrees=0,
 scale=c(1, 1),
 shift=c(0, 0),
 center=NULL,
 share_center=FALSE,
 ...)
{
   ## Polygons
   if (length(center) == 0 && share_center) {
      center <- rowMeans(bbox(ps));
   }
   ps@Polygons <- lapply(ps@Polygons, function(p){
      #if (length(center) == 0) {
      #   center <- rowMeans(bbox(ps));
      #}
      rescale_p(p,
         scale=scale,
         shift=shift,
         rotate_degrees=rotate_degrees,
         center=center,
         ...);
   })
   return(ps);
}

#' Rescale a Polygon object
#' 
#' Rescale a Polygon object
#' 
#' This function simply applies `rescale_coordinates()` to an
#' object `sp::Polygon`, and it does so by calling
#' `rescale_coordinates` on each `coords` object contained
#' in the `p` input.
#' 
#' @family venndir utility
#' 
#' @return object `sp::Polygon`
#' 
#' @inheritParams rescale_coordinates
#' 
#' @param p object `sp::Polygon`
#' 
#' @export
rescale_p <- function
(p,
 rotate_degrees=0,
 scale=c(1, 1),
 shift=c(0, 0),
 center=NULL,
 ...)
{
   ## Polygon
   if (length(center) == 0) {
      center <- rowMeans(sp::bbox(p));
   }
   if (length(scale) == 0) {
      scale <- c(1, 1);
   }
   if (length(scale) == 0) {
      scale <- c(0, 0);
   }
   scale <- rep(scale, length.out=2);
   shift <- rep(shift, length.out=2);
   p@coords <- rescale_coordinates(p@coords,
      scale=scale,
      shift=shift,
      rotate_degrees=rotate_degrees,
      center=center,
      ...);
   #p@coords <- (p@coords - rep(center, each=nrow(p@coords))) *
   #   rep(scale, each=nrow(p@coords)) +
   #   rep(center, each=nrow(p@coords))
   return(p);
}

#' Scale, rotate, and shift numeric coordinates
#' 
#' Scale, rotate, and shift numeric coordinates
#' 
#' This function takes a numeric matrix with two or more
#' numeric columns, and adjusts the coordinates in three
#' ways:
#' 
#' * scale: adjust coordinate range by a multiplier, relative to
#' a central point
#' * rotate: rotate coordinates around a central point in degrees
#' * shift: adjust coordinate range by adding a numeric value
#' 
#' The operations are performed in that order: rotate, scale, shift.
#' 
#' When `center` is not defined, the default behavior is to use
#' the mean of the range of each coordinate column. Using the mean
#' range is equivalent to using the mean of the bounding box `sp::bbox()`
#' for Spatial objects.
#' 
#' @param x `matrix` with one or more columns containing `numeric`
#'    coordinates.
#' @param scale `numeric` vector whose values are expanded to length
#'    `ncol(x)`. After subtracting the `center`, the coordinates 
#'    in each column are multiplied by the `scale`.
#' @param rotate_degrees `numeric` value in degrees indicating
#'    rotation around the `center`, where positive values are
#'    clockwise rotation. This rotation is only applied to two
#'    columns in `x` defined by `rotation_axes`.
#' @param shift `numeric` vector whose values are expanded to length
#'    `ncol(x)`. The coordinates in each column are added to
#'    the `shift`, after applying `scale` and `rotate_degrees`
#' @param center `numeric` vector whose values are expanded to length
#'    `ncol(x)`, indicating the center point used for `scale` and
#'    `rotate_degrees` transformations. When `center=NULL` it
#'    is derived from the bounding box, which is the mean of the range
#'    for each column in `x`.
#' @param rotation_axes `integer` vector length 2, indicating which
#'    two columns in `x` are used for `rotate_degrees`.
#' @param plot_debug `logical` indicating whether to plot starting,
#'   intermediate, and final polygons during the processing.
#' @param ... additional arguments are ignored.
#' 
#' @return `matrix` with `numeric` values after processing.
#' 
#' @family venndir utility
#' 
#' @examples
#' sp <- sp_ellipses(xcenter=2, ycenter=3, xradius=3, yradius=1);
#' 
#' plot(sp, col="#FF000077", border="#FF0000", asp=1);
#' 
#' x <- sp@polygons[[1]]@Polygons[[1]]@coords;
#' 
#' jamba::nullPlot(doBoxes=FALSE, xlim=c(-3, 9), ylim=c(-2, 10), asp=1);
#' rescale_coordinates(x,
#'    shift=c(1, 1),
#'    rotate_degrees=30,
#'    scale=c(1.5, 2.5),
#'    plot_debug=TRUE)
#'    
#' # example using SpatialPolygons
#' sp <- sp_ellipses(xcenter=c(2, 4),
#'    ycenter=c(3, 2),
#'    xradius=c(3, 2),
#'    yradius=c(1, 3));
#' # apply rescale to each polygon
#' sp_2 <- rescale_sp(sp,
#'    shift=c(1, 1),
#'    rotate_degrees=80,
#'    scale=c(1.5, 2.5))
#' par("mfrow"=c(1,2));
#' plot(sp,
#'    asp=1,
#'    col=jamba::alpha2col(alpha=0.7, c("red", "gold")),
#'    main="Input ellipses");
#' plot(sp_2,
#'    asp=1,
#'    col=jamba::alpha2col(alpha=0.7, c("red", "gold")),
#'    main="rescaled ellipses");
#' par("mfrow"=c(1,1));
#' 
#' # same as before but apply 180 degree rotation
#' sp_3 <- rescale_sp(sp,
#'    shift=c(1, 1),
#'    rotate_degrees=180,
#'    scale=c(1, 1),
#'    share_center=TRUE)
#' par("mfrow"=c(1,2));
#' plot(sp, asp=1,
#'    col=jamba::alpha2col(alpha=0.7, c("red", "gold")));
#' plot(sp_3, asp=1,
#'    col=jamba::alpha2col(alpha=0.7, c("red", "gold")));
#' par("mfrow"=c(1,1));
#' 
#' @export
rescale_coordinates <- function
(x,
 scale=c(1, 1),
 rotate_degrees=0,
 shift=c(0, 0),
 center=NULL,
 rotation_axes=c(1, 2),
 plot_debug=FALSE,
 ...)
{
   # Define default scale=1 if empty
   if (length(scale) == 0) {
      scale <- c(1, 1)
   }
   # apply scale to all coordinate columns
   scale <- rep(scale,
      length.out=ncol(x));
   
   # define default shift=0 if empty
   if (length(shift) == 0) {
      shift <- c(0, 0)
   }
   shift <- rep(shift,
      length.out=ncol(x));
   
   # define default center using colMeans of bounding box
   # which is the mean range
   if (length(center) == 0) {
      center <- rowMeans(matrixStats::colRanges(x, na.rm=TRUE));
   } else {
      center <- rep(center,
         length.out=ncol(x));
   }
   
   # determine whether we need to subtract the center
   # which is needed only when (rotate_degrees != 0) or (scale != 1)
   # and center is non-zero
   apply_center <- FALSE;
   if (any(center != 0) && (
         (length(rotate_degrees) == 1 & rotate_degrees != 0) ||
         any(scale != 1))) {
      apply_center <- TRUE;
   }
   
   # optionally plot polygons to visualize the process
   if (plot_debug) {
      polygon(x,
         col="#0000CDAA",
         border="#0000CD");
   }
   
   ## subtract center coordinate
   if (apply_center) {
      if (plot_debug) {
         points(rbind(center), pch="1", cex=1, col="#0000CD");
      }
      x <- x - rep(center, each=nrow(x));
   }
   
   ## apply scale to coordinates
   if (any(scale != 1)) {
      x <- x * rep(scale, each=nrow(x))
      # optionally plot polygons to visualize the process
      if (plot_debug) {
         polygon(x + rep(center, each=nrow(x)),
            col="#1874CDAA",
            lty="dashed",
            border="#1874CD");
      }
   }
   
   ## rotate coordinates
   if (length(rotate_degrees) == 1 && rotate_degrees != 0) {
      if (length(rotation_axes) != 2) {
         rotation_axes <- c(1, 2);
      }
      # prepare rotation matrix math
      co <- cos(-jamba::deg2rad(rotate_degrees));
      si <- sin(-jamba::deg2rad(rotate_degrees));
      # define axes of rotation
      axis1 <- rotation_axes[1];
      axis2 <- rotation_axes[2];
      # apply rotation matrix math
      axis12 <- cbind(
         co * x[,axis1] - si * x[,axis2],
         si * x[,axis1] + co * x[,axis2]);
      # optionally plot polygons to visualize the process
      if (plot_debug) {
         polygon(axis12 + rep(center, each=nrow(x)),
            col="#4F94CDAA",
            lty="dotted",
            lwd=2,
            border="#4F94CD");
      }
      # assign rotated coordinate column values to input matrix
      x[,c(axis1, axis2)] <- axis12;
   }
   
   ## apply shift to coordinates
   if (any(shift != 0)) {
      x <- x + rep(shift, each=nrow(x));
      # optionally plot polygons to visualize the process
      if (plot_debug) {
         polygon(x + rep(center, each=nrow(x)),
            col="#00BFFFAA",
            lty="dashed",
            lwd=2,
            border="#00BFFF");
      }
   }
   
   ## add center coordinate
   if (apply_center) {
      x <- x + rep(center, each=nrow(x));
      # optionally plot new center point and arrow to visualize
      # the change in the center point
      if (plot_debug) {
         arrows(x0=rbind(center)[,1],
            x1=rbind(center)[,1]+ rbind(shift)[,1],
            y0=rbind(center)[,2],
            y1=rbind(center)[,2]+ rbind(shift)[,2],
            lwd=2,
            col="#006FFF");
         points(rbind(center, rbind(center) + rbind(shift)),
            pch=c("1", "2"),
            cex=1.5,
            col=c("#0000CD", "#0000CD"));
      }
   }

   return(x)
}
