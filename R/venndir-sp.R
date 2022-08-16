
#' Convert euler output to polygons
#' 
#' Convert euler output to polygons
#' 
#' This function takes the output from `eulerr::euler()` and
#' converts it to polygons in `sp::SpatialPolygons` format.
#' Essentially the output from `eulerr::euler()` is either a
#' set of circles, or ellipses.
#' 
#' @return `sp::SpatialPolygons` object with one `sp::Polygons`
#'    element for each Euler circle or ellipse.
#' 
#' @family venndir utility
#' 
#' @param x output from `eulerr::euler()`
#' 
#' @import sp
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
#' sp::plot(spdf, col=spdf$color)
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
   
   # check input class
   if (!any(c("SpatialPolygons", "SpatialPolygonsDataFrame") %in% class(sp))) {
      stop("Input must be SpatialPolygons or SpatialPolygonsDataFrame");
   }
   
   ## define incidence matrix of overlaps
   if ("SpatialPolygonsDataFrame" %in% class(sp)) {
      el1 <- make_venn_combn_df(rownames(as.data.frame(sp)),
         sep=sep);
   } else{
      el1 <- make_venn_combn_df(names(sp),
         sep=sep);
   }
   if (verbose) {
      jamba::printDebug("find_vennpoly_overlaps(): ",
         "names(sp):", names(sp));
      jamba::printDebug("find_vennpoly_overlaps(): ",
         "head(el1):");
      print(head(el1, 30));
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
      venn_counts_names <- strsplit(names(venn_counts),
         fixed=TRUE,
         sep);
   }
   if (length(venn_items) > 0) {
      venn_items_names <- strsplit(names(venn_items),
         fixed=TRUE,
         sep);
   }
   
   ## calculate venn overlap polygons
   #vennCoords <- lapply(1:nrow(el1), function(j){
   venn_poly_coords <- lapply(seq_len(nrow(el1)), function(j){
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
         #jamba::mixedSort(colnames(el1)[whichYes]),
         (colnames(el1)[whichYes]),
         collapse=sep);
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
      if (verbose) {
         jamba::printDebug("find_vennpoly_overlaps(): ",
            "whichYes:", whichYes);
         jamba::printDebug("find_vennpoly_overlaps(): ",
            "whichNo:", whichNo);
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
      if (verbose) {
         jamba::printDebug("find_vennpoly_overlaps(): ",
            "length(overlap polygon):", length(ellUse));
      }
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
   if (verbose) {
      jamba::printDebug("find_vennpoly_overlaps(): ",
         "vennUse:", vennUse, ", vennMissing:", vennMissing);
   }
   venn_SP <- do.call(sp:::rbind.SpatialPolygons, venn_poly_coords[vennUse]);
   venn_spdf <- sp::SpatialPolygonsDataFrame(venn_SP,
      data.frame(color=venn_poly_colors[vennUse],
         label=names(venn_SP),
         stringsAsFactors=FALSE));
   
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
      sp_polylabelr(i)
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
#' @family venndir spatial
#' 
#' @param sp `list` object that contains one or more
#'    `sp::SpatialPolygons`.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' circles <- get_venn_shapes(c(A=1, B=1, C=1))
#' col3 <- c("#FF000077", "#DDDD0088", "#0000FF77")
#' sp::plot(circles, col=col3)
#' 
#' circle_intersect <- intersect_polygons(circles);
#' plot(circle_intersect, add=TRUE, col="gold", lwd=3);
#' 
#' @export
intersect_polygons <- function
(sp,
 ...)
{
   ## Purpose is to use rgeos gIntersection() on more than 2 Polygons objects,
   ## but allowing 1, 2, or more than 2.
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
#' @family venndir spatial
#' 
#' @param sp `list` object that contains one or more
#'    `sp::SpatialPolygons`.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' circles <- get_venn_shapes(c(A=1, B=1, C=1))
#' col3 <- c("#FF000077", "#DDDD0088", "#0000FF77")
#' sp::plot(circles, col=col3)
#' 
#' circle_union <- union_polygons(circles);
#' plot(circle_union, add=TRUE, col="#FFDD0088", lwd=3);
#' 
#' @export
union_polygons <- function
(sp,
 ...)
{
   ## Purpose is to use rgeos gUnion() on more than 2 Polygons objects,
   ## but allowing 1, 2, or more than 2.
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
         if (all(i %in% y[[j]]) &&
               all(y[[j]] %in% i) &&
               length(y[[j]]) == length(i)) {
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
#' @family venndir spatial
#' 
#' @param sp object with class `sp::SpatialPolygons` that may
#'    contain one or more closed polygons.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' circles <- get_venn_shapes(c(A=1, B=2, "A&B"=3), proportional=TRUE)
#' col3 <- c("#FF000077", "#0000FF77")
#' sp::plot(circles)
#' 
#' circles_u12 <- union_polygons(circles[1:2]);
#' circles_i12 <- intersect_polygons(circles[1:2]);
#' 
#' # one SpatialPolygons object with multiple polygon pieces
#' circles_d12 <- rgeos::gDifference(circles_u12, circles_i12)
#' plot(circles_d12, add=TRUE, col="gold", lwd=3)
#' 
#' circles_d12_largest <- get_largest_polygon(circles_d12);
#' plot(circles_d12_largest, add=TRUE, border="red", lwd=5);
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
         if (jamba::igrepHas("Spatial.*DataFrame", class(sp_dis))) {
            rgeos::gArea(sp_dis[i,]);
         } else {
            rgeos::gArea(sp_dis[i]);
         }
      });
      if (jamba::igrepHas("Spatial.*DataFrame", class(sp_dis))) {
         sp_out <- sp_dis[which.max(sp_area),];
      } else {
         sp_out <- sp_dis[which.max(sp_area)];
      }
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
#' @family venndir spatial
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
#' @examples
#' sp <- sp_circles(c(3, 2), c(2, 3))
#' plot(sp, col=c("#FF000077", "#FFDD0077"));
#' axis(1, las=2); axis(2, las=2);
#' points(x=c(3, 2), y=c(2, 3), pch=c("1", "2"), add=TRUE);
#' 
#' # same but with sf objects
#' sp <- sp_circles(c(3, 2), c(2, 3), return_type="sf")
#' plot(sp, col=c("#FF000077", "#FFDD0077"));
#' axis(1, las=2); axis(2, las=2);
#' points(x=c(3, 2), y=c(2, 3), pch=c("1", "2"), add=TRUE);
#' 
#' @export
sp_circles <- function
(xcenter,
 ycenter,
 setnames=NULL,
 radius=1,
 n=60,
 return_type=c("sp", "sf"),
 ...)
{
   #
   return_type <- match.arg(return_type);
   angle_seq <- head(seq(from=0,
      to=pi*2,
      length.out=n + 1), n);
   if (length(setnames) == 0) {
      setnames <- seq_along(xcenter);
   }
   xvals <- sin(angle_seq);
   yvals <- cos(angle_seq);
   if (length(radius) == 0) {
      radius <- 1;
   }
   radius <- rep(radius,
      length.out=length(xcenter));

   if ("sp" %in% return_type) {
      ell_sp <- sp::SpatialPolygons(lapply(seq_along(xcenter), function(i){
         sp::Polygons(list(
            sp::Polygon(cbind(
               head(xvals, n) * radius[i] + xcenter[i],
               head(yvals, n) * radius[i] + ycenter[i]))),
            setnames[i])
      }), pO=seq_along(xcenter));
   } else if ("sf" %in% return_type) {
      ell_sf_list <- lapply(seq_along(xcenter), function(i){
         ell_xy <- cbind(
            x=c(xvals, head(xvals, 1)) * radius[i] + xcenter[i],
            y=c(yvals, head(yvals, 1)) * radius[i] + ycenter[i]);
         ell_sf <- sf::st_polygon(list(ell_xy));
      })
      ell_sf <- sf::st_sf(sf::st_sfc(ell_sf_list),
         row.names=setnames);
      return(invisible(ell_sf));
   }
   invisible(ell_sp);
}

#' Make SpatialPolygons ellipses
#' 
#' Make SpatialPolygons ellipses
#' 
#' This function creates one or more ellipses as
#' `sp::SpatialPolygons` objects.
#' 
#' @family venndir spatial
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
#' @examples
#' sp <- sp_ellipses(c(3, 2), c(2, 3),
#'    xradius=c(1, 4),
#'    yradius=c(5, 2))
#' plot(sp, col=c("#FF000077", "#FFDD0077"));
#' axis(1, las=2); axis(2, las=2);
#' points(x=c(3, 2), y=c(2, 3), pch=c("1", "2"), add=TRUE);
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
 return_type=c("sp", "sf"),
 ...)
{
   #
   return_type <- match.arg(return_type);
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
   
   if ("sp" %in% return_type) {
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
   } else if ("sf" %in% return_type) {
      ell_sf_list <- lapply(seq_along(xcenter), function(i){
         i_xvals <- c(xvals, head(xvals, 1)) * xradius[i];
         i_yvals <- c(yvals, head(yvals, 1)) * yradius[i];
         e_xvals <- (i_xvals * cos(rotation_rad[i]) + i_yvals * sin(rotation_rad[i]));
         e_yvals <- (i_yvals * cos(rotation_rad[i]) - i_xvals * sin(rotation_rad[i]));
         ell_xy <- cbind(
            x=e_xvals + xcenter[i],
            y=e_yvals + ycenter[i]);
         ell_sf <- sf::st_polygon(list(ell_xy));
      })
      ell_sf <- sf::st_sf(sf::st_sfc(ell_sf_list),
         row.names=setnames);
      return(invisible(ell_sf));
   }
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
#' @family venndir spatial
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
#' @examples
#' sp <- sp_ellipses(c(3, 2), c(2, 3),
#'    xradius=c(1, 4),
#'    yradius=c(5, 2))
#' opar <- par("mfrow"=c(1,2));
#' on.exit(par(opar));
#' plot(sp, col=c("#FF000077", "#FFDD0077"),
#'    xlim=c(-2, 9));
#' axis(1, las=2); axis(2, las=2);
#' points(x=c(3, 2), y=c(2, 3), pch=c("1", "2"), add=TRUE);
#' 
#' sp2 <- nudge_sp(sp,
#'    sp_nudge=list("2"=c(3, -2))
#' )
#' plot(sp2, col=c("#FF000077", "#FFDD0077"),
#'    xlim=c(-2, 9));
#' plot(sp[2], border="blue", lty="dotted", add=TRUE);
#' axis(1, las=2); axis(2, las=2);
#' arrows(x0=2, x1=5, y0=3, y1=1)
#' points(x=c(3, 5), y=c(2, 1), pch=c("1", "2"), add=TRUE);
#' 
#' @export
nudge_sp <- function
(sp=NULL,
 sp_nudge=NULL,
 ...)
{
   ## Optionally nudge the polygon coordinates
   if (length(sp) == 0 || ("sf" %in% class(sp) && nrow(sp) == 0)) {
      return(sp);
   }
   is_sf <- inherits(sp, "sf");
   is_sp <- inherits(sp, "SpatialPolygons");
   if (!(is_sf || is_sf)) {
      stop("sp must inherit from 'SpatialPolygons' or 'sf' SimpleFeatures.");
   }
   is_spdf <- inherits(sp, "SpatialPolygonsDataFrame");
   if (is_spdf || "sf" %in% class(sp)) {
      sp_names <- rownames(data.frame(sp));
   } else {
      sp_names <- names(sp);
   }

   ## shift sp by coordinates
   if (length(sp_nudge) > 0 &&
         is.list(sp_nudge) &&
         any(names(sp_nudge) %in% sp_names)) {
      # iterate each name and adjust when the name is a m
      for (i in names(sp_nudge)[names(sp_nudge) %in% sp_names]) {
         j <- match(i, sp_names);
         i_nudge <- sp_nudge[[i]];
         if (is_sp) {
            sp@polygons[[j]] <- rescale_ps(sp@polygons[[j]],
               shift=i_nudge);
         } else if ("sf" %in% class(sp)) {
            st_geometry(sp[j,]) <- st_geometry(sp[j,]) + i_nudge;
         }
      }
   }
   
   ## update bbox - should probably be its own function
   if (is_sp) {
      bbox_m <- jamba::rbindList(lapply(seq_along(sp::geometry(sp)), function(i){
         as.vector(sp::bbox(sp::geometry(sp)[i]))
      }));
      colnames(bbox_m) <- c("xmin", "ymin", "xmax", "ymax");
      bbox_v <- matrix(ncol=2,
         c(min(bbox_m[,1]),
         min(bbox_m[,2]),
         max(bbox_m[,3]),
         max(bbox_m[,4])))
      colnames(bbox_v) <- c("min", "max");
      rownames(bbox_v) <- c("x", "y");
      sp@bbox <- bbox_v;
   }
   if (is_sf) {
      # st_bbox() is updated on Github (version "1.0.8" but dev version)
      # when it is updated on CRAN this step will not be required
      # probably version 1.0.9
      sp_seq <- seq_len(nrow(sp));
      attr(sp[[attr(sp, "sf_column")]], "bbox") <- sf::st_bbox(sp[sp_seq,]);
   }
      
   return(invisible(sp));
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
#' @family venndir spatial
#' 
#' @return object `sp::SpatialPolygons`
#' 
#' @inheritParams rescale_coordinates
#' 
#' @param sp object `sp::SpatialPolygons`
#' @param share_center `logical` indicating whether all polygons
#'    should share the same center, where `share_center=TRUE` will
#'    adjust everything collectively, and `share_center=FALSE` will
#'    adjust each polygon independently relative to its own center
#'    coordinate.
#' 
#' @examples
#' sp <- sp_ellipses(c(3, 2), c(2, 3),
#'    xradius=c(1, 4),
#'    yradius=c(5, 2))
#' sp1 <- intersect_polygons(sp);
#' sp2 <- rgeos::gDifference(sp[1], sp[2]);
#' sp3 <- rgeos::gDifference(sp[2], sp[1]);
#' sp123 <- sp::rbind.SpatialPolygons(sp1, sp2, sp3, makeUniqueIDs=TRUE);
#' sp123a <- rescale_sp(sp123,
#'    scale=c(1.5, 1.5),
#'    share_center=TRUE);
#' sp123b <- rescale_sp(sp123,
#'    scale=c(1.5, 1.5));
#' col3 <- c("#FF000077", "#FFDD0077", "#0000DD77");
#' par("mfrow"=c(2, 2));
#' plot(sp123, col=col3,
#'    main="original polygons",
#'    xlim=c(-10, 15), ylim=c(-5, 10));
#' axis(1, las=2); axis(2, las=2);
#' plot(sp123a, col=col3,
#'    main="share_center=TRUE",
#'    xlim=c(-10, 15), ylim=c(-5, 10));
#' axis(1, las=2); axis(2, las=2);
#' plot(sp123[1:2], col=col3[1:2],
#'    main="share_center=FALSE\nrescaling only the blue polygon",
#'    xlim=c(-10, 15), ylim=c(-5, 10));
#' axis(1, las=2); axis(2, las=2);
#' plot(sp123b[3], col=col3[3],
#'    add=TRUE);
#' plot(sp123[2:3], col=col3[2:3],
#'    main="share_center=FALSE\nrescaling only the red polygon",
#'    xlim=c(-10, 15), ylim=c(-5, 10));
#' axis(1, las=2); axis(2, las=2);
#' plot(sp123b[1], col=col3[1],
#'    add=TRUE);
#' par("mfrow"=c(1, 1));
#' 
#' {par("mfrow"=c(2, 2));
#' plot(sp123, col=col3,
#'    xlim=c(-4, 8), ylim=c(-4, 8))
#' title(main="Original polygons", line=0);
#' plot(rescale_sp(sp123, rotate_degrees=c(`11`=45, `12`=-10)), col=col3,
#'    xlim=c(-4, 8), ylim=c(-4, 8))
#' title(sub="yellow +45 degrees\nblue -10 degrees", line=0,
#'    main="share_polygon_center=TRUE (default)")
#' plot(rescale_sp(sp123, rotate_degrees=c(`11`=45, `12`=-10), share_polygon_center=FALSE), col=col3,
#'    xlim=c(-4, 8), ylim=c(-4, 8))
#' title(sub="yellow +45 degrees\nblue -10 degrees", line=0,
#'    main="share_polygon_center=FALSE\n(each polygon uses its center)")
#' plot(rescale_sp(sp123, rotate_degrees=c(`11`=45, `12`=-10), share_center=TRUE), col=col3,
#'    xlim=c(-4, 8), ylim=c(-4, 8))
#' title(sub="yellow +45 degrees\nblue -10 degrees", line=0,
#'    main="share_center=TRUE\n(all polygons share one global center)")
#' par("mfrow"=c(1, 1));}
#' 
#' 
#' @export
rescale_sp <- function
(sp,
 rotate_degrees=0,
 scale=c(1, 1),
 shift=c(0, 0),
 center=NULL,
 share_center=FALSE,
 share_polygon_center=TRUE,
 update_bbox=TRUE,
 ...)
{
   ## SpatialPolygons
   if (length(center) == 0) {
      if (share_center) {
         center <- rowMeans(sp::bbox(sp));
      } else if (share_polygon_center) {
         share_center <- TRUE;
      }
   }
   sp@polygons <- lapply(sp@polygons, function(ps){
      if (length(names(rotate_degrees)) > 0) {
         if (ps@ID %in% names(rotate_degrees)) {
            rescale_ps(ps,
               scale=scale,
               shift=shift,
               rotate_degrees=unname(rotate_degrees[ps@ID]),
               center=center,
               share_center=share_center,
               ...)
         } else {
            rescale_ps(ps,
               scale=scale,
               shift=shift,
               rotate_degrees=0,
               center=center,
               share_center=share_center,
               ...)
         }
      } else {
         rescale_ps(ps,
            scale=scale,
            shift=shift,
            rotate_degrees=rotate_degrees,
            center=center,
            share_center=share_center,
            ...)
      }
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
#' @family venndir spatial
#' 
#' @examples
#' sp <- sp_ellipses(xcenter=2, ycenter=3, xradius=3, yradius=1);
#' 
#' plot(sp, col="#FF000077", border="#FF0000");
#' axis(1, las=2); axis(2, las=2);
#' 
#' x <- sp@polygons[[1]]@Polygons[[1]]@coords;
#' 
#' jamba::nullPlot(doBoxes=FALSE, xlim=c(-3, 9), ylim=c(-2, 10), asp=1);
#' xnew <- rescale_coordinates(x,
#'    shift=c(1, 1),
#'    rotate_degrees=30,
#'    scale=c(1.5, 2.5),
#'    plot_debug=TRUE)
#' axis(1, las=2); axis(2, las=2);
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
#'    rotate_degrees=160,
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
#' sp <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.5, yradius=1)
#' sl2 <- polygon_label_segment(x0, x1, y0, y1, sp,
#'    return_class="SpatialLines",
#'    plot_debug=TRUE,
#'    main="segment drawn to boundary intersection");
#' 
#' # example of making a debug plot
#' lxy <- cbind(c(x0, x1), c(y0, y1));
#' sp <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.5, yradius=1);
#' sl <- sp::SpatialLines(list(sp::Lines(list(sp::Line(lxy)), ID="a")));
#' sp::plot(sp, col="#00007777", border="blue3", lwd=2,
#'    xlim=c(0, 2), ylim=c(0, 2), asp=1,
#'    main="segment drawn to boundary intersection");
#' sp::plot(sl, lty="dotted", lwd=2, col="blue2", add=TRUE);
#' sp::plot(sl2, col="red3", lwd=4, add=TRUE)
#' 
#' # example where the line intersects multiple boundaries
#' x0 <- 0;x1 <- 2;y0 <- 0; y1 <- 2;
#' sl2 <- polygon_label_segment(x0, x1, y0, y1, sp,
#'    return_class="SpatialLines",
#'    plot_debug=TRUE,
#'    main="segment drawn to first boundary intersection");
#' 
#' # example where starting line does not intersect
#' x0 <- 0;x1 <- 1;y0 <- 0; y1 <- 1;
#' sp <- sp_ellipses(xcenter=2, ycenter=1, xradius=0.5, yradius=1);
#' sl2 <- polygon_label_segment(x0, x1, y0, y1, sp,
#'    return_class="SpatialLines",
#'    plot_debug=TRUE,
#'    main="segment drawn to nearest boundary point");
#' 
#' lxy <- cbind(c(x0, x1), c(y0, y1));
#' sp <- sp_ellipses(xcenter=2, ycenter=1, xradius=0.5, yradius=1);
#' sl <- sp::SpatialLines(list(sp::Lines(list(sp::Line(lxy)), ID="a")));
#' sl2 <- polygon_label_segment(x0, x1, y0, y1, sp, "SpatialLines"); 
#' sp::plot(sl, xlim=c(0, 3), ylim=c(0, 2), asp=1, lty="dotted",
#'    main="segment drawn to nearest boundary point");
#' sp::plot(sp, col="#00007777", border="navy", lwd=2, add=TRUE)
#' sp::plot(sl2, col="red", lwd=4, add=TRUE)
#' 
#' # example showing line fully inside the polygon
#' x0 <- 0;x1 <- 2;y0 <- 0; y1 <- 2;
#' sp <- sp_ellipses(xcenter=1, ycenter=1, xradius=1.5, yradius=2.2);
#' sl2 <- polygon_label_segment(x0, x1, y0, y1, sp,
#'    return_class="SpatialLines",
#'    plot_debug=TRUE,
#'    main="segment contained inside returns one point");
#' 
#' # example showing polygon with a hole inside
#' sp <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.5, yradius=1);
#' sp_hole <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.2, yradius=0.5);
#' sp_donut <- rgeos::gDifference(sp, sp_hole);
#' sl2 <- polygon_label_segment(x0, x1, y0, y1, sp_donut,
#'    return_class="SpatialLines",
#'    plot_debug=TRUE,
#'    main="segment drawn to first outer boundary intersection");
#' 
#' # example with line inside the polygon hole
#' x0 <- 0.9;x1 <- 1.1;y0 <- 0.9; y1 <- 1.1;
#' sp <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.5, yradius=1);
#' sp_hole <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.2, yradius=0.5);
#' sp_donut <- rgeos::gDifference(sp, sp_hole);
#' sl2 <- polygon_label_segment(x0, x1, y0, y1, sp_donut,
#'    return_class="SpatialLines",
#'    plot_debug=TRUE,
#'    main="segment drawn to nearest boundary");
#' 
#' # line crosses inside the polygon hole
#' x0 <- 1;x1 <- 1.4;y0 <- 1; y1 <- 1.4;
#' sp <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.5, yradius=1);
#' sp_hole <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.2, yradius=0.5);
#' sp_donut <- rgeos::gDifference(sp, sp_hole);
#' sl2 <- polygon_label_segment(x0, x1, y0, y1, sp_donut,
#'    return_class="SpatialLines",
#'    plot_debug=TRUE,
#'    main="segment drawn to boundary intersection");
#' 
#' x0 <- 0.6;x1 <- 1;y0 <- 0.6; y1 <- 1;
#' sp <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.5, yradius=1);
#' sp_hole <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.2, yradius=0.5);
#' sp_donut <- rgeos::gDifference(sp, sp_hole);
#' sl2 <- polygon_label_segment(x0, x1, y0, y1, sp_donut,
#'    return_class="SpatialLines",
#'    plot_debug=TRUE,
#'    main="first point inside the polygon, returns one point");
#' 
#' # example showing multiple input points
#' x0 <- c(0.6, 1.1);
#' x1 <- c(1, 1.4);
#' y0 <- c(0.6, 1.1);
#' y1 <- c(1, 1.4);
#' sl2 <- polygon_label_segment(x0, x1, y0, y1, sp=sp_donut,
#'    return_class="point", verbose=TRUE, plot_debug=TRUE); 
#' 
#' # example showing sp_buffer
#' sl2 <- polygon_label_segment(x0, x1, y0, y1, sp=sp_donut,
#'    sp_buffer=-0.1,
#'    return_class="point", verbose=TRUE, plot_debug=TRUE); 
#' 
#' @param x0 `numeric` x-axis source position
#' @param x1 `numeric` x-axis target position
#' @param y0 `numeric` y-axis source position
#' @param y1 `numeric` y-axis target position
#' @param sp `sp::SpatialPolygons` object representing the polygon
#' @param sp_buffer `numeric` indicating an optional buffer to
#'    use for the `sp` polygon. By default `sp_buffer=0` uses no
#'    buffer, but a suggested buffer `sp_buffer=-0.01` would make
#'    the polygon `1%` smaller, therefore the line segment would be
#'    slightly inside the polygon border.
#' @param return_class `character` string where `"point"` returns a
#'    `matrix` with one row containing the new target point;
#'    `"matrix"` contains two rows with source and new target points;
#'    `"SpatialLines"` returns `sp::SpatialLines` with the line
#'    segment from source to new target points.
#' @param relative `logical` indicating whether `sp_buffer` is scaled
#'    relative to the size of the polygon, see `get_sp_buffer()`
#'    for more details.
#' @param verbose `logical` indicating whether to print verbose output,
#'    specifically describing which situation occurred.
#' @param ... additional arguments are passed to `get_sp_buffer()`.
#' 
#' @export
polygon_label_segment <- function
(x0,
 x1,
 y0,
 y1,
 sp,
 return_class=c("point", "matrix", "SpatialLines"),
 sp_buffer=0,
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
      sp_buffer <- rep(sp_buffer, length.out=length(x0));
      spi <- ((i - 1) %% length(sp)) + 1;
      listout <- lapply(i, function(j){
         if (verbose) {
            jamba::printDebug("polygon_label_segment(): ",
               "j:", j, ", spi[j]:", spi[j]);
         }
         if (is.list(sp)) {
            sp_use <- sp[[spi[j]]];
         } else {
            sp_use <- sp[spi[j]];
         }
         polygon_label_segment(x0=x0[j],
            x1=x1[j],
            y0=y0[j],
            y1=y1[j],
            sp=sp_use,
            sp_buffer=sp_buffer[[j]],
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
   sl <- sp::SpatialLines(list(sp::Lines(list(sp::Line(lxy)), ID="a")))
   spt <- sp::SpatialPoints(lxy);

   # optional debug plot
   if (TRUE %in% plot_debug) {
      xlim <- range(c(sp::bbox(sp)[1,],
         lxy[,1]), 
         na.rm=TRUE);
      ylim <- range(c(sp::bbox(sp)[2,], 
         lxy[,2]), 
         na.rm=TRUE);
      sp::plot(sp,
         col=jamba::alpha2col("blue2", alpha=0.2),
         border="blue2",
         lwd=3,
         xlim=xlim,
         ylim=ylim,
         lty="solid",
         ...)
      sp::plot(sl,
         col="blue4",
         lwd=2,
         lty="dotted",
         add=TRUE)
   }
   
   # optional polygon buffer
   sp_buffer <- head(sp_buffer, 1);
   if (is.list(sp)) {
      sp <- sp[[1]];
   }
   if (is.numeric(sp_buffer) && !sp_buffer %in% 0) {
      sp <- get_sp_buffer(sp,
         sp_buffer=sp_buffer,
         relative=relative,
         ...);
      # optional debug plot
      if (verbose) {
         jamba::printDebug("polygon_label_segment(): ",
            "Applied sp_buffer:", sp_buffer);
      }
      if (TRUE %in% plot_debug) {
         sp::plot(sp,
            col=jamba::alpha2col("gold", alpha=0.2),
            border=jamba::alpha2col("gold", alpha=0.4),
            lwd=2,
            lty="dashed",
            add=TRUE)
      }
   }
   if (rgeos::gContains(sp, sl)) {
      # return if sl is fully contained inside sp
      if (verbose) {
         jamba::printDebug("polygon_label_segment(): ",
            "The line is fully contained by the polygon.");
      }
      lxy[2,] <- rep(NA, 2);
   } else if (!rgeos::gIntersects(sl, sp)) {
      # nearest boundary point if sl does not intersect sp at all
      if (verbose) {
         jamba::printDebug("polygon_label_segment(): ",
            "The line does not intersect the polygon.");
      }
      spt_new <- rgeos::gNearestPoints(spt[1], sp);
      lxy[2,] <- spt_new@coords[2,];
   } else if (rgeos::gContains(sp, spt[1]) || rgeos::gIntersects(spt[1], sp)) {
      # return if first point is inside the polygon or intersects the boundary
      if (verbose) {
         jamba::printDebug("polygon_label_segment(): ",
            "The first point is inside the polygon.");
      }
      lxy[2,] <- rep(NA, 2);
   } else {
      
      # subtract the polygon from the line
      sl_diff <- rgeos::gDifference(sl, sp);
      if (plot_debug) {
         sp::plot(sl_diff,
            col="darkorange",
            lwd=4,
            lty="dashed",
            add=TRUE);
      }
      
      # The new line may have multiple segments,
      # take the first segment that includes the first input point
      if (verbose) {
         jamba::printDebug("polygon_label_segment(): ",
            "The first point is outside the polygon.");
      }
      sl_diff_int <- sapply(sl_diff@lines[[1]]@Lines, function(L){
         sl1 <- sp::SpatialLines(list(sp::Lines(list(L), ID="b")))
         rgeos::gIntersects(sl1, spt[1])
      });
      
      
      if (!any(sl_diff_int)) {
         # make new segment from the first original point
         # to the first diff line segment
         lxy[2,] <- sl_diff@lines[[1]]@Lines[[1]]@coords[2,];
         if (verbose) {
            jamba::printDebug("polygon_label_segment(): ",
               "No gDifference segment intersects the source point.");
         }
      } else {
         sl_diff_which <- head(which(sl_diff_int), 1);
         if (verbose) {
            jamba::printDebug("polygon_label_segment(): ",
               "Using first outer line segment ", sl_diff_which);
         }
         lxy <- sl_diff@lines[[1]]@Lines[[sl_diff_which]]@coords;
      }
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
         sl@lines[[1]]@Lines[[1]]@coords <- lxy;
         sp::plot(sl,
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
   } else {
      sl@lines[[1]]@Lines[[1]]@coords <- lxy;
      return(sl);
   }
}

#' Get SpatialPolygons polylabelr coordinate
#' 
#' Get SpatialPolygons polylabelr coordinate
#' 
#' This function is a simple wrapper function around
#' `polylabelr::poi()` for `sp::SpatialPolygons` input.
#' This function does two things:
#' 
#' 1. It finds the largest polygon in the `sp::SpatialPolygons`
#' input, which may contain multiple disconnected polygons.
#' 2. It applies "holes" inside the polygon when present,
#' to prevent the label from being chosen inside the hole.
#' 
#' @return `list` with items `x` with x coordinate, `y` with
#' y coordinate, and `dist` with distance to the enclosing
#' polygon.
#' 
#' @family venndir label
#' 
#' @param sp `sp::SpatialPolygons` object
#' @param apply_holes `logical` indicating whether to apply any
#'    polygon holes when present in `sp`.
#' @param ... additional arguments are ignored
#' 
#' @examples
#' setlist <- list(A=letters, B=sample(letters, 4));
#' vo <- venndir(setlist, proportional=TRUE, do_plot=FALSE);
#' sp <- vo$venn_spdf[1,];
#' plot(sp, col="gold")
#' xy <- sp_polylabelr(sp);
#' points(xy, pch=20, cex=4, col="navy")
#' 
#' # example with a hole inside
#' sp <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.5, yradius=1);
#' sp_hole <- sp_ellipses(xcenter=1, ycenter=1, xradius=0.2, yradius=0.5);
#' sp_donut <- rgeos::gDifference(sp, sp_hole);
#' plot(sp_donut, col="gold")
#' spt <- sp_polylabelr(sp_donut);
#' text(x=spt$x, y=spt$y, add=TRUE, labels="label")
#' 
#' @export
sp_polylabelr <- function
(sp,
 apply_holes=TRUE,
 ...)
{
   isp <- get_largest_polygon(sp);
   # get main polygon coordinates
   ixy <- isp@polygons[[1]]@Polygons[[1]]@coords;
   # if more than one sub-polygon exists, include it with NA space
   # so it is properly used as a hole inside the main polygon
   if (apply_holes) {
      for (i in seq_len(length(isp@polygons[[1]]@Polygons)-1)+1) {
         ixy2 <- isp@polygons[[1]]@Polygons[[i]]@coords;
         ixy <- rbind(ixy, c(NA, NA), ixy2)
      }
   }
   polylabelr::poi(ixy, precision=0.01)
}



#' Percent area for each polygon in SpatialPolygons
#' 
#' Percent area for each polygon in SpatialPolygons
#' 
#' @family venndir utility
#' 
#' @export
sp_percent_area <- function
(sp,
   ...)
{
   venn_areas <- sapply(seq_along(sp), function(i){
      rgeos::gArea(sp::geometry(sp)[i])
   })
   venn_area <- rgeos::gArea(rgeos::gUnaryUnion(sp));
   pct_areas <- round((venn_areas / venn_area) * 1000) / 10;
   pct_areas;
}

#' Get SpatialPolygons after applying buffer width
#' 
#' Get SpatialPolygons after applying buffer width
#' 
#' This function is intended to help apply negative
#' buffer width, where the intended buffer width may
#' result in a zero-size polygon, in which case a
#' progressively smaller buffer width is applied
#' until a polygon can be returned with non-zero area.
#' 
#' An added bonus, when `relative=TRUE` it will determine
#' the actual buffer value that would barely result in non-zero
#' area, and define that as 1x buffer, and the `sp_buffer`
#' will be scaled to that value. So `sp_buffer=-0.5` will
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
#' @family venndir spatial
#' 
#' @param sp `sp::SpatialPolygons` or equivalent object as
#'    input.
#' @param sp_buffer `numeric` value, usually negative, which
#'    will create a buffer inside the `sp` polygon.
#' @param steps `integer` number of steps to iterate between
#'    `sp_buffer` and `0`.
#' @param relative `logical` indicating whether `sp_buffer` is
#'    a fraction of the buffer width required to reduce the
#'    `sp` polygon to zero area.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' sp <- get_venn_shapes(counts=c(A=1))
#' plot(sp, col="red")
#' sp_inside <- get_sp_buffer(sp, -100, relative=FALSE)
#' plot(sp_inside, col="cornsilk", lty="dotted", add=TRUE);
#' 
#' # by default relative=TRUE
#' sp1 <- sp_ellipses(c(3.5, 2), c(2, 3), xradius=c(2, 3), yradius=c(3, 1))
#' sp <- rgeos::gDifference(sp1[1], sp1[2]);
#' opar <- par("mfrow"=c(2,2));
#' on.exit(par(opar));
#' for (i in -c(0.9, 0.75, 0.5, 0.25)) {
#'    plot(sp, col="red",
#'       main=paste0("sp_buffer=", i));
#'    plot(get_sp_buffer(sp, i, relative=TRUE),
#'       col="cornsilk", lty="dotted", add=TRUE);
#' }
#' par(opar);
#' 
#' @export
get_sp_buffer <- function
(sp,
 sp_buffer=-0.5,
 steps=50,
 relative_size=TRUE,
 verbose=FALSE,
 ...)
{
   if (sp_buffer == 0) {
      return(sp)
   }
   # relative size
   sp_max <- max(apply(sp::bbox(sp), 1, diff));
   if (relative_size) {
      buffer_seq <- seq(from=sp_max,
         to=0,
         length.out=100);
      for (max_buffer in buffer_seq) {
         sp1 <- rgeos::gBuffer(sp,
            width=-max_buffer);
         if (length(sp1) > 0 && rgeos::gArea(sp1) > 0) {
            break;
         }
      }
      sp_buffer <- sp_buffer * max_buffer;
      if (verbose) {
         jamba::printDebug("get_sp_buffer(): ",
            "max_buffer:", max_buffer);
         jamba::printDebug("get_sp_buffer(): ",
            "calculated sp_buffer:",
            format(sp_buffer, digits=3, big.mark=","));
      }
   } else if (sp_buffer < 0 && abs(sp_buffer) > sp_max) {
      sp_buffer <- -sp_max;
   }
   
   if (sp_buffer > 0) {
      buffer_seq <- sp_buffer;
   } else {
      buffer_seq <- seq(from=sp_buffer,
         to=0,
         length.out=steps);
   }
   for (buffer in buffer_seq) {
      if (verbose) {
         jamba::printDebug("get_sp_buffer(): ",
            "buffer:", buffer);
      }
      sp1 <- rgeos::gBuffer(sp,
         width=buffer);
      if (length(sp1) == 0 || rgeos::gArea(sp1) == 0) {
         next;
      }
      return(sp1);
   }
   return(sp);
}
