
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
#' @export
find_vennpoly_overlaps <- function
(sp,
 venn_counts=NULL,
 venn_items=NULL,
 venn_colors=NULL,
 preset="dichromat",
 x_nudge=NULL,
 y_nudge=NULL,
 do_plot=FALSE,
 verbose=FALSE,
 ...)
{
   numSets <- length(sp);
   if (length(venn_colors) == 0) {
      venn_colors <- colorjam::group2colors(names(sp),
         preset=preset);
   }
   if (length(names(venn_colors)) == 0) {
      names(venn_colors) <- names(sp);
   }
   
   ## define incidence matrix of overlaps
   el1 <- expand.grid(rep(list(c(0,1)), numSets));
   colnames(el1) <- names(sp);
   ## remove blank row
   el1 <- el1[!rowSums(el1) == 0,];
   rownames(el1) <- jamba::cPaste(sep="&", multienrichjam::im2list(t(el1)));
   svims_df <- data.frame(check.names=FALSE,
      el1);
   svims_df$sum <- rowSums(svims_df[,colnames(el1),drop=FALSE]);
   svims_df <- jamba::mixedSortDF(svims_df, byCols=c("sum", paste0("-", colnames(el1))));
   el1 <- el1[rownames(svims_df),,drop=FALSE];
   
   if (verbose) {
      jamba::printDebug("find_vennpoly_overlaps(): ",
         "head(el1):");
      print(head(el1, 20));
   }
   
   ## Optionally nudge the polygon coordinates
   if (length(x_nudge) > 0 && all(names(x_nudge) %in% names(sp))) {
      for (i in names(x_nudge)) {
         j <- match(i, names(sp));
         ixy <- sp@polygons[[j]]@Polygons[[1]]@coords;
         ixy[,1] <- ixy[,1] + x_nudge[i];
         sp@polygons[[j]]@Polygons[[1]]@coords <- ixy;
      }
   }
   if (length(y_nudge) > 0 && all(names(y_nudge) %in% names(sp))) {
      for (i in names(y_nudge)) {
         jamba::printDebug("y_nudge on ", i, ", for ", y_nudge[i]);
         j <- match(i, names(sp));
         ixy <- sp@polygons[[j]]@Polygons[[1]]@coords;
         ixy[,2] <- ixy[,2] + y_nudge[i];
         sp@polygons[[j]]@Polygons[[1]]@coords <- ixy;
      }
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
         preset=preset);
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
         label=names(venn_SP)));
   
   venn_spdf$venn_counts <- venn_poly_counts;
   venn_spdf$venn_color <- venn_poly_colors;
   
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
   venn_spdf$x_label <- venn_labels_xy[,1];
   venn_spdf$y_label <- venn_labels_xy[,2];
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
#' @examples
#' x <- list(a=LETTERS[1],
#'    b=LETTERS[1:2],
#'    c=LETTERS[2:4]);
#' x;
#' y <- list(
#'    a=LETTERS[1:2],
#'    b=LETTERS[2],
#'    c=LETTERS[2:4]);
#' y;
#' match_list(x, y)
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
      setnames <- seq_along(x);
   }
   
   xvals <- sin(angle_seq);
   yvals <- cos(angle_seq);

   #ellipse <- cbind(radius[1] * cos(angles), radius[2] * sin(angles));
   #ellipse <- cbind(ellipse[,1]*cos(rotate) + ellipse[,2]*sin(rotate), ellipse[,2]*cos(rotate) - ellipse[,1]*sin(rotate) );
   #ellipse <- cbind(center[1]+ellipse[,1], center[2]+ellipse[,2]);
   
   xvals <- sin(angle_seq);
   yvals <- cos(angle_seq);
   if (length(radius) == 0) {
      radius <- 1;
   }
   radius <- rep(radius,
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
#' @family venndir utility
#' 
#' @export
nudge_sp <- function
(sp,
 x_nudge=NULL,
 y_nudge=NULL,
 rotate_degrees=0,
 ...)
{
   ## Optionally nudge the polygon coordinates
   if (length(x_nudge) > 0 && all(names(x_nudge) %in% names(sp))) {
      for (i in names(x_nudge)) {
         j <- match(i, names(sp));
         ixy <- sp@polygons[[j]]@Polygons[[1]]@coords;
         ixy[,1] <- ixy[,1] + x_nudge[i];
         sp@polygons[[j]]@Polygons[[1]]@coords <- ixy;
      }
   }
   if (length(y_nudge) > 0 && all(names(y_nudge) %in% names(sp))) {
      for (i in names(y_nudge)) {
         jamba::printDebug("y_nudge on ", i, ", for ", y_nudge[i]);
         j <- match(i, names(sp));
         ixy <- sp@polygons[[j]]@Polygons[[1]]@coords;
         ixy[,2] <- ixy[,2] + y_nudge[i];
         sp@polygons[[j]]@Polygons[[1]]@coords <- ixy;
      }
   }
   return(invisible(sp));
}
