
#' Rescale a SimpleFeatures sf object
#' 
#' Rescale a SimpleFeatures sf object
#' 
#' This function simply applies `rescale_coordinates()` to an
#' object `sf`.
#' 
#' @family venndir spatial
#' 
#' @return object `sf`
#' 
#' @inheritParams rescale_coordinates
#' 
#' @param sf object `sf`
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
rescale_sf <- function
(sf,
 rotate_degrees=0,
 scale=c(1, 1),
 shift=c(0, 0),
 center=NULL,
 share_center=FALSE,
 share_polygon_center=TRUE,
 update_bbox=TRUE,
 ...)
{
   ## SimpleFeatures sf data.frame
   if (length(center) == 0) {
      if (share_center) {
         sf_bbox <- sf::st_bbox(sf);
         center <- c(
            x=mean(sf_bbox[c("xmin", "xmax")], na.rm=TRUE),
            y=mean(sf_bbox[c("ymin", "ymax")], na.rm=TRUE))
      } else if (share_polygon_center) {
         share_center <- TRUE;
      }
   }
   
   if (length(rownames(sf)) == 0) {
      rownames(sf) <- as.character(seq_len(nrow(sf)));
   }
   
   if (length(rotate_degrees) > 0 && !all(rotate_degrees %in% c(NA, 0))) {
      if (length(names(rotate_degrees)) == 0) {
         rotate_degrees <- rep(rotate_degrees,
            length.out=nrow(sf));
         names(rotate_degrees) <- rownames(sf);
      }
   } else {
      rotate_degrees <- rep(0,
         length.out=nrow(sf));
      names(rotate_degrees) <- rownames(sf)
   }
   
   sf_column <- attr(sf, "sf_column");
   sfc <- sf[[sf_column]];
   names(sfc) <- rownames(sf);
   names(sfc);
   # iterate each geometry in the sfc list of geometries
   sfc_new <- lapply(names(sfc), function(sfcname){
      sfg <- sfc[[sfcname]];
      rescale_sfg(sfg,
         scale=scale,
         shift=shift,
         rotate_degrees=rotate_degrees[sfcname],
         center=center,
         share_center=share_center,
         ...)
   });
   sf[[sf_column]] <- sfc_new;

   if (update_bbox) {
      sf_seq <- seq_len(nrow(sf));
      attr(sf[[sf_column]], "bbox") <- sf::st_bbox(sf[sf_seq,]);
   }
   return(sp);
}


#' Rescale a SimpleFeature geometry sfg
#' 
#' Rescale a SimpleFeature geometry sfg
#' 
#' This function simply applies `rescale_coordinates()` to an
#' object derived from `sf` from `data.frame` column `sf_column`,
#' referred to as `sfc` a simple features collection of geometries.
#' 
#' This function is intended to be called from `rescale_sf(sf, ...)`
#' on each element in the column defined in `attr(sf, "sf_column")`.
#' 
#' @family venndir utility
#' 
#' @return object SimpleFeatures geometry
#' 
#' @inheritParams rescale_coordinates
#' 
#' @param sfg SimpleFeatures sf object obtained from sf elements.
#' 
#' @examples
#' polygon1_xy <- cbind(
#'    x=c(3, 4, 3, 2, 3),
#'    y=c(3, 2, 1, 2, 3));
#' polygon2_xy <- t(t(polygon1_xy) + c(1.5, 0));
#' poly1 <- sf::st_polygon(list(polygon1_xy));
#' poly2 <- sf::st_polygon(list(polygon2_xy));
#' poly12 <- sf::st_polygon(list(polygon1_xy + 3.5,
#'    polygon2_xy + 1.5));
#' poly10 <- sf::st_polygon(list(polygon1_xy + 5,
#'    t(t(polygon1_xy) / 2 + c(6.25, 6))));
#' sf <- sf::st_sf(
#'    geometry=sf::st_sfc(list(poly1, poly2, poly12, poly10)),
#'    row.names=c("poly1", "poly2", "poly12", "poly10"))
#' plot(sf, col=c("red", "blue", "gold", "purple"))
#' sfg <- sf[["geometry"]][[3]];
#' 
#' @export
rescale_sfg <- function
(sfg,
 rotate_degrees=0,
 scale=c(1, 1),
 shift=c(0, 0),
 center=NULL,
 share_center=TRUE,
 ...)
{
   ## simple features geometry
   if (length(center) == 0 && share_center) {
      center <- sf::st_coordinates(sf::st_centroid(sfg))
      # sf_bbox <- sf::st_bbox(sfg);
      # center <- c(
      #    x=mean(sf_bbox[c("xmin", "xmax")], na.rm=TRUE),
      #    y=mean(sf_bbox[c("ymin", "ymax")], na.rm=TRUE))
   }
   
   if (length(scale) == 0) {
      scale <- c(1, 1);
   }
   if (length(shift) == 0) {
      shift <- c(0, 0);
   }
   scale <- rep(scale, length.out=2);
   shift <- rep(shift, length.out=2);
   sfg_coords <- sf::st_coordinates(sfg);
   
   Lcolnames <- jamba::vigrep("^L[0-9]+", colnames(sfg_coords));
   coord_colnames <- setdiff(colnames(sfg_coords), Lcolnames);

   sfg_coords[,coord_colnames] <- rescale_coordinates(
      sfg_coords[, coord_colnames, drop=FALSE],
      scale=scale,
      shift=shift,
      rotate_degrees=rotate_degrees,
      center=center)
      # ...);
   
   # Note: use st_set_geometry() here somehow
   sf::st_coordinates(sfg) <- sfg_coords;
   # sfg_coords_l <- split(
   #    data.frame(sfg_coords),
   #    data.frame(sfg_coords[, Lcolnames, drop=FALSE]))
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
