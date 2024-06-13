
#' Convert euler output to polygons
#' 
#' Convert euler output to polygons
#' 
#' This function takes the output from `eulerr::euler()` and
#' converts it to polygons in `list` format.
#' 
#' @return `list` polygon object with one polygon
#'    for each Euler circle or ellipse.
#' 
#' @family venndir polygons
#' 
#' @param x output from `eulerr::euler()`
#' 
#' @returns `list` with polygons for each unique set defined by `names(x)`,
#'    where each list contains `numeric` vectors named `"x"` and `"y"`.
#' 
#' @examples
#' counts <- c(A=1, B=2, `A&B`=3)
#' x <- eulerr::euler(counts)
#' polygon_list <- eulerr_to_polygon_list(x)
#' 
#' polygon_areas(polygon_list)
#' 
#' @export
eulerr_to_polygon_list <- function
(x)
{
   # x <- va
   ellipses1 <- simple_ellipse(h=x$ellipses$h,
      k=x$ellipses$k,
      a=x$ellipses$a,
      b=x$ellipses$b,
      phi=x$ellipses$phi);
   names(ellipses1) <- rownames(x$ellipses);
   return(ellipses1);
}

#' Simple ellipse function
#' 
#' @family venndir polygons
#' 
#' @export
simple_ellipse <- function
(h,
 k,
 a,
 b=a,
 phi=0,
 n=200L)
{
   theta <- seq.int(0, 2 * pi, length.out = n)
   m <- length(h)
   out <- vector("list", m)
   for (i in seq_along(h)) {
      out[[i]]$x <- h[i] + a[i] * cos(theta) * cos(phi[i]) - 
         b[i] * sin(theta) * sin(phi[i])
      out[[i]]$y <- k[i] + b[i] * sin(theta) * cos(phi[i]) + 
         a[i] * cos(theta) * sin(phi[i])
   }
   out
}

#' Convert eulerr output to JamPolygon
#' 
#' @returns `JamPolygon` object
#' 
#' @family JamPolygon
#' 
#' @export
eulerr_to_JamPolygon <- function
(x)
{
   # x <- va
   ellipses1 <- simple_ellipse(h=x$ellipses$h,
      k=x$ellipses$k,
      a=x$ellipses$a,
      b=x$ellipses$b,
      phi=x$ellipses$phi);
   names(ellipses1) <- rownames(x$ellipses);
   
   # convert polygon_list to JamPolygon
   df <- data.frame(check.names=FALSE,
      name=names(ellipses1),
      x=I(lapply(ellipses1, function(i){i$x})),
      y=I(lapply(ellipses1, function(i){i$y})));
   jp <- new("JamPolygon",
      polygons=df);
   return(jp);
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
#' @returns `JamPolygon` object, which contains columns:
#'    * `"name"`
#'    * `"x"`, `"y"`
#'    * `"fill"`
#'    * `"venn_name"`
#'    * `"venn_count"`
#'    * `"venn_items"`
#'    * `"venn_color"`
#'    * `"label"`
#'    * `"label_x"`
#'    * `"label_y"`
#' 
#' @family JamPolygon
#' 
#' @param jp `JamPolygon` that contains one polygon per set, named
#'    using set names.
#' @param venn_counts `vector` with `integer` values, whose names
#'    represent each Venn overlap set combination, using
#'    `sep` as delimiter between set names.
#' @param venn_items `list` or `NULL` that contains items in each
#'    overlap set.
#' @param sep `character` string used as a delimiter between set names.
#' @param preset,blend_preset `character` string passed to
#'    `colorjam::rainbowJam()` and `colorjam::blend_colors()`,
#'    respectively, to define the color hue wheel used for categorical
#'    colors, and for color blending. The default `preset="dichromat2"`
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
#' x <- eulerr::euler(test_counts)
#' jp1 <- eulerr_to_JamPolygon(x)
#' polygon_colors <- colorjam::rainbowJam(length(jp1))
#' jp1@polygons$fill <- polygon_colors;
#' plot(jp1)
#' 
#' xo <- find_venn_overlaps_JamPolygon(jp=jp1, venn_counts=test_counts)
#' xo@polygons$border <- jamba::makeColorDarker(darkFactor=1.2,
#'    xo@polygons$venn_color)
#'    xo@polygons$border.lwd <- 2;
#' plot(xo, flip_sign=-1);
#' 
#' @export
# find_venn_polygon_list_overlaps <- function
find_venn_overlaps_JamPolygon <- function
(jp,
 venn_counts=NULL,
 venn_items=NULL,
 venn_colors=NULL,
 sep="&",
 preset="dichromat2",
 blend_preset="ryb",
 sp_nudge=NULL,
 rotate_degrees=0,
 do_plot=FALSE,
 verbose=FALSE,
 ...)
{
   # check input class
   if (length(jp) == 0) {
      stop("Either polygon_list or jp must be supplied.")
   }
   if (!inherits(jp, "JamPolygon")) {
      stop("jp must be supplied as JamPolygon");
   }

   # determine number of sets and define colors
   numSets <- length(jp);
   if (length(venn_colors) == 0) {
      if ("fill" %in% colnames(jp@polygons)) {
         venn_colors <- jamba::nameVector(jp@polygons$fill, names(jp));
      } else {
         venn_colors <- colorjam::group2colors(names(jp),
            preset=preset,
            ...);
      }
   }
   if (length(venn_colors) != length(jp)) {
      venn_colors <- rep(venn_colors, length.out=length(jp));
   }
   if (length(names(venn_colors)) == 0) {
      names(venn_colors) <- names(jp);
   }
   
   ## define incidence matrix of overlaps
   el1 <- make_venn_combn_df(names(jp),
      sep=sep);
   if (verbose) {
      jamba::printDebug("find_venn_overlaps_JamPolygon(): ",
         "names(jp):", names(jp));
      jamba::printDebug("find_venn_overlaps_JamPolygon(): ",
         "head(el1):");
      print(head(el1, 30));
   }
   
   ## Optionally nudge individual polygon coordinates
   # if (length(sp_nudge) > 0 && any(unlist(sp_nudge) != 0)) {
   #    sp <- nudge_sp(sp,
   #       sp_nudge=sp_nudge,
   #       ...);
   # }
   
   ## Optionally rotate all polygon coordinates
   # if (length(rotate_degrees) > 0 && any(rotate_degrees != 0)) {
   #    sp <- rescale_sp(sp,
   #       rotate_degrees=rotate_degrees,
   #       ...);
   # }
   
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
         jamba::printDebug("find_venn_overlaps_JamPolygon(): ",
            rownames(el1)[j]);
      }
      whichYes <- which(i %in% 1);
      whichNo <- which(i %in% 0);
      i_names <- colnames(el1)[whichYes];
      venn_color <- colorjam::blend_colors(venn_colors[i_names],
         preset=blend_preset,
         ...);
      border <- jamba::makeColorDarker(venn_color,
         darkFactor=1.3);
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
         jamba::printDebug("find_venn_overlaps_JamPolygon(): ",
            "whichYes:", whichYes);
         jamba::printDebug("find_venn_overlaps_JamPolygon(): ",
            "whichNo:", whichNo);
      }
      ## Intersection of relevant sets
      if (length(whichYes) >= 1) {
         if (verbose) {
            jamba::printDebug("find_venn_overlaps_JamPolygon(): ",
               "intersect_JamPolygon(jp[whichYes, ]), whichYes: ", whichYes);
            # print(jp[whichYes, ]);# debug
         }
         ellYes <- intersect_JamPolygon(jp[whichYes, ]);
         # ellYes <- intersect_polygon_list(polygon_list[whichYes]);
      }
      if (length(ellYes) == 0) {
         ellUse <- list();
      } else {
         if (length(whichNo) >= 1) {
            if (verbose) {
               jamba::printDebug("find_venn_overlaps_JamPolygon(): ",
                  "minus_JamPolygon(jp[whichNo, ]), whichNo: ", whichNo);
               # print(jp[whichNo, ]);# debug
            }
            ellUse <- minus_JamPolygon(rbind2(ellYes, jp[whichNo, ]));
         } else {
            ellNo <- NULL;
            ellUse <- ellYes;
         }
         if (length(ellUse) > 0) {
            names(ellUse) <- poly_name;
         } else {
            ellUse <- list();
         }
      }
      # store into the data.frame
      ellUse@polygons$venn_name <- poly_name;
      ellUse@polygons$venn_counts <- venn_poly_count;
      ellUse@polygons$venn_items <- I(list(venn_poly_items));
      ellUse@polygons$venn_color <- venn_color;
      ellUse@polygons$border <- NA;
      ellUse@polygons$border.lwd <- 1;
      ellUse@polygons$innerborder <- border;
      ellUse@polygons$innerborder.lwd <- 3;
      ellUse@polygons$fill <- venn_color;
      # attr(ellUse, "venn_name") <- poly_name;
      # attr(ellUse, "venn_count") <- venn_poly_count;
      # attr(ellUse, "venn_items") <- venn_poly_items;
      # attr(ellUse, "venn_color") <- venn_color;
      if (verbose) {
         jamba::printDebug("find_venn_overlaps_JamPolygon(): ",
            "length(overlap polygon):", lengths(ellUse));
      }
      # jamba::printDebug("ellYes:");print(ellYes);# debug
      # jamba::printDebug("ellUse:");print(ellUse);# debug
      ellUse;
   })
   # jamba::printDebug("venn_poly_coords (before rbind2):");print(venn_poly_coords);# debug
   # return(venn_poly_coords);
   venn_poly_coords <- do.call(rbind2, venn_poly_coords);
   # Note venn_poly_coords is JamPolygon
   # jamba::printDebug("venn_poly_coords (after rbind2):");print(venn_poly_coords);# debug
   venn_poly_coords@polygons$name <- rownames(el1);
   rownames(venn_poly_coords@polygons) <- names(venn_poly_coords);
   # names(venn_poly_coords) <- rownames(el1);
   
   venn_poly_colors <- venn_poly_coords@polygons$venn_color;
   # venn_poly_counts <- venn_poly_coords@polygons$venn_count;
   venn_poly_items <- venn_poly_coords@polygons$venn_items;

   vennUse <- which(lengths(venn_poly_coords@polygons$x) > 0);
   vennMissing <- which(lengths(venn_poly_coords@polygons$x) == 0);
   if (verbose) {
      jamba::printDebug("find_vennpoly_overlaps(): ",
         "vennUse:", vennUse,
         ", vennMissing:", vennMissing);
   }
   # OMIT since JamPolygon already contains data.frame
   # new data.frame format
   # venn_xy_coords <- polygon_list_to_xy_list(venn_poly_coords);
   # if (verbose > 1) {
   #    jamba::printDebug("ssdim(venn_poly_coords):");print(jamba::ssdim(venn_poly_coords));# debug
   #    jamba::printDebug("ssdim(venn_xy_coords):");print(jamba::ssdim(venn_xy_coords));# debug
   # }
   # return(invisible(venn_poly_coords));
   
   venn_poly_coords@polygons$label <- names(venn_poly_coords);
   
   # venn_pcdf <- data.frame(check.names=FALSE,
   #    stringsAsFactors=FALSE,
   #    label=names(venn_poly_coords[vennUse]),
   #    x=I(venn_xy_coords$x[vennUse]),
   #    y=I(venn_xy_coords$y[vennUse]),
   #    venn_poly_coords=I(venn_poly_coords[vennUse]),
   #    color=venn_poly_colors[vennUse],
   #    venn_counts=venn_poly_counts[vennUse],
   #    venn_color=venn_poly_colors[vennUse])
   
   # Define label position for each polygon
   label_xy <- labelr_JamPolygon(venn_poly_coords);
   venn_poly_coords@polygons[, c("label_x", "label_y")] <- as.data.frame(
      label_xy);
   # venn_pcdf[, c("x_label", "y_label")] <- jamba::rbindList(
   #    polygon_list_labelr(venn_pcdf$venn_poly_coords));
   
   # Port this function: sp_polylabelr(i)
   # which calls polylabelr::poi(x, y) on each polygon
   return(invisible(venn_poly_coords));

}


#' Intersect one or more polygons
#' 
#' Intersect one or more polygons
#' 
#' This function takes a `list` of polygons and iteratively
#' calls `polyclip::polyclip(A, B, op="intersect")` to produce the intersect
#' across one or more polygons, which otherwise only works with two
#' polygons.
#' 
#' @return object `list` of polygons
#' 
#' @family venndir polygons
#' 
#' @param polygon_list `list` object that contains one or more polygons.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' counts <- c(A=1, B=2, `A&B`=3)
#' x <- eulerr::euler(counts)
#' polygon_list <- eulerr_to_polygon_list(x)
#' 
#' circle_intersect <- intersect_polygon_list(polygon_list);
#' jamba::ssdim(circle_intersect)
#' circle_colors <- colorjam::rainbowJam(2);
#' plot_polygon_list(polygon_list, col=circle_colors, main="intersect")
#' plot_polygon_list(circle_intersect, col="#FFDD0088", border="red", lwd=3, add=TRUE)
#' 
#' @export
intersect_polygon_list <- function
(polygon_list,
 new_name=NULL,
 ...)
{
   # Purpose is to use polyclip::polyclip(A, B, op="intersect")
   # on two or more polygons
   if (all(c("x", "y") %in% names(polygon_list))) {
      # convert xy_list to polygon_list
      polygon_list <- xy_list_to_polygon_list(polygon_list);
   }
   if (length(polygon_list) <= 1) {
      return(polygon_list);
   }
   output_polygon <- polygon_list[1];
   
   for (i in 2:length(polygon_list)) {
      output_polygon <- polyclip::polyclip(
         A=output_polygon,
         B=polygon_list[i],
         op="intersect");
   }
   if (length(new_name) == 1) {
      names(output_polygon) <- new_name;
   } else {
      names(output_polygon) <- head(names(polygon_list), 1);
   }
   return(output_polygon);
}

#' Intersect one or more JamPolygon objects
#' 
#' Intersect one or more JamPolygon objects
#' 
#' @family JamPolygon
#' 
#' @returns `JamPolygon` after applying the intersection
#' 
#' @param jp `JamPolygon`
#' @param new_name `character` string used to populate the `"name"`
#'    column in the output data. Only the first value is used,
#'    otherwise the first existing value in `names(jp)` is kept.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' 
#' @export
intersect_JamPolygon <- function
(jp,
 new_name=NULL,
 ...)
{
   # Purpose is to use polyclip::polyclip(A, B, op="intersect")
   # on two or more polygons

   if (length(jp) == 1) {
      return(jp)
   }
   
   # if any polygons are empty, return empty
   blank_jp <- function(jp, new_name=NULL){
      jp <- jp[1, ];
      jp@polygons$x <- I(list(numeric(0)));
      jp@polygons$y <- I(list(numeric(0)));
      if (length(new_name) > 0) {
         names(jp) <- head(new_name, 1);
      }
      return(jp);
   }
   if (any(lengths(jp) == 0)) {
      return(blank_jp(jp, new_name))
   }
   Ax <- jp@polygons$x[[1]];
   Ay <- jp@polygons$y[[1]];
   if (length(jamba::rmNA(unlist(Ax))) == 0) {
      # if polygon is empty, return empty
      return(blank_jp(jp, new_name))
   }
   if (!is.list(Ax)) {
      Ax <- list(Ax);
      Ay <- list(Ay);
   }
   A <- lapply(seq_along(Ax), function(i){
      list(x=Ax[[i]], y=Ay[[i]])
   })
   
   pseq <- seq(from=2, to=nrow(jp@polygons));
   for (i in pseq) {
      Bx <- jp@polygons$x[[i]];
      By <- jp@polygons$y[[i]];
      if (length(jamba::rmNA(unlist(Ax))) == 0) {
         # if polygon is empty, return empty
         return(blank_jp(jp, new_name))
      }
      if (!is.list(Bx)) {
         Bx <- list(Bx);
         By <- list(By);
      }
      B <- lapply(seq_along(Bx), function(i){
         list(x=Bx[[i]], y=By[[i]])
      })
      A <- polyclip::polyclip(A=A,
         B=B,
         op="intersection")
      if (length(A) == 0 || any(lengths(A) == 0)) {
         # if result is empty, return empty jp
         return(blank_jp(jp, new_name))
      }
   }
   jp <- jp[1, ];
   # Todo:
   # - check what happens when return contains multiple polygons
   newx <- lapply(A, function(i){i$x});
   newy <- lapply(A, function(i){i$y});
   if (length(newx) == 0) {
      newx <- list(NULL);
      newy <- list(NULL);
   } else if (length(newx) > 1) {
      # handle multipart polygons
      newx <- list(newx);
      newy <- list(newy);
   }
   jp@polygons$x <- I(newx);
   jp@polygons$y <- I(newy);
   if (length(new_name) > 0) {
      names(jp) <- head(new_name, 1);
   }
   return(jp);
}

#' Union one or more JamPolygon objects
#' 
#' Union one or more JamPolygon objects
#' 
#' @family JamPolygon
#' 
#' @returns `JamPolygon` after applying the union
#' 
#' @param jp `JamPolygon`
#' @param new_name `character` string to define optional `names(jp)` of
#'    the output. Otherwise it uses the first name in `jp`.
#' @param verbose `logical` indicating whether to print verbose output.
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
#' jp3b <- union_JamPolygon(jp3, new_name="polygons 1,2")
#' plot(jp3b)
#' 
#' # test empty polygon
#' jp3na <- jp3;
#' jp3na@polygons[2, "x"] <- I(list(NA))
#' jp3na@polygons[2, "y"] <- I(list(NA))
#' jp3na
#' union_JamPolygon(jp3na[1,])
#' union_JamPolygon(jp3na)
#' 
#' @export
union_JamPolygon <- function
(jp,
 new_name=NULL,
 verbose=FALSE,
 ...)
{
   # Purpose is to use polyclip::polyclip(A, B, op="union")
   # on two or more polygons
   
   if (nrow(jp@polygons) == 1) {
      return(jp)
   }
   
   # start with first non-empty polygon
   for (i in seq_len(length(jp))) {
      Ax <- jp@polygons$x[[i]];
      Ay <- jp@polygons$y[[i]];
      if (length(jamba::rmNA(unlist(Ax))) > 0) {
         break;
      }
   }
   if (length(jamba::rmNA(unlist(jp@polygons$x))) == 0) {
      # if all polygons are empty, return first entry
      if (verbose) {
         jamba::printDebug("union_JamPolygon(): ",
            "empty input.")
      }
      return(jp[1, ]);
   }
   
   if (!is.list(Ax)) {
      Ax <- list(Ax);
      Ay <- list(Ay);
   }
   A <- lapply(seq_along(Ax), function(i){
      list(x=Ax[[i]], y=Ay[[i]])
   })
   pseq <- tail(seq_len(length(jp)), -i);
   for (i in pseq) {
      Bx <- jp@polygons$x[[i]];
      By <- jp@polygons$y[[i]];
      if (length(jamba::rmNA(unlist(Bx))) == 0) {
         # if B is empty, keep A
         next;
      }
      if (!is.list(Bx)) {
         Bx <- list(Bx);
         By <- list(By);
      }
      B <- lapply(seq_along(Bx), function(i){
         list(x=Bx[[i]], y=By[[i]])
      })
      A <- polyclip::polyclip(A=A,
         B=B,
         op="union")
      # if (length(A) == 0) {
      #    break;
      # }
   }
   jp <- jp[1, ];
   newx <- lapply(A, function(i){i$x});
   newy <- lapply(A, function(i){i$y});
   if (length(newx) == 0) {
      newx <- list(NULL);
      newy <- list(NULL);
   } else if (length(newx) > 1) {
      # handle multipart polygons
      newx <- list(newx);
      newy <- list(newy);
   }
   jp@polygons$x <- I(newx);
   jp@polygons$y <- I(newy);
   if (length(new_name) > 0) {
      names(jp) <- head(new_name, 1);
   }
   return(jp);
}

#' Subtract one or more JamPolygon objects
#' 
#' Subtract one or more JamPolygon objects
#' 
#' @family JamPolygon
#' 
#' @returns `JamPolygon` after applying the subtraction
#' 
#' @param closed `logical` indicating whether the first polygon
#'    is considered a closed polygon, or when `closed=FALSE`
#'    only the line is maintained.
#' 
#' @export
minus_JamPolygon <- function
(jp,
 new_name=NULL,
 closed=TRUE,
 ...)
{
   # Purpose is to use polyclip::polyclip(A, B, op="minus")
   # on two or more polygons
   
   if (nrow(jp@polygons) == 1) {
      return(jp)
   }
   
   # if first polygon is already empty, return first entry
   if (0 %in% head(lengths(jp), 1)) {
      return(jp[1, ])
   }
   Ax <- jp@polygons$x[[1]];
   Ay <- jp@polygons$y[[1]];
   if (!is.list(Ax)) {
      Ax <- list(Ax);
      Ay <- list(Ay);
   }
   A <- lapply(seq_along(Ax), function(i){
      list(x=Ax[[i]], y=Ay[[i]])
   })
   
   pseq <- seq(from=2, to=nrow(jp@polygons));
   for (i in pseq) {
      Bx <- jp@polygons$x[[i]];
      By <- jp@polygons$y[[i]];
      if (length(jamba::rmNA(unlist(Bx))) == 0) {
         # if polygon is empty, there is nothing to subtract
         next;
      }
      if (!is.list(Bx)) {
         Bx <- list(Bx);
         By <- list(By);
      }
      B <- lapply(seq_along(Bx), function(i){
         list(x=Bx[[i]], y=By[[i]])
      })
      A <- polyclip::polyclip(A=A,
         B=B,
         closed=closed,
         op="minus")
      if (0 %in% head(lengths(A), 1)) {
         break;
      }
   }
   jp <- jp[1, ];
   newx <- lapply(A, function(i){i$x});
   newy <- lapply(A, function(i){i$y});
   if (length(newx) == 0) {
      newx <- list(NULL);
      newy <- list(NULL);
   } else if (length(newx) > 1) {
      # handle multipart polygons
      newx <- list(newx);
      newy <- list(newy);
   }
   jp@polygons$x <- I(newx);
   jp@polygons$y <- I(newy);
   if (length(new_name) == 1) {
      jp@polygons$name <- new_name;
   }
   return(jp);
}


#' Union one or more polygons
#' 
#' Union one or more polygons
#' 
#' This function takes a `list` of polygons and iteratively
#' calls `polyclip::polyclip(A, B, op="union")` to produce a union
#' across one or more polygons, which otherwise only works with two
#' polygons.
#' 
#' @return object `list` of polygons
#' 
#' @family venndir polygons
#' 
#' @param polygon_list `list` object that contains one or more polygons.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' counts <- c(A=1, B=2, `A&B`=3)
#' x <- eulerr::euler(counts)
#' polygon_list <- eulerr_to_polygon_list(x)
#' 
#' circle_union <- union_polygon_list(polygon_list);
#' jamba::ssdim(circle_union)
#' circle_colors <- colorjam::rainbowJam(2);
#' plot_polygon_list(polygon_list, col=circle_colors, main="union")
#' plot_polygon_list(circle_union, col="#FFDD0088", border="red", lwd=3, add=TRUE)
#' 
#' counts2 <- c(A=1, B=2, `A&B`=3, C=4)
#' x2 <- eulerr::euler(counts2)
#' polygon_list2 <- eulerr_to_polygon_list(x2)
#' plot_polygon_list(polygon_list2)
#' 
#' @export
union_polygon_list <- function
(polygon_list,
 ...)
{
   # Purpose is to use polyclip::polyclip(A, B, op="union")
   # on two or more polygons
   if (all(c("x", "y") %in% names(polygon_list))) {
      # convert xy_list to polygon_list
      polygon_list <- xy_list_to_polygon_list(polygon_list);
   }
   if (length(polygon_list) <= 1) {
      return(polygon_list);
   }
   output_polygon <- polygon_list[1];
   
   for (i in 2:length(polygon_list)) {
      output_polygon <- polyclip::polyclip(
         A=output_polygon,
         B=polygon_list[i],
         op="union");
   }
   names(output_polygon) <- head(names(polygon_list), 1);
   return(output_polygon);
}


#' Subtract one or more polygons
#' 
#' Subtract one or more polygons
#' 
#' This function takes a `list` of polygons and iteratively
#' calls `polyclip::polyclip(A, B, op="minus")` to produce a union
#' across one or more polygons, which otherwise only works with two
#' polygons.
#' 
#' @return object `list` of polygons
#' 
#' @family venndir polygons
#' 
#' @param polygon_list `list` object that contains one or more polygons.
#' @param new_name `character` string with optional new name for the
#'    output polygon.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' counts <- c(A=1, B=2, `A&B`=3)
#' counts <- c(A=1, B=2, `A&B`=3, C=5, `B&C`=2, `A&C`=2, `A&B&C`=1)
#' x <- eulerr::euler(counts)
#' polygon_list <- eulerr_to_polygon_list(x)
#' 
#' circle_minus <- minus_polygon_list(polygon_list);
#' circle_colors <- colorjam::rainbowJam(length(polygon_list), alpha=0.5);
#' plot_polygon_list(polygon_list, col=circle_colors, main="minus")
#' plot_polygon_list(circle_minus, col="#FFDD0088", border="red", lwd=3, add=TRUE)
#' 
#' circle_minus2 <- minus_polygon_list(polygon_list[c(2,1,3)]);
#' plot_polygon_list(circle_minus2, col="#FFDD0088", border="blue", lwd=3, add=TRUE)
#' @export
minus_polygon_list <- function
(polygon_list,
 new_name=NULL,
 ...)
{
   # Purpose is to use polyclip::polyclip(A, B, op="minus")
   # on two or more polygons
   if (all(c("x", "y") %in% names(polygon_list))) {
      # convert xy_list to polygon_list
      polygon_list <- xy_list_to_polygon_list(polygon_list);
   }
   if (length(polygon_list) <= 1) {
      return(polygon_list);
   }
   output_polygon <- polygon_list[1];
   
   for (i in 2:length(polygon_list)) {
      output_polygon <- polyclip::polyclip(
         A=output_polygon,
         B=polygon_list[i],
         op="minus");
   }
   if (length(output_polygon) > 1) {
      output_polygon <- list(output_polygon);
   }
   if (length(new_name) == 1) {
      names(output_polygon) <- new_name;
   } else {
      names(output_polygon) <- head(names(polygon_list), 1);
   }
   return(output_polygon);
}

#' Plot polygon_list using base R
#' 
#' Plot polygon_list using base R
#' 
#' @family venndir polygons
#'
#' @examples
#' counts <- c(A=1, B=2, `A&B`=3, C=5, `B&C`=2, `A&C`=2, `A&B&C`=1)
#' x <- eulerr::euler(counts)
#' polygon_list <- eulerr_to_polygon_list(x)
#' plot_polygon_list(polygon_list,
#'    col=colorjam::rainbowJam(length(polygon_list), alpha=0.5))
#' 
#' polygon_list2 <- list(A=polygon_list$A, BC=polygon_list[c("B", "C")])
#' plot_polygon_list(polygon_list2,
#'    col=colorjam::rainbowJam(length(polygon_list2), alpha=0.5))
#' 
#' @export
plot_polygon_list <- function
(polygon_list,
 col=NULL,
 border="black",
 lwd=1,
 add=FALSE,
 asp=1,
 bty="n",
 xaxt="n",
 yaxt="n",
 xlab="",
 ylab="",
 xlim=NULL,
 ylim=NULL,
 rule=c("evenodd", "none"),
 ...)
{
   # rule
   rule <- match.arg(rule);
   
   #
   if (all(c("x", "y") %in% names(polygon_list))) {
      # input is xy_list format
      xy_list <- polygon_list;
      polygon_list <- xy_list_to_polygon_list(xy_list);
   } else {
      # input is polygon_list
      xy_list <- polygon_list_to_xy_list(polygon_list);
   }
   if (FALSE %in% add) {
      xy_ranges <- bbox_polygon_list(polygon_list);
      if (length(xlim) == 0) {
         xlim <- xy_ranges$x
      }
      if (length(ylim) == 0) {
         ylim <- xy_ranges$y
      }
      plot(NULL,
         xlim=xlim,
         ylim=ylim,
         asp=asp,
         bty=bty,
         xaxt=xaxt,
         yaxt=yaxt,
         xlab=xlab,
         ylab=ylab,
         ...)
   }
   if (length(col) >= 1) {
      col <- rep(col,
         length.out=length(polygon_list));
   }
   if (length(border) >= 1) {
      border <- rep(border,
         length.out=length(polygon_list));
   }
   for (i in seq_along(polygon_list)) {
      if (is.list(polygon_list[[i]]) &&
            !all(c("x", "y") %in% names(polygon_list[[i]]))) {
         # option 1: plot each polygon component independently
         if ("none" %in% rule) {
            jamba::printDebug("polygon_list[[i]] as nested polygon, rule='none':");print(polygon_list[[i]]);
            plot_polygon_list(
               polygon_list=polygon_list[[i]],
               col=col[[i]],
               border=border[[i]],
               lwd=lwd,
               add=TRUE,
               ...)
         }
         # option 2: graphics::polypath(x, y, rule="evenodd") to allow holes
         # - convert polygon_list to xy_list then to coord_list
         if ("evenodd" %in% rule) {
            if (length(polygon_list[[i]]) == 1) {
               # single polygon, no need for fancy polypath()
               coord_list <- list(
                  x=polygon_list[[i]][[1]]$x,
                  y=polygon_list[[i]][[1]]$y);
            } else {
               coord_list <- list(
                  x=head(unname(unlist(
                     lapply(polygon_list[[i]], function(i){c(i$x, NA)}))), -1),
                  y=head(unname(unlist(
                     lapply(polygon_list[[i]], function(i){c(i$y, NA)}))), -1));
            }
            # use_xy_list <- polygon_list_to_xy_list(polygon_list[[i]])
            # coord_list <- xy_list_to_coord_list(use_xy_list)
            # jamba::printDebug("coord_list:");print(coord_list);# debug
            polypath(x=coord_list$x,
               y=coord_list$y,
               rule=rule,
               col=col[[i]],
               border=border[[i]],
               lwd=lwd,
               ...)
         }
      } else {
         polygon(
            x=polygon_list[[i]],
            col=col[[i]],
            border=border[[i]],
            lwd=lwd,
            ...)
      }
   }
}


#' Bounding box for polygon list
#' 
#' Bounding box for polygon list
#' 
#' @family venndir polygons
#' 
#' @examples
#' counts <- c(A=1, B=2, `A&B`=3, C=5, `B&C`=2, `A&C`=2, `A&B&C`=1)
#' x <- eulerr::euler(counts)
#' polygon_list <- eulerr_to_polygon_list(x)
#' bbox_polygon_list(polygon_list)
#' 
#' @export
bbox_polygon_list <- function
(polygon_list,
 ...)
{
   #
   if (all(c("x", "y") %in% names(polygon_list))) {
      # input is xy_list format
      xy_list <- polygon_list;
      polygon_list <- xy_list_to_polygon_list(xy_list);
   } else {
      # input is polygon_list
      xy_list <- polygon_list_to_xy_list(polygon_list);
   }
   range_x <- range(unlist(xy_list$x), na.rm=TRUE);
   range_y <- range(unlist(xy_list$y), na.rm=TRUE);
   return(list(x=range_x, y=range_y))
}


#' Calculate polygon label positions using Pole of Inaccessibility
#' 
#' Calculate polygon label positions using Pole of Inaccessibility, otherwise
#' known as the Visual Center.
#' 
#' This function is a wrapper for `polylabelr::poi()` except that it
#' is applied to a `list` of polygons individually.
#' 
#' When any one polygon is composed of two smaller polygon components,
#' as encoded with a nested list of coordinates,
#' first the polygons are combined using `union_polygon_list()`.
#' If the result is a single polygon, that is used to define the
#' label position. If the result is multiple separate polygon
#' components, the largest polygon component is used to find the label.
#' 
#' @param polygon_list `list` containing elements `"x"` and `"y"` each
#'    with `numeric` vectors, or `list` of `numeric` vectors.
#' @param add_labels `logical` indicating whether to plot the labels
#'    using `text()`
#' @param ... additional arguments are passed to `text()` when
#'    `add_labels=TRUE`
#' 
#' @family venndir polygons
#' 
#' @examples
#' counts <- c(A=1, B=2, `A&B`=3, C=5, `B&C`=2, `A&C`=2, `A&B&C`=1)
#' x <- eulerr::euler(counts)
#' polygon_list <- eulerr_to_polygon_list(x)
#' 
#' # default is to label each polygon in its center
#' plot_polygon_list(polygon_list,
#'    col=colorjam::rainbowJam(length(polygon_list), alpha=0.5))
#' labelr_polygon_list(polygon_list, add_labels=TRUE)
#' 
#' # create unique polygons for each label
#' A_only <- minus_polygon_list(polygon_list, new_name="A_only");
#' B_only <- minus_polygon_list(polygon_list[c(2,1,3)], new_name="B_only");
#' C_only <- minus_polygon_list(polygon_list[c(3,1,2)], new_name="C_only");
#' 
#' plot_polygon_list(polygon_list,
#'    col=colorjam::rainbowJam(length(polygon_list), alpha=0.5))
#' ABC_only <- c(A_only, B_only, C_only);
#' polygon_list_labelr(ABC_only, add_labels=TRUE)
#' 
#' # label ABC intersection
#' ABC_int <- intersect_polygon_list(polygon_list[c(1,2,3)], new_name="ABC");
#' plot_polygon_list(ABC_int, add=TRUE, col="gold")
#' polygon_list_labelr(ABC_int, add_labels=TRUE)
#' 
#' # label AB intersection
#' AB_only <- minus_polygon_list(
#'    c(intersect_polygon_list(polygon_list[c(1,2)], new_name="BC_only"),
#'       polygon_list[3]))
#' plot_polygon_list(AB_only, add=TRUE, col="darkviolet")
#' polygon_list_labelr(AB_only, add_labels=TRUE, col="white")
#' 
#' # label BC intersection
#' BC_only <- minus_polygon_list(
#'    c(intersect_polygon_list(polygon_list[c(2,3)], new_name="BC_only"),
#'       polygon_list[1]))
#' plot_polygon_list(BC_only, add=TRUE, col="skyblue")
#' polygon_list_labelr(BC_only, add_labels=TRUE)
#' 
#' @export
labelr_polygon_list <- function
(polygon_list,
 add_labels=FALSE,
 ...)
{
   #
   if (all(c("x", "y") %in% names(polygon_list))) {
      # input is xy_list format
      xy_list <- polygon_list;
      polygon_list <- xy_list_to_polygon_list(xy_list);
   } else {
      # input is polygon_list
      xy_list <- polygon_list_to_xy_list(polygon_list);
   }
   
   polygon_seq <- jamba::nameVector(seq_along(polygon_list),
      names(polygon_list));
   xy_coords <- jamba::rbindList(lapply(polygon_seq, function(i){
      if (is.list(polygon_list[[i]]) &&
            !all(c("x", "y") %in% names(polygon_list[[i]]))) {
         # jamba::printDebug("i: ", i, ", list(xy_list)")
         union_poly <- union_polygon_list(polygon_list[[i]])
         if (length(union_poly) > 1) {
            union_poly <- get_largest_polygon_list(union_poly_areas)[[1]];
         }
         polylabelr::poi(union_poly)
      } else {
         # jamba::printDebug("i: ", i, ", xy_list")
         polylabelr::poi(polygon_list[[i]])
      }
   }))
   if (TRUE %in% add_labels) {
      text(x=xy_coords[, "x"],
         y=xy_coords[, "y"],
         labels=rownames(xy_coords),
         ...)
   }
}


#' Largest polygon in a polygon list
#' 
#' Largest polygon in a polygon list
#' 
#' This function returns the largest polygon in a polygon list,
#' intended when there are multiple polygons contained in one object.
#' 
#' If two polygons have identical area, the first
#' polygon is returned.
#' 
#' ## Todo:
#' 
#' * Verify correct output when polygon(s) have holes.
#' 
#' @family venndir polygons
#' 
#' @returns `list` with polygon coordinates `"x"` and `"y"`
#' 
#' @param polygon_list `list` with `"x"` and `"y"` elements with
#'    polygon coordinates.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' counts <- c(A=1, B=2, `A&B`=3, C=4)
#' x <- eulerr::euler(counts)
#' polygon_list <- eulerr_to_polygon_list(x)
#' largest_poly <- get_largest_polygon_list(polygon_list)
#' plot_polygon_list(polygon_list, col=colorjam::rainbowJam(3, alpha=0.5))
#' plot_polygon_list(largest_poly, add=TRUE, border="red", lwd=3)
#' 
#' 
#' @export
get_largest_polygon_list <- function
(polygon_list,
 ...)
{
   #
   if (all(c("x", "y") %in% names(polygon_list))) {
      # input is xy_list format
      xy_list <- polygon_list;
      polygon_list <- xy_list_to_polygon_list(xy_list);
   } else {
      # input is polygon_list
      xy_list <- polygon_list_to_xy_list(polygon_list);
   }
   
   # Subset multi-polygon entries to use the largest polygon
   poly_areas <- polygon_areas(polygon_list, simplify=TRUE)
   polygon_list <- polygon_list[which.max(poly_areas)]
   return(polygon_list);
}


#' Make polygon_list circles
#' 
#' Make polygon_list circles
#' 
#' This function creates one or more circles as polygon_list `list` objects.
#' 
#' @family venndir polygons
#' 
#' @return object `list` with a number of circles encoded as polygons.
#' 
#' @param xcenter,ycenter `numeric` vector that defines the x and y
#'    coordinate position of the center of each circle.
#' @param setnames `vector` that contains names for each circle, stored
#'    as `names()` for each polygon.
#'    When `setnames` is `NULL` then index numbers are used.
#' @param radius `numeric` vector that defines the radius of each circle.
#'    This `vector` is recycled to `length(xcenter)`.
#' @param n `integer` value indicating the number of subdivisions to
#'    use in the circle.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' polygon_list <- polygon_circles(c(3, 2), c(2, 3))
#' plot_polygon_list(polygon_list)
#' points(x=c(3, 2), y=c(2, 3), pch=c("1", "2"), add=TRUE);
#' 
#' @export
polygon_circles <- function
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
      setnames <- as.character(seq_along(xcenter));
   }
   xvals <- sin(angle_seq);
   yvals <- cos(angle_seq);
   if (length(radius) == 0) {
      radius <- 1;
   }
   radius <- rep(radius,
      length.out=length(xcenter));
   
   x_seq <- jamba::nameVector(seq_along(xcenter),
      setnames);
   polygon_list <- lapply(x_seq, function(i){
      list(
         x=xvals * radius[i] + xcenter[i],
         y=yvals * radius[i] + ycenter[i])
   })
   return(polygon_list);
}



#' Make polygon_list ellipses
#' 
#' Make polygon_list ellipses
#' 
#' This function creates one or more ellipses as polygon_list `list` objects.
#' 
#' @family venndir polygons
#' 
#' @return object `list` with a number of circles encoded as polygons.
#' 
#' @param xcenter,ycenter `numeric` vector that defines the x and y
#'    coordinate position of the center of each ellipse.
#' @param setnames `vector` that contains names for each circle, stored
#'    as `names()` for each polygon.
#'    When `setnames` is `NULL` then index numbers are used.
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
#' polygon_list <- polygon_ellipses(c(3, 2), c(2, 3),
#'    xradius=c(1, 4),
#'    yradius=c(5, 2))
#' plot_polygon_list(polygon_list, col=c("#FF000077", "#FFDD0077"));
#' points(x=c(3, 2), y=c(2, 3), pch=c("1", "2"), add=TRUE);
#' 
#' @export
polygon_ellipses <- function
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
      setnames <- as.character(seq_along(xcenter));
   }
   
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

   x_seq <- jamba::nameVector(seq_along(xcenter),
      setnames);
   polygon_list <- lapply(x_seq, function(i){
      i_xvals <- (xvals * xradius[i]);
      i_yvals <- (yvals * yradius[i]);
      e_xvals <- (i_xvals * cos(rotation_rad[i]) + i_yvals * sin(rotation_rad[i]));
      e_yvals <- (i_yvals * cos(rotation_rad[i]) - i_xvals * sin(rotation_rad[i]));
      list(
         x=e_xvals + xcenter[i],
         y=e_yvals + ycenter[i])
   })
   return(polygon_list);
}


#' Nudge polygon_list
#' 
#' Nudge polygon_list
#' 
#' This helper function is intended to take `list` polygon_list coordinates
#' and "nudge" (move by adding a scalar value to each coordinate)
#' only a subset of polygons identified by name.
#' 
#' @family venndir polygons
#' 
#' @return object `list` polygon_list object with `"x"` and `"y"` elements.
#' 
#' @param polygon_list `list` object with `"x"` and `"y"` elements.
#' @param nudge `list` whose names are found in `names(polygon_list)`,
#'    and whose values are `x` and `y` coordinates to be moved.
#' @param rotate_degrees `numeric` value in degrees (0, 360) to
#'    rotate the `polygon_list` object and all contained polygons.
#'    (Not yet implemented.)
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' D <- list(
#'    x=c(-3, 3, 3, 0, -3),
#'    y=c(-3, -3, 1.5, 4, 1.5))
#' E <- list(
#'    x=c(-3, 3, 3, -3),
#'    y=c(-3, -3, 3, 3))
#' DElist <- list(D=D, E=E, DE=list(D=D, E=E))
#' nudge <- list(D=c(x=0, y=10), E=c(x=0, y=-10), DE=c(x=10, y=0))
#' new_polygon_list <- nudge_polygon_list(polygon_list=DElist,
#'    nudge=nudge)
#' poly_colors <- colorjam::rainbowJam(3, alpha=0.5);
#' plot_polygon_list(DElist, col=poly_colors)
#' plot_polygon_list(new_polygon_list, col=poly_colors)
#' 
#' polygon_list <- polygon_ellipses(c(3, 2), c(2, 3),
#'    xradius=c(1, 4),
#'    yradius=c(5, 2))
#' plot_polygon_list(polygon_list,
#'    col=c("#FF000077", "#FFDD0000"),
#'    xlim=c(-2, 9));
#' polygon_list2 <- nudge_polygon_list(polygon_list,
#'    nudge=list(`2`=c(x=3, y=-2))
#' )
#' plot_polygon_list(polygon_list2,
#'    col=c("#FF000077", "#FFDD0077"),
#'    add=TRUE,
#'    xlim=c(-2, 9));
#' 
#' plot_polygon_list(polygon_list[2], border="blue", lty="dotted", lwd=3, add=TRUE);
#' plot_polygon_list(polygon_list2[2], border="blue", lty="dotted", lwd=3, add=TRUE);
#' arrows(x0=2, x1=5, y0=3, y1=1)
#' 
#' @export
nudge_polygon_list <- function
(polygon_list=NULL,
 nudge=NULL,
 ...)
{
   #
   if (length(polygon_list) == 0) {
      return(polygon_list);
   }
   if (all(c("x", "y") %in% names(polygon_list) && length(polygon_list) == 2)) {
      # input is xy_list format
      xy_list <- polygon_list;
      polygon_list <- xy_list_to_polygon_list(xy_list);
   } else {
      # input is polygon_list
      xy_list <- polygon_list_to_xy_list(polygon_list);
   }
   
   # Optionally nudge the polygon coordinates
   polygon_names <- names(polygon_list);

   ## shift by coordinates
   if (length(nudge) > 0 &&
         is.list(nudge) &&
         any(names(nudge) %in% polygon_names)) {
      use_nudge <- nudge[names(nudge) %in% polygon_names];
      for (i in seq_along(use_nudge)) {
         iname <- names(use_nudge)[i];
         j <- match(iname, polygon_names);
         i_nudge <- nudge[[i]];
         polygon_list[[j]] <- nudge_polygon_coords(
            polygon=polygon_list[[j]],
            nudge=nudge[[i]])
      }
   }
   
   return(invisible(polygon_list));
}

#' Nudge polygon coordinates
#' 
#' Nudge polygon coordinates
#' 
#' This function differs from `nudge_polygon_list()` in that all polygons
#' are nudged the exact same amount. If there are nested polygons, they
#' are iteratively all nudged the same.
#' 
#' @family venndir polygons
#' 
#' @examples
#' D <- list(
#'    x=c(-3, 3, 3, 0, -3),
#'    y=c(-3, -3, 1.5, 4, 1.5))
#' E <- list(
#'    x=c(-3, 3, 3, -3),
#'    y=c(-3, -3, 3, 3))
#' DElist <- list(D=D, E=E, DE=list(D=D, E=E))
#' nudge <- c(x=10, y=-10)
#' new_polygon_list <- nudge_polygon_coords(polygon_list=DElist, nudge=nudge)
#' plot_polygon_list(new_polygon_list)
#' 
#' @export
nudge_polygon_coords <- function
(polygon_list,
 nudge,
 ...)
{
   #
   if (length(nudge) == 1) {
      nudge <- unname(rep(nudge, length.out=2))
   }
   if (length(names(nudge)) == 0) {
      names(nudge) <- c("x", "y")
   }
   if (is.list(polygon_list) &&
         all(c("x", "y") %in% names(polygon_list))) {
      # simple list with "x","y"
      polygon_list$x <- polygon_list$x + nudge["x"];
      polygon_list$y <- polygon_list$y + nudge["y"];
   } else {
      # nested list
      polygon_list <- lapply(polygon_list, function(ipolygon_list){
         nudge_polygon_coords(polygon_list=ipolygon_list,
            nudge=nudge,
            ...)
      })
   }
   return(polygon_list);
}


#' Rescale a polygon_list object
#' 
#' Rescale a polygon_list object
#' 
#' This function simply applies `rescale_coordinates()` to an
#' `list` polygon_list object.
#' 
#' @family venndir polygons
#' 
#' @return object `list` polygon_list
#' 
#' @inheritParams rescale_coordinates
#' 
#' @param polygon_list `list` object
#' @param share_center `logical` indicating whether all polygons
#'    should share the same center, where `share_center=TRUE` will
#'    adjust everything collectively, and `share_center=FALSE` will
#'    adjust each polygon independently relative to its own center
#'    coordinate.
#' 
#' @examples
#' polygon_list <- polygon_ellipses(c(3, 2), c(2, 3),
#'    xradius=c(1, 4),
#'    yradius=c(5, 2))
#' polygon_list1 <- intersect_polygon_list(polygon_list);
#' polygon_list2 <- minus_polygon_list(polygon_list[1:2]);
#' polygon_list3 <- minus_polygon_list(polygon_list[2:1]);
#' polygon_list123 <- c(polygon_list1,
#'    polygon_list2,
#'    polygon_list3);
#' 
#' polygon_list123a <- rescale_polygon_list(polygon_list123,
#'    scale=c(1.5, 1.5),
#'    share_center=TRUE);
#' polygon_list123b <- rescale_polygon_list(polygon_list123,
#'    scale=c(1.5, 1.5));
#' col3 <- c("#FF000077", "#FFDD0077", "#0000DD77");
#' par("mfrow"=c(2, 2));
#' plot_polygon_list(polygon_list123,
#'    col=col3,
#'    main="original polygons",
#'    xlim=c(-10, 15), ylim=c(-5, 10));
#' axis(1, las=2); axis(2, las=2);
#' plot_polygon_list(polygon_list123a,
#'    col=col3,
#'    main="share_center=TRUE",
#'    xlim=c(-10, 15), ylim=c(-5, 10));
#' axis(1, las=2); axis(2, las=2);
#' plot_polygon_list(polygon_list123[1:2],
#'    col=col3[1:2],
#'    main="share_center=FALSE\nrescaling only the blue polygon",
#'    xlim=c(-10, 15), ylim=c(-5, 10));
#' axis(1, las=2); axis(2, las=2);
#' plot_polygon_list(polygon_list123b[3],
#'    col=col3[3],
#'    add=TRUE);
#' plot_polygon_list(polygon_list123[2:3],
#'    col=col3[2:3],
#'    main="share_center=FALSE\nrescaling only the red polygon",
#'    xlim=c(-10, 15), ylim=c(-5, 10));
#' axis(1, las=2); axis(2, las=2);
#' plot_polygon_list(polygon_list123b[1],
#'    col=col3[1],
#'    add=TRUE);
#' par("mfrow"=c(1, 1));
#' 
#' {par("mfrow"=c(2, 2));
#' plot_polygon_list(polygon_list123, col=col3,
#'    xlim=c(-4, 8), ylim=c(-4, 8))
#' title(main="Original polygons", line=0);
#' plot_polygon_list(rescale_polygon_list(polygon_list123, rotate_degrees=c(`11`=45, `12`=-10)), col=col3,
#'    xlim=c(-4, 8), ylim=c(-4, 8))
#' title(sub="yellow +45 degrees\nblue -10 degrees", line=0,
#'    main="share_polygon_center=TRUE (default)")
#' plot_polygon_list(rescale_polygon_list(polygon_list123, rotate_degrees=c(`11`=45, `12`=-10), share_polygon_center=FALSE), col=col3,
#'    xlim=c(-4, 8), ylim=c(-4, 8))
#' title(sub="yellow +45 degrees\nblue -10 degrees", line=0,
#'    main="share_polygon_center=FALSE\n(each polygon uses its center)")
#' plot_polygon_list(rescale_polygon_list(polygon_list123, rotate_degrees=c(`11`=45, `12`=-10), share_center=TRUE), col=col3,
#'    xlim=c(-4, 8), ylim=c(-4, 8))
#' title(sub="yellow +45 degrees\nblue -10 degrees", line=0,
#'    main="share_center=TRUE\n(all polygons share one global center)")
#' par("mfrow"=c(1, 1));}
#' 
#' 
#' @export
rescale_polygon_list <- function
(polygon_list,
 rotate_degrees=0,
 scale=c(1, 1),
 shift=c(0, 0),
 center=NULL,
 share_center=FALSE,
 share_polygon_center=TRUE,
 ...)
{
   ## SpatialPolygons
   if (length(center) == 0) {
      if (share_center) {
         center <- sapply(bbox_polygon_list(polygon_list), mean);
         center <- rep(list(center),
            length.out=length(polygon_list))
         names(center) <- names(polygon_list);
      } else if (share_polygon_center) {
         center <- lapply(polygon_list, function(i){
            sapply(bbox_polygon_list(i), mean)
         })
         share_center <- TRUE;
      }
   }
   if (is.atomic(center)) {
      center <- rep(list(center),
         length.out=length(polygon_list))
      names(center) <- names(polygon_list);
   }
   if (is.atomic(scale)) {
      scale <- rep(list(scale),
         length.out=length(polygon_list))
      names(scale) <- names(polygon_list);
   }
   if (is.atomic(shift)) {
      shift <- rep(list(shift),
         length.out=length(polygon_list))
      names(shift) <- names(polygon_list);
   }
   if (is.atomic(rotate_degrees)) {
      rotate_degrees <- rep(rotate_degrees,
         length.out=length(polygon_list))
      names(rotate_degrees) <- names(polygon_list);
   }
   
   poly_seq <- jamba::nameVector(seq_along(polygon_list),
      names(polygon_list));
   polygon_list <- lapply(poly_seq, function(i){
      if (is.list(polygon_list[[i]]) &&
            all(c("x", "y") %in% names(polygon_list[[i]]))) {
         xym <- rescale_coordinates(x=do.call(cbind, polygon_list[[i]]),
            scale=scale[[i]],
            rotate_degrees=rotate_degrees[[i]],
            shift=shift[[i]],
            center=center[[i]],
            ...)
         list(x=xym[, "x"],
            y=xym[, "y"])
      } else {
         rescale_polygon_list(polygon_list=polygon_list[[i]],
            scale=scale[[i]],
            rotate_degrees=rotate_degrees[[i]],
            shift=shift[[i]],
            center=center[[i]],
            ...)
      }
   })
   
   return(polygon_list);
}


#' Simple wrapper to polylabelr::poi() for polygon_list
#' 
#' @family venndir polygons
#' 
#' @returns `matrix` with nrow `length(polygon_list)` with x,y coordinates
#'    representing the visual center of each polygon in the list.
#' 
#' @param polygon_list `list` object
#' 
#' @examples
#' counts <- c(A=1, B=2, `A&B`=3, C=5, `B&C`=2, `A&C`=2, `A&B&C`=1)
#' x <- eulerr::euler(counts)
#' polygon_list <- eulerr_to_polygon_list(x)
#' 
#' # default is to label each polygon in its center
#' plot_polygon_list(polygon_list,
#'    col=colorjam::rainbowJam(length(polygon_list), alpha=0.5))
#' polygon_list_labelr(polygon_list, add_labels=TRUE)
#' 
#' # create unique polygons for each label
#' A_only <- minus_polygon_list(polygon_list, new_name="A_only");
#' B_only <- minus_polygon_list(polygon_list[c(2,1,3)], new_name="B_only");
#' C_only <- minus_polygon_list(polygon_list[c(3,1,2)], new_name="C_only");
#' 
#' plot_polygon_list(polygon_list,
#'    col=colorjam::rainbowJam(length(polygon_list), alpha=0.5))
#' ABC_only <- c(A_only, B_only, C_only);
#' polygon_list_labelr(ABC_only, add_labels=TRUE)
#' 
#' # label ABC intersection
#' ABC_int <- intersect_polygon_list(polygon_list[c(1,2,3)], new_name="ABC");
#' plot_polygon_list(ABC_int, add=TRUE, col="gold")
#' polygon_list_labelr(ABC_int, add_labels=TRUE)
#' 
#' # label AB intersection
#' AB_only <- minus_polygon_list(
#'    c(intersect_polygon_list(polygon_list[c(1,2)], new_name="BC_only"),
#'       polygon_list[3]))
#' plot_polygon_list(AB_only, add=TRUE, col="darkviolet")
#' polygon_list_labelr(AB_only, add_labels=TRUE, col="white")
#' 
#' # label BC intersection
#' BC_only <- minus_polygon_list(
#'    c(intersect_polygon_list(polygon_list[c(2,3)], new_name="BC_only"),
#'       polygon_list[1]))
#' plot_polygon_list(BC_only, add=TRUE, col="skyblue")
#' polygon_list_labelr(BC_only, add_labels=TRUE)
#' 
#' # test with fully overlapping polygon (to create a hole)
#' counts <- c(A=5, B=0, C=3, `A&B`=1)
#' x <- eulerr::euler(counts)
#' polygon_list <- eulerr_to_polygon_list(x)
#' plot_polygon_list(polygon_list[1:3], col=c("red"))
#' A_only <- minus_polygon_list(polygon_list[c(1, 2, 3)], new_name="A_only");
#' plot_polygon_list(A_only, col="gold", add=TRUE)
#' polygon_list_labelr(A_only, add_labels=TRUE)
#' 
#' polygon_list_labelr(c(A_only, polygon_list[2:3]), add_labels=TRUE)
#' @export
polygon_list_labelr <- function
(polygon_list,
 precision=1,
 add_labels=FALSE,
 ...)
{
   # validate input
   if (all(c("x", "y") %in% names(polygon_list))) {
      # input is xy_list format
      xy_list <- polygon_list;
      polygon_list <- xy_list_to_polygon_list(xy_list);
   } else {
      # input is polygon_list
      xy_list <- polygon_list_to_xy_list(polygon_list);
   }
   
   # iterate each polygon
   label_xy_list <- lapply(polygon_list, function(ixy){
      if (is.list(ixy) && !all(c("x", "y") %in% names(ixy))) {
         ixy <- get_largest_polygon_list(ixy)[[1]];
      }
      ixy <- do.call(cbind, ixy);
      # Todo: fallback plan using mean x- and y-ranges?
      polylabelr::poi(ixy,
         precision=precision)
   })
   # assemble into a matrix
   label_xy_dist <- jamba::rbindList(label_xy_list,
      newColnames=c("x", "y", "dist"))
   xy_coords <- label_xy_dist[, c("x", "y"), drop=FALSE];
   
   # optionally plot labels
   if (TRUE %in% add_labels) {
      text(x=xy_coords[, "x"],
         y=xy_coords[, "y"],
         labels=rownames(xy_coords),
         ...)
   }
   
   
   return(xy_coords);
}

#' Define label positions for JamPolygon using polylabelr::poi()
#' 
#' Define label positions for JamPolygon using polylabelr::poi()
#' 
#' This function is a simple wrapper for `polylabelr::poi()`, which
#' recognizes holes inside polygons.
#' 
#' ## Todo:
#' 
#' * Consider basic support for non-overlapping label positions.
#' 
#'    1. First pass might be to use non-overlapping regions of a polygon
#'    among a set of polygons.
#'    2. Second pass would be to use the region with fewest overlaps
#'    among other polygons in the set, to define the label position.
#'    This rule could serve to solve (1) as well.
#' 
#' @family JamPolygon
#' 
#' @returns `matrix` with columns `"x"`, `"y"`, `"dist"`
#' 
#' @param jp `JamPolygon`
#' @param precision `numeric` passed to `polylabelr::poi()`
#' @param add_to_jp `logical` indicating whether to add `"label_x"`,
#'    `"label_y"` into the `jp@polygons` `data.frame`.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' 
#' @export
labelr_JamPolygon <- function
(jp,
 precision=1,
 add_to_jp=FALSE,
 # add_labels=FALSE,
 ...)
{
   # validate input
   # - SKIPPED

   # iterate each polygon
   row_seq <- seq_len(nrow(jp@polygons));
   label_xy_list <- lapply(row_seq, function(irow){
      ix <- jp@polygons$x[[irow]];
      iy <- jp@polygons$y[[irow]];
      if (!is.list(ix)) {
         ix <- list(ix);
         iy <- list(iy);
      }
      plx <- head(unlist(lapply(seq_along(ix), function(j){
         c(ix[[j]], NA)
      })), -1)
      ply <- head(unlist(lapply(seq_along(ix), function(j){
         c(iy[[j]], NA)
      })), -1)
      if (length(plx) == 0) {
         return(list(x=NA, y=NA, dist=NA));
      }
      as.data.frame(
         polylabelr::poi(x=plx, y=ply, precision=1))
   })
   
   # assemble into a matrix
   label_xy_dist <- as.matrix(jamba::rbindList(label_xy_list,
      newColnames=c("x", "y", "dist")));
   rownames(label_xy_dist) <- names(jp);
   
   xy_coords <- label_xy_dist[, c("x", "y"), drop=FALSE];
   
   # optionally add into the JamPolygon data
   if (TRUE %in% add_to_jp) {
      jp@polygons[, c("label_x", "label_y")] <- xy_coords;
      return(jp);
   }

   return(xy_coords);
}
