

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
#' xo@polygons$outerborder <- jamba::makeColorDarker(darkFactor=1.2,
#'    xo@polygons$venn_color)
#' xo@polygons$outerborder.lwd <- 4;
#' plot(xo);
#' 
#' testlist <- list(set_A=LETTERS, set_B=LETTERS[1:10], set_C=LETTERS[7:11])
#' so <- subset(signed_overlaps(testlist), count > 0)
#' test_counts <- setNames(so$count, so$sets)
#' x <- eulerr::euler(test_counts)
#' jp1 <- eulerr_to_JamPolygon(x)
#' jp1@polygons$fill <- polygon_colors;
#' plot(jp1)
#' xo <- find_venn_overlaps_JamPolygon(jp=jp1, venn_counts=test_counts, verbose=TRUE)
#' xo@polygons$outerborder <- jamba::makeColorDarker(darkFactor=1.2,
#'    xo@polygons$venn_color)
#' xo@polygons$outerborder.lwd <- 4;
#' plot(xo);
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
      ellUse@polygons$outerborder <- NA;
      ellUse@polygons$outerborder.lwd <- 1;
      ellUse@polygons$innerborder <- border;
      ellUse@polygons$innerborder.lwd <- 1;
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
   # venn_poly_coords <- do.call(rbind2, venn_poly_coords);
   # jamba::printDebug("venn_poly_coords:");print(venn_poly_coords);# debug
   venn_poly_coords <- rbind2.JamPolygon(venn_poly_coords);
   # Note venn_poly_coords is JamPolygon
   venn_poly_coords@polygons$name <- rownames(el1);
   rownames(venn_poly_coords@polygons) <- names(venn_poly_coords);

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

   venn_poly_coords@polygons$label <- names(venn_poly_coords);
   
   # Define label position for each polygon
   label_xy <- labelr_JamPolygon(venn_poly_coords);
   venn_poly_coords@polygons[, c("label_x", "label_y")] <- as.data.frame(
      label_xy);

   return(invisible(venn_poly_coords));

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
#' df3 <- data.frame(name=c("polygon1", "polygon2"),
#'    label=c("polygon1", "polygon2"),
#'    x=I(list(
#'       list(c(1, 6, 6, 1),
#'          c(2, 5, 5, 2),
#'          c(3, 4, 4, 3)),
#'       list(
#'          c(12, 15, 15, 12) - 7.5,
#'          c(13, 14, 14, 13) - 7.5)
#'    )),
#'    y=I(list(
#'       list(c(1, 1, 6, 6),
#'          c(2, 2, 5, 5),
#'          c(3, 3, 4, 4)),
#'       list(
#'          c(2, 2, 5, 5),
#'          c(3, 3, 4, 4))
#'    )),
#'    fill=jamba::alpha2col(c("gold", "firebrick"), alpha=0.7))
#' jp3 <- new("JamPolygon", polygons=df3);
#' plot(jp3);
#' 
#' jp3i <- intersect_JamPolygon(jp3)
#' jp3i@polygons$fill <- "red"
#' jp3i@polygons$border <- "red3";
#' jp3i@polygons$border.lwd <- 3;
#' jp3c <- rbind2(jp3, jp3i)
#' plot(jp3c)
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
      usex <- Ax[[i]];
      usey <- Ay[[i]];
      keepxy <- (!is.na(usex) & !is.na(usey))
      list(x=usex[keepxy], y=usey[keepxy])
   })
   
   # handle empty input polygon
   if (0 %in% length(unlist(A))) {
      jp <- jp[1, ];
      newx <- list(NULL);
      newy <- list(NULL);
      jp@polygons$x <- I(newx);
      jp@polygons$y <- I(newy);
      if (length(new_name) == 1) {
         jp@polygons$name <- new_name;
      }
      return(jp)
   }
   
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
      if (length(unlist(A)) == 0 || 0 %in% head(lengths(A), 1)) {
         break;
      }
   }
   
   jp <- jp[1, ];
   if (length(unlist(A)) == 0 || 0 %in% head(lengths(A), 1)) {
      newx <- list(NULL);
      newy <- list(NULL);
   } else {
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
   }
   jp@polygons$x <- I(newx);
   jp@polygons$y <- I(newy);
   if (length(new_name) == 1) {
      jp@polygons$name <- new_name;
   }
   return(jp);
}







#' Make polygon_list circles
#' 
#' Make polygon_list circles
#' 
#' This function creates one or more circles as polygon_list `list` objects.
#' 
#' @family JamPolygon
#' 
#' @returns `JamPolygon` object
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
#' circle_jp <- polygon_circles(c(3, 2), c(2, 3))
#' plot(circle_jp, fill=c("red", "gold"))
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
   cjp <- rbind2.JamPolygon(lapply(polygon_list, function(i){
      polyclip_to_JamPolygon(A=list(i), ...)
   }))
   names(cjp) <- setnames;
   rownames(cjp@polygons) <- setnames;
   return(cjp);
}



#' Make polygon_list ellipses
#' 
#' Make polygon_list ellipses
#' 
#' This function creates one or more ellipses as polygon_list `list` objects.
#' 
#' @family JamPolygon
#' 
#' @returns `JamPolygon` object
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
#' ejp <- polygon_ellipses(c(3, 2), c(2, 3),
#'    xradius=c(1, 4),
#'    yradius=c(5, 2))
#' plot(ejp, fill=c("#FF000077", "#FFDD0077"))
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
      e_xvals <- (i_xvals * cos(rotation_rad[i]) +
            i_yvals * sin(rotation_rad[i]));
      e_yvals <- (i_yvals * cos(rotation_rad[i]) -
            i_xvals * sin(rotation_rad[i]));
      list(
         x=e_xvals + xcenter[i],
         y=e_yvals + ycenter[i])
   })
   cjp <- rbind2.JamPolygon(lapply(polygon_list, function(i){
      polyclip_to_JamPolygon(A=list(i), ...)
   }))
   names(cjp) <- setnames;
   rownames(cjp@polygons) <- setnames;
   return(cjp);
}


#' Nudge JamPolygon coordinates
#' 
#' Nudge JamPolygon coordinates
#' 
#' Polygon coordinates within a `JamPolygon` object are nudged by name
#' or polygon number, such that all parts of each polygon are adjusted
#' together. For multi-part polygons, and/or polygons with internal holes,
#' all parts are moved the identical amount.
#' 
#' @family JamPolygon
#' 
#' @param jp `JamPolygon` object
#' @param nudge `list` whose names match `names(jp)`, containing `numeric`
#'    vector with coordinates x and y. Alternatively, an atomic `numeric`
#'    vector with length=2, which will be applied to the x,y coordinates
#'    for all polygons in `jp`.
#'    An example of list input:
#'    `nudge=list(polyname1=c(1, 0))`
#' @param rotate_degrees `numeric` optional rotation in degrees,
#'    named using `names(jp)`.
#'    When a single unnamed value is provided, it is applied to all polygons
#'    in `jp`.
#'    When a `numeric` vector is provided with `length(jp)`, it is applied
#'    in order to each polygon in `jp`.
#'    Note: 90 degrees will rotate clockwise
#'    (top the right, bottom to the left) by 1/4 turn.
#' @param center `numeric` coordinates with optional center position.
#'    Default `NULL` will use the center of the bounding box of `jp`.
#'    All parts of the polygon, and all polygons, use the same `center`.
#' @param scale `numeric` optional scalar to enlarge or shrink the polygon,
#'    named using `names(jp)`. Alternatively, if a single unnamed value
#'    is provided, it is applied to all polygons in `jp`.
#'    If a `numeric` vector is provided with `length(jp)`, it is applied
#'    in order to each polygon in `jp`.
#'    The scale is applied relative to the `center` as provided or calculated.
#' @param ... additional arguments are ignored
#' 
#' @examples
#' DEdf <- data.frame(check.names=FALSE,
#'    name=c("D", "E"),
#'    x=I(list(
#'       c(-3, 3, 3, 0, -3),
#'       c(-4, 2, 2, -4))),
#'    y=I(list(
#'       c(-3, -3, 1.5, 4, 1.5),
#'       c(-2, -2, 4, 4))),
#'    fill=c("#FFD70055", "#B2222255"))
#' DEjp <- new("JamPolygon", polygons=DEdf)
#' plot(DEjp)
#' nudge <- list(D=c(7, 1), E=c(-1, -1));
#' DEjp_nudged <- nudge_JamPolygon(DEjp, nudge=nudge)
#' plot(DEjp_nudged)
#' 
#' # plot the difference
#' plot_jpdiff <- function(a, b) {
#'   fillb <- jamba::alpha2col(alpha=0.9, b@polygons$fill);
#'   filla <- jamba::alpha2col(alpha=0.3, fillb)
#'   plot(rbind2(a, b),
#'     fill=c(filla, fillb),
#'     label=c(paste(a@polygons$name, "old"),
#'       paste(b@polygons$name, "new")),
#'     border.lty=rep(c(2, 1), c(length(a), length(b))))
#' }
#' plot_jpdiff(DEjp, DEjp_nudged)
#' 
#' # rotate, nudge, and scale
#' DEjp_rotated <- nudge_JamPolygon(DEjp,
#'    rotate_degrees=c(E=45),
#'    scale=c(E=0.7),
#'    nudge=list(E=c(5, 0), D=c(-1, -4)))
#' plot_jpdiff(DEjp, DEjp_rotated)
#' 
#' @export
nudge_JamPolygon <- function
(jp,
 nudge=NULL,
 rotate_degrees=0,
 center=NULL,
 scale=c(1, 1),
 verbose=FALSE,
 ...)
{
   #
   if (length(jp) == 0) {
      return(jp)
   }
   # validate rotate_degrees
   if (length(rotate_degrees) == 0) {
      rotate_degrees <- 0;
   }
   rotate_degrees <- rotate_degrees %% 360;
   if (length(names(rotate_degrees)) > 0) {
      if (!all(names(rotate_degrees) %in% names(jp))) {
         stop("names(rotate_degrees) does not match names(jp)")
      }
      use_rotate_degrees <- rep(0, length(jp));
      names(use_rotate_degrees) <- names(jp);
      use_rotate_degrees[names(rotate_degrees)] <- rotate_degrees;
      rotate_degrees <- use_rotate_degrees;
   } else {
      if (length(unique(rotate_degrees)) == 1) {
         rotate_degrees <- rep(rotate_degrees, length.out=length(jp))
         names(rotate_degrees) <- names(jp);
      } else if (length(rotate_degrees) == length(jp)) {
         names(rotate_degrees) <- names(jp);
      } else {
         stop("length(rotate_degrees) must be length=1 or equal length(jp)")
      }
   }

   # validate scale
   if (length(scale) == 0) {
      scale <- 1;
   }
   if (any(scale <= 0)) {
      stop("scale must be greater than 0.")
   }
   if (length(names(scale)) > 0) {
      if (!all(names(scale) %in% names(jp))) {
         stop("names(scale) does not match names(jp)")
      }
      use_scale <- rep(1, length(jp));
      names(use_scale) <- names(jp);
      use_scale[names(scale)] <- scale;
      scale <- use_scale;
   } else {
      if (length(unique(scale)) == 1) {
         scale <- rep(scale, length.out=length(jp))
         names(scale) <- names(jp);
      } else if (length(scale) == length(jp)) {
         names(scale) <- names(jp);
      } else {
         stop("length(scale) must be length=1 or equal length(jp)")
      }
   }
   
   # exit early if no values need to be changed
   if ((length(nudge) == 0 || all(unlist(nudge) %in% 0)) &&
      all(rotate_degrees %in% 0) &&
      all(scale %in% 1)) {
      # return jp unchanged
      return(jp)
   }
   # validate nudge
   if (length(jp) == 1) {
      if (is.atomic(nudge) && length(nudge) == 2) {
         nudge <- rep(list(nudge), length(jp));
         names(nudge) <- names(jp);
      }
      if (is.list(nudge) && length(names(nudge)) == 0) {
         if (length(nudge) == 1) {
            nudge <- rep(nudge, length(jp));
            names(nudge) <- names(jp);
         } else if (length(nudge) == length(jp)) {
            names(nudge) <- names(jp)
         } else {
            stop("length(nudge) must be length=1 or equal length(jp)")
         }
      }
   }
   
   ## Define center if necessary
   if (!length(center) == 2) {
      use_bbox <- bbox_JamPolygon(jp);
      center <- c(x=mean(use_bbox[1, 1:2]),
         y=mean(use_bbox[2, 1:2]));
   }
   
   ## rotate polygons
   if (any(rotate_degrees != 0) || any(scale != 1)) {
      for (irow in seq_len(length(jp))) {
         jpname <- names(jp)[irow];
         use_rotate <- rotate_degrees[jpname];
         use_scale <- scale[jpname];
         if (use_rotate %in% 0 && use_scale %in% 1) {
            next;
         }
         use_x <- jp@polygons$x[[irow]];
         use_y <- jp@polygons$y[[irow]];
         if (!is.list(use_x)) {
            use_x <- list(use_x);
            use_y <- list(use_y);
         }
         use_x_v <- unlist(use_x);
         use_y_v <- unlist(use_y);
         if (length(names(use_x)) > 0) {
            use_split <- rep(jamba::makeNames(names(use_x)),
               lengths(use_x));
            use_split <- factor(use_split,
               levels=jamba::makeNames(names(use_x)))
         } else {
            use_split <- rep(as.character(seq_along(use_x)),
               lengths(use_x));
            use_split <- factor(use_split,
               levels=as.character(seq_along(use_x)))
         }
         new_xy <- rescale_coordinates(
            x=cbind(x=use_x_v, y=use_y_v),
            rotate_degrees=use_rotate,
            scale=c(1, 1) * use_scale,
            center=center)
         new_x_v <- new_xy[, 1];
         new_y_v <- new_xy[, 2];
         new_x <- split(new_x_v, use_split);
         new_y <- split(new_y_v, use_split);
         if (length(names(use_x)) == 0) {
            new_x <- unname(new_x);
            new_y <- unname(new_y);
         }
         if (!is.list(use_x)) {
            new_x <- unname(new_x[[1]]);
            new_y <- unname(new_y[[1]]);
         }
         jp@polygons$x[[irow]] <- new_x;
         jp@polygons$y[[irow]] <- new_y;
      }
   }
   
   ## nudge polygons by name
   if (length(nudge) > 0) {
      if (length(names(nudge)) == 0) {
         stop("There must be names(nudge).")
      }
      if (length(names(nudge)) == 0 && !any(names(nudge) %in% names(jp))) {
         # check for numeric names
         nudge_names <- as.integer(names(nudge));
         use_nudge <- (!is.na(nudge_names) &
               nudge_names == as.numeric(names(nudge)) &
               nudge_names %in% seq_along(jp))
         if (!all(use_nudge)) {
            stop("names(nudge) must match names(jp) or seq_along(jp)")
         }
         nudge_names <- names(jp)[nudge_names]
      } else {
         nudge_names <- intersect(names(nudge), names(jp))
      }
      
      # custom function to apply nudge to nested numeric list
      apply_nudge <- function(i, offset) {
         if (is.list(i)) {
            lapply(i, function(j){
               apply_nudge(j, offset)
            })
         } else {
            i + offset
         }
      }
      for (nudge_name in nudge_names) {
         n <- match(nudge_name, names(jp));
         if (all(c("x", "y") %in% names(nudge[[nudge_name]]))) {
            nudge_x <- nudge[[nudge_name]][["x"]];
            nudge_y <- nudge[[nudge_name]][["y"]];
         } else {
            nudge_x <- nudge[[nudge_name]][[1]];
            nudge_y <- nudge[[nudge_name]][[2]];
         }
         if (verbose) {
            jamba::printDebug("nudge_JamPolygon(): ",
               "applying nudge (", c(nudge_x, nudge_y), ") ",
               "to '", nudge_name, "'");
         }
         if (!all(nudge_x %in% c(NA, 0))) {
            old_x <- jp@polygons$x[n];
            new_x <- apply_nudge(jp[nudge_name, ]@polygons$x, offset=nudge_x)
            jp@polygons$x[n] <- new_x;
         } else {
            new_x <- jp[nudge_name, ]@polygons$x;
         }
         if (!all(nudge_y %in% c(NA, 0))) {
            old_y <- jp@polygons$y[n];
            new_y <- apply_nudge(jp[nudge_name, ]@polygons$y, offset=nudge_y)
            jp@polygons$y[n] <- new_y;
         } else {
            new_y <- jp[nudge_name, ]@polygons$y;
         }
      }
   }
   return(jp);
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
#' @param precision `numeric` passed to `polylabelr::poi()`, default 1.
#' @param add_to_jp `logical` default FALSE, indicating whether to
#'    add `"label_x"`,`"label_y"` into `jp@polygons` for persistence.
#' @param subset_multipart `logical` default TRUE, whether to subset
#'    multipart polygons to label the largest area.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' df3 <- data.frame(name=c("polygon1", "polygon2"),
#' label=c("polygon A", "polygon B"),
#' x=I(list(
#'    list(c(1, 6, 6, 1),
#'       c(2, 5, 5, 2),
#'       c(3, 4, 4, 3)),
#'    list(
#'       c(12, 15, 15, 12) - 3,
#'       c(13, 14, 14, 13) - 3)
#' )),
#' y=I(list(
#'    list(c(1, 1, 6, 6),
#'       c(2, 2, 5, 5),
#'       c(3, 3, 4, 4)),
#'    list(
#'       c(2, 2, 5, 5),
#'       c(3, 3, 4, 4))
#' )),
#' fill=jamba::alpha2col(c("gold", "firebrick"), alpha=0.7))
#' jp3 <- new("JamPolygon", polygons=df3);
#' plot(jp3);
#'
#' @export
labelr_JamPolygon <- function
(jp,
 precision=1,
 add_to_jp=FALSE,
 subset_multipart=TRUE,
 ...)
{
   # validate input
   # - SKIPPED

   # iterate each polygon
   row_seq <- seq_len(nrow(jp@polygons));
   label_xy_list <- lapply(row_seq, function(irow){
      # extract coordinates
      ix <- jp@polygons$x[[irow]];
      iy <- jp@polygons$y[[irow]];
      if (!is.list(ix)) {
         ix <- list(ix);
         iy <- list(iy);
      }
      # check multi-part polygons
      iarea <- area_JamPolygon(jp[irow, ], return_list=TRUE)[[1]];
      if (length(iarea) > 1) {
         iori <- add_orientation_JamPolygon(jp[irow, ], include_parent=TRUE)
         ioridf <- data.frame(
            num=seq_along(iarea),
            area=iarea,
            orientation=unlist(iori@polygons$orientation),
            polygon_parent=unlist(iori@polygons$polygon_parent))
         # for multi-part polygons, use the parent polygon with largest
         # net area, accounting for holes
         if (TRUE %in% subset_multipart &&
               length(unique(ioridf$polygon_parent)) > 1) {
            iareas <- sapply(split(ioridf$area, ioridf$polygon_parent), sum)
            ioridf$parent_area <- jamba::rmNA(naValue=0,
               iareas[as.character(ioridf$polygon_parent)]);
            # jamba::printDebug("ioridf:");print(ioridf);# debug
            keeporidf <- subset(ioridf, parent_area %in% max(iareas))
            keeppolys <- keeporidf$num;
            ix <- ix[keeppolys]
            iy <- iy[keeppolys]
         }
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
