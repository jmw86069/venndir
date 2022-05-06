
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
#' `label_method="hexagon"` and `degrees=20`, since hexagonal
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
#' @param ref_sp object `sp::SpatialPolygons` used as a reference to
#'    compare the size of `sp` when `apply_n_scale=TRUE`. In general,
#'    fewer labels are placed more toward the center; also in general,
#'    this effect is applied less for smaller polygons.
#' @param fontsize `numeric` value indicating the font size in points.
#' @param cex `numeric` value used to resize all text labels by
#'    multiplying the font size.
#' @param degrees `numeric` `vector` indicating the angle in degrees
#'    to rotate each text label, where positive values rotate
#'    in clockwise direction.
#' @param dither_cex `numeric` or `NULL`, where a numeric value
#'    is applied to `cex` in random fashion to provide some
#'    visual heterogeneity in the `cex` for item labels. When
#'    `dither_cex=0` or `dither_cex=NULL` then no adjustment
#'    is performed.
#' @param dither_color `numeric` or `NULL`, where a numeric value
#'    is use to adjust each `color` slightly lighter or darker
#'    via `jamba::makeColorDarker()`. The effect is to make adjacent
#'    labels visibly different but in a subtle way.
#' @param dither_degrees `numeric` or `NULL`, where a numeric value
#'    is used to adjust the text angle slightly more or less that
#'    the `degrees` value.
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
#' @param seed `numeric` or `NULL`, where a `numeric` value is
#'    passed to `set.seed()` to make the `dither_cex` process reproducible.
#'    Set `seed=NULL` to disable this step.
#' @param plot_style `character` string indicating the expected output
#'    plot style: `"base"` uses base R graphics, `gridtext::richtext_grob()`;
#'    `"gg"` uses `ggplot2` style; `"none"` does not plot anything.
#' @param verbose `logical` indicating whether to print verbose output.
#' 
#' @return `list` that contains: `items_df` as a `data.frame` of item
#'    label coordinates; and `g_labels` as output from
#'    `gridtext::richtext_grob()` whose coordinates are defined
#'    as `"native"`, or `g_labels=NULL` when `plot_style="gg"`;
#'    `scale_width` with the `numeric` value used; and
#'    `sp_buffer` with the `sp::SpatialPolygons` object representing
#'    the buffer region used for item labels.
#' 
#' @family venndir label
#' 
#' @examples
#' sp <- sp_ellipses(3, 3, xradius=1.2, yradius=3, rotation_degrees=15)
#' words <- jamba::unvigrep("[0-9]",
#'    jamba::vigrep("[a-zA-Z]", 
#'       unique(unlist(
#'       strsplit(as.character(packageDescription("venndir")),
#'       '[", _()<>:;/\n\t.@&=]+')))));
#' words <- words[nchar(words) > 2];
#' plot(sp, col="gold", border="gold4", lwd=2);
#' polygon_label_fill(sp=sp,
#'    degrees=-10,
#'    labels=words,
#'    dither_color=0.2,
#'    color="red2",
#'    cex=1.2)
#' 
#' plot(sp, col="gold", border="gold4", lwd=2);
#' polygon_label_fill(sp=sp,
#'    degrees=0,
#'    draw_buffer=FALSE,
#'    layout_degrees=45/2,
#'    buffer_w=0.4,
#'    label_method="regular",
#'    labels=jamba::mixedSort(words),
#'    dither_color=0.2,
#'    dither_cex=0.2,
#'    dither_degrees=0,
#'    color="red2",
#'    cex=1.2)
#' 
#' # iterate various options for reducing label overlap
#' par("mfrow"=c(2, 4));
#' for (lm in c("hexagonal", "regular")) {
#' for (ld in c(0, -45/2, -30, -45)) {
#' plot(sp, col="gold", border="gold4", lwd=2);
#' id <- ifelse(ld == 0, -20,
#'    ifelse(ld == -45, 15, 0));
#' polygon_label_fill(sp=sp,
#'    degrees=id,
#'    layout_degrees=ld,
#'    buffer_w=0.4,
#'    label_method=lm,
#'    #labels=seq_along(words),
#'    #labels=rep("word", length(words)),
#'    labels=paste0("word", seq_along(words)),
#'    dither_color=0,
#'    dither_cex=0,
#'    dither_degrees=0,
#'    color="navy",
#'    cex=0.7)
#' title(main=paste0("layout_degrees=",
#'    format(ld, digits=2),
#'    "\nlabel_method='", lm, "'",
#'    "\ndegrees=", id));
#' }
#' }
#' par("mfrow"=c(1, 1));
#' 
#' plot(sp, col="gold", border="gold4", lwd=2);
#' polygon_label_fill(sp=sp,
#'    degrees=-10,
#'    scale_width=-0.3,
#'    draw_buffer=TRUE,
#'    labels=words, dither_color=0.2, color="red2", cex=1.2)
#' 
#' setlist <- make_venn_test(100, 3);
#' vo <- venndir(setlist, return_items=TRUE, font_cex=0.01, proportional=FALSE);
#' 
#' # labels inside each venn overlap polygon
#' venn_spdf <- vo$venn_spdf;
#' label_df <- vo$label_df;
#' for (i in seq_len(nrow(venn_spdf))) {
#'    j <- match(venn_spdf$label[i], label_df$overlap_set);
#'    if (length(unlist(label_df[j,"items"])) > 0) {
#'    polygon_label_fill(sp=venn_spdf[i,],
#'       ref_sp=venn_spdf,
#'       color=venn_spdf$border[i],
#'       scale_width=-0.1,
#'       draw_buffer=TRUE,
#'       labels=unlist(label_df[j,"items"]));
#'    }
#' }
#' 
#' # same example as above but using proportional circles
#' vo <- venndir(setlist, font_cex=0.01, proportional=TRUE);
#' 
#' # labels inside each venn overlap polygon
#' venn_spdf <- vo$venn_spdf;
#' label_df <- vo$label_df;
#' for (i in seq_len(nrow(venn_spdf))) {
#'    j <- match(venn_spdf$label[i], label_df$overlap_set);
#'    if (length(unlist(label_df[j,"items"])) > 0) {
#'    polygon_label_fill(sp=venn_spdf[i,],
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
polygon_label_fill <- function
(sp,
 labels,
 color="black",
 border=NA,
 ref_sp=NULL,
 fontsize=10,
 cex=1,
 degrees=0,
 dither_cex=0.04,
 dither_color=0.07,
 dither_degrees=0,
 scale_width=-0.15,
 apply_n_scale=TRUE,
 buffer_w=0,
 buffer_h=0,
 label_method=c("hexagonal",
    "nonaligned",
    "regular",
    "random",
    "stratified",
    "clustered"),
 layout_degrees=-20,
 draw_buffer=FALSE,
 buffer_fill="#FFFFFF77",
 buffer_border="red",
 draw_points=FALSE,
 draw_labels=TRUE,
 seed=NULL,
 plot_style=c("base", "gg", "none"),
 verbose=FALSE,
 ...)
{
   ##
   label_method <- match.arg(label_method);
   plot_style <- match.arg(plot_style);
   if (length(seed) == 1) {
      set.seed(seed);
   }
   n <- length(labels);
   if (n == 0) {
      return(NULL);
   }
   
   ## expand vectors to the number of labels
   color <- rep(color, length.out=n);
   border <- rep(border, length.out=n);
   cex <- rep(cex, length.out=n);
   if (length(dither_cex) > 0 & all(dither_cex != 0)) {
      #cex <- rnorm(n) * dither_cex * cex + dither_cex/3 + cex;
      cex <- runif(n, min=-dither_cex, max=dither_cex) * cex + dither_cex/3 + cex;
   }
   degrees <- rep(degrees, length.out=n);
   
   if (length(dither_degrees) > 0 & all(dither_degrees != 0)) {
      #cex <- rnorm(n) * dither_cex * cex + dither_cex/3 + cex;
      degrees <- runif(n,
         min=-dither_degrees,
         max=dither_degrees) + degrees;
   }
   
   # optionally dither colors for slight visual distinction
   if (length(dither_color) == 1 & dither_color > 0) {
      df <- runif(n, min=-dither_color, max=dither_color);
      sf <- (abs(df)*2 + 1) * sign(df);
      df <- (abs(df) + 1) * sign(df) 
      color <- jamba::makeColorDarker(color,
         darkFactor=df,
         sFactor=sf)
   }
   
   ## resize polygon before applying labels
   #ellYesDiffsmall11 <- shrinkPolygon(ellYesDiffsmall11,
   #   scale=c(labelPolyScale[j]*parPinRatio, labelPolyScale[j]));
   if (length(scale_width) == 0) {
      scale_width <- -0.001;
   }
   
   ## apply additional scaling based upon n
   if (length(apply_n_scale) > 0 && apply_n_scale) {
      # sp_pct is an adjustment for smaller polygons
      # which reduces the effect of n on the n_scale
      # starting at about 1/3 polygon area to total area
      if (length(ref_sp) > 0) {
         sp_pct1 <- head(sp_percent_area(rbind(sp, ref_sp)), 1) / 100;
         sp_pct <- jamba::noiseFloor(sp_pct1 * 3, ceiling=1);
      } else {
         sp_pct <- 1;
      }
      n_scale <- 1 - (1 / (sqrt(n)*2)) * sp_pct;
      if (verbose) {
         jamba::printDebug("polygon_label_fill(): ",
            "n_scale:",
            format(digits=4, n_scale));
         jamba::printDebug("polygon_label_fill(): ",
            "scale_width (before):",
            format(digits=4, scale_width));
      }
      scale_width <- (scale_width + 1) * (1 - (1 - n_scale) * 1.1) - 1;
   }
   
   ## Apply polygon buffer
   if (verbose) {
      jamba::printDebug("polygon_label_fill(): ",
         "scale_width  (after):",
         format(digits=4, scale_width));
   }
   if (scale_width != 0) {
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
      scale_width <- 0;
   }
   
   buffer_ws <- unique(c(0, buffer_w * c(-1, -0.5, 0.5, 1)));
   buffer_hs <- unique(c(0, buffer_h * c(-1, -0.5, 0.5, 1)));
   if (verbose) {
      jamba::printDebug("polygon_label_fill(): ",
         "buffer_ws:",
         format(digits=2, buffer_ws));
      jamba::printDebug("polygon_label_fill(): ",
         "buffer_hs:",
         format(digits=2, buffer_hs));
   }
   for (w1 in buffer_ws) {
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
   
   
   ###################################################################
   # item coordinates
   # note that it sometimes requires iterations with increasing
   # number of labels for the procedure to return at least
   # that many label positions
   get_poly_points <- function
   (sp,
      n,
      type,
      iter,
      offset=c(0.5, 0.5),
      tries=100,
      singlet_polylabelr=TRUE,
      rotate_degrees=0,
      ...)
   {
      if (n == 0) {
         return(NULL);
      }
      if (n == 1 && singlet_polylabelr) {
         i_sp <- get_largest_polygon(sp);
         ixy <- i_sp@polygons[[1]]@Polygons[[1]]@coords;
         pt_xy <- sp_polylabelr(sp);
         pt_m <- as.matrix(as.data.frame(pt_xy[c("x","y")]));
         spt <- sp::SpatialPoints(pt_m);
         if (rgeos::gContains(sp, spt)) {
            return(pt_m[,1:2,drop=FALSE]);
         }
      }
      # optionally rotate sp before finding points
      # then un-rotate the points back
      if (length(rotate_degrees) != 1) {
         rotate_degrees <- 0;
      }
      if (rotate_degrees != 0) {
         sp_center <- rowMeans(sp::bbox(sp));
         sp <- rescale_sp(sp=sp,
            rotate_degrees=rotate_degrees,
            center=sp_center);
      }
      
      try_step <- ceiling(n/200);
      for (k in (seq_len(tries)-1)*try_step) {
         if (n + k == 1 && "hexagonal" %in% type) {
            type <- "regular";
         }
         for (offset_i in seq(from=0, to=0.99, by=0.2)) {
            spts <- tryCatch({
               spts <- sp::spsample(sp,
                  n=n + k,
                  type=type,
                  offset=(offset + offset_i) %% 1,
                  iter=iter);
            }, error=function(e){
               NULL;
            });
            if (length(spts) >= n) {
               # optionally un-rotate points
               if (rotate_degrees != 0) {
                  label_xy <- rescale_coordinates(spts@coords,
                     center=sp_center,
                     rotate_degrees=-rotate_degrees);
               } else {
                  label_xy <- sp::coordinates(spts);
               }
               # sort coordinates top to bottom
               label_xy <- jamba::mixedSortDF(label_xy,
                  byCols=c(-2, 1));
               label_xy <- head(label_xy, n);
               return(label_xy);
            }
         }
      }
      stop(paste0("spsample failed to return ", n, " points, ", nrow(label_xy),
         ", k:", k));
   }
   
   label_xy <- tryCatch({
      get_poly_points(sp_buffer,
         n,
         type=label_method,
         iter=50,
         rotate_degrees=layout_degrees,
         singlet_polylabelr=TRUE,
         ...);
   }, error=function(e){
      print("Error in get_poly_points()");
      print(e);
      NULL;
   });
   if (length(label_xy) == 0) {
      plot(sp_buffer, add=TRUE, color="green");
      print(n);
      print(label_method);
      label_xy <- sp::coordinates(sp_buffer);
   }
   
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
      rot=degrees,
      color=color,
      fontsize=fontsize * cex,
      border=border,
      stringsAsFactors=FALSE);
   
   # define text label grob
   g_labels <- NULL;
   if ("base" %in% plot_style) {
      g_labels <- gridtext::richtext_grob(
         x=label_xy[,1],
         y=label_xy[,2],
         text=labels,
         rot=-degrees,
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
   }
   
   return(invisible(
      list(
         items_df=items_df,
         g_labels=g_labels,
         scale_width=scale_width,
         sp_buffer=sp_buffer)));
}
