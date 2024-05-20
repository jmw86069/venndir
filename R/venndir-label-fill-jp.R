#' Arrange text labels inside a polygon
#' 
#' Arrange text labels inside a polygon
#' 
#' This function is modeled after `sp::spsample()` which is no longer
#' available. It is intended to define points inside a polygon area
#' which are evenly spaced, used to place text labels also inside
#' the polygon. There are limited options to define a buffer region
#' so that labels do not overlap the polygon boundary.
#' 
#' ## Strategy
#' 
#' * Determine bounding box with rectangular area that encompases the polygon.
#' * Define evenly spaced points across the rectangular area sufficient
#' to produce at least `n` total points.
#' * Retain only the subset of points which are inside the polygon.
#' * If there are fewer than `n` remaining points, repeat the process
#' using a higher target value for `n`.
#' 
#' Challenges:
#' 
#' * `polyclip::pointinpolygon()` does not appear to handle polygons with
#' holes. If not, then we have to use logic like: "overlaps any solid polygon,
#' does not overlap any polygon hole".
#' 
#' Options:
#' 
#' * Consider applying a buffer with fixed width inside the original
#' polygon, so that points have at least this minimum width to the
#' polygon border.
#' * Bonus points for applying the buffer more to the width than height,
#' since text labels are typically wider than they are tall.
#' 
#' @family JamPolygon
#'
#' @param jp `JamPolygon` where only the first row is processed.
#' @param labels `character` vector of labels
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
#' label_fill_JamPolygon(jp3[1,], labels=1:20)
#' test_x <- jp3[1,]@polygons$x[[1]];
#' test_y <- jp3[1,]@polygons$y[[1]];
#' P <- list(x=c(3.5, 4.5), y=c(3.5, 4.5))
#' A <- lapply(seq_along(test_x), function(i){
#'    list(x=test_x[[i]], y=test_y[[i]])})
#' 
#' @export
label_fill_JamPolygon <- function
(jp,
 labels,
 buffer=-0.15,
 relative=TRUE,
 color="black",
 border=NA,
 ref_jp=NULL,
 xyratio=1.1,
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
 label_method=c("hexagonal"),
 layout_degrees=-20,
 draw_buffer=FALSE,
 buffer_fill="#FFFFFF77",
 buffer_border="red",
 draw_points=FALSE,
 draw_labels=TRUE,
 seed=1,
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
   if (length(jp) == 0) {
      return(NULL)
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
         min=-abs(dither_degrees),
         max=abs(dither_degrees)) + degrees;
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
   
   # adjust buffer based upon number of labels 
   if (length(apply_n_scale) > 0 && TRUE %in% apply_n_scale) {
      if (length(ref_jp) > 0) {
         jp_area <- sum(area_JamPolygon(jp));
         ref_jp_area <- sum(area_JamPolygon(union_JamPolygon(ref_jp)));
         jp_pct1 <- jp_area / ref_jp_area;
         jp_pct <- jamba::noiseFloor(jp_pct1 * 3, ceiling=1);
      } else {
         jp_pct <- 1;
      }
      # n_scale <- 1 - (1 / (sqrt(n)*2)) * jp_pct;
      n_scale <- 1 - (1 / (sqrt(n) * 1.5)) * jp_pct;# 0.0.30.900
      # apply to buffer
      # scale_width <- (scale_width + 1) * (1 - (1 - n_scale) * 1.1) - 1;
      new_buffer <- (buffer + 1) * (1 - (1 - n_scale) * 1.1) - 1;
      buffer <- new_buffer;
   }
   
   # Todo: apply resizing to contract the polygon before arraying points
   #
   if (length(buffer) > 0 && buffer != 0) {
      jp <- buffer_JamPolygon(jp,
         buffer=buffer,
         relative=relative,
         ...)
   }
   #
   
   if (FALSE) {
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
         # try(
         #    sp::plot(sp_buffer,
         #       add=TRUE,
         #       col=buffer_fill,
         #       border=buffer_border,
         #       lwd=2,
         #       lty="dotted")
         # )
      }
      ## gArea(ellYesDiffDis[i]);
   }
   
   
   ###################################################################
   # item coordinates
   # note that it sometimes requires iterations with increasing
   # number of labels for the procedure to return at least
   # that many label positions

   label_sampled <- sample_JamPolygon(jp=jp,
      n=n,
      xyratio=xyratio);

   label_xy <- do.call(cbind, label_sampled)
   if (FALSE) {
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
         # jamba::printDebug("sp:");print(sp);# debug
         # jamba::printDebug("sp_buffer:");print(sp_buffer);# debug
         NULL;
      });
      if (length(label_xy) == 0) {
         plot(sp_buffer, add=TRUE, color="green");
         print(n);
         print(label_method);
         label_xy <- sp::coordinates(sp_buffer);
      }
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
   # jamba::printDebug("items_df:");print(items_df);
   
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
         scale_width=buffer,
         jp_buffer=jp)));
}
