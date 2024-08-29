#' Arrange text labels inside a polygon
#' 
#' Arrange text labels inside a polygon
#' 
#' This function is intended to define points inside a polygon area
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
#' lfj <- label_fill_JamPolygon(jp3[1,], labels=1:20)
#' plot(lfj$items_df[, c("x", "y")], cex=0)
#' text(lfj$items_df[, c("x", "y")], labels=lfj$items_df$text)
#' 
#' #test_x <- jp3[1,]@polygons$x[[1]];
#' #test_y <- jp3[1,]@polygons$y[[1]];
#' #P <- list(x=c(3.5, 4.5), y=c(3.5, 4.5))
#' #A <- lapply(seq_along(test_x), function(i){
#' #   list(x=test_x[[i]], y=test_y[[i]])})
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

   ###################################################################
   # item coordinates
   # note that it sometimes requires iterations with increasing
   # number of labels for the procedure to return at least
   # that many label positions

   label_sampled <- sample_JamPolygon(jp=jp,
      n=n,
      xyratio=xyratio,
      ...);

   label_xy <- do.call(cbind, label_sampled)

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
