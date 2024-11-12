#' Arrange text labels inside a polygon
#' 
#' Arrange one or more text labels to fit inside a polygon.
#' 
#' This function is intended to define points inside a polygon area
#' so that text labels can be roughly evenly spaced, with relatively
#' good default positioning to minimize overlapping labels.
#' This function does not prevent overlapping labels, nor does it
#' fully prevent labels overlapping the polygon border.
#' 
#' There are options to help minimize overlapping labels, such as `xyratio`
#' which defines the default width-to-height ratio of resulting points.
#' For wider text labels, a higher value for `xyratio` may be helpful.
#' To minimize adjacent (side-by-side) label overlaps, it can be helpful
#' to use `degrees` to rotate labels slightly, for example `degrees=5`
#' may be enough to prevent wide labels from overlapping the next label
#' beside it.
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
#' ## Todo
#' 
#' * Modify options that plot the result so they work together with
#' `plot.JamPolygon()`.
#' 
#' @family JamPolygon
#' 
#' @returns `list` when there are valid coordinates, `NULL` otherwise.
#'    The `list` contains these elements:
#'    * `"items_df"`: a `data.frame` with columns
#'    `x,y,text,rot,color,fontsize,border`
#'    * `"g_labels"`: a grid `grob` graphical object only when
#'    `plot_style="base"`, otherwise `NULL`
#'    * `"scale_width"`: a `numeric` value indicating the `buffer` used
#'    * `"jp_buffer"`: a `JamPolygon` object after adjusting with `buffer`
#'
#' @param jp `JamPolygon` where only the first row is processed.
#' @param labels `character` vector of labels
#' @param buffer `numeric` value (default `-0.15`) buffer to resize the
#'    polygon, where negative values shrink the polygon size relative
#'    to the size of the polygon. For example `-0.9` will reduce the polygon
#'    90% of the way toward a completely empty polygon, where that range
#'    is defined by the width inside the polygon border.
#' @param relative `logical` passed to `buffer_JamPolygon()` (default `TRUE`)
#'    to define `buffer` with relative coordinates.
#' @param color `character` string to define the color of resulting labels.
#' @param border `character` string used to define an optional item border,
#'    which is only useful when `gridtext` is used to visualize labels.
#' @param ref_jp `JamPolygon` (optional) used only when `apply_n_scale=TRUE`,
#'    used to define the overall plot range before determining whether
#'    the internal area of `jp` should be reduced before arraying item
#'    label coordinates.
#'    The general idea is that polygons which are a smaller percentage
#'    of the total area should not be reduced as much by `apply_n_scale`
#'    because they have limited area, but larger polygons should receive
#'    closer to the full `apply_n_scale` adjustment.
#' @param xyratio `numeric` value indicating the relative ratio of x (width)
#'    to y (height) when arraying coordinates inside the polygons.
#'    Values larger than `1` will place points wider from each other than
#'    they are tall, which can help with longer text labels.
#' @param fontsize `numeric` font size in points, default `10`.
#' @param cex `numeric` multiplied by `fontsize` as an alternative convenient
#'    way to adjust text label sizes.
#' @param degrees `numeric` value used to rotate labels, default `0`,
#'    in degrees between 0 and 359.
#' @param `dither_cex,dither_color,dither_degrees` values used to provide
#'    some variability to the repeated pattern of text labels.
#'    * `dither_cex` provides a range to adjust `cex` and `fontsize` slightly
#'    for each label, making some slightly larger or smaller to help
#'    distinguish adjacent labels from one another.
#'    * `dither_color` provides a range for adjusting the font color, which
#'    is applied with `darkFactor` in `jamba::makeColorDarker()`.
#'    * `dither_degrees` provides a range for adjusting font `degrees`,
#'    the default `0` means the values are not adjusted. The `text()` function
#'    does not permit multiple vectorized rotations, however `gridtext`
#'    does permit multiple angles. Best to use `dither_degrees` only
#'    when displaying labels with `gridtext`.
#' @param apply_n_scale `logical` indicating whether to adjust the buffer
#'    based upon the number of items, where more items uses less buffer,
#'    and fewer items imposes a larger buffer.
#'    The intent is for a single label or small number of labels to
#'    appear near the center.
#' @param label_method `character` string (default "hexagonal") to define
#'    the label layout. Currently only `"hexagonal"` is implemented.
#'    After testing some approaches in `sf::st_sample()` and
#'    `spatstat.random::rThomas()`, the other options were interesting
#'    but ultimately not more effective specifically for character labels.
#'    Both packages have heavy R dependencies and were avoided.
#' @param draw_labels `logical` (default `TRUE`) indicating whether to
#'    draw labels, however it is only used when `plot_style="base"`.
#' @param seed `numeric` value (default `1`) used to define the random seed
#'    with `set.seed(seed)` for reproducible output.
#'    When `seed` is `NULL` there is no call to `set.seed()`.
#' @param plot_style `character` string (deprecated) to define the
#'    plot output, used only when `draw_labels=TRUE`.
#'    Currently, labels are only rendered for `plot_style="base"`.
#'    * `"base"`: Use R base plotting, using `gridBase` with `gridtext`.
#'    * `"gg"`: Use `ggplot2` style plotting.
#'    * `"none"`: No plot output.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to internal functions:
#'    `buffer_JamPolygon()`, and `sample_JamPolygon()`.
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
#' lfj12 <- label_fill_JamPolygon(jp3, labels=1:20)
#' plot(lfj$items_df[, c("x", "y")], cex=0)
#' text(lfj$items_df[, c("x", "y")], labels=lfj$items_df$text)
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
 apply_n_scale=TRUE,
 label_method=c("hexagonal"),
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
   ## Check for empty coordinates
   if (length(jamba::rmNA(unlist(jp@polygons$x))) == 0) {
      return(NULL)
   }

   ## expand vectors to the number of labels
   color <- rep(color, length.out=n);
   border <- rep(border, length.out=n);
   cex <- rep(cex, length.out=n);
   # jamba::printDebug("label_fill_JamPolygon(): ", "cex: ", cex);# debug
   if (length(dither_cex) > 0 & all(dither_cex != 0)) {
      #cex <- rnorm(n) * dither_cex * cex + dither_cex/3 + cex;
      cex <- runif(n, min=-dither_cex, max=dither_cex) * cex + dither_cex/3 + cex;
   }
   # jamba::printDebug("label_fill_JamPolygon(): ", "cex: ", cex);# debug
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
   ## Check for empty coordinates
   if (length(jamba::rmNA(unlist(jp@polygons$x))) == 0) {
      return(NULL)
   }
   
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
      if (TRUE %in% draw_labels) {
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
