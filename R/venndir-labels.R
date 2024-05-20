
#' Draw boundary around groups of gridtext labels
#' 
#' Draw boundary around groups of gridtext labels
#' 
#' This function is an internal function used to take individual
#' labels defined by `gridtext::richtext_grob()`, and creating
#' `grid::roundrectGrob()` objects that cover each group of
#' labels with one rounded rectangular boundary.
#' 
#' @family venndir label
#' 
#' @param g_labels `richtext_grob` class as returned by
#'    `gridtext::richtext_grob()`.
#' @param gdf `data.frame` with one row per `grob` in `g_labels`,
#'    with colnames `"x"`, `"y"`, `"overlap_set"`, `"r"`,
#'    `"r_unit"`, `"border_col"`, `"box_fill"`. The rows
#'    are grouped using `c("x", "y", "overlap_set")`.
#'    The `"r"` and `"r_unit"` values are used to define
#'    the radius and radius unit for the rounded corner.
#'    The row in `gdf` with the highest `"r"` value is the
#'    reference row, for the `"r"` radius, and the `"border_col"`
#'    and `"box_fill"` values.
#' @param segment_df `data.frame` with segment coordinates, optional.
#' @param adjust_center `logical` indicating whether to adjust the x-centering
#'    for a group of labels when `adjx=0.5`, using the total width of the
#'    group of labels. When FALSE (default) the label x-position is not
#'    adjusted, which assumes labels are already oriented properly with
#'    respect to the x-axis position.
#'    This option is `FALSE` specifically for two-column labels positioned
#'    around a fixed centerpoint, where one column could be wider than the
#'    other, and it would otherwise push the other column the other way.
#' @param do_draw `logical` indicating whether to draw the finished
#'    result in the context of the current open graphics device.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#' 
#' @export
draw_gridtext_groups <- function
(g_labels,
 gdf,
 segment_df=NULL,
 adjust_center=FALSE,
 do_draw=TRUE,
 verbose=FALSE,
 ...)
{
   if (length(gdf) == 0) {
      if (verbose) {
         jamba::printDebug("draw_gridtext_groups(): ",
            "length(gdf) == 0");
         print(gdf);
      }
      return(gdf);
   }
   
   # internal helper function
   grob_group_roundrect <- function
   (g_labels,
    gdf,
    k=seq_along(g_labels$children),
    adjx=NA,
    adjy=NA,
    # adjx_fn=c,
    # adjy_fn=c,
    adjust_center=FALSE,
    verbose=FALSE)
   {
      # adjust_centered=FALSE turns off adjustment for adj=0.5
      #
      # adapted from gridtext::richtext_grob() internals
      if (verbose) {
         jamba::printDebug("draw_gridtext_groups(): ",
            "grob_group_roundrect(),  k:", k);
         print(head(gdf[k,,drop=FALSE]));
      }
      # jamba::printDebug("g_labels$children[[1]]:");print(str(g_labels$children[[1]]));# debug
      # xext and yext are in "pt" units and describe
      # gridtext width and height, respectively
      # for each text component in a group of labels
      xmin_pt <- vapply(g_labels$children[k], function(i){
         min(i$xext)}, numeric(1))
      xmax_pt <- vapply(g_labels$children[k], function(i){
         max(i$xext)}, numeric(1))
      ymin_pt <- vapply(g_labels$children[k], function(i){
         min(i$yext)}, numeric(1))
      ymax_pt <- vapply(g_labels$children[k], function(i){
         max(i$yext)}, numeric(1))
      
      # Todo: this is a convenient place for labels to be
      # repositioned, since the height/width of each grob is defined here.
      # They could be laid out differently here, or based upon this code.
      use_unit <- "snpc";
      
      ## Todo: debug x/y coordinates
      # x_init should represent actual label position in "snpc" units
      # (and is identical to adjx_fn(gdf$x) when adjusted and stored as "snpc"
      # - it takes the first label coordinate, assumes all labels are
      #   relative to the same coordinate
      x_init <- (g_labels$children[k][[1]]$x);
      y_init <- (g_labels$children[k][[1]]$y);

      # grid::showViewport();# debug
      if (!is.na(adjx[[1]]) && adjx[[1]] == 1) {
         # right-align the group of labels, so the box edge is on the right
         xmax <- x_init;
         xmin <- x_init + grid::unit(min(xmin_pt) - max(xmax_pt), "pt");
         # xmax <- grid::unit(adjx_fn(gdf$x[k][1]), use_unit);
         # xmin <- grid::unit(adjx_fn(gdf$x[k][1]), use_unit) +
         #    grid::unit(min(xmin_pt) - max(xmax_pt), "pt")
         
         
         # adjust g_labels coordinated, shift x to left by the group width
         for (k1 in k) {
            g_labels$children[[k1]]$x <- (
               g_labels$children[[k1]]$x -
                  grid::unit(max(xmax_pt), "pt"));
            g_labels$children[[k1]]$vp$x <- (
               g_labels$children[[k1]]$vp$x -
                  grid::unit(max(xmax_pt), "pt"));
         }
         # jamba::printDebug("g_labels$children[[1]]$x:");print(g_labels$children[[1]]$x);# debug
      } else if (!is.na(adjx[[1]]) && adjx[[1]] == 0) {
         # left-align the group of labels, so the box edge is on the left
         xmax <- x_init + grid::unit(max(xmax_pt) - min(xmin_pt), "pt");
         xmin <- x_init;
         # xmin <- grid::unit(adjx_fn(gdf$x[k][1]), use_unit);
         # xmax <- grid::unit(adjx_fn(gdf$x[k][1]), use_unit) + 
         #    grid::unit(max(xmax_pt) - min(xmin_pt), "pt")
         
         # adjust g_labels coordinated, shift x to right by the group width
         for (k1 in k) {
            g_labels$children[[k1]]$x <- (
               g_labels$children[[k1]]$x -
                  grid::unit(min(xmin_pt), "pt"));
            g_labels$children[[k1]]$vp$x <- (
               g_labels$children[[k1]]$vp$x -
                  grid::unit(min(xmin_pt), "pt"));
         }
      } else if (!is.na(adjx[[1]]) && adjx[[1]] == 0.5) {
         # middle-align
         x_diff <- (min(xmin_pt) + max(xmax_pt)) / 2;
         if (adjust_center) {
            xmin <- x_init + grid::unit(min(xmin_pt) - x_diff, "pt");
            xmax <- x_init + grid::unit(max(xmax_pt) - x_diff, "pt");
         } else {
            xmin <- x_init + grid::unit(min(xmin_pt), "pt");
            xmax <- x_init + grid::unit(max(xmax_pt), "pt");
         }
         # xmin <- (grid::unit(adjx_fn(gdf$x[k][1]), use_unit) + 
         #       grid::unit(min(xmin_pt) - x_diff, "pt"))
         # xmax <- (grid::unit(adjx_fn(gdf$x[k][1]), use_unit) + 
         #       grid::unit(max(xmax_pt) - x_diff, "pt"))

         # shift x to right
         if (adjust_center) {
            for (k1 in k) {
               g_labels$children[[k1]]$x <- (
                  g_labels$children[[k1]]$x -
                     grid::unit(x_diff, "pt"));
               g_labels$children[[k1]]$vp$x <- (
                  g_labels$children[[k1]]$vp$x -
                     grid::unit(x_diff, "pt"));
            }
         }
      } else {
         xmin <- x_init + grid::unit(xmin_pt, "pt");
         xmax <- x_init + grid::unit(xmax_pt, "pt");
         # xmin <- min(grid::unit(adjx_fn(gdf$x[k]), use_unit) + 
         #       grid::unit(xmin_pt, "pt"))
         # xmax <- max(grid::unit(adjx_fn(gdf$x[k]), use_unit) + 
         #       grid::unit(xmax_pt, "pt"))
      }
      if (!is.na(adjy[[1]]) && adjy[[1]] == 0) {
         # top align
         ymin <- y_init;
         ymax <- y_init + grid::unit(max(ymax_pt) - min(ymin_pt), "pt");
         # ymin <- grid::unit(adjy_fn(gdf$y[k][1]), use_unit);
         # ymax <- grid::unit(adjy_fn(gdf$y[k][1]), use_unit) + 
         #       grid::unit(max(ymax_pt) - min(ymin_pt), "pt");
         # shift y down
         for (k1 in k) {
            g_labels$children[[k1]]$y <- (
               g_labels$children[[k1]]$y -
                  grid::unit(min(ymin_pt), "pt"));
            g_labels$children[[k1]]$vp$y <- (
               g_labels$children[[k1]]$vp$y -
                  grid::unit(min(ymin_pt), "pt"));
         }
      } else if (!is.na(adjy[[1]]) && adjy[[1]] == 1) {
         # bottom align
         ymin <- y_init + grid::unit(min(ymin_pt) - max(ymax_pt), "pt");
         ymax <- y_init;
         # ymin <- grid::unit(adjy_fn(gdf$y[k][1]), use_unit) + 
         #       grid::unit(min(ymin_pt) - max(ymax_pt), "pt");
         # ymax <- grid::unit(adjy_fn(gdf$y[k][1]), use_unit);
         # shift y up
         for (k1 in k) {
            g_labels$children[[k1]]$y <- (
               g_labels$children[[k1]]$y -
                  grid::unit(max(ymax_pt), "pt"));
            g_labels$children[[k1]]$vp$y <- (
               g_labels$children[[k1]]$vp$y -
                  grid::unit(max(ymax_pt), "pt"));
         }
      } else if (!is.na(adjy[[1]]) && adjy[[1]] == 0.5) {
         # middle height align
         y_diff <- (min(ymin_pt) + max(ymax_pt)) / 2;
         ymin <- y_init + grid::unit(min(ymin_pt) - y_diff, "pt");
         ymax <- y_init + grid::unit(max(ymax_pt) - y_diff, "pt");
         # ymin <- (grid::unit(adjy_fn(gdf$y[k][1]), use_unit) + 
         #       grid::unit(min(ymin_pt) - y_diff, "pt"))
         # ymax <- (grid::unit(adjy_fn(gdf$y[k][1]), use_unit) + 
         #       grid::unit(max(ymax_pt) - y_diff, "pt"))
         # shift y up
         for (k1 in k) {
            g_labels$children[[k1]]$y <- (
               g_labels$children[[k1]]$y -
                  grid::unit(y_diff, "pt"));
            g_labels$children[[k1]]$vp$y <- (
               g_labels$children[[k1]]$vp$y -
                  grid::unit(y_diff, "pt"));
         }
      } else {
         ymin <- y_init + grid::unit(ymin_pt, "pt");
         ymax <- y_init + grid::unit(ymax_pt, "pt");
         # ymin <- min(grid::unit(adjy_fn(gdf$y[k]), use_unit) + 
         #       grid::unit(ymin_pt, "pt"))
         # ymax <- max(grid::unit(adjy_fn(gdf$y[k]), use_unit) + 
         #       grid::unit(ymax_pt, "pt"))
      }
      ki <- k[which.max(gdf$r[k])]
      newgrob <- grid::roundrectGrob(x=xmin,
         y=ymin,
         default.units=use_unit,
         width=xmax-xmin,
         height=ymax-ymin,
         r=grid::unit(gdf$r[ki],
            gdf$r_unit[ki]),
         gp=grid::gpar(
            col=gdf$border_col[ki],
            fill=gdf$box_fill[ki]),
         just=c("left", "bottom"))
      # adjusted labels are returned
      return(list(newgrob=newgrob,
         g_labels=g_labels))
   }

   # determine if any labels have a line segment
   if (length(segment_df) > 0) {
      segment_df$label_group <- paste0(segment_df$group, "_",
         round(segment_df$x, digits=3), "_",
         round(segment_df$y, digits=3),
         ifelse(segment_df$point_order == 2,
            "_2", ""))
      if (verbose) {
         jamba::printDebug("draw_gridtext_groups(): ",
            "head(segment_df)");
         print(head(segment_df));
      }
   }
   
   
   # iterate each group of labels and draw roundrect
   gdf$num <- seq_len(nrow(gdf));
   if (verbose) {
      jamba::printDebug("draw_gridtext_groups(): ",
         "gdf:");
      print(gdf);
   }
   gdf_group <- paste0(gdf$overlap_set, "_",
      round(gdf$x, digits=3), "_",
      round(gdf$y, digits=3));
   gdf_split <- split(gdf$num, gdf_group);
   
   # text adjustment when line segments are drawn to the label
   adj_df <- jamba::rbindList(lapply(gdf_split, function(k){
      # determine if any labels have a line segment
      if (length(segment_df) > 0 && gdf_group[k][1] %in% segment_df$label_group) {
         si <- match(gdf_group[k][1], segment_df$label_group)
         line_degree <- jamba::rad2deg(
            atan2(y=diff(segment_df$y[si+c(1,0)]),
               x=diff(segment_df$x[si+c(1,0)])))
         degrees_to_adj(line_degree)
      } else {
         data.frame(adjx=0.5, adjy=0.5)
      }
   }));
   rownames(adj_df) <- names(gdf_split);

   rr_grobs <- list();
   #rr_grobs <- lapply(split(gdf$num, gdf_group), function(k){
   for (ki in seq_along(gdf_split)) {
      k <- gdf_split[[ki]];
      # determine if any labels have a line segment
      kgroup <- gdf_group[k][1];
      # jamba::printDebug("adj_df[kgroup, ]:");print(adj_df[kgroup, ]);
      rr_grob_l <- grob_group_roundrect(
         g_labels=g_labels,
         gdf=gdf,
         k=k,
         adjx=adj_df[kgroup,"adjx"],
         adjy=adj_df[kgroup,"adjy"],
         adjust_center=adjust_center,
         # adjx_fn=adjx,
         # adjy_fn=adjy,
         verbose=verbose);
      rr_grobs <- c(rr_grobs,
         list(rr_grob_l$newgrob));
      g_labels <- rr_grob_l$g_labels;
      if (do_draw) {
         grid::grid.draw(rr_grob_l$newgrob);
      }
   }
   names(rr_grobs) <- names(gdf_split);
   if (do_draw) {
      # grid::grid.draw(g_labels);
      # grid::popViewport(3);
   }
   return(invisible(list(grobs=rr_grobs,
      g_labels=g_labels)));
}
