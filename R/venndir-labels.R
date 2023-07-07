
#' Draw boundary around groups of gridtext labels
#' 
#' Draw boundary around groups of gridtext labels
#' 
#' This function is a helper function used to take individual
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
    verbose=FALSE)
   {
      # adapted from gridtext::richtext_grob() internals
      if (TRUE || verbose) {
         jamba::printDebug("draw_gridtext_groups(): ",
            "grob_group_roundrect(),  k:", k);
         print(head(gdf[k,,drop=FALSE]));
      }
      xmin_pt <- vapply(g_labels$children[k], function(i){
         min(i$xext)}, numeric(1))
      xmax_pt <- vapply(g_labels$children[k], function(i){
         max(i$xext)}, numeric(1))
      ymin_pt <- vapply(g_labels$children[k], function(i){
         min(i$yext)}, numeric(1))
      ymax_pt <- vapply(g_labels$children[k], function(i){
         max(i$yext)}, numeric(1))
      
      # 0.0.27.900 - TODO: this is a convenient place for labels to be
      # repositioned, since the height/width of each grob is defined here.
      # They could be laid out differently here, or based upon this code.
      
      if (!is.na(adjx[[1]]) && adjx[[1]] == 1) {
         # right-align the group of labels, so the box edge is on the right
         xmax <- grid::unit(gdf$x[k][1], "native");
         xmin <- grid::unit(gdf$x[k][1], "native") + 
               grid::unit(min(xmin_pt) - max(xmax_pt), "pt")
         # shift x to left
         for (k1 in k) {
            g_labels$children[[k1]]$x <- (
               g_labels$children[[k1]]$x -
                  grid::unit(max(xmax_pt), "pt"));
            g_labels$children[[k1]]$vp$x <- (
               g_labels$children[[k1]]$vp$x -
                  grid::unit(max(xmax_pt), "pt"));
         }
      } else if (!is.na(adjx[[1]]) && adjx[[1]] == 0) {
         # left-align the group of labels, so the box edge is on the left
         xmin <- grid::unit(gdf$x[k][1], "native");
         xmax <- grid::unit(gdf$x[k][1], "native") + 
            grid::unit(max(xmax_pt) - min(xmin_pt), "pt")
         # shift x to right
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
         xmin <- (grid::unit(gdf$x[k][1], "native") + 
               grid::unit(min(xmin_pt) - x_diff, "pt"))
         xmax <- (grid::unit(gdf$x[k][1], "native") + 
               grid::unit(max(xmax_pt) - x_diff, "pt"))
         # shift x to right
         for (k1 in k) {
            g_labels$children[[k1]]$x <- (
               g_labels$children[[k1]]$x -
                  grid::unit(x_diff, "pt"));
            g_labels$children[[k1]]$vp$x <- (
               g_labels$children[[k1]]$vp$x -
                  grid::unit(x_diff, "pt"));
         }
      } else {
         xmin <- min(grid::unit(gdf$x[k], "native") + 
               grid::unit(xmin_pt, "pt"))
         xmax <- max(grid::unit(gdf$x[k], "native") + 
               grid::unit(xmax_pt, "pt"))
      }
      if (!is.na(adjy[[1]]) && adjy[[1]] == 0) {
         # top align
         ymin <- grid::unit(gdf$y[k][1], "native");
         ymax <- grid::unit(gdf$y[k][1], "native") + 
               grid::unit(max(ymax_pt) - min(ymin_pt), "pt");
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
         ymin <- grid::unit(gdf$y[k][1], "native") + 
               grid::unit(min(ymin_pt) - max(ymax_pt), "pt");
         ymax <- grid::unit(gdf$y[k][1], "native");
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
         ymin <- (grid::unit(gdf$y[k][1], "native") + 
               grid::unit(min(ymin_pt) - y_diff, "pt"))
         ymax <- (grid::unit(gdf$y[k][1], "native") + 
               grid::unit(max(ymax_pt) - y_diff, "pt"))
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
         ymin <- min(grid::unit(gdf$y[k], "native") + 
               grid::unit(ymin_pt, "pt"))
         ymax <- max(grid::unit(gdf$y[k], "native") + 
               grid::unit(ymax_pt, "pt"))
      }
      #grid.points(x=unit.c(xmin, xmax), y=unit.c(ymin, ymax), gp=gpar(col="purple"))
      #grid.points(x=xmin, y=ymin, gp=gpar(col="blue"))
      #grid.points(x=xmax, y=ymax, gp=gpar(col="red"))
      ki <- k[which.max(gdf$r[k])]
      newgrob <- grid::roundrectGrob(x=xmin,
         y=ymin,
         width=xmax-xmin,
         height=ymax-ymin,
         r=grid::unit(gdf$r[ki],
            gdf$r_unit[ki]),
         gp=grid::gpar(
            col=gdf$border_col[ki],
            fill=gdf$box_fill[ki]),
         just=c("left", "bottom"))
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
   }
   if (verbose) {
      jamba::printDebug("draw_gridtext_groups(): ",
         "head(segment_df)");
      print(head(segment_df));
   }
   
   
   # iterate each group of labels and draw roundrect
   if (do_draw) {
      vps <- gridBase::baseViewports();
      grid::pushViewport(vps$inner, vps$figure, vps$plot);
      on.exit(grid::popViewport(3));
   }
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
   for (k in gdf_split) {
      # determine if any labels have a line segment
      kgroup <- gdf_group[k][1];
      rr_grob_l <- grob_group_roundrect(
         g_labels=g_labels,
         gdf=gdf,
         k=k,
         adjx=adj_df[kgroup,"adjx"],
         adjy=adj_df[kgroup,"adjy"],
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
      #grid::grid.draw(g_labels);
      #grid::popViewport(3);
   }
   return(invisible(list(grobs=rr_grobs,
      g_labels=g_labels)));
}
