
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
#' @param groupdf `data.frame` with information about label groupings,
#'    with columns: overlap_set, counttype, type, location, roworder,
#'    label_df_rowname, name, childName, gdf_group.
#'    Importantly, it must be in identical row order as `gdf`.
#' @param segment_df `data.frame` with segment coordinates, optional.
#' @param realign `logical` experimental feature (now default) indicating
#'    whether to realign grouped labels, thus ignoring the hjust/vjust
#'    for each label, instead arranging the labels using `groupdf`.
#'    In principle, overlap label (set name) should be top-center,
#'    and counts should be centered below. When there are main counts
#'    and signed counts, they should be in two columns, collectively
#'    centered below the main label.
#' @param adjust_center `logical` indicating whether to adjust the x-centering
#'    for a group of labels when `adjx=0.5`, using the total width of the
#'    group of labels. When FALSE (default) the label x-position is not
#'    adjusted, which assumes labels are already oriented properly with
#'    respect to the x-axis position.
#'    This option is `FALSE` specifically for two-column labels positioned
#'    around a fixed centerpoint, where one column could be wider than the
#'    other, and it would otherwise push the other column the other way.
#' @param template `character` string (default "wide") with experimental
#'    layout templates for count and signed labels.
#'    * `"wide"` - original strategy with signed counts beside main counts.
#'    * `"tall"` - new strategy with main counts and signed counts in
#'    the same column.
#' @param do_draw `logical` indicating whether to draw the finished
#'    result in the context of the current open graphics device.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#' 
#' @export
draw_gridtext_groups <- function
(g_labels,
 gdf,
 groupdf=NULL,
 segment_df=NULL,
 realign=TRUE,
 adjust_center=FALSE,
 template=c("wide",
    "tall"),
 adjx_fn=c,
 adjy_fn=c,
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
   template <- match.arg(template);
   
   # internal helper function
   grob_group_roundrect <- function
   (g_labels,
    gdf,
    groupdf,
    k=seq_along(g_labels$children),
    adjx=NA,
    adjy=NA,
    realign=TRUE,
    segment_df=NULL,
    adjx_fn=c,
    adjy_fn=c,
    adjust_center=FALSE,
    template=c("wide", "tall"),
    verbose=FALSE)
   {
      # adjust_centered=FALSE turns off adjustment for adj=0.5
      template <- match.arg(template);
      #
      # adapted from gridtext::richtext_grob() internals
      # if (all(c(1,2,3) %in% k)) { verbose <- TRUE }
      if (verbose > 1) {
         jamba::printDebug("draw_gridtext_groups(): ",
            "grob_group_roundrect(),  k:", k);
         print(head(gdf[k,,drop=FALSE]));
      }
      
      korig <- k;
      if (length(groupdf) > 0) {
         use_groupdf <- groupdf[k, , drop=FALSE];
         k_childnames <- use_groupdf$childName;
         k <- match(k_childnames, grid::childNames(g_labels));
         if (verbose > 1) {
            jamba::printDebug("draw_gridtext_groups(): ",
               "grob_group_roundrect(), use_groupdf:");print(use_groupdf);# debug
         }
      }
      
      ## Experimental realign strategy
      if (TRUE %in% realign && length(groupdf) > 0) {
         # check if main count label exists
         # jamba::printDebug("use_groupdf:");print(use_groupdf);# debug
         is_main_count <- (grepl("count", use_groupdf$type) &
               grepl("main", use_groupdf$counttype));
         is_signed_count <- (grepl("count", use_groupdf$type) &
               grepl("signed", use_groupdf$counttype));
         is_overlap_label <- (grepl("overlap", use_groupdf$type));
         if (verbose > 1) {
            jamba::printDebug("draw_gridtext_groups(): ",
               "grob_group_roundrect(), params:");
            print(data.frame(is_main_count, is_signed_count, is_overlap_label));# debug
         }
         
         use_segment_df <- head(segment_df, 0);
         if ("label_group" %in% colnames(segment_df)) {
            use_segment_df <- subset(segment_df,
               label_group %in% use_groupdf$gdf_group)
         }
         # jamba::printDebug("realignment");# debug
         # jamba::printDebug("use_groupdf:");print(use_groupdf);# debug
         
         ## Steps:
         # - arrange signed count labels top to bottom: grobs_stack()
         #    - left-align: grobs_xalign()
         #    - make into a gTree
         # - assemble main count labels top to bottom if more than one
         #    - right-align: grobs_xalign()
         #    - make into a gTree
         # - assemble main and signed count labels side-by-side
         #    - center by height: grobs_yalign()
         #    - make into a gTree
         # - assemble overlap and count labels top to bottom: grobs_stack()
         #    - center-align: grobs_xalign()
         
         g_labels_cns <- use_groupdf$childName;
         # jamba::printDebug("g_labels_cns:");print(g_labels_cns);# debug
         use_vp <- NULL;
         gdfbasename <- head(gdf[korig, "gdf_group"], 1);
         ## signed counts
         if (any(is_signed_count)) {
            signed_cns <- jamba::nameVector(g_labels_cns[is_signed_count]);
            signed_grobs <- lapply(signed_cns, function(icn){
               grid::getGrob(g_labels, icn)
            })
            if (length(signed_grobs[[1]]$vp) > 0) {
               use_vp <- signed_grobs[[1]]$vp
            }
            # stack then left-align
            use_xalign <- "center";
            if ("wide" %in% template) {
               use_xalign <- "left";
            }
            gt_signed <- grobs_xalign(xalign=use_xalign,
               grobs_stack(signed_grobs,
                  name=paste0(gdfbasename, ".signed")))
         }
         ## main counts
         if (any(is_main_count)) {
            main_cns <- jamba::nameVector(g_labels_cns[is_main_count]);
            main_grobs <- lapply(main_cns, function(icn){
               grid::getGrob(g_labels, icn)
            })
            if (length(main_grobs[[1]]$vp) > 0) {
               use_vp <- main_grobs[[1]]$vp
            }
            # stack then center-align,
            # or right-align if there are also signed counts
            use_xalign <- "center"
            if (any(is_signed_count) && "wide" %in% template) {
               use_xalign <- "right"
            }
            gt_main <- grobs_xalign(xalign=use_xalign,
               grobs_stack(main_grobs,
                  name=paste0(gdfbasename, ".main")))
         }
         ## combine count labels if needed
         gt_signed_count <- NULL;
         if (any(is_main_count) && any(is_signed_count)) {
            if ("wide" %in% template) {
               # use_yalign <- "top";
               use_yalign <- "middle";
               gt_signed_count <- grobs_yalign(yalign=use_yalign,
                  grobs_tile(list(gt_main, gt_signed),
                     name=paste0(gdfbasename, ".main_signed")))
            } else if ("tall" %in% template) {
               use_xalign <- "center";
               gt_signed_count <- grobs_xalign(xalign=use_xalign,
                  grobs_stack(list(gt_main, gt_signed),
                     name=paste0(gdfbasename, ".main_signed")))
            }
         } else if (any(is_main_count)) {
            gt_signed_count <- gt_main;
         } else if (any(is_signed_count)) {
            gt_signed_count <- gt_signed;
         }
         ## overlap label
         if (any(is_overlap_label)) {
            overlap_cns <- jamba::nameVector(g_labels_cns[is_overlap_label]);
            overlap_grobs <- lapply(overlap_cns, function(icn){
               grid::getGrob(g_labels, icn)
            })
            if (length(overlap_grobs[[1]]$vp) > 0) {
               use_vp <- overlap_grobs[[1]]$vp
            }
            # stack then center-align
            # or right-align if there are also signed counts
            gt_overlap <- grobs_xalign(xalign="center",
               grobs_stack(overlap_grobs,
                  name=paste0(gdfbasename, ".overlap")))
         }
         ## Put it together
         if (any(is_overlap_label)) {
            if (length(gt_signed_count) > 0) {
               gt_final <- grobs_xalign(xalign="center",
                  grobs_stack(list(gt_overlap, gt_signed_count),
                     name=paste0(gdfbasename, ".overlap_count")))
            } else {
               gt_final <- gt_overlap
            }
         } else if (length(gt_signed_count) > 0) {
            gt_final <- gt_signed_count
         } else {
            return(NULL)
         }
         
         ## orient labels relative to the segment endpoint
         ## or for internal labels, center on the label point
         use_xalign <- "center";
         use_yalign <- "middle";
         if (length(use_segment_df) > 0 && nrow(use_segment_df) > 0) {
            if (adjx == 0) { use_xalign <- "left"; }
            if (adjx == 1) { use_xalign <- "right"; }
            if (adjy == 0) { use_yalign <- "bottom"; }
            if (adjy == 1) { use_yalign <- "top"; }
            use_x <- grid::unit(adjx_fn(use_segment_df$x), "snpc");
            use_y <- grid::unit(adjy_fn(use_segment_df$y), "snpc");
         } else {
            ## Center on the original label coordinate
            use_x <- grid::unit(adjx_fn(head(gdf$x[korig], 1)), "snpc");
            use_y <- grid::unit(adjy_fn(head(gdf$y[korig], 1)), "snpc");
         }
         # jamba::printDebug("adjx:", adjx, ", adjy:", adjy);# debug
         if (verbose) jamba::printDebug("use_xalign:", use_xalign, ", use_yalign:", use_yalign);# debug
         # jamba::printDebug("adjy_fn(use_segment_df$y):");print(adjy_fn(use_segment_df$y));
         # ge1 <- grobs_exts(gt_final);
         # jamba::printDebug("ge1:");print(ge1);
         # newgrob
         gt_final2 <- grobs_xalign(xalign=use_xalign,
            use_x=use_x,
            grobs_yalign(yalign=use_yalign,
               verbose=verbose,
               use_y=use_y,
               list(gt_final)))
         gt_final <- gt_final2;
         if (verbose > 1) {
            jamba::printDebug("str(gt_final):");
            print(str(gt_final, max.level=3));# debug
         }
         
         ## Now create roundrectGrob to bind them
         scm_exts <- grobs_exts(gt_final)
         if (verbose) {
            # jamba::printDebug("scm_exts:");
            # print(str(scm_exts, max.level=5));# debug
            jamba::printDebug("length(scm_exts$xext):");
            print(length(scm_exts$xext));# debug
         }
         scm_exts_x <- (scm_exts$xext[[1]] + scm_exts$xext[[2]]) / 2;
         scm_exts_y <- (scm_exts$yext[[1]] + scm_exts$yext[[2]]) / 2;
         scm_exts_w <- (scm_exts$xext[[2]] - scm_exts$xext[[1]]);
         scm_exts_h <- (scm_exts$yext[[2]] - scm_exts$yext[[1]]);
         ki <- korig[which.max(gdf$r[korig])]
         newgrob <- grid::roundrectGrob(
            x=scm_exts_x,
            y=scm_exts_y,
            width=scm_exts_w,
            height=scm_exts_h,
            r=grid::unit(gdf$r[ki],
               gdf$r_unit[ki]),
            gp=grid::gpar(
               lwd=gdf$box_lwd[ki],
               lty=gdf$box_lty[ki],
               col=gdf$border_col[ki],
               fill=gdf$box_fill[ki]))
         # adjusted labels are returned
         return(list(newgrob=newgrob,
            g_labels=gt_final))
      }
      
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
      # jamba::printDebug("xmin_pt:");print(xmin_pt);# debug
      # jamba::printDebug("xmax_pt:");print(xmax_pt);# debug
      
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
      # jamba::printDebug("k:");print(k);# debug
      # jamba::printDebug("x_init:");print(x_init);# debug

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
      # determine largest radius
      # jamba::printDebug("gdf[korig, , drop=FALSE]:");print(gdf[korig, , drop=FALSE]);# debug
      ki <- korig[which.max(gdf$r[korig])]
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
   # jamba::printDebug("segment_df:");print(segment_df);# debug
   # jamba::printDebug("str(g_labels):");print(str(g_labels));# debug
   
   # iterate each group of labels and draw roundrect
   gdf$num <- seq_len(nrow(gdf));
   if (verbose) {
      jamba::printDebug("draw_gridtext_groups(): ",
         "gdf:");
      print(gdf);
   }
   # 0.0.34.900 - ignore overlap_set for now
   gdf_group <- paste0(
      gdf$ref_polygon, "_",
      # gdf$overlap_set, "_",
      round(gdf$x, digits=3), "_",
      round(gdf$y, digits=3));
   gdf$gdf_group <- gdf_group;
   if (length(groupdf) > 0) {
      groupdf$gdf_group <- gdf_group;
   }
   gdf_split <- split(gdf$num, gdf_group);
   # jamba::printDebug("gdf:");print(gdf);# debug
   # jamba::printDebug("head(gdf_split, 2):");print(head(gdf_split, 2));# debug
   # jamba::printDebug("unique(segment_df):");print(unique(segment_df));# debug
   
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
   # jamba::printDebug("adj_df:");print(adj_df);# debug
   # jamba::printDebug("gdf_group:");print(gdf_group);# debug
   rr_grobs <- list();
   new_g_labels <- list();
   #rr_grobs <- lapply(split(gdf$num, gdf_group), function(k){
   for (ki in seq_along(gdf_split)) {
      k <- gdf_split[[ki]];
      # determine if any labels have a line segment
      kgroup <- gdf_group[k][1];
      # jamba::printDebug("k:");print(k);# debug
      # jamba::printDebug("adj_df[kgroup, ]:");print(adj_df[kgroup, ]);# debug
      # jamba::printDebug("prior to grob_group_roundrect() childNames(g_labels):");print(childNames(g_labels));# debug
      rr_grob_l <- grob_group_roundrect(
         g_labels=g_labels,
         gdf=gdf,
         groupdf=groupdf,
         k=k,
         realign=realign,
         adjx=adj_df[kgroup,"adjx"],
         adjy=adj_df[kgroup,"adjy"],
         adjx_fn=adjx_fn,
         adjy_fn=adjy_fn,
         adjust_center=adjust_center,
         template=template,
         segment_df=segment_df,
         verbose=verbose);
      rr_grobs <- c(rr_grobs,
         list(rr_grob_l$newgrob));
      # g_labels <- rr_grob_l$g_labels;
      new_g_labels <- c(new_g_labels,
         list(rr_grob_l$g_labels))
      if (do_draw) {
         # grid::popViewport(0);
         # grid::pushViewport(rr_grob_l$newgrob$vp);
         grid::grid.draw(rr_grob_l$newgrob);
      }
   }
   names(rr_grobs) <- names(gdf_split);
   # re-assemble g_labels
   # jamba::printDebug("jamba::sdim(new_g_labels)):");print(jamba::sdim(new_g_labels));# debug
   g_labels <- do.call(grid::gList, new_g_labels);
   if (do_draw) {
      # grid::grid.draw(g_labels);
      # grid::popViewport(3);
   }
   return(invisible(list(grobs=rr_grobs,
      g_labels=g_labels)));
}
