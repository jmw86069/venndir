
#' Render venndir output
#' 
#' Render venndir output
#' 
#' @family venndir core
#' 
#' @inheritParams venndir
#' @param venndir_output `Venndir` output from `venndir()`
#' @param expand_fraction `numeric` value indicating how much to
#'    expand the figure range beyond the default calculated for
#'    the Venn diagram. Values above zero cause the Venn diagram
#'    to be slighly smaller.
#' @param item_cex_factor `numeric` value used to adjust pre-calculated
#'    item fontsizes.
#' @param plot_warning `logical` indicating whether to include a warning
#'    when one or more non-zero overlap counts cannot be displayed
#'    in the figure. Not yet re-implemented for version 0.0.30.900.
#' @param item_degrees `numeric` angle (default 0) in degrees used
#'    to adjust item label display.
#' @param show_segments `logical` (default TRUE) indicating whether to
#'    draw a line segment from the Venn region to any label
#'    positioned outside.
#' @param segment_buffer `numeric` (default -0.1) indicating the depth
#'    inside each Venn region a line segment will be drawn, relevant
#'    only when `show_segments=TRUE`.
#' @param fontfamily `character` font family
#' @param group_labels `logical` (default TRUE) indicating whether to group
#'    label components together, therefore drawing fill and border
#'    around the group instead of each component. In most cases this
#'    setting should be TRUE.
#' @param adjust_center `logical` (default TRUE) used when labels are grouped,
#'    whether the group should be re-centered on the target point.
#'    Try `adjust_center=FALSE` if wide label groups are adjusted
#'    so that the count label is too far left.
#' @param draw_legend `logical` (default TRUE) indicating whether to draw
#'    a legend, calling `venndir_legender()`.
#' @param legend_x `character` passed to `venndir_legender()` to customize
#'    the position of the legend.
#' @param legend_font_cex `numeric` scalar to adjust the legend font size.
#' 
#' @export
render_venndir <- function
(venndir_output=NULL,
 # venn_jp=NULL,
 # label_df=NULL,
 # asp=1,
 # xlim=NULL,
 # ylim=NULL,
 expand_fraction=0,
 # xpd=NA,
 font_cex=1,
 item_cex=NULL,
 item_cex_factor=4,
 plot_warning=TRUE,
 show_labels=NULL,
 show_items=c(NA,
    "none",
    "sign item",
    "item",
    "sign"),
 item_degrees=0,
 max_items=100,
 show_zero=TRUE,
 show_segments=TRUE,
 segment_buffer=-0.10,
 label_preset=c("none"),
 label_style=c("custom",
    "basic",
    "fill",
    "shaded",
    "shaded_box",
    "lite",
    "lite_box"),
 fontfamily="Arial",
 inside_percent_threshold=0,
 item_style=c("text",
    "gridtext"),
 item_buffer=-0.15,
 group_labels=TRUE,
 adjust_center=FALSE,
 draw_legend=TRUE,
 legend_x="bottomright",
 legend_font_cex=1,

 show_label=NA,
 display_counts=TRUE,
 draw_buffer=FALSE,
 ...)
{
   if ("Venndir" %in% class(venndir_output)) {
      venn_jp <- venndir_output@jps;
      label_df <- venndir_output@label_df;
   } else {
      if (length(venndir_output) > 0 && is.list(venndir_output)) {
         if (!any(c("venn_jp", "label_df") %in% names(venndir_output))) {
            stop("List input must contain element names 'venn_jp' or 'label_df'.");
         }
         if (!inherits(venndir_output[["venn_jp"]], "JamPolygon")) {
            stop("Element 'venn_jp' must inherit from 'JamPolygon'.");
         }
         venn_jp <- venndir_output[["venn_jp"]];
         if (!inherits(venndir_output[["label_df"]], "data.frame")) {
            stop("Element 'label_df' must inherit from 'data.frame'.");
         }
         label_df <- venndir_output[["label_df"]];
      }
   }
   show_items <- head(setdiff(label_df$show_items, c(NA, "none")), 1);
   if (length(show_items) == 0) {
      show_items <- NA;
   }
   # jamba::printDebug("show_items: ", show_items);# debug
   # show_items <- head(show_items, 1);
   item_style <- match.arg(item_style);
   
   # Apply label_style
   if (!"custom" %in% label_style) {
      # venndir_output <- venndir_label_style(
      vls <- venndir_label_style(
         list(venn_spdf=venn_jp@polygons, label_df=label_df),
         label_preset=label_preset,
         label_style=label_style,
         show_labels=show_labels,
         inside_percent_threshold=inside_percent_threshold,
         show_zero=show_zero,
         ...);
      venn_jp@polygons <- vls$venn_spdf;
      label_df <- vls$label_df;
   }
   
   # Process existing JamPolygon
   if (length(venn_jp) > 0) {
      # Fill missing attribute colnames with default values
      venn_jp_defaults <- c(
         alpha=venn_jp@polygons$alpha,#jamba::col2alpha(venn_spdf$color),
         lwd=2,
         border=NA
      );
      venn_jp_add <- setdiff(colnames(venn_jp@polygons),
         names(venn_jp_defaults));
      # for (i in venn_jp_add) {
      #    venn_jp@polygons[, i] <- rep(venn_jp_defaults[[i]],
      #       length(venn_jp));
      # }
   }
   
   # Process labels   
   if (length(label_df) > 0) {
      ## Verify label_df contains required columns
      label_df_required <- c(
         "x",
         "y",
         "text");
      if (!all(label_df_required %in% colnames(label_df))) {
         warning(paste0("label_df must contain colnames: ",
            jamba::cPaste(label_df_required)));
      }
      
      # auto-scale item_cex based upon number of items and polygon area
      if (length(item_cex) <= 1) {
         if (length(item_cex) == 1) {
            if (is.na(item_cex)) {
               item_cex <- 1;
            }
         } else {
            item_cex <- 1;
         }
         # recipe to calculate item_cex
         item_cex <- tryCatch({
            poly_rows <- which(!is.na(venn_jp@polygons$venn_counts));
            so_counts <- venn_jp@polygons$venn_counts[poly_rows];
            names(so_counts) <- rownames(venn_jp@polygons)[poly_rows];
            # crude scaling by total number of items
            so_cex <- jamba::noiseFloor(1/sqrt(so_counts) * item_cex_factor,#2.5,
               ceiling=0.9,
               minimum=0.2)
            # update here in case area fails, it will use this crude item_cex
            item_cex <- item_cex * so_cex;
            # area of each polygon
            so_areas <- area_JamPolygon(venn_jp[poly_rows, ]);
            names(so_areas) <- rownames(venn_jp@polygons)[poly_rows];
            # total area of all polygons
            # (not used currently but might be preferred for proportional)
            # so_total_areas <- rgeos::gArea(
            #    rgeos::gSimplify(venn_spdf,
            #       tol=1));
            
            # take median of the larger area polygons
            so_big <- median(so_areas[so_areas / max(so_areas) >= 0.5])
            so_areas_cex <- sqrt(so_areas) / sqrt(so_big);
            
            # weight the effect by number of item labels
            # so that 2 labels would not be scaled as much as 10
            so_areas_wt <- (1 + 0.5) / (so_counts + 0.5)
            so_areas_cex_wt <- so_areas_wt + (1 - so_areas_wt) * so_areas_cex
            # adjust the crude scaling by the relative polygon area
            item_cex <- item_cex * so_areas_cex_wt;
            # for debugging, the data.frame can be printed
            #print(data.frame(so_counts, so_cex, so_areas_cex, so_areas_wt, so_areas_cex_wt, item_cex));
            item_cex;
         }, error=function(e){
            item_cex;
         });
      }
      if (length(item_cex) == 0 || all(is.na(item_cex))) {
         item_cex <- 1;
      }
      
      ## Fill any missing optional colnames with defaults
      label_df_defaults <- list(
         type="main",
         show_label=NA,
         show_items=NA,
         item_degrees=item_degrees,
         vjust=0.5,
         hjust=0.5,
         halign=0.5,
         rot=0,
         color="black",
         fontsize=14,
         item_cex=item_cex,
         border=NA,
         lty=1,
         lwd=1,
         fill=NA,
         padding=3,
         x_offset=0,
         y_offset=0,
         padding_unit="pt",
         r=3,
         segment_buffer=segment_buffer,
         r_unit="pt");
      label_df_add <- setdiff(names(label_df_defaults),
         colnames(label_df));
      for (i in label_df_add) {
         label_df[[i]] <- rep(label_df_defaults[[i]],
            length.out=nrow(label_df));
      }
      
      # replace NA with 0
      label_df$x_offset <- jamba::rmNA(label_df$x_offset,
         naValue=0);
      label_df$y_offset <- jamba::rmNA(label_df$y_offset,
         naValue=0);
      
      # define show_label and show_items for each label_df row
      if (length(show_label) == 0) {
         show_label <- NA;
      }
      if (length(show_items) == 0) {
         show_items <- "none";
      }
      if (length(show_zero) == 0) {
         show_zero <- eval(formals(render_venndir)$show_zero);
      }
      if (length(max_items) == 0) {
         max_items <- Inf;
      }
      # logic:
      # - show_label function argument takes priority over label_df$show_label
      # - show_items function argument takes priority over label_df$show_items
      #
      # - x=NA or y=NA --> show_label=FALSE, show_items=FALSE
      # - show_zero=FALSE, venn_counts=0 --> show_label=FALSE
      # - venn_counts=0 --> show_items=FALSE
      # - show_label=NA, show_items=NA --> show_label=TRUE
      # - show_label=TRUE, show_items=NA --> show_items=FALSE
      # - show_label=FALSE, show_items=NA --> show_items=FALSE
      # - show_label=NA, show_items=TRUE --> show_label=FALSE
      # - show_label=NA, show_items=FALSE --> show_label=TRUE
      # - show_items=TRUE, items column empty or does not exist --> show_items=FALSE
      #
      # TODO:
      # - if x,y + x_offset,y_offset is outside the polygon, allow
      #   show_label=NA --> show_label=TRUE when show_items=TRUE

      if (!"overlap" %in% colnames(label_df)) {
         label_df$overlap <- "inside";
      }
      if (!"count" %in% colnames(label_df)) {
         label_df$count <- "inside";
      }
      show_label <- (label_df$overlap %in% c("outside", "inside") |
            label_df$count %in% c("outside", "inside"));
      # jamba::printDebug("label_df:");print(label_df);

      # warn about hidden non-zero labels
      warn_rows <- (
         (label_df$x %in% NA |
               label_df$y %in% NA) &
            label_df$venn_counts != 0 &
            label_df$type %in% "main");
      #label_df_list <- split(label_df, label_df$overlap_set);
      #warn_by_set <- lapply(label_df_list, function(idf){
      #   (idf$show_label %in% TRUE | !idf$show_items %in% c(FALSE,NA))
      #});
      warning_label <- NULL;
      if (any(warn_rows)) {
         warn_labels <- paste0("'",
            label_df$overlap_set[warn_rows],
            "' (",
            jamba::formatInt(label_df$venn_counts[warn_rows]),
            ")");
         warning_base <- paste0(
            ifelse(sum(warn_rows) > 1, "These overlap counts", "This overlap count"),
            " cannot be displayed: ");
         warning_text <- paste0(warning_base,
            jamba::cPaste(warn_labels, sep=", "));
         warning(warning_text);
         if (TRUE %in% plot_warning) {
            warning_label <- paste0(warning_base,
               "\n",
               jamba::cPaste(warn_labels,
                  sep="; "));
         }
      }
      
      # prepare line segments for labels outside their respective polygons
      g_labels <- NULL;
      segment_df <- NULL;
      if (any(show_label %in% TRUE)) {
         # jamba::printDebug("show_label");
         # jamba::printDebug("table(label_df$overlap):");print(table(label_df$overlap));
         label_outside <- (label_df$overlap %in% "outside" | label_df$count %in% "outside");
         
         # Determine if any offset labels require line segment
         has_offset <- label_outside & (label_df$x_offset != 0 | label_df$y_offset != 0);
         #
         # Todo: Deal with has_offset, for now set to FALSE
         # has_offset <- rep(FALSE, length(has_offset));
         # jamba::printDebug("label_outside:");print(table(label_outside));
         # jamba::printDebug("has_offset:");print(table(has_offset));
         #
         if (any(show_label & has_offset)) {
            use_offset <- (show_label & has_offset);
            offset_sets <- label_df$overlap_set[use_offset];
            # 0.0.20.900 - fix order of preferred polygon labels
            #sp_index <- match(offset_sets, venn_spdf$label);
            # sp_index <- (length(venn_spdf$label) + 1 - 
            #       match(offset_sets, 
            #          rev(venn_spdf$label)));
            sp_index <- (length(venn_jp@polygons$label) + 1 - 
                  match(offset_sets, 
                     rev(venn_jp@polygons$label)));
            segment_buffer <- ifelse(label_df$items %in% "inside",
               label_df$segment_buffer / 2,
               label_df$segment_buffer);
            test_xy <- data.frame(
               check.names=FALSE,
               stringsAsFactors=FALSE,
               x0=label_df$x[use_offset] + label_df$x_offset[use_offset],
               x1=label_df$x[use_offset],
               y0=label_df$y[use_offset] + label_df$y_offset[use_offset],
               y1=label_df$y[use_offset],
               segment_buffer=jamba::rmNA(segment_buffer[use_offset], naValue=-0.1),
               sp_index=sp_index,
               label_color=label_df$color[use_offset],
               label_fill=label_df[use_offset, "fill"],
               label_border=label_df$border[use_offset],
               poly_color=venn_jp@polygons$fill[sp_index],
               poly_border=venn_jp@polygons$border[sp_index]);
            # jamba::printDebug("test_xy:");print(test_xy);# debug
            # sp_list <- lapply(sp_index, function(i){
            #    venn_spdf[i,]});
            jp_list <- lapply(sp_index, function(i){
               venn_jp[i, ]
            });
            # new_xy <- polygon_label_segment(
            new_xy <- label_segment_JamPolygon(
               x0=test_xy$x0,
               y0=test_xy$y0,
               x1=test_xy$x1,
               y1=test_xy$y1,
               jp=jp_list,
               buffer=test_xy$segment_buffer,
               verbose=FALSE,
               ...);
            # jamba::printDebug("new_xy:");print(new_xy);# debug
            # non-NULL result means we draw a line segment
            if (any(!is.na(new_xy[,1]))) {
               has_segment <- !is.na(new_xy[,1]);
               # priority of colors to use for the line segment
               sc_cols <- c(
                  "label_border",
                  "poly_border",
                  "poly_color",
                  "label_fill",
                  "label_color");
               seg_colors <- apply(test_xy[has_segment, sc_cols, drop=FALSE], 1,
                  function(sc1){
                  sc2 <- jamba::rmNA(sc1);
                  sc2a <- jamba::col2alpha(sc2);
                  sc2 <- sc2[sc2a > 0];
                  head(sc2, 1)
               });
               segment_df <- data.frame(
                  check.names=FALSE,
                  stringsAsFactors=FALSE,
                  x=as.vector(rbind(test_xy$x0[has_segment], new_xy[,1][has_segment])),
                  y=as.vector(rbind(test_xy$y0[has_segment], new_xy[,2][has_segment])),
                  group=rep(venn_jp@polygons$label[test_xy$sp_index[has_segment]], each=2),
                  color=rep(seg_colors, each=2),
                  lwd=rep(jamba::rmNULL(
                     venn_jp@polygons$border.lwd[test_xy$sp_index[has_segment]],
                     nullValue=1), each=2),
                  point_order=c(1, 2)
               );
               # jamba::printDebug("segment_df:");print(segment_df);# debug
            }
         }
         
      }
   }
   
   ## Prepare item labels
   itemlabels_df <- NULL;
   #
   # Todo: Handle item display
   #
   # print(jamba::middle(label_df, 5))
   # print(table(label_df$show_items))
   if (any(!label_df$show_items %in% c("none", NA))) {
      # jamba::printDebug("show_items");
      items_dfs <- subset(label_df, !label_df$show_items %in% c("none", NA));
      # jamba::printDebug("unique(items_dfs$overlap_set): ", unique(items_dfs$overlap_set));
      items_dfs <- split(items_dfs,
         factor(items_dfs$overlap_set,
            levels=unique(items_dfs$overlap_set)));
      # jamba::printDebug("sdim(items_dfs):");print(jamba::sdim(items_dfs));# debug
      # jamba::printDebug("items_dfs:");print(items_dfs);

      #for (items_df1 in items_dfs) {
      itemlabels_list <- lapply(items_dfs, function(items_df1){
         items_list <- items_df1$items;
         items_list <- items_list[lengths(items_list) > 0];
         if (length(items_list) > 0) {
            items <- unname(unlist(jamba::mixedSorts(items_list)));
         } else {
            return(NULL)
         }
         color1 <- rep(items_df1$color, lengths(items_df1$items));
         vis <- which(venn_jp@polygons$label %in% items_df1$overlap_set);
         # vi <- tail(venn_jp@polygons$label %in% items_df1$overlap_set, 1);
         vi <- venn_jp@polygons$label %in% tail(items_df1$overlap_set, 1);
         vdf <- venn_jp@polygons[vi, , drop=FALSE];
         prefixes <- rep(
            gsub(":.+", "", items_df1$text),
            lengths(items_df1$items));
         labels <- NULL;
         # note currently uses the same show_items format per polygon
         # not for each row in items_dfs, so it is not possible to
         # use different show_items format for up-up and down-down within
         # the same polygon
         #show_items_order <- strsplit(items_df1$show_items[1], "[- _.]")[[1]];
         use_show_items <- head(items_df1$item_style, 1);
         # jamba::printDebug("use_show_items:");print(use_show_items);# debug
         # show_items_order <- strsplit(show_items[1], "[- _.]")[[1]];
         show_items_order <- strsplit(use_show_items, "[- _.]")[[1]];
         for (dio in show_items_order) {
            if (grepl("sign", dio)) {
               labels <- paste(labels, prefixes);
            } else if (grepl("item", dio)) {
               labels <- paste(labels, items);
            }
         }
         # jamba::printDebug("unique(items_df1$item_cex):", unique(items_df1$item_cex));
         items_df1$item_cex <- item_cex[as.character(items_df1$overlap_set)];
         labels <- gsub("^[ ]+|[ ]+$", "", labels);
         bg <- jamba::alpha2col(vdf$color, vdf$alpha)
         color <- make_color_contrast(color1,
            y=bg,
            ...);
         
         lpf <- label_fill_JamPolygon(jp=venn_jp[tail(vis, 1), ],
            ref_jp=venn_jp,
            color=color,
            cex=head(items_df1$item_cex, 1),
            draw_points=FALSE,
            labels=labels,
            plot_style="none",
            draw_labels=FALSE,
            degrees=items_df1$item_degrees[1],
            buffer=item_buffer,
            seed=123,
            verbose=TRUE,
            ...);
         # add overlap label to items_df
         lpf$items_df$overlap_set <- head(items_df1$overlap_set, 1);
         # jamba::printDebug("lpf$items_df:");print(lpf$items_df);
         # jamba::printDebug("lpf$items_df:");print(lpf$items_df);
         lpf;
      });
      # combine item label into one data.frame
      itemlabels_df <- jamba::rbindList(lapply(itemlabels_list, function(i1){
         i1$items_df;
      }));
      itemlabels_jp <- NULL;
      # itemlabels_sp <- lapply(itemlabels_list, function(i1){
      #    if (!grepl("Spatial", class(i1$sp_buffer))) {
      #       return(NULL)
      #    }
      #    i1$sp_buffer
      # });
   }
   
   gdf <- NULL;
   if (any(show_label)) {
      # generate data.frame of label coordinates
      show_overlap_outside <- (label_df$overlap %in% "outside" & !is.na(label_df$x))
      show_overlap_inside <- (label_df$overlap %in% "inside" & !is.na(label_df$x))
      show_count_outside <- (label_df$count %in% "outside" & !is.na(label_df$x))
      show_count_inside <- (label_df$count %in% "inside" & !is.na(label_df$x));
      overlap_set <- paste0("**", label_df$overlap_set, "**");
      is_left <- (label_df$type %in% "main") * 1;
      # enhancement to apply fontsize from venn_spdf to main set labels
      label_df$overlap_fontsize <- label_df$fontsize;
      if ("fontsize" %in% colnames(venn_jp@polygons)) {
         setmatch <- match(label_df$overlap_set, venn_jp@polygons$label);
         setmatchupdate <- !is.na(setmatch);
         label_df$overlap_fontsize[setmatchupdate] <- venn_jp@polygons$fontsize[setmatch[setmatchupdate]];
      }
      # gdf is the expanded data.frame of label coordinates
      if (!"vjust_outside" %in% colnames(label_df)) {
         label_df$vjust_outside <- label_df$vjust;
      }
      if (!"hjust_outside" %in% colnames(label_df)) {
         label_df$hjust_outside <- label_df$hjust;
      }
      if (!"vjust_inside" %in% colnames(label_df)) {
         label_df$vjust_inside <- label_df$vjust;
      }
      if (!"hjust_inside" %in% colnames(label_df)) {
         label_df$hjust_inside <- label_df$hjust;
      }
      gdf <- data.frame(
         check.names=FALSE,
         stringsAsFactors=FALSE,
         location=rep(
            c("show_overlap_outside",
               "show_overlap_inside",
               "show_count_outside",
               "show_count_inside"),
            c(sum(show_overlap_outside),
               sum(show_overlap_inside),
               sum(show_count_outside),
               sum(show_count_inside))),
         overlap_set=c(
            label_df$overlap_set[show_overlap_outside],
            label_df$overlap_set[show_overlap_inside],
            label_df$overlap_set[show_count_outside],
            label_df$overlap_set[show_count_inside]),
         text=c(
            overlap_set[show_overlap_outside],
            overlap_set[show_overlap_inside],
            label_df$text[show_count_outside],
            label_df$text[show_count_inside]),
         x=c(
            label_df$x[show_overlap_outside] + label_df$x_offset[show_overlap_outside],
            label_df$x[show_overlap_inside],
            label_df$x[show_count_outside] + label_df$x_offset[show_count_outside],
            label_df$x[show_count_inside]),
         y=c(
            label_df$y[show_overlap_outside] + label_df$y_offset[show_overlap_outside],
            label_df$y[show_overlap_inside],
            label_df$y[show_count_outside] + label_df$y_offset[show_count_outside],
            label_df$y[show_count_inside]),
         vjust=c(
            1 - label_df$vjust_outside[show_overlap_outside],
            1 - label_df$vjust_inside[show_overlap_inside],
            label_df$vjust_outside[show_count_outside],
            label_df$vjust_inside[show_count_inside]),
         hjust=c(
            label_df$hjust_outside[show_overlap_outside],
            label_df$hjust_inside[show_overlap_inside],
            label_df$hjust_outside[show_count_outside],
            label_df$hjust_inside[show_count_inside]),
         halign=c(
            label_df$halign[show_overlap_outside],
            label_df$halign[show_overlap_inside],
            label_df$halign[show_count_outside],
            label_df$halign[show_count_inside]),
         rot=c(
            label_df$rot[show_overlap_outside],
            label_df$rot[show_overlap_inside],
            label_df$rot[show_count_outside],
            label_df$rot[show_count_inside]),
         padding=c(
            label_df$padding[show_overlap_outside],
            label_df$padding[show_overlap_inside],
            label_df$padding[show_count_outside],
            label_df$padding[show_count_inside]) * 1,#font_cex,
         r=c(
            label_df$r[show_overlap_outside],
            label_df$r[show_overlap_inside],
            label_df$r[show_count_outside],
            label_df$r[show_count_inside]),
         r_unit=c(
            label_df$r_unit[show_overlap_outside],
            label_df$r_unit[show_overlap_inside],
            label_df$r_unit[show_count_outside],
            label_df$r_unit[show_count_inside]),
         label_col=c(
            label_df$color[show_overlap_outside],
            label_df$color[show_overlap_inside],
            label_df$color[show_count_outside],
            label_df$color[show_count_inside]),
         fontsize=c(
            label_df$overlap_fontsize[show_overlap_outside],
            label_df$overlap_fontsize[show_overlap_inside],
            label_df$fontsize[show_count_outside],
            label_df$fontsize[show_count_inside]) * font_cex,
         border_col=c(
            label_df$border[show_overlap_outside],
            label_df$border[show_overlap_inside],
            label_df$border[show_count_outside],
            label_df$border[show_count_inside]),
         box_fill=c(
            label_df$fill[show_overlap_outside],
            label_df$fill[show_overlap_inside],
            label_df$fill[show_count_outside],
            label_df$fill[show_count_inside]),
         box_lty=c(
            label_df$lty[show_overlap_outside],
            label_df$lty[show_overlap_inside],
            label_df$lty[show_count_outside],
            label_df$lty[show_count_inside]),
         box_lwd=c(
            label_df$lwd[show_overlap_outside]*2,
            label_df$lwd[show_overlap_inside],
            label_df$lwd[show_count_outside]*2,
            label_df$lwd[show_count_inside]),
         padding_unit=c(
            label_df$padding_unit[show_overlap_outside],
            label_df$padding_unit[show_overlap_inside],
            label_df$padding_unit[show_count_outside],
            label_df$padding_unit[show_count_inside])
      );
      # Update label_col using overlap fill color
      # Todo: Use label_df$fill when not NA
      #
      # get polygon fill color
      omatch <- match(gdf$overlap_set, rownames(venn_jp@polygons));
      omatch_fill <- jamba::alpha2col(venn_jp@polygons$fill[omatch],
         alpha=venn_jp@polygons$alpha[omatch]);
      gdf$bg_fill <- ifelse(grepl("outside", gdf$location),
         "#FFFFFFFF",
         omatch_fill);
      # we need to blend box_fill on top of bg_fill
      # - ensure background alpha is no greater than 1-box_alpha
      box_fill <- jamba::rmNA(naValue="#00000000", gdf$box_fill);
      box_alpha <- jamba::col2alpha(box_fill);
      bg_fill <- jamba::rmNA(naValue="#00000000", gdf$bg_fill);
      max_bg_alpha <- (1 - box_alpha)
      bg_alpha <- jamba::col2alpha(bg_fill);
      bg_alpha_adj <- jamba::noiseFloor(bg_alpha, ceiling=max_bg_alpha)
      bg_fill_adj <- jamba::alpha2col(bg_fill, alpha=bg_alpha_adj);
      canvas_alpha <- max_bg_alpha - bg_alpha_adj;
      canvas_adj <- jamba::alpha2col(rep("white", length(canvas_alpha)), alpha=canvas_alpha)
      todo_color_list <- lapply(seq_along(box_fill), function(jc){
         c(box_fill[jc], bg_fill_adj[jc], canvas_adj[jc])
      })
      gdf$final_fill <- colorjam::blend_colors(todo_color_list);

      # adjust label color to contrast with the polygon fill color
      new_label_col <- make_color_contrast(x=gdf$label_col,
         L_threshold=63,
         y=gdf$final_fill)
         # y=omatch_fill)
      ## update all labels
      gdf$label_col <- new_label_col;
   }
   
   #############################################
   # grid graphics from here on
   jp_xrange <- expand_range(expand_fraction=0.2,
      range(c(
         unlist(venn_jp@polygons$x),
         gdf$x),
         na.rm=TRUE));
   jp_yrange <- expand_range(expand_fraction=0.2,
      range(c(
         unlist(venn_jp@polygons$x),
         gdf$y),
         na.rm=TRUE));
   jp <- plot(venn_jp,
      buffer=expand_fraction + 0.1,
      xlim=jp_xrange,
      ylim=jp_yrange,
      show_labels=FALSE,
      do_pop_viewport=TRUE);
      # do_pop_viewport=FALSE);
   # on.exit(grid::popViewport());
   # adjx,adjy are functions to transform x,y into grid "snpc" coordinates
   adjx <- attr(jp, "adjx");
   adjy <- attr(jp, "adjy");
   jp_viewport <- attr(jp, "viewport");
   
   ############################################
   # Item labels
   # draw using text()
   if (length(itemlabels_df) > 0) {
      #
      # add overlap fill color
      omatch <- match(itemlabels_df$overlap_set, rownames(venn_jp@polygons));
      itemlabels_df$fill <- jamba::alpha2col(venn_jp@polygons$fill[omatch],
         alpha=venn_jp@polygons$alpha[omatch]);
      # adjust label color to contrast with the polygon fill color
      new_item_color <- make_color_contrast(x=itemlabels_df$color,
         L_threshold=63,
         y=itemlabels_df$fill)
      itemlabels_df$color <- new_item_color;
      # jamba::printDebug("middle(itemlabels_df):");print(jamba::middle(itemlabels_df));
      #
      text_grob <- grid::textGrob(
         x=adjx(itemlabels_df$x),
         y=adjy(itemlabels_df$y),
         label=itemlabels_df$text,
         rot=jamba::rmNULL(nullValue=0, itemlabels_df$rot),
         check.overlap=FALSE,
         default.units="snpc",
         gp=grid::gpar(
            col=itemlabels_df$color,
            fontsize=itemlabels_df$fontsize),
         vp=jp_viewport,
         hjust=0.5,
         vjust=0.5);
      # print(jamba::middle(itemlabels_df));
      grid::grid.draw(text_grob);
      # grid::grid.points(
      #    x=adjx(itemlabels_df$x),
      #    y=adjy(itemlabels_df$y),
      #    pch=20);
         # srt is a hack because text() only handles one srt per call
         # srt=-head(itemlabels_df$rot, 1),
         #default.units="native",
         #padding=grid::unit(0, "pt"),
         #r=grid::unit(0, "pt"),
         # adj=c(0.5, 0.5),
         # cex for now is a hack estimate of cex for a given fontsize
         # font could be used for fontfamily but mapping is unclear
         # fontfamily
         #
         # itemlabels_df$border is currently not handled
   }
   
   ############################################
   ## Todo:
   # - draw label line segments (if needed)
   # - draw the warning text label (if needed)
   # - display items (if needed)

   # - display the counts / setlabels
   g_labels <- NULL;
   if (any(show_label)) {
      #
      # g_labels <- gridtext::richtext_grob(
      # print("gdf$padding:");print(gdf$padding);
      g_labels <- gridtext_richtext_grob(
         # default.units="snpc",
         text=gdf$text,
         x=adjx(gdf$x),
         y=adjy(gdf$y),
         default.units="snpc",
         vjust=gdf$vjust,
         hjust=gdf$hjust,
         halign=gdf$halign,
         rot=gdf$rot,
         padding=grid::unit(gdf$padding,
            gdf$padding_unit),
         r=grid::unit(gdf$r,
            gdf$r_unit),
         gp=grid::gpar(
            fontfamily=fontfamily,
            col=gdf$label_col,
            fontsize=gdf$fontsize
         ),
         box_gp=grid::gpar(
            col=if(group_labels){NA}else{gdf$border_col},
            # col=if(group_labels){NA}else{"black"},
            fill=if(group_labels){NA}else{gdf$box_fill},
            lty=gdf$box_lty,
            lwd=gdf$box_lwd)
      );
      # draw grouped label background
      if (TRUE %in% group_labels) {
         grid::pushViewport(attr(jp, "viewport"));
         g_labels <- tryCatch({
            dgg <- draw_gridtext_groups(
               g_labels=g_labels,
               gdf=gdf,
               segment_df=segment_df,
               adjust_center=adjust_center,
               do_draw=TRUE,
               verbose=FALSE)
            dgg$g_labels;
         }, error=function(e){
            print(e);
            g_labels;
         });
         grid::popViewport();
      }
      
      grid::pushViewport(attr(jp, "viewport"))
      grid::grid.draw(g_labels);
      grid::popViewport();
   }
   
   # segments
   if (show_segments && length(segment_df) > 0) {
      # jamba::printDebug("segment_df:");print(segment_df);# debug
      segment_df1 <- subset(segment_df, point_order %in% 1);
      segment_df2 <- subset(segment_df, point_order %in% 2);
      # make unique data.frame to avoid overplotting the same line
      segment_wide <- unique(data.frame(
         check.names=FALSE,
         stringsAsFactors=FALSE,
         x0=segment_df1$x,
         x1=segment_df2$x,
         y0=segment_df1$y,
         y1=segment_df2$y,
         color=segment_df1$color,
         lwd=ifelse(segment_df1$lwd == 0, 1, segment_df1$lwd)));
      # create segments grob
      segments_grob <- grid::segmentsGrob(
         x0=adjx(segment_wide$x0),
         x1=adjx(segment_wide$x1),
         y0=adjy(segment_wide$y0),
         y1=adjy(segment_wide$y1),
         default.units="snpc",
         gp=grid::gpar(col=segment_wide$color,
            lwd=segment_wide$lwd),
         vp=jp_viewport);
      grid::grid.draw(segments_grob);
   }
   
   # venndir legender
   if (TRUE %in% draw_legend) {
      venndir_legender(venndir_out=list(venn_jps=venn_jp),
         x=legend_x,
         font_cex=legend_font_cex,
         ...)
   }
   
   # warning in case not all overlaps can be displayed
   if (length(warning_label) > 0) {
      jamba::printDebug("warning_label exists");
   }
   # debug
   return(list(
      jp=jp,
      label_df=label_df,
      gdf=gdf));
   
   
   # Render the different aspects of the venndir plot
   if (!"sf" %in% class(venn_spdf)) {
      vosf <- sf::st_as_sf(venn_spdf);
   } else {
      vosf <- venn_spdf;
   }
   if ("base" %in% plot_style) {
      # Plot the Venn polygons
      # Display the warning text label
      g_label <- NULL;
      vps <- NULL;
      if (length(warning_label) > 0) {
         cp <- jamba::coordPresets(preset="bottom",
            preset_type="figure");
         # new method using gridtext::
         g_warning <- gridtext::textbox_grob(
            text=gsub(": ", ":<br>", warning_label),
            x=cp$x,
            y=cp$y,
            width=grid::unit(1, "npc"),
            maxwidth=grid::unit(0.7, "npc"),
            default.units="native",
            padding=grid::unit(2, "pt"),
            r=grid::unit(2, "pt"),
            gp=grid::gpar(
               fontfamily=fontfamily,
               col="#444444",
               fontsize=12 * font_cex
            ),
            box_gp=grid::gpar(
               #col="#444444",
               #fill="#FFEECC99",
               col=NA,
               fill=NA,
               lty=1,
               lwd=1)
         );
         if (length(dev.list()) > 0) {
            vps <- gridBase::baseViewports();
            grid::pushViewport(vps$inner, vps$figure, vps$plot);
            grid::grid.draw(g_warning);
            grid::popViewport(3);
         }
      }
      # display count/set labels
      g_labels <- NULL;
      if (any(show_label)) {
         #
         # g_labels <- gridtext::richtext_grob(
         # print("gdf$padding:");print(gdf$padding);
         g_labels <- gridtext_richtext_grob(
            default.units="native",
            text=gdf$text,
            x=grid::unit(gdf$x, "native"),
            y=grid::unit(gdf$y, "native"),
            vjust=gdf$vjust,
            hjust=gdf$hjust,
            halign=gdf$halign,
            rot=gdf$rot,
            padding=grid::unit(gdf$padding,
               gdf$padding_unit),
            r=grid::unit(gdf$r,
               gdf$r_unit),
            gp=grid::gpar(
               fontfamily=fontfamily,
               col=gdf$label_col,
               fontsize=gdf$fontsize
            ),
            box_gp=grid::gpar(
               col=if(group_labels){NA}else{gdf$border_col},
               fill=if(group_labels){NA}else{gdf$box_fill},
               lty=gdf$box_lty,
               lwd=gdf$box_lwd)
         );
      }
      # display item labels if available
      g_labels_items <- NULL;
      if (length(itemlabels_df) > 0) {
         # display item label buffer
         if (draw_buffer) {
            for (isp in itemlabels_sp) {
               try(
                  sp::plot(isp,
                     add=TRUE,
                     col="#FFFFFF77",
                     border="#FF999977",
                     lwd=2,
                     lty="dotted")
               )
            }
         }
         # display items
         #item_color <- make_color_contrast(itemlabels_df$color,
         #   bg=
         if ("gridtext" %in% item_style) {
            g_labels_items <- gridtext::richtext_grob(
               x=itemlabels_df$x,
               y=itemlabels_df$y,
               text=itemlabels_df$text,
               rot=-itemlabels_df$rot,
               default.units="native",
               padding=grid::unit(0, "pt"),
               r=grid::unit(0, "pt"),
               vjust=0.5,
               hjust=0.5,
               halign=0.5,
               gp=grid::gpar(
                  fontfamily=fontfamily,
                  col=itemlabels_df$color,
                  fontsize=itemlabels_df$fontsize
               ),
               box_gp=grid::gpar(
                  col=itemlabels_df$border
               )
            );
            if (length(dev.list()) > 0) {
               vps <- gridBase::baseViewports();
               grid::pushViewport(vps$inner, vps$figure, vps$plot);
               grid::grid.draw(g_labels_items);
               grid::popViewport(3);
            }
         } else {
            # draw using text()
            text(
               x=itemlabels_df$x,
               y=itemlabels_df$y,
               labels=itemlabels_df$text,
               # srt is a hack because text() only handles one srt per call
               srt=-head(itemlabels_df$rot, 1),
               #default.units="native",
               #padding=grid::unit(0, "pt"),
               #r=grid::unit(0, "pt"),
               adj=c(0.5, 0.5),
               # cex for now is a hack estimate of cex for a given fontsize
               cex=itemlabels_df$fontsize / 12,
               col=itemlabels_df$color,
               # font could be used for fontfamily but mapping is unclear
               # fontfamily
               #
               # itemlabels_df$border is currently not handled
               ...
            );
         }
      }
   } else if ("gg" %in% plot_style) {
      # create ggplot2 output
      # convert spdf to sf
      # vosf <- sf::st_as_sf(venn_spdf);
      # Venn overlap polygons
      ggv <- ggplot2::ggplot(data=vosf) + 
         ggplot2::geom_sf(
            ggplot2::aes(
               fill=jamba::alpha2col(color, alpha=alpha),
               linewidth=vosf$lwd,
               color=border)) + 
         ggplot2::scale_linewidth_identity() +
         ggplot2::scale_fill_identity() +
         ggplot2::scale_color_identity();
      if (length(ggtheme) > 0) {
         ggv <- ggv + ggtheme();
      }
      # optional segment to count labels
      if (length(segment_df) > 0 && show_segments) {
         segment_df <- unique(segment_df);
         ggv <- ggv + ggplot2::geom_line(
            data=segment_df,
            inherit.aes=FALSE,
            ggplot2::aes(x=x,
               y=y,
               group=group,
               color=color)
         );
      }
      # warning label
      if (length(warning_label) > 0) {
         warning_df <- data.frame(
            check.names=FALSE,
            stringsAsFactors=FALSE,
            label=gsub(": ", ":<br>", warning_label),
            x=-Inf,
            y=-Inf,
            hjust=-0.02,
            vjust=-0.1
         );
         ggv <- ggv + ggtext::geom_textbox(
            data=warning_df,
            inherit.aes=FALSE,
            ggplot2::aes(x=x,
               y=y,
               family=fontfamily,
               hjust=hjust,
               vjust=vjust,
               label=label
            ),
            text.colour="#444444",
            fill="#FFFFFF00",
            box.colour="#FFFFFF00",
            size=12 * 5/14 * font_cex,
            box.padding=grid::unit(2, "pt"),
            box.r=grid::unit(2, "pt"),
            width=grid::unit(1, "npc"),
            maxwidth=grid::unit(0.95, "npc"),
            show.legend=FALSE
         );
      }
      
      # count labels
      if (any(show_label)) {
         #show_label_df <- subset(label_df, show_label %in% c(TRUE));
         ggv <- ggv + ggtext::geom_richtext(
            data=gdf,
            ggplot2::aes(
               x=x,
               y=y,
               label=text,
               group=overlap_set,
               hjust=hjust,
               vjust=vjust,
               #halign=halign,
               family=fontfamily,
               text.colour=label_col,
               fill=box_fill,
               label.colour=border_col),
            label.padding=grid::unit(
               gdf$padding,
               gdf$padding_unit),
            label.r=grid::unit(
               gdf$r,
               gdf$r_unit),
            size=gdf$fontsize * 5/14
         )
      }
      # optional item labels
      if (length(itemlabels_df) > 0) {
         if (draw_buffer) {
            # optionally draw item polygon buffer
            rbind_sp <- function(...){
               sp::rbind.SpatialPolygons(..., makeUniqueIDs=TRUE)
            }
            buffer_sp <- do.call(rbind_sp, itemlabels_sp);
            buffer_sf <- sf::st_as_sf(buffer_sp);
            # Venn overlap polygons
            ggv <- ggv + ggplot2::geom_sf(
               data=buffer_sf,
               ggplot2::aes(
                  fill="#FFFFFF77",
                  color="#FF999977"));
         }
         if ("gridtext" %in% item_style) {
            ggitems <- ggtext::geom_richtext(
               data=itemlabels_df,
               ggplot2::aes(x=x,
                  y=y,
                  label=text,
                  family=fontfamily,
                  #group=group,
                  angle=-rot,
                  hjust=0.5,
                  vjust=0.5,
                  #halign=0.5,
                  text.colour=color,
                  fill=NA,
                  label.colour=NA),
               size=itemlabels_df$fontsize * 5/14);
         } else {
            ggitems <- ggplot2::geom_text(
               data=itemlabels_df,
               ggplot2::aes(x=x,
                  y=y,
                  label=text,
                  family=fontfamily,
                  #group=group,
                  angle=-rot,
                  hjust=0.5,
                  vjust=0.5,
                  #halign=0.5,
                  #text.colour=color,
                  colour=color,
                  fill=NA),
               size=itemlabels_df$fontsize * 5/14);
         }
         ggv <- ggv + ggitems;
      }
      
      # optional xlim, ylim
      if (length(xlim) > 0 & length(ylim) > 0) {
         ggv <- ggv + ggplot2::coord_sf(xlim=xlim, ylim=ylim);
      }
      
      print(ggv);
      return(ggv);
   }
   
   
   return(invisible(list(venn_spdf=venn_spdf,
      label_df=label_df,
      gdf=gdf,
      segment_df=segment_df,
      g_labels=g_labels,
      vosf=vosf)));
}
