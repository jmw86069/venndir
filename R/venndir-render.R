
#' Render Venn or Euler diagram
#' 
#' Render Venn or Euler diagram
#' 
#' This function is intended to render a Venn or Euler diagram
#' directly from the output of `venndir()` to allow customization
#' of various aspects of the figure.
#' 
#' @family venndir core
#' 
#' @param venndir_output `list` object produced by `venndir()` with
#'    at least two elements: `"venn_spdf"` which is a
#'    `sp::SpatialPolygonsDataFrame`; and `label_df` which is
#'    a `data.frame`. See argument descriptions below for the
#'    requirements of each object.
#' @param venn_spdf `sp::SpatialPolygonsDataFrame` that contains
#'    one polygon for each displayed overlap set. This object is
#'    expected to contain colnames:
#'    
#'    * `"color"` with fill color;
#'    * `"alpha"` with transparency optionally applied to `"color"`;
#'    * `"border"` with border color, where `NA` draws no border;
#'    * `"lwd"` with line width; and `"lty"` with line type. Only
#'    `"color"` is required, the others are filled with defaults
#'    as needed.
#' @param label_df `data.frame` that contains these required
#'    colnames:
#'    
#'    * `"x"` with x coordinate for each label,
#'    * `"y"` with y coordinate for each label,
#'    * `"text"` with the label to be displayed, in any
#'    format compatible with `gridtext::richtext_grob()`.
#'    
#'    Optional colnames are filled with defaults as needed: 
#'    
#'    * `"show_label"` a `logical` vector for which labels to show `[TRUE]`;
#'    * `"show_items"` a `logical` indicating whether to show item labels `[FALSE]`;
#'    * `"vjust"`, `"hjust"` vertical/horizontal position `[0.5, 0.5]`;
#'    * `"halign"` with horizontal alignment `[0.5]`;
#'    * `"rot"` label rotation `[0]`;
#'    * `"color"` label color `[black]`;
#'    * `"fontsize"` fontsize used by `grid::par()` `[14]`;
#'    * `"border"` border color, where `NA` draws no border `[NA]`;
#'    * `"lty"`, `"lwd"` line type and line width `[1, 1]`;
#'    * `"fill"` label background fill, where `NA` draws no background fill `[NA]`;
#'    * `"padding"`, `"padding_unit"` passed to `gridtext::richtext_grob()`
#'    to define padding around each label `[2, "pt"]`;
#'    * `"r"`, `"r_unit"` passed to `gridtext::richtext_grob()` to define
#'    rounded corners, relevant only when `"border"` or `"fill"` are
#'    not `NA` `[2, "pt"]`.
#'    * `"item_cex"` cex adjustment for item labels
#'    * `"show_label"` `logical` indicating whether to show each label
#'    * `"show_items"` `logical` indicating whether to show items
#'    
#' @param asp `numeric` value indicating the aspect ratio, passed
#'    to `plot()`. The default `asp=1` ensures that circles are
#'    always drawn with correct aspect ratio so they are displayed
#'    as proper circles.
#' @param xlim,ylim `numeric` range for x- and y-axis, respectively.
#'    When `xlim` or `ylim` are `NULL`, values are derived from the
#'    coordinates from `venn_spdf` and `label_df`.
#' @param xpd see `graphics::par()`, when `xpd=FALSE` it clips text
#'    labels and polygons to the plot boundary; when `xpd=TRUE` it
#'    clips to the figure region, and when `xpd=NA` it is clipped
#'    to the full device region. This option is mainly helpful
#'    as `xpd=NA` ensures labels are displayed even when they overlap
#'    the plot boundary.
#' @param plot_warning `logical` indicating whether to draw a text
#'    label on the bottom of the plot whenever a non-zero overlap
#'    count cannot be displayed given the `label_df` data. This
#'    occurs when a proportional Venn (Euler) diagram does not
#'    or cannot represent every possible overlap, causing some
#'    overlaps to be hidden from the plot.
#' @param label_style `character` string indicating the style of label
#'    to display. The values `"basic","none","shaded","lite","fill"`
#'    style the label background fill, while presence of `"box"` in
#'    the string will draw a border around the label:
#'    `"basic"` or `"none"` uses no background fill,
#'    `"lite"` uses lite background fill,
#'    `"fill"` uses opaque fill with the overlap set color,
#'    `"shaded"` uses slightly transparent fill with overlap set color,
#'    `"box"` displays border around the label.
#' @param fontfamily `character` string indicating the font family,
#'    which can be useful if Unicode characters are displayed as empty
#'    boxes instead of proper Unicode characters. This value may be
#'    device-dependent, for example fonts available for PDF output
#'    may differ from those for PNG.
#' @param plot_style `character` string indicating the style
#'    of plot: `"base"` uses base R graphics; `"gg"` uses
#'    ggplot2 graphics (not yet implemented).
#' @param ... additional arguments are passed to `plot()`
#'    when plotting `venn_spdf` which is expected to be a
#'    `sp::SpatialPolygonsDataFrame`.
#'    
#' @examples
#' setlist <- make_venn_test(100, 3, do_signed=TRUE);
#' venndir_output <- venndir(setlist, 1:2, overlap_type="each", do_plot=FALSE);
#' render_venndir(venndir_output);
#' render_venndir(venndir_output, plot_style="gg");
#' 
#' vo <- venndir(setlist, 1:2, overlap_type="each", do_plot=FALSE, show_items="sign item", show_set="all");
#' vo <- nudge_venndir_label(vo, set="set_A",
#'    label_type="main",
#'    x_offset=-0.5, y_offset=1.5)
#' vo <- nudge_venndir_label(vo, set="set_B",
#'    label_type="main",
#'    x_offset=0.5, y_offset=1.5)
#' vo <- nudge_venndir_label(vo, set="set_A&set_B",
#'    label_type="main",
#'    x_offset=0, y_offset=2)
#' vo$label_df$hjust[1:3] <- 0.5;
#' vo$label_df$vjust[1:3] <- c(0, 0, 0.5);
#' render_venndir(vo, show_set="all", show_items="sign item", draw_buffer=TRUE, buffer_w=0.3)
#' render_venndir(vo, show_set="all",
#'    label_style="lite_box",
#'    show_items="sign item", draw_buffer=TRUE, buffer_w=0.3)
#' render_venndir(vo, plot_style="gg", show_set="all",
#'    label_style="lite_box",
#'    show_items="sign item", draw_buffer=TRUE, buffer_w=0.3)
#' 
#' venndir_output$label_df[1,c("x", "y")] <- c(2.5, 6.5);
#' venndir_output$label_df[2,c("x", "y")] <- c(7.5, 6.5);
#' venndir_output$label_df[2,c("hjust")] <- c(0);
#' venndir_output$label_df[1:2,c("vjust")] <- c(0, 0);
#' render_venndir(venndir_output, font_cex=1.5);
#' 
#' venndir_output$label_df[1,c("x", "y")] <- c(2, 6);
#' venndir_output$label_df[2,c("x", "y")] <- c(8.3, 6);
#' render_venndir(venndir_output);
#' render_venndir(venndir_output, xpd=NA, xlim=c(2, 9));
#' 
#' @export
render_venndir <- function
(venndir_output=NULL,
 venn_spdf=NULL,
 label_df=NULL,
 asp=1,
 xlim=NULL,
 ylim=NULL,
 expand_fraction=0,
 xpd=NA,
 font_cex=1,
 item_cex=0.9,
 plot_warning=TRUE,
 show_label=NA,
 show_items=c(NA, "none", "sign item", "item", "sign"),
 show_zero=TRUE,
 item_angle=-18,
 display_counts=TRUE,
 max_items=100,
 label_style=c("custom",
    "basic",
    "fill",
    "shaded",
    "shaded_box",
    "lite",
    "lite_box"),
 fontfamily="Arial",
 plot_style=c("base", "gg"),
 ggtheme=ggplot2::theme_void,
 draw_buffer=FALSE,
 ...)
{
   if (length(venndir_output) > 0 && is.list(venndir_output)) {
      if (!any(c("venn_spdf", "label_df") %in% names(venndir_output))) {
         stop("List input must contain element names 'venn_spdf' or 'label_df'.");
      }
      if (!inherits(venndir_output[["venn_spdf"]], "SpatialPolygonsDataFrame")) {
         stop("Element 'venn_spdf' must inherit from 'SpatialPolygonsDataFrame'.");
      }
      venn_spdf <- venndir_output[["venn_spdf"]];
      if (!inherits(venndir_output[["label_df"]], "data.frame")) {
         stop("Element 'label_df' must inherit from 'data.frame'.");
      }
      label_df <- venndir_output[["label_df"]];
   }
   show_items <- head(show_items, 1);
   plot_style <- match.arg(plot_style);
   
   ## Determine suitable xlim and ylim
   if ((length(xlim) == 0 || length(ylim) == 0) && !"gg" %in% plot_style) {
      xlim_1 <- NULL;
      ylim_1 <- NULL;
      if (length(venn_spdf) > 0) {
         venn_bbox <- sp::bbox(venn_spdf);
         xlim_1 <- range(venn_bbox["x",], na.rm=TRUE);
         ylim_1 <- range(venn_bbox["y",], na.rm=TRUE);
      }
      if (length(label_df) > 0) {
         xlim_1 <- range(c(xlim_1, label_df$x), na.rm=TRUE);
         ylim_1 <- range(c(ylim_1, label_df$y), na.rm=TRUE);
      }
      if (length(xlim) == 0) {
         xlim <- expand_range(xlim_1, expand_fraction);
      }
      if (length(ylim) == 0) {
         ylim <- expand_range(ylim_1, expand_fraction);
      }
   }
   
   # Apply label_style
   if (!"custom" %in% label_style) {
      venndir_output <- venndir_label_style(list(venn_spdf=venn_spdf, label_df=label_df),
         label_style=label_style,
         ...);
      venn_spdf <- venndir_output$venn_spdf;
      label_df <- venndir_output$label_df;
   }
   
   # Process SpatialPolygonsDataFrame
   if (length(venn_spdf) > 0) {
      # Fill missing attribute colnames with default values
      venn_spdf_defaults <- c(
         alpha=venn_spdf$alpha,#jamba::col2alpha(venn_spdf$color),
         lwd=2,
         border=NA
      );
      venn_spdf_add <- setdiff(colnames(venn_spdf),
         names(venn_spdf_defaults));
      for (i in venn_spdf_add) {
         venn_spdf[[i]] <- rep(venn_spdf_defaults[[i]],
            nrow(venn_spdf));
      }
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
      
      ## Fill any missing optional colnames with defaults
      label_df_defaults <- list(
         type="main",
         show_label=NA,
         show_items=NA,
         item_angle=item_angle,
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
         padding=2,
         x_offset=0,
         y_offset=0,
         padding_unit="pt",
         r=2,
         segment_buffer=-0.05,
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
         show_zero <- TRUE;
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
      
      show_label <- rep(show_label, length.out=nrow(label_df));
      show_items <- rep(show_items, length.out=nrow(label_df));
      # build show_items
      show_items <- ifelse(
         is.na(show_items),
         label_df$show_items,
         show_items);
      if (!"items" %in% colnames(label_df)) {
         show_items <- rep(FALSE, length(show_items));
      } else {
         show_items <- ifelse(label_df$overlap_set %in% venn_spdf$label,
            show_items,
            FALSE);
         # max_items requires grouping by overlap_set
         items_by_set <- tapply(label_df$items, label_df$overlap_set, function(ldi){
            sum(lengths(ldi))
         });
         show_items <- ifelse(
            lengths(label_df$items) == 0 |
               items_by_set[label_df$overlap_set] > max_items,
            FALSE,
            show_items);
      }
      # show_items grouped by overlap_set
      show_items_by_set <- tapply(show_items, label_df$overlap_set, function(ldi){
         any(!ldi %in% c(NA, FALSE, "none"))
      })[label_df$overlap_set];
      
      # determine if label coordinates are inside the polygon
      sp_index <- match(label_df$overlap_set,
         venn_spdf$label);
      label_overlaps_poly <- sapply(seq_len(nrow(label_df)), function(i){
         if (is.na(sp_index[i])) {
            return(NA);
         }
         ixy <- cbind(
            sum(c(label_df$x[i],
               label_df$x_offset[i])),
            sum(c(label_df$y[i],
               label_df$y_offset[i])));
         if (any(is.na(ixy))) {
            return(NA);
         }
         spt <- sp::SpatialPoints(ixy);
         rgeos::gContains(venn_spdf[sp_index[i],], spt);
      });
      
      # build use_show_label
      show_label <- ifelse(
         show_label %in% NA,
         label_df$show_label,
         show_label);
      # if no x,y coordinates we cannot display the label
      show_label <- ifelse(
         is.na(label_df$x) | 
            is.na(label_df$y),
         FALSE,
         show_label);
      # if show_label=NA, show_items=TRUE -> show_label=FALSE
      # if show_label=NA, show_items=FALSE -> show_label=FALSE
      show_label <- ifelse(
         show_label %in% NA,
         ifelse(
            !show_items_by_set %in% c(FALSE,NA) & label_overlaps_poly,
            FALSE,
            TRUE),
         show_label);
      # hide zero label when show_zero=FALSE and venn_counts=0
      show_label <- ifelse(
         show_label %in% TRUE & 
            show_zero %in% FALSE & 
            label_df$venn_counts == 0,
         FALSE,
         show_label);
      
      # adjust remaining show_items=NA to FALSE
      show_items <- ifelse(
         show_items %in% NA,
         FALSE,
         show_items);
      # replace TRUE with "sign item" as a default
      show_items <- ifelse(
         show_items %in% TRUE,
         "sign item",
         show_items);
      show_items <- ifelse(
         label_df$type %in% "main" &
            !show_items %in% c(NA, FALSE),
         gsub("[ ]*sign[ ]*", "", show_items),
         show_items);
      label_df$show_label <- show_label;
      label_df$show_items <- show_items;

      
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
         if (plot_warning) {
            warning_label <- paste0(warning_base,
               "\n",
               jamba::cPaste(warn_labels,
                  sep="; "));
         }
      }
      
      # display labels
      g_labels <- NULL;
      segment_df <- NULL;
      if (any(show_label)) {
         # Determine if any offset labels require line segment
         has_offset <- (label_df$x_offset != 0 | 
               label_df$y_offset != 0);
         if (any(show_label & has_offset)) {
            use_offset <- (show_label & has_offset);
            offset_sets <- label_df$overlap_set[use_offset];
            sp_index <- match(offset_sets, venn_spdf$label);
            segment_buffer <- ifelse(show_items_by_set %in% c(NA, FALSE),
               (label_df$segment_buffer + -1) / 3,
               label_df$segment_buffer);
            test_xy <- data.frame(
               x0=label_df$x[use_offset] + label_df$x_offset[use_offset],
               x1=label_df$x[use_offset],
               y0=label_df$y[use_offset] + label_df$y_offset[use_offset],
               y1=label_df$y[use_offset],
               segment_buffer=jamba::rmNA(segment_buffer[use_offset], naValue=0),
               sp_index=sp_index,
               label_color=label_df$color[use_offset],
               label_fill=label_df$fill[use_offset],
               label_border=label_df$border[use_offset],
               poly_color=venn_spdf$color[sp_index],
               poly_border=venn_spdf$border[sp_index]);

            sp_list <- lapply(sp_index, function(i){
               venn_spdf[i,]});
            new_xy <- polygon_label_segment(
               x0=test_xy$x0,
               x1=test_xy$x1,
               y0=test_xy$y0,
               y1=test_xy$y1,
               sp=sp_list,
               sp_buffer=test_xy$segment_buffer,
               verbose=FALSE);
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
               seg_colors <- apply(test_xy[has_segment,sc_cols,drop=FALSE], 1, function(sc1){
                  head(jamba::rmNA(sc1), 1)
               });
               segment_df <- data.frame(
                  x=as.vector(rbind(test_xy$x0[has_segment], new_xy[,1][has_segment])),
                  y=as.vector(rbind(test_xy$y0[has_segment], new_xy[,2][has_segment])),
                  group=rep(venn_spdf$label[test_xy$sp_index[has_segment]], each=2),
                  color=rep(seg_colors, each=2),
                  lwd=rep(venn_spdf$lwd[test_xy$sp_index[has_segment]], each=2),
                  point_order=c(1, 2)
               );
            }
         }
         
      }
   }
   
   ## Draw item labels
   itemlabels_df <- NULL;
   if (any(!show_items %in% FALSE)) {
      items_dfs <- subset(label_df, !show_items %in% FALSE);
      items_dfs <- split(items_dfs, items_dfs$overlap_set);
      
      #for (items_df1 in items_dfs) {
      itemlabels_list <- lapply(items_dfs, function(items_df1){
         items <- unname(unlist(jamba::mixedSorts(items_df1$items)));
         color1 <- rep(items_df1$color, lengths(items_df1$items));
         vi <- which(data.frame(venn_spdf)$label %in% items_df1$overlap_set);
         vdf <- data.frame(venn_spdf)[vi,,drop=FALSE];
         prefixes <- rep(
            gsub(":.+", "", items_df1$text),
            lengths(items_df1$items));
         labels <- NULL;
         # note currently uses the same show_items format per polygon
         # not for each row in items_dfs, so it is not possible to
         # use different show_items format for up-up and down-down within
         # the same polygon
         show_items_order <- strsplit(items_df1$show_items[1], "[- _.]")[[1]];
         for (dio in show_items_order) {
            if (grepl("sign", dio)) {
               labels <- paste(labels, prefixes);
            } else if (grepl("item", dio)) {
               labels <- paste(labels, items);
            }
         }
         labels <- gsub("^[ ]+|[ ]+$", "", labels);
         bg <- jamba::alpha2col(vdf$color, vdf$alpha)
         color <- make_color_contrast(color1,
            bg,
            ...);
         
         lpf <- label_polygon_fill(sp=venn_spdf[vi,],
            ref_sp=venn_spdf,
            color=color,
            cex=items_df1$item_cex[1],
            draw_points=FALSE,
            labels=labels,
            plot_style="none",
            draw_labels=FALSE,
            angle=items_df1$item_angle[1],
            ...);
      });
      # combine item label into one data.frame
      itemlabels_df <- jamba::rbindList(lapply(itemlabels_list, function(i1){
         i1$items_df;
      }));
      itemlabels_sp <- lapply(itemlabels_list, function(i1){
         if (!grepl("Spatial", class(i1$sp_buffer))) {
            return(NULL)
         }
         i1$sp_buffer
      });
   }
   
   # Render the different aspects of the venndir plot
   if ("base" %in% plot_style) {
      # Plot the Venn polygons
      if (length(venn_spdf) > 0) {
         sp::plot(venn_spdf,
            asp=asp,
            col=jamba::alpha2col(venn_spdf$color,
               alpha=venn_spdf$alpha),
            lwd=venn_spdf$lwd,
            lty=venn_spdf$lty,
            border=venn_spdf$border,
            xlim=xlim,
            ylim=ylim,
            xpd=xpd,
            ...);
      }
      # Display the warning text label
      if (length(warning_label) > 0) {
         cp <- jamba::coordPresets("bottom");
         jamba::drawLabels(
            x=cp$x,
            y=cp$y,
            adjX=cp$adjX,
            adjY=0.5,
            #preset="bottom",
            txt=warning_label,
            labelCex=1,
            xpd=NA,
            boxColor="#FFFFFFAA",
            boxBorderColor="#999999AA",
         );
      }
      # draw label line segments if needed
      if (length(segment_df) > 0) {
         #jamba::printDebug("segment_df:");print(segment_df);
         segment_df1 <- subset(segment_df, point_order %in% 1);
         segment_df2 <- subset(segment_df, point_order %in% 2);
         # make unique data.frame to avoid overplotting the same line
         segment_wide <- unique(data.frame(
            x0=segment_df1$x,
            x1=segment_df2$x,
            y0=segment_df1$y,
            y1=segment_df2$y,
            color=segment_df1$color,
            lwd=segment_df1$lwd));
         xpd1 <- par(xpd=NA);
         segments(
            x0=segment_wide$x0,
            x1=segment_wide$x1,
            y0=segment_wide$y0,
            y1=segment_wide$y1,
            col=jamba::makeColorDarker(segment_wide$color,
               darkFactor=1.2),
            lwd=segment_wide$lwd);
         par(xpd1);
      }
      # display count/set labels
      g_labels <- NULL;
      if (any(show_label)) {
         g_labels <- gridtext::richtext_grob(
            text=label_df$text[show_label],
            x=grid::unit(label_df$x[show_label] +
                  label_df$x_offset[show_label],
               "native"),
            y=grid::unit(label_df$y[show_label] +
                  label_df$y_offset[show_label],
               "native"),
            default.units="native",
            vjust=label_df$vjust[show_label],
            hjust=label_df$hjust[show_label],
            halign=label_df$halign[show_label],
            rot=label_df$rot[show_label],
            padding=grid::unit(label_df$padding[show_label],
               label_df$padding_unit[show_label]),
            r=grid::unit(label_df$r[show_label],
               label_df$r_unit[show_label]),
            gp=grid::gpar(
               fontfamily=fontfamily,
               col=label_df$color[show_label],
               fontsize=label_df$fontsize[show_label] * font_cex
            ),
            box_gp=grid::gpar(
               col=label_df$border[show_label],
               fill=label_df$fill[show_label],
               lty=label_df$lty[show_label],
               lwd=label_df$lwd[show_label])
         );
         if (length(dev.list()) > 0) {
            vps <- gridBase::baseViewports();
            grid::pushViewport(vps$inner, vps$figure, vps$plot);
            grid::grid.draw(g_labels);
            grid::popViewport(3);
         }
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
      }
   } else if ("gg" %in% plot_style) {
      # create ggplot2 output
      # convert spdf to sf
      vosf <- sf::st_as_sf(venn_spdf);
      # Venn overlap polygons
      ggv <- ggplot2::ggplot(data=vosf) + 
         ggplot2::geom_sf(
            ggplot2::aes(fill=jamba::alpha2col(color, alpha=alpha),
               color=border)) + 
         ggplot2::scale_fill_identity() +
         ggplot2::scale_color_identity();
      if (length(ggtheme) > 0) {
         ggv <- ggv + ggtheme();
      }
      # optional segment to count labels
      if (length(segment_df) > 0) {
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
      # count labels
      if (any(show_label)) {
         show_label_df <- subset(label_df, show_label %in% c(TRUE));
         ggv <- ggv + ggtext::geom_richtext(
            data=show_label_df,
            ggplot2::aes(
               x=x + x_offset,
               y=y + y_offset,
               label=text,
               group=overlap_set,
               hjust=hjust,
               vjust=vjust,
               #halign=halign,
               family=fontfamily,
               text.colour=color,
               fill=fill,
               label.colour=border),
            label.padding=grid::unit(show_label_df$padding,
               show_label_df$padding_unit),
            label.r=grid::unit(show_label_df$r,
               show_label_df$r_unit),
            size=show_label_df$fontsize * 5/14 * font_cex)
      }
      # optional xlim, ylim
      if (length(xlim) > 0 & length(ylim) > 0) {
         jamba::printDebug("xlim:", xlim, ", ylim:", ylim);
         ggv <- ggv + ggplot2::coord_sf(xlim=xlim, ylim=ylim);
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
         ggitems <- ggtext::geom_richtext(
            data=itemlabels_df,
            ggplot2::aes(x=x,
               y=y,
               label=text,
               #group=group,
               angle=-rot,
               hjust=0.5,
               vjust=0.5,
               #halign=0.5,
               text.colour=color,
               fill=NA,
               label.colour=NA),
            size=itemlabels_df$fontsize * 5/14);
         ggv <- ggv + ggitems;
      }
      print(ggv);
      return(ggv);
   }
   
   
   return(invisible(list(venn_spdf=venn_spdf,
      label_df=label_df,
      g_labels=g_labels)));
}

#' Render Venn or Euler diagram using ggplot2
#' 
#' Render Venn or Euler diagram using ggplot2
#' 
#' This function is intended to render a Venn or Euler diagram
#' directly from the output of `venndir()` to allow customization
#' of various aspects of the figure.
#' 
#' 
#' @inheritParams render_venndir
#' 
#' @examples
#' options("warn"=-1); # oml the warnings
#' 
#' setlist <- make_venn_test(100, 3, do_signed=TRUE);
#' venndir_output <- venndir(setlist, 1:2, overlap_type="each", do_plot=FALSE);
#' ggrender_venndir(venndir_output);
#' 
#' venndir_output$label_df[1,c("x", "y")] <- c(2.5, 6.5);
#' venndir_output$label_df[2,c("x", "y")] <- c(7.5, 6.5);
#' venndir_output$label_df[2,c("hjust")] <- c(0);
#' venndir_output$label_df[1:2,c("vjust")] <- c(0, 0);
#' ggrender_venndir(venndir_output, font_cex=1.5, family="Arial");
#' 
ggrender_venndir <- function
(venndir_output=NULL,
 venn_spdf=NULL,
 label_df=NULL,
 xlim=NULL,
 ylim=NULL,
 xpd=NA,
 font_cex=1,
 plot_warning=TRUE,
 show_items=c("none", "sign label", "label", "sign"),
 item_angle=20,
 display_counts=TRUE,
 max_items=1000,
 ggtheme=ggplot2::theme_void,
 family="Arial",
 ...)
{
   has_deps <- sapply(c("sf", "ggplot2", "ggtext"), function(i){
      suppressPackageStartupMessages(require(i, character.only=TRUE))
   });
   if (any(!has_deps)) {
      stop(paste0("ggrender_venndir() requires: ",
         paste(names(has_deps)[!has_deps], collapse=", ")));
   }
   
   if (length(venndir_output) > 0 && is.list(venndir_output)) {
      if (!any(c("venn_spdf", "label_df") %in% names(venndir_output))) {
         stop("List input must contain element names 'venn_spdf' or 'label_df'.");
      }
      if (!inherits(venndir_output[["venn_spdf"]], "SpatialPolygonsDataFrame")) {
         stop("Element 'venn_spdf' must inherit from 'SpatialPolygonsDataFrame'.");
      }
      venn_spdf <- venndir_output[["venn_spdf"]];
      if (!inherits(venndir_output[["label_df"]], "data.frame")) {
         stop("Element 'label_df' must inherit from 'data.frame'.");
      }
      label_df <- venndir_output[["label_df"]];
   }
   show_items <- head(show_items, 1);
   
   ## Convert to sf
   vosf <- st_as_sf(venn_spdf);
   
   ggv <- ggplot2::ggplot(data=vosf) + 
      ggplot2::geom_sf(
         aes(fill=jamba::alpha2col(color, alpha=alpha),
            color=border)) + 
      ggplot2::scale_fill_identity() +
      ggplot2::scale_color_identity() +
      ggtheme()

   if (length(label_df) > 0 &&
         any(label_df$show_label) &&
         "none" %in% show_items) {
      label_df <- subset(label_df, show_label);
      ggv <- ggv + ggtext::geom_richtext(
         data=label_df,
         aes(x=x,
            y=y,
            label=text,
            group=overlap_set,
            hjust=hjust,
            vjust=vjust,
            #halign=halign,
            family=family,
            text.colour=color,
            fill=fill,
            label.colour=border),
         label.padding=grid::unit(label_df$padding,
            label_df$padding_unit),
         label.r=grid::unit(label_df$r,
            label_df$r_unit),
         size=label_df$fontsize * 5/14 * font_cex)
   }
   
   if (length(xlim) > 0 || length(ylim) > 0) {
      ggv <- ggv + ggplot2::coord_sf(xlim=xlim, ylim=ylim);
   }

   # optionally display items
   ## Draw item labels
   if (!"none" %in% show_items && "items" %in% colnames(data.frame(label_df, check.names=FALSE, stringsAsFactors=FALSE))) {
      jamba::printDebug("ggrender_venndir(): ", "show_items:", show_items);
      label_dfs <- subset(label_df, lengths(label_df$items) > 0);
      label_dfs <- split(label_dfs, label_dfs$overlap_set);
      label_df1 <- label_dfs[[2]];
      show_items_order <- strsplit(show_items, "[- _.]")[[1]];
      #for (label_df1 in label_dfs) {
      items_df <- jamba::rbindList(lapply(names(label_dfs), function(iname) {
         label_df1 <- label_dfs[[iname]];
         items <- unname(unlist(jamba::mixedSorts(label_df1$items)));
         color1 <- rep(label_df1$color, lengths(label_df1$items));
         vi <- which(data.frame(venn_spdf)$label %in% label_df1$overlap_set);
         vdf <- data.frame(venn_spdf)[vi,,drop=FALSE];
         prefixes <- rep(
            gsub(":.+", "", label_df1$text),
            lengths(label_df1$items));
         labels <- NULL;
         for (dio in show_items_order) {
            if (grepl("sign", dio)) {
               labels <- paste(labels, prefixes);
            } else if (grepl("item", dio)) {
               labels <- paste(labels, items);
            }
         }
         labels <- gsub("^[ ]+|[ ]+$", "", labels);
         #labels <- paste(prefixes,
         #   items);
         #labels <- prefixes;
         bg <- jamba::alpha2col(vdf$color, vdf$alpha)
         color <- make_color_contrast(color1, bg)
         
         lpf <- label_polygon_fill(sp=venn_spdf[vi,],
            ref_sp=venn_spdf,
            color=color,
            cex=1,
            draw_points=FALSE,
            polygon_scale=-0.01,
            labels=labels,
            angle=item_angle,
            draw_labels=FALSE)$items_df;
         lpf$group <- iname;
         lpf;
      }));
      
      ## create ggtext geom
      ggitems <- ggtext::geom_richtext(
         data=items_df,
         aes(x=x,
            y=y,
            label=text,
            group=group,
            angle=rot,
            hjust=0.5,
            vjust=0.5,
            #halign=0.5,
            text.colour=color,
            fill=NA,
            label.colour=NA),
         #label.padding=grid::unit(label_df$padding,
         #   label_df$padding_unit),
         #label.r=grid::unit(label_df$r,
         #   label_df$r_unit),
         size=items_df$fontsize * 5/14 * font_cex);
      ggv <- ggv + ggitems;
      
   }
   if (length(family) > 0) {
      ggv <- ggv + ggplot2::theme(text=ggplot2::element_text(family=family));
   }
      
   return(ggv);
}

#' Nudge venndir label
#' 
#' @family venndir utility
#' 
#' @examples
#' setlist <- make_venn_test(100, 3, do_signed=TRUE)
#' 
#' vo <- venndir(setlist,
#'    overlap_type="each",
#'    return_items=TRUE,
#'    label_style="lite_box",
#'    main="Default venndir")
#' 
#' vo <- nudge_venndir_label(vo,
#'    set=c("set_A&set_B&set_C"),
#'    x_offset=3,
#'    y_offset=-1)
#' render_venndir(vo,
#'    main="After nudging one count label")
#' 
#' @export
nudge_venndir_label <- function
(venndir_output,
 set=NULL,
 x_offset=0,
 y_offset=0,
 unit_type=c("absolute", "relative"),
 label_types=c("main", "signed"),
 show_label=TRUE,
 ...)
{
   if (!any(set %in% venndir_output$label_df$overlap_set)) {
      warning(paste0("No values found in label_df$overlap_set using set:",
         jamba::cPaste(paste0("'", set, "'"))));
      return(venndir_output);
   }
   unit_type <- match.arg(unit_type);
   x_offset <- rep(x_offset, length.out=length(set));
   y_offset <- rep(y_offset, length.out=length(set));
   show_label <- rep(show_label, length.out=length(set));
   
   # adjust by unit_type "relative" by using the bounding box dimensions
   if ("relative" %in% unit_type) {
      unit_scalars <- apply(sp::bbox(venndir_output$venn_spdf), 1, diff);
      x_offset <- x_offset * unit_scalars[1];
      y_offset <- y_offset * unit_scalars[2];
   }
   use_set <- which(set %in% venndir_output$label_df$overlap_set);

   # create new column if needed
   if (!"x_offset" %in% colnames(venndir_output$label_df)) {
      venndir_output$label_df$x_offset <- 0;
   }
   if (!"y_offset" %in% colnames(venndir_output$label_df)) {
      venndir_output$label_df$y_offset <- 0;
   }
   
   # adjust each set x_offset, y_offset
   for (i in use_set) {
      iset <- set[i];
      voi <- which(venndir_output$label_df$overlap_set %in% iset &
            venndir_output$label_df$type %in% label_types);
      venndir_output$label_df$x_offset[voi] <- x_offset[i];
      venndir_output$label_df$y_offset[voi] <- y_offset[i];
      venndir_output$label_df$show_label[voi] <- show_label[i];
   }
   return(venndir_output);
}


#' venndir label style
#' 
#' venndir label style
#' 
#' This function applies a label style to `venndir()` output,
#' overwriting the existing label style as necessary.
#' 
#' This function adjusts the label text color for contrast with
#' `make_color_contrast()`, which is useful when positioning
#' the label on top of dark or bright colors. If the label
#' is positioned outside the `sp` polygons, the text is assumed
#' to be on a white background, using argument `bg`.
#' 
#' @family venndir utility
#' 
#' @param label_style `character` string indicating the style of label
#'    to display. The values `"basic","none","shaded","lite","fill"`
#'    style the label background fill, while presence of `"box"` in
#'    the string will draw a border around the label:
#'    `"basic"` or `"none"` uses no background fill,
#'    `"lite"` uses lite background fill,
#'    `"fill"` uses opaque fill with the overlap set color,
#'    `"shaded"` uses slightly transparent fill with overlap set color,
#'    `"box"` displays border around the label.
#' 
#' @export
venndir_label_style <- function
(venndir_output,
 label_style=c("basic",
    "fill",
    "shaded",
    "shaded_box",
    "lite",
    "lite_box"),
 lite="#FFEEAABB",
 bg="white",
 ...)
{
   # TODO:
   # check whether label coordinate overlaps the polygon
   # if yes then use contrasting text with combined background colors
   # if no then assume white background
   
   label_style <- head(label_style, 1);
   
   # match rows in label_df with venn_spdf
   sp_index <- match(venndir_output$label_df$overlap_set,
      venndir_output$venn_spdf$label);
   
   # determine whether label coordinates overlap a polygon
   # xy_overlaps is NA when the point does not overlap a polygon,
   # and integer rownum when it overlaps a polygon
   xy_overlaps <- sapply(seq_len(nrow(venndir_output$label_df)), function(i){
      ixy <- cbind(
         sum(c(venndir_output$label_df$x[i],
            venndir_output$label_df$x_offset[i])),
         sum(c(venndir_output$label_df$y[i],
            venndir_output$label_df$y_offset[i])));
      if (any(is.na(ixy))) {
         return(NA)
      }
      spt <- sp::SpatialPoints(ixy)
      for (j in seq_len(nrow(venndir_output$venn_spdf))) {
         sp <- venndir_output$venn_spdf[j,];
         if (rgeos::gContains(sp, spt)) {
            return(j);
         }
      }
      return(NA)
   });
   #venndir_output$label_df$xy_overlaps <- xy_overlaps;

   label_bg <- ifelse(is.na(xy_overlaps),
      "white",
      venndir_output$venn_spdf$color[xy_overlaps])

   # box is darker version of polygon color with alpha=0.8
   if (grepl("box", label_style)) {
      venndir_output$label_df$border <- jamba::alpha2col(alpha=0.8,
         jamba::makeColorDarker(venndir_output$venn_spdf$color[sp_index],
            darkFactor=1.2));
   } else {
      venndir_output$label_df$border <- NA;
   }
   
   if (grepl("fill", label_style)) {
      # fill matches polygon fill, label is contrasting white or black color
      venndir_output$label_df$fill <- venndir_output$venn_spdf$color[sp_index];
      venndir_output$label_df$color <- ifelse(
         venndir_output$label_df$type %in% "main",
         jamba::setTextContrastColor(venndir_output$venn_spdf$color[sp_index]),
         make_color_contrast(
            x=venndir_output$label_df$color,
            y=venndir_output$label_df$fill,
            bg=label_bg));
   } else if (grepl("shaded", label_style)) {
      # shaded uses polygon fill with alpha=0.5
      venndir_output$label_df$fill <- jamba::alpha2col(
         venndir_output$venn_spdf$color[sp_index],
         alpha=0.5);
      # note: need to revisit how to combine two overlapping alpha values
      # (1 - x1) * x2 + x1
      venndir_output$label_df$color <- ifelse(
         venndir_output$label_df$type %in% "main",
         jamba::setTextContrastColor(venndir_output$label_df$fill),
         make_color_contrast(
            x=venndir_output$label_df$color,
            y=venndir_output$label_df$fill,
            bg=label_bg));
   } else if (grepl("lite", label_style)) {
      # lite uses lite color fill with slight transparency
      venndir_output$label_df$fill <- rep(lite,
         nrow(venndir_output$label_df));
      venndir_output$label_df$color <- ifelse(
         venndir_output$label_df$type %in% "main",
         jamba::setTextContrastColor(venndir_output$label_df$fill),
         make_color_contrast(
            x=venndir_output$label_df$color,
            y=venndir_output$label_df$fill,
            bg=label_bg));
   } else if (grepl("none|basic", label_style)) {
      # none removes fill
      venndir_output$label_df$fill <- NA;
      venndir_output$label_df$color <- ifelse(
         venndir_output$label_df$type %in% "main",
         jamba::setTextContrastColor(label_bg),
         make_color_contrast(
            x=venndir_output$label_df$color,
            y=label_bg,
            bg=bg));
   }
   
   return(venndir_output);
}


#' Expand numeric range
#' 
#' Expand numeric range
#' 
#' This function takes a `numeric` range (or numeric vector
#' and calculates its range) and expands this range by
#' a fraction given by `expand_fraction`.
#' 
#' When the input range is zero, the minimum absolute range
#' can be given by `minimum_range`.
#' 
#' The input may be a `list` that contains `numeric` vectors,
#' in which case the `list` will be iterated to produce an
#' expanded range for each `numeric` vector. Each `numeric`
#' vector is expanded independently.
#' 
#' This function is intended to be a simple technique to expand
#' x-axis and y-axis ranges of a graphical plot.
#' 
#' @return `numeric` vector, or when input `x` is `list` the
#'    output will also be a `list` of `numeric` vectors.
#'    The numeric vector will contain the range after
#'    expansion.
#' 
#' @family venndir utility
#' 
#' @param x `numeric` vector, or `list` of `numeric` vectors.
#'    The input is converted to a range using `range(x, na.rm=TRUE)`
#'    to ensure no `NA` values are included. Note that this
#'    step will force the range to be in ascending order.
#' @param expand_fraction `numeric` value indicating the
#'    fraction of the range defined by `diff(range(x, na.rm=TRUE))`
#'    to add to the total range.
#' @param minimum_range `numeric` value indicating the minimum
#'    range of the output, useful when the input has zero
#'    range, for example if `x=c(10, 10)`.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' x <- c(1, 10);
#' expand_range(x, 0.1)
#' diff(x);
#' diff(expand_range(x, 0.1));
#' 
#' # input with no range is extended to some minimum value
#' expand_range(1, minimum_range=1)
#' 
#' # list input is iterated, for example xlim, ylim
#' xlim <- c(1, 10)
#' ylim <- c(1, 100)
#' expand_range(list(xlim=xlim, ylim=ylim))
#' 
#' @export
expand_range <- function
(x,
 expand_fraction=0.1,
 minimum_range=0.01,
 ...)
{
   if (is.list(x)) {
      x <- lapply(x, function(xi){
         expand_range(x=xi,
            expand_percent=expand_percent,
            minimum_range=minimum_range,
            ...)
      });
      return(x);
   }
   if (!is.numeric(x)) {
      stop("x must be a numeric vector, or a list of numeric vectors.");
   }
   x <- range(x, na.rm=TRUE);
   xdiff <- diff(x);
   xmean <- mean(x);
   if (xdiff == 0) {
      minimum_range <- head(minimum_range, 1);
      x_expand <- minimum_range * c(-0.5, 0.5);
   } else {
      expand_fraction <- head(expand_fraction, 1);
      x_expand <- xdiff * expand_fraction * c(-0.5, 0.5);
   }
   x <- x + x_expand;
   return(x);
}

#' Degrees to text adjustment
#' 
#' Degrees to text adjustment
#' 
#' Utility function to define `adj` values suitable
#' for text plotting, which arranges text relative
#' to the angle in degrees.
#' 
#' @family venndir utility
#' 
#' @param degrees `numeric` value for angles in degrees
#' @param top `numeric` value indicating the angle at the
#'    top position
#' @param clockwise `logical` indicating whether the angle
#'    increases in clockwise direction
#' @param expand `numeric` value intended to expand the adjust
#'    value. For example `expand=0.5` will expand the adjust
#'    value 50%.
#' @param ... additional arguments are ignored
#' 
#' @examples
#' degrees <- seq(from=1, to=360, by=33);
#' adjdf <- degrees_to_adj(degrees);
#' x <- cos(jamba::deg2rad(degrees));
#' y <- sin(jamba::deg2rad(degrees));
#' plot(x, y,
#'    pch=20, asp=1,
#'    xlim=c(-1.3, 1.3),
#'    ylim=c(-1.3, 1.3));
#' for (i in seq_along(degrees)) {
#'    text(labels=i,
#'       x=x[i], y=y[i],
#'       adj=unlist(adjdf[i,]))
#' }
#' 
#' @export
degrees_to_adj <- function
(degrees,
 top=90,
 clockwise=FALSE,
 expand=0,
 ...)
{
   if (length(top) != 1) {
      top <- 90
   }
   if (length(expand) == 0) {
      expand <- 0;
   }
   expand <- rep(expand, length.out=2);
   degrees <- degrees + (top - 90)
   if (length(clockwise) && any(clockwise)) {
      degrees <- -degrees1;
   }
   degrees <- degrees %% 360;
   degreebreaks <- seq(from=-45/2,
      to=360 + 45/2,
      by=45);
   degreecut <- cut(degrees,
      degreebreaks,
      labels=c("right", "topright", "top",
         "topleft", "left", "bottomleft",
         "bottom", "bottomright", "right"));
   degreeset <- as.character(degreecut);
   adjx <- ifelse(grepl("right", degreeset),
      0,
      ifelse(grepl("left", degreeset),
         1,
         0.5));
   if (expand[1] != 0) {
      adjx <- (adjx - 0.5) * (1 + expand[1]) + 0.5;
   }
   adjy <- ifelse(grepl("top", degreeset),
      0,
      ifelse(grepl("bottom", degreeset),
         1,
         0.5));
   if (expand[2] != 0) {
      adjy <- (adjy - 0.5) * (1 + expand[2]) + 0.5;
   }
   data.frame(adjx=adjx, adjy=adjy);
}
