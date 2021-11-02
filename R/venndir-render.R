
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
#' @param expand_fraction `numeric` vector expanded to length 4,
#'    used to expand the `xlim` and `ylim` ranges, where the vector
#'    corresponds to: bottom, left, top, right.
#' @param xpd see `graphics::par()`, when `xpd=FALSE` it clips text
#'    labels and polygons to the plot boundary; when `xpd=TRUE` it
#'    clips to the figure region, and when `xpd=NA` it is clipped
#'    to the full device region. This option is mainly helpful
#'    as `xpd=NA` ensures labels are displayed even when they overlap
#'    the plot boundary.
#' @param font_cex `numeric` value used to resize text font used in
#'    Venn overlap labels overall. A value `font_cex=1.2` will make
#'    text labels 20% larger than normal.
#' @param item_cex `numeric` value used to resize item labels,
#'    used when `show_items` is used. This value can be supplied as
#'    a vector, in which case it will be applied to each polygon
#'    in the order they are drawn, typically the order presented
#'    in `label_df`.
#' @param plot_warning `logical` indicating whether to draw a text
#'    label on the bottom of the plot whenever a non-zero overlap
#'    count cannot be displayed given the `label_df` data. This
#'    occurs when a proportional Venn (Euler) diagram does not
#'    or cannot represent every possible overlap, causing some
#'    overlaps to be hidden from the plot.
#' @param show_items `character` indicating how to display item labels,
#'    relevant when items are contained in `label_df` and when a
#'    relevant label preset is used that includes item display,
#'    for example `label_preset="main items"` and `show_items="item"`.
#'    The `show_items` character string can include `"item"` and/or
#'    `"sign"` as substrings, for example `show_items="sign item"`
#'    or `show_items="item sign"` or `show_items="sign'"`.
#' @param item_degrees `numeric` value used to adjust the angle of
#'    item labels within each polygon. It can be useful to adjust
#'    the angle of text as a method of reducing label overlaps.
#'    Another alternative is to pass argument `layout_degrees`
#'    through `...` to `polygon_label_fill()`, which adjusts the
#'    angle of the label points inside each polygon.
#' @param max_items `numeric` value indicating the maximum number
#'    of item labels to display in each polygon, after which the
#'    only the count summary is displayed. (This argument is not
#'    yet implemented.)
#' @param show_zero `logical` indicating whether to display `0` zero
#'    in overlap regions that contain no items. For some diagrams
#'    it can look cleaner not to display the zero, with
#'    `show_zero=FALSE`.
#' @param show_segments `logical` indicating whether to draw a line
#'    segment from labels outside their respective polygons into
#'    the polygon.
#' @param segment_buffer `numeric` value indicating the buffer size
#'    inside a polygon when drawing a line segment, when
#'    `show_segments=TRUE`. A value `segment_buffer=-0.5` will create
#'    an internal polygon buffer half the size it would take to remove
#'    the polygon, and the line segment will be drawn to this border.
#'    A value `segment_buffer=-1` will draw the line segment very near
#'    the center of the polygon. A value `segment_buffer=0` will
#'    draw the line segment to the border of the polygon, however
#'    in some cases two polygon borders can be near each other, or
#'    overlap, so this setting can be adjusted accordingly.
#' @param label_style `character` string indicating the style of label
#'    to display, passed to `venndir_label_style()`.
#'    The values `"basic","none","shaded","lite","fill"`
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
#' @param inside_percent_threshold `numeric` value indicating the percent
#'    threshold, below which a polygon label is moved outside the polygon
#'    by default. When a count label is automatically positioned outside
#'    or inside a polygon, this argument defines the threshold for that
#'    decision.
#'    The threshold is calculated by area of the polygon
#'    divided by total area of the enclosing polygon, multiplied by 100.
#'    Therefore `inside_percent_threshold=5` will require a polygon to
#'    represent at least 5 percent of the total area.
#' @param plot_style `character` string indicating the style
#'    of plot: `"base"` uses base R graphics; `"gg"` uses
#'    ggplot2 graphics (not yet implemented).
#' @param group_labels `logical` to enable a relatively new feature that
#'    groups multiple `gridtext::richtext_grob()` elements together
#'    by overlap set and position, so there is one border and fill
#'    color for the set of labels. This feature is currently only
#'    implemented for base R plots, and not yet available for
#'    `plot_style="gg"` using ggplot2.
#' @param ggtheme `function` that outputs class `"theme", "gg"`,
#'    compatible with output from `ggplot2::theme_void()`. This argument
#'    is used to define the ggplot2 theme, when `plot_style="gg"`.
#' @param draw_buffer `logical` indicating whether to draw the item
#'    buffer used to determine item label positions inside each
#'    polygon, only relevant when `label_preset` includes items,
#'    and `show_items` is active.
#' @param ... additional arguments are passed to `plot()`
#'    when plotting `venn_spdf` which is expected to be a
#'    `sp::SpatialPolygonsDataFrame`.
#'    
#' @examples
#' setlist <- make_venn_test(100, 3, do_signed=TRUE);
#' vo <- venndir(setlist, 1:2, overlap_type="each", do_plot=FALSE);
#' render_venndir(vo);
#' render_venndir(vo, plot_style="gg");
#' 
#' vo <- venndir(setlist, 1:2, overlap_type="each", do_plot=FALSE,
#'    label_preset="main items",
#'    show_items="sign item");
#' render_venndir(vo,
#'    show_items="sign",
#'    draw_buffer=TRUE, buffer_w=0.2)
#'    
#' # turn off line segments, increase item inside buffer width
#' render_venndir(vo,
#'    show_segments=FALSE,
#'    label_preset="main items",
#'    label_style="lite_box",
#'    show_items="sign item",
#'    buffer_w=0.5)
#' render_venndir(vo, plot_style="gg", show_set="all",
#'    expand_fraction=c(0, 0.2, 0, 0.2),
#'    label_preset="main items",
#'    label_style="lite_box",
#'    show_items="sign item", buffer_w=0.5)
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
 show_items=c(NA,
    "none",
    "sign item",
    "item",
    "sign"),
 item_degrees=0,
 max_items=100,
 show_zero=TRUE,
 show_segments=TRUE,
 segment_buffer=-0.2,
 label_style=c("custom",
    "basic",
    "fill",
    "shaded",
    "shaded_box",
    "lite",
    "lite_box"),
 fontfamily="Arial",
 inside_percent_threshold=5,
 plot_style=c("base", "gg"),
 item_style=c("text", "gridtext"),
 group_labels=TRUE,
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
   item_style <- match.arg(item_style);

   # Apply label_style
   if (!"custom" %in% label_style) {
      venndir_output <- venndir_label_style(
         list(venn_spdf=venn_spdf, label_df=label_df),
         label_style=label_style,
         inside_percent_threshold=inside_percent_threshold,
         show_zero=show_zero,
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
         padding=2,
         x_offset=0,
         y_offset=0,
         padding_unit="pt",
         r=2,
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
      
      show_label <- (label_df$overlap %in% c("outside", "inside") |
            label_df$count %in% c("outside", "inside"));

      
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
      
      # prepare line segments for labels outside their respective polygons
      g_labels <- NULL;
      segment_df <- NULL;
      if (any(show_label)) {
         label_outside <- (label_df$overlap %in% "outside" | label_df$count %in% "outside");
         
         # Determine if any offset labels require line segment
         has_offset <- label_outside & (label_df$x_offset != 0 | label_df$y_offset != 0);
         if (any(show_label & has_offset)) {
            use_offset <- (show_label & has_offset);
            offset_sets <- label_df$overlap_set[use_offset];
            sp_index <- match(offset_sets, venn_spdf$label);
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
               segment_buffer=jamba::rmNA(segment_buffer[use_offset], naValue=0),
               sp_index=sp_index,
               label_color=label_df$color[use_offset],
               label_fill=label_df[use_offset,"fill"],
               label_border=label_df$border[use_offset],
               poly_color=venn_spdf$color[sp_index],
               poly_border=venn_spdf$border[sp_index]);
            sp_list <- lapply(sp_index, function(i){
               venn_spdf[i,]});
            new_xy <- polygon_label_segment(
               x0=test_xy$x0,
               y0=test_xy$y0,
               x1=test_xy$x0,
               y1=test_xy$y0,
               #x1=test_xy$x1,
               #y1=test_xy$y1,
               sp=sp_list,
               sp_buffer=test_xy$segment_buffer,
               verbose=FALSE,
               ...);
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
                  group=rep(venn_spdf$label[test_xy$sp_index[has_segment]], each=2),
                  color=rep(seg_colors, each=2),
                  lwd=rep(venn_spdf$lwd[test_xy$sp_index[has_segment]], each=2),
                  point_order=c(1, 2)
               );
            }
         }
         
      }
   }
   
   ## Prepare item labels
   itemlabels_df <- NULL;
   if (any(label_df$show_items %in% "inside")) {
      items_dfs <- subset(label_df, label_df$show_items %in% "inside");
      items_dfs <- split(items_dfs, items_dfs$overlap_set);
      
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
         vis <- which(data.frame(venn_spdf)$label %in% items_df1$overlap_set);
         vi <- tail(which(data.frame(venn_spdf)$label %in% items_df1$overlap_set), 1);
         vdf <- data.frame(venn_spdf,
            check.names=FALSE,
            stringsAsFactors=FALSE)[vi,,drop=FALSE];
         prefixes <- rep(
            gsub(":.+", "", items_df1$text),
            lengths(items_df1$items));
         labels <- NULL;
         # note currently uses the same show_items format per polygon
         # not for each row in items_dfs, so it is not possible to
         # use different show_items format for up-up and down-down within
         # the same polygon
         #show_items_order <- strsplit(items_df1$show_items[1], "[- _.]")[[1]];
         show_items_order <- strsplit(show_items[1], "[- _.]")[[1]];
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
            y=bg,
            ...);
         
         lpf <- polygon_label_fill(sp=venn_spdf[vi,],
            ref_sp=venn_spdf,
            color=color,
            cex=items_df1$item_cex[1],
            draw_points=FALSE,
            labels=labels,
            plot_style="none",
            draw_labels=FALSE,
            degrees=items_df1$item_degrees[1],
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
      if ("fontsize" %in% names(venn_spdf)) {
         setmatch <- match(label_df$overlap_set, venn_spdf$label);
         setmatchupdate <- !is.na(setmatch);
         label_df$overlap_fontsize[setmatchupdate] <- venn_spdf$fontsize[setmatch[setmatchupdate]];
      }
      # gdf is the expanded data.frame of label coordinates
      gdf <- data.frame(
         check.names=FALSE,
         stringsAsFactors=FALSE,
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
            label_df$padding[show_count_inside]),
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
      #jamba::printDebug("gdf:");
      #print(gdf);
      #print(dim(gdf));
   }

   ## Determine suitable xlim and ylim
   if ((length(xlim) == 0 || length(ylim) == 0) &&
         ("base" %in% plot_style ||
               ("gg" %in% plot_style && expand_fraction != -10))) {
      if ("gg" %in% plot_style) {
         expand_fraction <- expand_fraction + 0.05;
      }
      xlim_1 <- NULL;
      ylim_1 <- NULL;
      if (length(venn_spdf) > 0) {
         venn_bbox <- sp::bbox(venn_spdf);
         xlim_1 <- range(venn_bbox["x",], na.rm=TRUE);
         ylim_1 <- range(venn_bbox["y",], na.rm=TRUE);
      }
      if (length(gdf) > 0) {
         xlim_1 <- range(c(xlim_1,
            gdf$x),
            na.rm=TRUE);
         ylim_1 <- range(c(ylim_1,
            gdf$y),
            na.rm=TRUE);
      }
      if (length(label_df) > 0) {
         xlim_1 <- range(c(xlim_1, label_df$x), na.rm=TRUE);
         ylim_1 <- range(c(ylim_1, label_df$y), na.rm=TRUE);
      }
      expand_fraction <- rep(expand_fraction, length.out=4);
      if (length(xlim) == 0) {
         xlim <- expand_range(xlim_1,
            expand_fraction[c(2, 4)]);
      }
      if (length(ylim) == 0) {
         ylim <- expand_range(ylim_1,
            expand_fraction[c(1, 3)]);
      }
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
      # draw label line segments if needed
      if (show_segments && length(segment_df) > 0) {
         #jamba::printDebug("segment_df:");print(segment_df);
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
            lwd=segment_df1$lwd));
         xpd1 <- par(xpd=NA);
         segments(
            x0=segment_wide$x0,
            x1=segment_wide$x1,
            y0=segment_wide$y0,
            y1=segment_wide$y1,
            col=jamba::makeColorDarker(segment_wide$color,
               darkFactor=1.5),
            lwd=segment_wide$lwd);
         par(xpd1);
      }
      # Display the warning text label
      g_label <- NULL;
      vps <- NULL;
      if (length(warning_label) > 0) {
         cp <- jamba::coordPresets("bottom");
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
         g_labels <- gridtext::richtext_grob(
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
               #col=NA,
               col=if(group_labels){NA}else{gdf$border_col},
               #col=gdf$border_col, # used when draw_gridtext_groups() is not used
               #fill=NA,
               fill=if(group_labels){NA}else{gdf$box_fill},
               #fill=gdf$box_fill, # used when draw_gridtext_groups() is not used
               lty=gdf$box_lty,
               lwd=gdf$box_lwd)
         );
         # draw grouped label background
         if (group_labels) {
            g_labels <- tryCatch({
               dgg <- draw_gridtext_groups(
                  g_labels=g_labels,
                  gdf=gdf,
                  segment_df=segment_df,
                  do_draw=TRUE,
                  verbose=FALSE)
               dgg$g_labels;
            }, error=function(e){
               print(e);
               g_labels;
            });
         }
         
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
      g_labels=g_labels)));
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
 label_preset=c("none",
    "main inside",
    "main outside",
    "outside",
    "all outside",
    "concept", "meme",
    "items",
    "main items",
    "main count items",
    "custom"),
 label_style=c("basic",
    "box",
    "fill",
    "fill_box",
    "shaded",
    "shaded_box",
    "lite",
    "lite_box"),
 lite="#FFEEAABB",
 bg="white",
 set=c("inside",
    "outside",
    "none"),
 overlap=c("none",
    "inside",
    "outside"),
 count=c("inside",
    "outside",
    "ifneeded",
    "detect",
    "none"),
 signed=c("inside",
    "outside",
    "ifneeded",
    "detect",
    "none"),
 items=c("none",
    "inside"),
 max_items=3000,
 inside_percent_threshold=5,
 label_types=c("main", "signed"),
 show_zero=TRUE,
 sep="&",
 useGrey=15,
 verbose=FALSE,
 ...)
{
   # TODO:
   # check whether label coordinate overlaps the polygon
   # if yes then use contrasting text with combined background colors
   # if no then assume white background
   label_types <- match.arg(label_types,
      several.ok=TRUE);   
   label_style <- rep(head(label_style, 1),
      length.out=nrow(venndir_output$label_df));
   
   # validate arguments
   label_preset <- match.arg(label_preset);
   set <- match.arg(set);
   overlap <- match.arg(overlap);
   count <- match.arg(count);
   signed <- match.arg(signed);
   items <- match.arg(items);
   
   # handle presets
   if ("main inside" %in% label_preset) {
      set <- "inside";
      overlap <- "none";
      count <- "inside";
      signed <- "inside";
      items <- "none";
   } else if ("outside" %in% label_preset) {
      set <- "outside";
      overlap <- "none";
      count <- "outside";
      signed <- "outside";
      items <- "none";
   } else if ("main outside" %in% label_preset) {
      set <- "outside";
      overlap <- "none";
      count <- "inside";
      signed <- "inside";
      items <- "none";
   } else if (any(c("meme", "concept") %in% label_preset)) {
      set <- "none";
      overlap <- "none";
      count <- "none";
      signed <- "none";
      items <- "inside";
   } else if ("main items" %in% label_preset) {
      set <- "outside";
      overlap <- "none";
      count <- "ifneeded";
      signed <- "none";
      items <- "inside";
   } else if ("main count items" %in% label_preset) {
      set <- "outside";
      overlap <- "none";
      count <- "outside";
      signed <- "outside";
      items <- "inside";
   } else if ("items" %in% label_preset) {
      set <- "none";
      overlap <- "none";
      count <- "none";
      signed <- "none";
      items <- "inside";
   }
   if (verbose) {
      jamba::printDebug("venndir_label_style(): ",
         "set: ", set,
         ", overlap: ", overlap,
         ", count: ", count,
         ", signed: ", signed,
         ", items: ", items);
   }

   # match rows in label_df with venn_spdf
   n <- length(venndir_output$venn_spdf$label) + 1;
   #sp_index <- (n - 
   #      match(venndir_output$label_df$overlap_set,
   #         rev(venndir_output$venn_spdf$label)));
   sp_index2 <- match(
      venndir_output$label_df$overlap_set,
      venndir_output$venn_spdf$label);
   sp_index2new <- match(
      venndir_output$label_df$overlap_set,
      ifelse(venndir_output$venn_spdf$type %in% "set",
         NA,
         venndir_output$venn_spdf$label));

   # associate label_df entries with the appropriate polygons
   venn_spdf_df <- as.data.frame(venndir_output$venn_spdf);
   venn_spdf_df$rownum <- seq_along(venndir_output$venn_spdf);
   venn_spdf_df_sub <- subset(venn_spdf_df, !is.na(venn_counts));
   sp_index <- venn_spdf_df_sub$rownum[match(
      venndir_output$label_df$overlap_set,
      venn_spdf_df_sub$label)];
   
   # handle label preset
   # check if any set label is hidden
   label_nsets <- lengths(strsplit(venndir_output$label_df$overlap_set, split=sep));
   venndir_output$label_df$nsets <- label_nsets;
   label_is_set <- (label_nsets == 1 & venndir_output$label_df$type %in% "main");
   
   # make sure each set has a shape to use, otherwise skip it
   # 02nov2021 - changed to delineate overlap_label from set label
   label_has_shape <- (venndir_output$label_df$overlap_set %in%
         venndir_output$venn_spdf$label);
   overlap_label_has_shape <- (venndir_output$label_df$overlap_set %in%
         subset(venn_spdf_df, type %in% "overlap")$label);

   # check if there is room for label inside via inside_percent_threshold
   sp_pct_area <- sp_percent_area(venndir_output$venn_spdf);
   poly_pct_area <- jamba::rmNA(sp_pct_area[sp_index],
      naValue=-1);
   if (length(inside_percent_threshold) == 0) {
      inside_percent_threshold <- c(0)
   }
   label_area_ok <- (poly_pct_area >= inside_percent_threshold);

   # we need the total counts per overlap_set in order to apply max_items
   main_label_df <- subset(venndir_output$label_df, type %in% "main");
   main_match <- match(venndir_output$label_df$overlap_set,
      main_label_df$overlap_set);
   venndir_output$label_df$main_venn_counts <- main_label_df$venn_counts[main_match];
   
   # update label positions only when label_preset is not "custom"
   if (!"custom" %in% label_preset) {
   
      # overlap labels
      if (any(c("none", "inside", "outside") %in% overlap)) {
         venndir_output$label_df$overlap <- ifelse(
            venndir_output$label_df$type %in% "main",
            overlap,
            "none");
      }
   
      # set labels
      if (!"none" %in% set) {
         set_is_hidden <- (label_is_set & is.na(venndir_output$label_df$x) & label_has_shape);
         set_is_not_hidden <- (label_is_set & !is.na(venndir_output$label_df$x) & label_has_shape);
         venndir_output$label_df$set_is_hidden <- set_is_hidden;
         if (any(set_is_hidden)) {
            set_hidden <- venndir_output$label_df$overlap_set[set_is_hidden];
            set_hidden_match <- match(set_hidden,
               venndir_output$venn_spdf$label);
            venndir_output$label_df[set_is_hidden, c("x", "y", "x_offset", "y_offset")] <- 
               data.frame(venndir_output$venn_spdf)[set_hidden_match, c("x_label", "x_label", "x_offset", "y_offset")];
            venndir_output$label_df$overlap[set_is_hidden] <- "outside";
            if (verbose) {
               jamba::printDebug("venndir_label_style(): ",
                  "moved hidden set label outside:",
                  set_hidden);
            }
         }
         if (any(set_is_not_hidden)) {
            venndir_output$label_df$overlap[set_is_not_hidden] <- set;
         }
      }

      ######################################################
      # item labels
      # - determine which overlaps display items inside
      #   which determines where to display venn_counts
      if ("inside" %in% items) {
         venndir_output$label_df$show_items <- ifelse(
            venndir_output$label_df$main_venn_counts > 0 &
               venndir_output$label_df$main_venn_counts <= max_items,
            "inside",
            "none"
         );
      } else {
         venndir_output$label_df$show_items <- "none";
      }
      
      ######################################################
      # count labels
      #jamba::printDebug("count labels, count:", count,
      #   ", signed:", signed, ", items:", items, ", max_items:", max_items);
      #print(venndir_output$label_df);
      # new logic
      venndir_output$label_df$count <- ifelse(
         venndir_output$label_df$show_items %in% "none",
         # inside can display count, no items are inside
         ifelse(
            venndir_output$label_df$type %in% "main",
            ifelse(
               venndir_output$label_df$venn_counts > 0 | show_zero,
               ifelse(
                  any(c("inside", "ifneeded", "detect") %in% count),
                  "inside",
                  count #"none"
               ),
               "none"
            ),
            ifelse(
               venndir_output$label_df$venn_counts > 0 | show_zero,
               ifelse(
                  any(c("inside", "ifneeded", "detect") %in% signed),
                  "inside",
                  signed #"none"
               ),
               "none"
            )
         ),
         # inside cannot display count, items are inside
         ifelse(
            venndir_output$label_df$type %in% "main",
            ifelse(
               venndir_output$label_df$venn_counts > 0 | show_zero,
               ifelse(
                  any(c("detect", "outside") %in% count),
                  "outside",
                  "none"
               ),
               "none"
            ),
            ifelse(
               venndir_output$label_df$venn_counts > 0 | show_zero,
               ifelse(
                  any(c("detect", "outside") %in% signed),
                  "outside",
                  "none"
               ),
               "none"
            )
         )
      )
      # update hidden to "none"
      venndir_output$label_df$count[venndir_output$label_df$set_is_hidden] <- "none"


      # check for inside area threshold
      # label_area_ok TRUE/FALSE
      if (any(!label_area_ok)) {
         for (itype in c("count", "overlap")) {
            venndir_output$label_df[[itype]] <- ifelse(
               venndir_output$label_df[[itype]] %in% "inside" &
                  !label_area_ok,
               "outside",
               venndir_output$label_df[[itype]]);
         }
      }
      

      # update offset coordinates
      has_outside <- (venndir_output$label_df$overlap %in% "outside" |
            venndir_output$label_df$count %in% "outside");
      if (any(has_outside)) {
         venndir_output$label_df$x_offset[has_outside] <- (venndir_output$venn_spdf$x_outside[sp_index2[has_outside]] - 
               venndir_output$label_df$x[has_outside]);
         venndir_output$label_df$y_offset[has_outside] <- (venndir_output$venn_spdf$y_outside[sp_index2[has_outside]] - 
               venndir_output$label_df$y[has_outside]);
      }
   }
   
   # group labels
   label_left_outside <- (venndir_output$label_df$type %in% "main" &
      (venndir_output$label_df$overlap %in% "outside" |
         venndir_output$label_df$count %in% "outside"));
   label_left_inside <- (venndir_output$label_df$type %in% "main" &
      (venndir_output$label_df$overlap %in% "inside" |
            venndir_output$label_df$count %in% "inside"));
   label_right_outside <- (!venndir_output$label_df$type %in% "main" &
         venndir_output$label_df$count %in% "outside");
   label_right_inside <- (!venndir_output$label_df$type %in% "main" &
         venndir_output$label_df$count %in% "inside");
   venndir_output$label_df$label_left_outside <- label_left_outside;
   venndir_output$label_df$label_left_inside <- label_left_inside;
   venndir_output$label_df$label_right_outside <- label_right_outside;
   venndir_output$label_df$label_right_inside <- label_right_inside;
   
   # define text adjust for left-right alignment relative to the label coordinate
   # left outside labels
   llo_adjx <- rep(0.5, length(label_right_inside));
   llo_adjx[label_left_outside] <- ifelse(
      venndir_output$label_df$overlap_set[label_left_outside] %in%
         venndir_output$label_df$overlap_set[label_right_outside],
      1,
      jamba::rmNA(venndir_output$venn_spdf$vjust[sp_index2[label_left_outside]],
         naValue=0.5));
   llo_adjx[label_right_outside] <- ifelse(
      venndir_output$label_df$overlap_set[label_right_outside] %in%
         venndir_output$label_df$overlap_set[label_left_outside],
      0,
      jamba::rmNA(venndir_output$venn_spdf$vjust[sp_index2[label_right_outside]],
         naValue=0.5));
   llo_adjy <- rep(0.5, length(label_right_inside));
   llo_adjy[label_left_outside] <- ifelse(
      (venndir_output$label_df$overlap[label_left_outside] %in% "outside" &
         venndir_output$label_df$count[label_left_outside] %in% "outside"),
      0,
      ifelse(
         venndir_output$label_df$overlap_set[label_left_outside] %in%
            venndir_output$label_df$overlap_set[label_right_outside],
         0.5,
         ifelse(venndir_output$label_df$count[label_left_outside] %in% "outside",
            1 - jamba::rmNA(venndir_output$venn_spdf$hjust[sp_index2[label_left_outside]],
               naValue=0.5),
            jamba::rmNA(venndir_output$venn_spdf$hjust[sp_index2[label_left_outside]],
               naValue=0.5))
      )
   );

   # left inside labels
   lli_adjx <- rep(0.5, length(label_right_inside));
   lli_adjx[label_left_inside] <- ifelse(
      venndir_output$label_df$overlap_set[label_left_inside] %in%
         venndir_output$label_df$overlap_set[label_right_inside],
      1,
      0.5);
   lli_adjx[label_right_inside] <- ifelse(
      venndir_output$label_df$overlap_set[label_right_inside] %in%
         venndir_output$label_df$overlap_set[label_left_inside],
      0,
      0.5);

   lli_adjy <- rep(0.5, length(label_right_inside));
   lli_adjy[label_left_inside] <- ifelse(
      venndir_output$label_df$overlap[label_left_inside] %in% "inside" &
         venndir_output$label_df$count[label_left_inside] %in% "inside",
      0,
      ifelse(venndir_output$label_df$type %in% "main",
         0.5,
         jamba::rmNA(venndir_output$venn_spdf$hjust[sp_index2[label_left_inside]],
            naValue=0.5)));

   # apply text adjust to each label
   # hjust does x-axis justification
   #   (0=right side of point, 1=left side of point, 0.5=centered)
   venndir_output$label_df$hjust_outside <- llo_adjx;
   venndir_output$label_df$hjust_inside <- lli_adjx;

   # vjust does y-axis justification
   #   (0=above point, 1=below point, 0.5=centered)
   venndir_output$label_df$vjust_outside <- ifelse(venndir_output$label_df$type %in% "main",
      1 - llo_adjy,
      venndir_output$label_df$vjust);
   venndir_output$label_df$vjust_inside <- ifelse(venndir_output$label_df$type %in% "main",
      1 - lli_adjy,
      venndir_output$label_df$vjust);

   # toupdate
   toupdate <- venndir_output$label_df$type %in% label_types;
   
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
      spt <- sp::SpatialPoints(ixy);
      venn_spdf <- venndir_output$venn_spdf[venndir_output$venn_spdf$type %in% "overlap",];
      spwhich <- which(venndir_output$venn_spdf$type %in% "overlap")
      for (j in spwhich) {
         sp <- venndir_output$venn_spdf[j,];
         if (rgeos::gContains(sp, spt)) {
            return(j);
         }
      }
      return(NA)
   });

   # label_bg is the background color when the label is inside
   label_bg <- ifelse(is.na(xy_overlaps),
      rep(bg, length.out=length(xy_overlaps)),
      jamba::alpha2col(
         venndir_output$venn_spdf$color[xy_overlaps],
         alpha=venndir_output$venn_spdf$alpha[xy_overlaps])
      );

   # box is darker version of polygon color with alpha=0.8
   venndir_output$label_df$border[toupdate] <- ifelse(
      grepl("box", label_style),
      jamba::alpha2col(alpha=0.8,
         jamba::makeColorDarker(venndir_output$venn_spdf$color[sp_index],
            darkFactor=1.5)),
      NA)[toupdate];

   # label background fill
   venndir_output$label_df$fill[toupdate] <- ifelse(
      grepl("fill", label_style),
      venndir_output$venn_spdf$color[sp_index],
      ifelse(
         grepl("shaded", label_style),
         jamba::alpha2col(
            venndir_output$venn_spdf$color[sp_index],
            alpha=0.5),
         ifelse(
            grepl("lite", label_style),
            rep(lite, nrow(venndir_output$label_df)),
            ifelse(
               grepl("none|basic", label_style),
               NA,
               venndir_output$label_df$fill)
         )
      )
   )[toupdate];

   # label color
   venndir_output$label_df$color[toupdate] <- ifelse(
      grepl("fill", label_style),
      ifelse(
         venndir_output$label_df$type %in% "main",
         jamba::rmNA(naValue="black",
            jamba::setTextContrastColor(
               jamba::alpha2col(venndir_output$venn_spdf$color[sp_index],
                  alpha=venndir_output$venn_spdf$alpha[sp_index]),
               useGrey=useGrey)),
         make_color_contrast(
            x=venndir_output$label_df$color,
            y=venndir_output$label_df$fill,
            bg=label_bg,
            ...)),
      ifelse(
         grepl("shaded", label_style),
         ifelse(
            venndir_output$label_df$type %in% "main",
            jamba::rmNA(naValue="black",
               make_color_contrast("black",
                  venndir_output$label_df$fill,
                  bg=jamba::alpha2col(venndir_output$venn_spdf$color[sp_index],
                     alpha=venndir_output$venn_spdf$alpha[sp_index])),
               #jamba::setTextContrastColor(venndir_output$label_df$fill,
               #   useGrey=useGrey)
            ),
            make_color_contrast(
               x=venndir_output$label_df$color,
               y=venndir_output$label_df$fill,
               bg=label_bg,
               ...)),
         ifelse(
            grepl("lite", label_style),
            ifelse(
               venndir_output$label_df$type %in% "main",
               jamba::rmNA(naValue="black",
                  jamba::setTextContrastColor(venndir_output$label_df$fill,
                     useGrey=useGrey)),
               make_color_contrast(
                  x=venndir_output$label_df$color,
                  y=venndir_output$label_df$fill,
                  bg=label_bg,
                  ...)),
            ifelse(
               grepl("none|basic|box", label_style),
               ifelse(
                  venndir_output$label_df$type %in% "main",
                  jamba::rmNA(naValue="black",
                     jamba::setTextContrastColor(label_bg,
                        useGrey=useGrey)),
                  make_color_contrast(
                     x=venndir_output$label_df$color,
                     y=label_bg,
                     bg=bg)#,...)
                  ),
               venndir_output$label_df$color)
         )
      )
   )[toupdate];

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
#'    to add to the total range. When `expand_fraction` has
#'    only one value, it is applied across the whole range,
#'    which means each side is extended by half the `expand_fraction`.
#'    When `expand_fraction` contains two values, it is
#'    applied in order to the low, then high side of the
#'    numeric range, and each full `expand_range` value is
#'    applied.
#' @param minimum_range `numeric` value indicating the minimum
#'    range of the output, useful when the input has zero
#'    range, for example if `x=c(10, 10)`.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' x <- c(0, 10);
#' 
#' # expand the total range by 0.1
#' expand_range(x, 0.1);
#' 
#' # the original range is 10 units
#' diff(x);
#' 
#' # the expanded range is 11 units
#' diff(expand_range(x, 0.1));
#' 
#' # expand one side but not the other
#' expand_range(x, c(0.1, 0));
#' # this new range is 11 units
#' diff(expand_range(x, c(0.1, 0)))
#' 
#' # input with no range is extended to some minimum value
#' expand_range(1, minimum_range=1)
#' expand_range(1, minimum_range=c(1, 0))
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
   if (length(expand_fraction) == 1) {
      expand_fraction <- rep(expand_fraction, 2) / 2;
   }
   if (length(minimum_range) == 1) {
      minimum_range <- rep(minimum_range, 2) / 2;
   }
   expand_fraction <- head(expand_fraction, 2);
   minimum_range <- head(minimum_range, 2);
   if (xdiff == 0) {
      expand_fraction <- minimum_range;
      xdiff <- 1;
   }
   xexpand <- xdiff * expand_fraction * c(-1, 1);
   x <- x + xexpand;
   return(x);
}

