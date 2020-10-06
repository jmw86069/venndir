
#' Render Venn or Euler diagram
#' 
#' Render Venn or Euler diagram
#' 
#' This function is intended to render a Venn or Euler diagram
#' directly from the output of `venndir()` to allow customization
#' of various aspects of the figure.
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
#'    * `"color"` is required, the others are filled with defaults
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
#'    * `"vjust"`, `"hjust"` vertical/horizontal position `[0.5, 0.5]`;
#'    * `"halign"` with horizontal alignment `[0.5]`;
#'    * `"rot"` label rotation `[0]`;
#'    * `"col"` label color `[black]`;
#'    * `"fontsize"` fontsize used by `grid::par()` `[14]`;
#'    * `"border"` border color, where `NA` draws no border `[NA]`;
#'    * `"lty"`, `"lwd"` line type and line width `[1, 1]`;
#'    * `"fill"` label background fill, where `NA` draws no background fill `[NA]`;
#'    * `"padding"`, `"padding_unit"` passed to `gridtext::richtext_grob()`
#'    to define padding around each label `[2, "pt"]`;
#'    * `"r"`, `"r_unit"` passed to `gridtext::richtext_grob()` to define
#'    rounded corners, relevant only when `"border"` or `"fill"` are
#'    not `NA` `[2, "pt"]`.
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
#' @param ... additional arguments are passed to `plot()`
#'    when plotting `venn_spdf` which is expected to be a
#'    `sp::SpatialPolygonsDataFrame`.
#'    
#' @examples
#' venndir_output <- venndir(setlist, 1:2, overlap_type="each", do_plot=FALSE)
#' render_venndir(venndir_output);
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
 xpd=NA,
 plot_warning=TRUE,
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
   
   ## Determine suitable xlim and ylim
   if (length(xlim) == 0 || length(ylim) == 0) {
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
         xlim <- xlim_1;
      }
      if (length(ylim) == 0) {
         ylim <- ylim_1;
      }
   }
   
   ## Process SpatialPolygonsDataFrame
   if (length(venn_spdf) > 0) {
      ## Fill any missing optional colnames with defaults
      venn_spdf_defaults <- c(
         alpha=jamba::col2alpha(venn_spdf$color),
         lwd=2,
         border=NA
      );
      venn_spdf_add <- setdiff(colnames(venn_spdf),
         names(venn_spdf_defaults));
      for (i in venn_spdf_add) {
         venn_spdf[[i]] <- rep(venn_spdf_defaults[[i]],
            nrow(venn_spdf));
      }
      ## Plot the Venn polygons
      plot(venn_spdf,
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
   
   
   ## Process labels   
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
         show_label=TRUE,
         vjust=0.5,
         hjust=0.5,
         halign=0.5,
         rot=0,
         col="black",
         fontsize=14,
         border=NA,
         lty=1,
         lwd=1,
         fill=NA,
         padding=2,
         padding_unit="pt",
         r=2,
         r_unit="pt");
      label_df_add <- setdiff(names(label_df_defaults),
         colnames(label_df));
      for (i in label_df_add) {
         label_df[[i]] <- rep(label_df_defaults[[i]],
            nrow(label_df));
      }
      
      ## Create gridtext object
      show_label <- (label_df$show_label %in% c(TRUE, 1));
      
      ## warn about hidden non-zero labels
      warn_rows <- (!show_label &
            label_df$venn_counts != 0 &
            label_df$type %in% "main");
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
                  sep="; "))
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
               boxColor="#FFFFFF99",
               boxBorderColor="#99999999",
            );
         }
      }
      
      ## display labels
      if (any(show_label)) {
         g_labels <- gridtext::richtext_grob(
            text=label_df$text[show_label],
            x=grid::unit(label_df$x[show_label], "native"),
            y=grid::unit(label_df$y[show_label], "native"),
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
               col=label_df$col[show_label],
               fontsize=label_df$fontsize[show_label]
            ),
            box_gp=grid::gpar(
               col=label_df$border[show_label],
               fill=label_df$fill[show_label],
               lty=label_df$lty[show_label],
               lwd=label_df$lwd[show_label])
         )
         
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
   
}
