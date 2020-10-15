
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
#' setlist <- make_venn_test(100, 3, do_signed=TRUE);
#' venndir_output <- venndir(setlist, 1:2, overlap_type="each", do_plot=FALSE);
#' render_venndir(venndir_output);
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
 xpd=NA,
 font_cex=1,
 plot_warning=TRUE,
 display_items=c("none", "sign label", "label", "sign"),
 item_angle=20,
 display_counts=TRUE,
 max_items=100,
 fontfamily="Arial",
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
   display_items <- head(display_items, 1);
   
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
               fontfamily=fontfamily,
               col=label_df$col[show_label],
               fontsize=label_df$fontsize[show_label] * font_cex
            ),
            box_gp=grid::gpar(
               col=label_df$border[show_label],
               fill=label_df$fill[show_label],
               lty=label_df$lty[show_label],
               lwd=label_df$lwd[show_label])
         )
         
         ## Draw labels
         # to draw using grid we have to use a custom viewport
         if (display_counts) {
            if (length(dev.list()) > 0) {
               vps <- gridBase::baseViewports();
               grid::pushViewport(vps$inner, vps$figure, vps$plot);
               grid::grid.draw(g_labels);
               grid::popViewport(3);
            }
         }
      }
   }
   
   ## Draw item labels
   if (!"none" %in% display_items && "items" %in% colnames(data.frame(label_df, stringsAsFactors=FALSE, check.names=FALSE))) {
      label_dfs <- subset(label_df, lengths(label_df$items) > 0);
      label_dfs <- split(label_dfs, label_dfs$overlap_set);
      label_df1 <- label_dfs[[2]];
      display_items_order <- strsplit(display_items, "[- _.]")[[1]];
      for (label_df1 in label_dfs) {
         items <- unname(unlist(jamba::mixedSorts(label_df1$items)));
         color1 <- rep(label_df1$col, lengths(label_df1$items));
         vi <- which(data.frame(venn_spdf)$label %in% label_df1$overlap_set);
         vdf <- data.frame(venn_spdf)[vi,,drop=FALSE];
         prefixes <- rep(
            gsub(":.+", "", label_df1$text),
            lengths(label_df1$items));
         labels <- NULL;
         for (dio in display_items_order) {
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
         
         label_polygon_fill(sp=venn_spdf[vi,],
            ref_sp=venn_spdf,
            color=color,
            cex=1,
            draw_points=FALSE,
            polygon_scale=-0.01,
            labels=labels,
            angle=item_angle);
      }
      
   }
   
   if (length(label_df) > 0) {
      return(invisible(g_labels));
   }
}

#' Render Venn or Euler diagram using ggplot2
#' 
#' Render Venn or Euler diagram using ggplot2
#' 
#' This function is intended to render a Venn or Euler diagram
#' directly from the output of `venndir()` to allow customization
#' of various aspects of the figure.
#' 
#' @family venndir core
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
#' @export
ggrender_venndir <- function
(venndir_output=NULL,
 venn_spdf=NULL,
 label_df=NULL,
 xlim=NULL,
 ylim=NULL,
 xpd=NA,
 font_cex=1,
 plot_warning=TRUE,
 display_items=c("none", "sign label", "label", "sign"),
 item_angle=20,
 display_counts=TRUE,
 max_items=100,
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
   display_items <- head(display_items, 1);
   
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
         "none" %in% display_items) {
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
            text.colour=col,
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
   if (!"none" %in% display_items && "items" %in% colnames(data.frame(label_df, check.names=FALSE, stringsAsFactors=FALSE))) {
      jamba::printDebug("ggrender_venndir(): ", "display_items:", display_items);
      label_dfs <- subset(label_df, lengths(label_df$items) > 0);
      label_dfs <- split(label_dfs, label_dfs$overlap_set);
      label_df1 <- label_dfs[[2]];
      display_items_order <- strsplit(display_items, "[- _.]")[[1]];
      #for (label_df1 in label_dfs) {
      items_df <- jamba::rbindList(lapply(names(label_dfs), function(iname) {
         label_df1 <- label_dfs[[iname]];
         items <- unname(unlist(jamba::mixedSorts(label_df1$items)));
         color1 <- rep(label_df1$col, lengths(label_df1$items));
         vi <- which(data.frame(venn_spdf)$label %in% label_df1$overlap_set);
         vdf <- data.frame(venn_spdf)[vi,,drop=FALSE];
         prefixes <- rep(
            gsub(":.+", "", label_df1$text),
            lengths(label_df1$items));
         labels <- NULL;
         for (dio in display_items_order) {
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
      print(head(items_df, 6));
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
            text.colour=col,
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

