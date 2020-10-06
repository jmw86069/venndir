
#' Directional Venn diagram
#' 
#' Directional Venn diagram
#' 
#' This function displays a Venn diagram, or when `proportional=TRUE`
#' it displays a Euler diagram, representing counts in each overlap
#' set.
#' 
#' Input data is supplied either as a `list` or `incidence matrix`,
#' or `signed incidence matrix` (whose values indicate direction),
#' and is passed to `signed_overlaps()` to summarize counts by
#' Venn overlaps.
#' 
#' By default, when input data contains signed direction, the
#' counts include summary counts for the different forms of
#' agreement in direction. See `signed_overlaps()` for description
#' of each `overlap_type`, for different methods of summarizing
#' the overlapping directions.
#' 
#' @inheritParams signed_overlaps
#' @param sets `integer` index of sets in `setlist` to display in
#'    the Venn diagram. This subset is useful when creating a Venn
#'    diagram for a subset of a list, because it defines consistent
#'    colors across all sets, and uses the appropriate subset of
#'    colors in the Venn diagram.
#' @param set_colors `character` vector of R colors, or `NULL` to
#'    use default colors defined by `colorjam::rainbowJam()`.
#' @param proportional `logical` indicating whether the Venn circles
#'    are proportionally sized, also known as a Euler diagram. Note
#'    that proportionally sized circles are not guaranteed to represent
#'    every possible overlap.
#' @param display_zero `logical` indicating whether empty overlaps
#'    are labeled with zero `0` when `display_zero=TRUE`, or are
#'    blank when `display_zero=FALSE`.
#' @param font_cex `numeric` `vector` length 2, indicating relative
#'    font size for the main label, and directional label, respectively.
#'    The default `c(1, 0.8)` defines the main label with 1x size,
#'    and the directional labels with 80% that size.
#' @param poly_alpha `numeric` value between 0 and 1, indicating the
#'    alpha transparency of the polygon backgroun color, where
#'    `poly_alpha=1` is completely 100% opaque (no transparency), and
#'    `poly_alpha=0.4` is 40% opaque, therefore 60% transparency.
#' @param label_style `character` string indicating the style of label
#'    to display:
#'    `"basic"` displays text with no background shading or border,
#'    `"fill"` displays text on opaque colored background,
#'    `"shaded"` displays text on partially transparent colored background,
#'    `"lite"` displays text on partially transparent lite background,
#'    `"lite_box"` displays text on lite background with border.
#' 
#' @family venndir overlaps
#' 
#' @examples
#' setlist <- make_venn_test(100, 3);
#' venndir(setlist)
#' 
#' setlist <- make_venn_test(100, 3, do_signed=TRUE);
#' venndir(setlist)
#' venndir(setlist)
#' 
#' venndir(setlist, label_style="basic")
#' venndir(setlist, label_style="fill")
#' venndir(setlist, label_style="shaded")
#' venndir(setlist, label_style="shaded_box")
#' venndir(setlist, label_style="lite")
#' venndir(setlist, label_style="lite_box")
#' 
#' # proportional Euler diagram
#' venndir(setlist,
#'    proportional=TRUE);
#' 
#' # nudge circles and hide the zero
#' venndir(setlist,
#'    proportional=TRUE,
#'    display_zero=FALSE,
#'    x_nudge=c(set_C=-0.4),
#'    y_nudge=c(set_C=0.1))
#' 
#' setlist2k <- make_venn_test(2000, 3, 80, do_signed=TRUE);
#' venndir(setlist2k)
#' venndir(setlist2k, proportional=TRUE)
#' 
#' @export
venndir <- function
(setlist,
 sets=seq_along(setlist),
 set_colors=NULL,
 overlap_type=c("concordance", "each", "overlap", "concordant"),
 proportional=FALSE,
 return_items=FALSE,
 display_zero=TRUE,
 font_cex=c(1, 0.8),
 poly_alpha=0.5,
 label_style=c("basic",
    "fill",
    "shaded",
    "shaded_box",
    "lite",
    "lite_box"),
 x_nudge=NULL,
 y_nudge=NULL,
 circle_nudge=NULL,
 unicode=TRUE,
 ...)
{
   # basic workflow:
   # - get signed_overlaps()
   # - use counts per Venn set to create Venn circles/ovals
   #   - proportional Euler, or not proportional Venn
   # - find polygon overlaps for the circles/ovals
   #   - identify any overlaps not represented (to label on the side)
   
   overlap_type <- match.arg(overlap_type);
   label_style <- match.arg(label_style);

   if (length(font_cex) < 2) {
      if (length(font_cex) == 0) {
         font_cex <- c(1, 0.8);
      }
      font_cex <- rep(font_cex, length.out=2);
   }
   # define colors
   if (length(set_colors) == 0) {
      set_colors <- colorjam::rainbowJam(length(setlist),
         ...);
      set_color <- set_colors[sets];
   } else {
      set_color <- rep(set_colors,
         length.out=length(sets));
   }
   
   # get overlap data
   sv <- signed_overlaps(setlist[sets],
      overlap_type=overlap_type,
      return_items=return_items);
   
   # numeric counts by set
   nCounts <- sapply(unique(sv$sets), function(i){
      sum(subset(sv, sets %in% i)$count)
   });
   # formatted numeric counts
   fCounts <- format(big.mark=",",
      trim=TRUE,
      nCounts);
   # subgrouped counts (directional)
   if ("overlap" %in% overlap_type) {
      gCounts <- list(NULL);
   } else {
      gCounts <- lapply(jamba::nameVector(unique(sv$sets)), function(i){
         j <- subset(sv, sets %in% i);
         jamba::nameVector(j$count, j$overlap_label)
      });
   }
   
   # get Venn circles   
   venn_sp <- get_venn_shapes(counts=nCounts,
      proportional=proportional,
      x_nudge=x_nudge,
      y_nudge=y_nudge,
      ...);

   # convert to venn overlap polygons
   venn_spdf <- find_vennpoly_overlaps(venn_sp,
      venn_counts=nCounts,
      venn_colors=set_color);
   
   # Now add the main count labels
   venn_text <- ifelse(grepl("&", venn_spdf$label),
      jamba::formatInt(jamba::rmNA(naValue=0, venn_spdf$venn_counts)),
      paste0("**",
         venn_spdf$label,
         "**<br>\n",
         jamba::formatInt(jamba::rmNA(naValue=0, venn_spdf$venn_counts))));
   x_main <- venn_spdf$x_label;
   y_main <- venn_spdf$y_label;
   vjust_main <- rep(0.5, length(x_main));
   halign_main <- rep(0.5, length(x_main));
   #hjust_main <- ifelse(nchar(up_text) == 0 & nchar(dn_text) == 0, 0.5, 1);
   hjust_main <- rep(1, length(x_main));
   if ("overlap" %in% overlap_type) {
      hjust_main <- rep(0.5, length(x_main));
   }
   
   ## Hide zero if display_zero=FALSE
   show_main <- rep(TRUE, length(x_main));
   if (!display_zero && any(venn_spdf$venn_counts == 0)) {
      show_main <- !(venn_spdf$venn_counts == 0);
   }

   ## Labels for each overlap
   gbase_labels <- curate_venn_labels(
      names(unlist(unname(gCounts))),
      unicode=unicode,
      "sign");
   gbase_colors <- (curate_venn_labels(
      names(unlist(unname(gCounts))),
      unicode=unicode,
      "color"));
   gcount_labels <- sapply(seq_along(unlist(gCounts)), function(i){
      ilabel <- paste0(
         gbase_labels[i],
         ": ",
         format(trim=TRUE,
            big.mark=",",
            unlist(gCounts)[i]));
   });
   ## order labels again?
   gdf <- jamba::mixedSortDF(data.frame(
      group=rep(seq_along(gCounts), lengths(gCounts)),
      label=gbase_labels,
      index=seq_along(gbase_labels)), byCols=c(1, 2));
   gbase_labels <- gbase_labels[gdf$index];
   gbase_colors <- gbase_colors[gdf$index];
   gcount_labels <- gcount_labels[gdf$index];
   
   x_signed <- rep(x_main, lengths(gCounts));
   y_signed <- rep(y_main, lengths(gCounts));
   vjust_signed <- unname(unlist(lapply(lengths(gCounts), function(i){
      iseq <- seq(from=0, by=1, length.out=i);
      iseq - mean(iseq);
   }))) + 0.5;
   #vjust_signed + ifelse(grepl("&", venn_spdf$label), 0.5, 0)
   hjust_signed <- rep(0, length(x_signed));
   halign_signed <- rep(0, length(x_signed));
   
   ## label_style
   label_fill_main <- rep(NA, length(x_main));
   label_border_main <- rep(NA, length(x_main));
   label_color_main <- jamba::setTextContrastColor(jamba::alpha2col(venn_spdf$color,
      alpha=poly_alpha));
   label_fill_signed <- rep(NA, length(x_signed));
   label_border_signed <- rep(NA, length(x_signed));
   label_color_signed <- jamba::setTextContrastColor(
      jamba::alpha2col(rep(venn_spdf$color, lengths(gCounts)),
         alpha=poly_alpha));
   if ("fill" %in% label_style) {
      label_fill_main <- venn_spdf$color;
      label_border_main <- jamba::makeColorDarker(venn_spdf$color,
         darkFactor=1.2);
      label_color_main <- jamba::setTextContrastColor(venn_spdf$color);
      label_fill_signed <- rep(label_fill_main,
         lengths(gCounts));
      label_border_signed <- rep(label_border_main,
         lengths(gCounts));
      label_color_signed <- rep(label_color_main,
         lengths(gCounts));
   } else if ("shaded" %in% label_style) {
      label_fill_main <- jamba::alpha2col(venn_spdf$color,
         alpha=0.5);
      label_border_main <- rep(NA, length(x_main));
      label_color_main <- jamba::setTextContrastColor(
         jamba::alpha2col(venn_spdf$color,
            alpha=mean(c(poly_alpha, 0.9))));
      label_fill_signed <- rep(label_fill_main,
         lengths(gCounts));
      label_border_signed <- rep(label_border_main,
         lengths(gCounts));
      label_color_signed <- rep(label_color_main,
         lengths(gCounts));
   } else if ("shaded_box" %in% label_style) {
      label_fill_main <- jamba::alpha2col(venn_spdf$color,
         alpha=0.5);
      label_border_main <- jamba::alpha2col(alpha=0.8,
         jamba::makeColorDarker(venn_spdf$color,
            darkFactor=1.2));
      label_color_main <- jamba::setTextContrastColor(
         jamba::alpha2col(venn_spdf$color,
            alpha=mean(c(poly_alpha, 0.9))));
      label_fill_signed <- rep(label_fill_main,
         lengths(gCounts));
      label_border_signed <- rep(label_border_main,
         lengths(gCounts));
      label_color_signed <- rep(label_color_main,
         lengths(gCounts));
   } else if ("lite_box" %in% label_style) {
      label_fill_main <- rep("#FFEEAA", length(x_main));
      label_border_main <- jamba::alpha2col(alpha=0.8,
         jamba::makeColorDarker(venn_spdf$color,
            darkFactor=1.2));
      label_color_main <- rep("#000000", length(x_main));
      label_fill_signed <- rep(label_fill_main,
         lengths(gCounts));
      label_border_signed <- rep(label_border_main,
         lengths(gCounts));
      label_color_signed <- rep(label_color_main,
         lengths(gCounts));
   } else if ("lite" %in% label_style) {
      label_fill_main <- rep("#FFEEAABB", length(x_main));
      label_border_main <- rep(NA, length(x_main));
      label_color_main <- rep("#000000", length(x_main));
      label_fill_signed <- rep(label_fill_main,
         lengths(gCounts));
      label_border_signed <- rep(label_border_main,
         lengths(gCounts));
      label_color_signed <- rep(label_color_main,
         lengths(gCounts));
   }
   ## Adjust signed labels for contrast
   gx <- jamba::unalpha(gbase_colors)
   gy <- ifelse(is.na(label_fill_signed),
      rep(
         jamba::alpha2col(venn_spdf$color,
            alpha=poly_alpha),
         lengths(gCounts)),
      label_fill_signed);
   gbase_colors1 <- gbase_colors;
   gbase_colors <- make_color_contrast(x=jamba::unalpha(gbase_colors),
      y=ifelse(is.na(label_fill_signed),
         rep(
            jamba::alpha2col(venn_spdf$color,
               alpha=poly_alpha),
            lengths(gCounts)),
         label_fill_signed),
      ...);
   
   #print(label_fill_signed);
   #print(venn_spdf$color);
   #print(lengths(gCounts));
   #print(data.frame(gx=gx,
   #   gy=gy,
   #   gbase_colors1=gbase_colors1,
   #   gbase_colors=gbase_colors,
   #   gbase_labels=gbase_labels));
   
   ## Hide zero if display_zero=FALSE
   show_main <- rep(TRUE, length(x_main));
   show_signed <- rep(TRUE, length(x_signed));
   if (!display_zero && any(venn_spdf$venn_counts == 0)) {
      show_main <- !(venn_spdf$venn_counts == 0);
      show_signed <- !(unlist(unname(gCounts)) == 0);
   }
   
   g_labels <- gridtext::richtext_grob(
      text=c(venn_text[show_main],
         gcount_labels[show_signed]),
      x=c(x_main[show_main],
         x_signed[show_signed]),
      y=c(y_main[show_main],
         y_signed[show_signed]),
      default.units="native",
      vjust=c(vjust_main[show_main],
         vjust_signed[show_signed]),
      hjust=c(hjust_main[show_main],
         hjust_signed[show_signed]),
      halign=c(halign_main[show_main],
         halign_signed[show_signed]),
      rot=0,
      padding=grid::unit(c(2), "pt"),
      r=grid::unit(c(2), "pt"),
      gp=grid::gpar(
         col=c(label_color_main[show_main],
            gbase_colors[show_signed]),
         fontsize=rep(c(14, 14) * font_cex,
            c(sum(show_main), sum(show_signed)))
      ),
      box_gp=grid::gpar(
         col=c(label_border_main[show_main],
            label_border_signed[show_signed]),
         fill=c(label_fill_main[show_main],
            label_fill_signed[show_signed]),
         lty=1)
   )

   # Now plot the polygons
   plot(venn_spdf,
      asp=1,
      col=jamba::alpha2col(venn_spdf$color,
         alpha=poly_alpha),
      lwd=2,
      border=jamba::makeColorDarker(
         venn_spdf$color,
         darkFactor=1.2,
         sFactor=1.2));
   
   # to draw using grid we have to use a custom viewport
   vps <- gridBase::baseViewports();
   grid::pushViewport(vps$inner, vps$figure, vps$plot);
   grid::grid.draw(g_labels);
   grid::popViewport(3);

   return(invisible(venn_spdf));   
}
