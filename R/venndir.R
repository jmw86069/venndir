
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
#' ## Detailed workflow
#' 
#' In more detail, input `setlist` is named by set names, and each
#' `vector` contains items.
#' 
#' When the `vector` elements in `setlist` are not named,
#' the values are considered items. In this case, the values are all
#' defined as `1` for the purpose of defining overlaps, and
#' `overlap_type` is automatically set to `overlap_type="overlap"`.
#' At this point the `"sign"` is no longer used.
#' 
#' When the `vector` elements in `setlist` are named, the
#' vector element names are considered items, and vector values
#' are considered the `"sign"`.
#' For common scenarios, the `"sign"` is usually one of the
#' values `c(-1, 1)` to indicate `"up"` and `"down"`. However,
#' the `"sign"` may contain any atomic value, including `numeric`,
#' `integer`, or `character` values for example.
#' 
#' The `setlist` data is passed to `signed_overlaps()` which
#' in turn calls `list2im_signed()`. At this point the incidence
#' matrix values represent the values from each `vector` in `setlist`.
#' 
#' For each item, the `"sign"` is defined as the concatenated signs
#' from each `vector` in `setlist` for that item. For example
#' the `"sign"` may be `"1 1 -1"`, which indicates the item is
#' present in all three `vectors` of `setlist`, and is `"up"`, `"up"`,
#' `"down"` in these vectors. The sign `"0 1 0"` indicates an
#' item is present only in the second `vector` of `setlist` and
#' is `"up"`.
#' 
#' Each item sign is curated by calling `curate_venn_labels()`.
#' This function is used to convert `"sign"` to visual symbols,
#' for example `"1"` may be converted to a Unicode up arrow
#' `"\u2191"`. Unicode output can be disabled with `unicode=FALSE`.
#' The same function converts `"sign"` to color, which can be
#' a helpful visual cue. This step can be customized to use
#' any output valid in R and recognized by `gridtext::richtext_grob()`
#' or `ggtext::geom_richtext()`. Specifically, it can contain
#' Unicode characters, or limited markdown format recognized
#' by these functions.
#' 
#' ## Display of Venn or Euler circles
#' 
#' The overlap counts are used to define suitable Venn circles
#' or ellipses when `proportional=FALSE`, or Euler proportional
#' circles when `proportional=TRUE`. This step is performed
#' by `get_venn_shapes()`.
#' 
#' For Venn circles, the method allows 1, 2, or 3 sets.
#' 
#' For Venn ellipses, the method allows 4 or 5 sets.
#' 
#' For Euler circles, the method allows as many sets as are
#' supported by `eulerr::euler()`.
#' 
#' In the event the circles or ellipses does not include
#' an overlap, a label is printed below the plot. See
#' `render_venndir()` and the argument `plot_warning=TRUE`.
#' For proportional Euler diagrams, even for 3-way diagrams
#' there are often missing overlaps, and this warning is
#' helpful to reinforce what is missing.
#' 
#' 
#' ## Adjusting Venn or Euler circles
#' 
#' As indicated above, when `proportional=TRUE` sometimes
#' the Euler circles do not represent all set overlaps.
#' It may be helpful to nudge one or more circles to
#' represent a missing overlap, using the argument
#' `circle_nudge`. This argument takes a `list` named
#' by one or more `names(setlist)`, of vectors with `c(x, y)`
#' values to "nudge" that set circle.
#' 
#' 
#' ## Display of counts
#' 
#' By default, total counts are displayed for each set overlap.
#' When `setlist` contains signed data, count signs are summarized
#' and displayed beside the total counts. The
#' summary options are defined by `overlap_type`.
#' 
#' Count labels can be styled using `label_style`, which
#' customizes the background color fill and optional border.
#' 
#' 
#' ## Display of items
#' 
#' Displaying item labels inside the polygons can be a convenient
#' way to answer the question, "What are those shared items?"
#' This step can also include the `"sign"`, showing which shared items
#' also have the same or different `"sign"` values.
#' 
#' Note that when items are displayed, summary counts are currently
#' hidden. In future the counts may be positioned outside the
#' polygons.
#' 
#' 
#' ## More customizations
#' 
#' This function actually calls `render_venndir()` to display the
#' diagram. The output from this function can be customized and
#' passed to `render_venndir()` or `ggrender_venndir()` to allow
#' much more customized options.
#' 
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
#' @param show_items `character` indicating the type of item
#'    label: `"none"` does not display item labels; `"sign"`
#'    displays the sign; `"item"` displays the item; `"sign item"`
#'    will display the sign and item together; `"item sign"` will
#'    reverse the order, with item followed by sign. In this
#'    context `"sign"` refers to the incidence values concatenated
#'    by space, sent to `curate_venn_labels(..., type="sign")`.
#' @param show_zero `logical` indicating whether empty overlaps
#'    are labeled with zero `0` when `show_zero=TRUE`, or are
#'    blank when `show_zero=FALSE`.
#' @param font_cex `numeric` `vector` length 2, indicating relative
#'    font size for the main label, and directional label, respectively.
#'    The default `c(1, 0.8)` defines the main label with 1x size,
#'    and the directional labels with 80% that size.
#' @param poly_alpha `numeric` value between 0 and 1, indicating the
#'    alpha transparency of the polygon backgroun color, where
#'    `poly_alpha=1` is completely 100% opaque (no transparency), and
#'    `poly_alpha=0.4` is 40% opaque, therefore 60% transparency.
#' @param alpha_by_counts `logical` indicating whether to define
#'    alpha transparency to Venn polygon fill based upon the counts
#'    contained in each polygon.
#' @param label_style `character` string indicating the style of label
#'    to display:
#'    `"basic"` displays text with no background shading or border,
#'    `"fill"` displays text on opaque colored background,
#'    `"shaded"` displays text on partially transparent colored background,
#'    `"lite"` displays text on partially transparent lite background,
#'    `"lite_box"` displays text on lite background with border.
#' @param unicode `logical` passed to `curate_venn_labels()`
#'    indicating whether the directional label can include special
#'    Unicode characters.
#' @param big.mark `character` passed to `format()` for numeric labels.
#' @param curate_df `data.frame` or `NULL` passed to `curate_venn_labels()`.
#' @param plot_style `character` indicating the style of graphics plot:
#'    `"gg"` uses ggplot2 and calls `ggrender_venndir()`; `"base"`
#'    uses base R graphics and calls `render_venndir()`.
#' @param ... additional arguments are passed to `render_venndir()`.
#' 
#' @family venndir core
#' 
#' @examples
#' setlist <- make_venn_test(100, 3);
#' print(setlist);
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
#'    show_zero=FALSE,
#'    circle_nudge=list(set_C=c(-0.5, 0.1))
#' )
#' 
#' # nudge circles so one overlap is no longer shown
#' venndir(setlist,
#'    proportional=TRUE,
#'    show_zero=FALSE,
#'    circle_nudge=list(set_C=c(-1.4, 0.1))
#' )
#' 
#' setlist2k <- make_venn_test(2000, 3, 80, do_signed=TRUE);
#' venndir(setlist2k)
#' venndir(setlist2k, proportional=TRUE)
#' 
#' # example using character values
#' setlist <- make_venn_test(100, 3, do_signed=TRUE)
#' # make a simple character vector list
#' setlistv <- lapply(setlist, function(i){
#'    j <- letters[i+3];
#'    names(j) <- names(i);
#'    j;
#' });
#' # make custom curate_df
#' curate_df <- data.frame(
#'    from=c("b", "d"),
#'    sign=c("b", "d"),
#'    color=c("blue", "red"),
#'    stringsAsFactors=FALSE)
#' vo <- venndir(setlistv,
#'    overlap_type="each",
#'    font_cex=c(1.5, 0.9), 
#'    curate_df=curate_df,
#'    show_zero=TRUE);
#' 
#' 
#' @export
venndir <- function
(setlist,
 overlap_type=c("concordance", "each", "overlap", "agreement"),
 sets=NULL,
 set_colors=NULL,
 proportional=FALSE,
 return_items=FALSE,
 show_set=c("main", "all", "none"),
 show_label=NA,
 show_items=c(NA, "none", "sign item", "sign", "item"),
 max_items=3000,
 display_counts=TRUE,
 show_zero=FALSE,
 font_cex=c(1, 0.8),
 poly_alpha=0.5,
 alpha_by_counts=FALSE,
 label_style=c("basic",
    "fill",
    "shaded",
    "shaded_box",
    "lite",
    "lite_box"),
 circle_nudge=NULL,
 rotate_degrees=0,
 unicode=TRUE,
 big.mark=",",
 sep="&",
 curate_df=NULL,
 plot_style=c("base", "gg"),
 do_plot=TRUE,
 verbose=FALSE,
 ...)
{
   # basic workflow:
   # - get signed_overlaps()
   # - use counts per Venn set to create Venn circles/ovals
   #   - proportional Euler, or not proportional Venn
   # - find polygon overlaps for the circles/ovals
   #   - identify any overlaps not represented (to label on the side)
   
   overlap_type <- match.arg(overlap_type);
   plot_style <- match.arg(plot_style);
   show_set <- match.arg(show_set);
   
   label_style <- head(label_style, 1);
   show_items <- head(show_items, 1);
   if (length(show_items) == 0) {
      show_items <- NA;
   }
   if (!all(show_items %in% c("none","FALSE"))) {
      return_items <- TRUE;
   }

   if (length(font_cex) < 2) {
      if (length(font_cex) == 0) {
         font_cex <- c(1, 0.8);
      }
      font_cex <- rep(font_cex, length.out=2);
   }
   if (length(sets) == 0) {
      sets <- seq_along(setlist);
   } else if ("character" %in% class(sets)) {
      sets <- jamba::rmNA(match(sets, names(setlist)));
      if (length(sets) == 0) {
         stop("sets did not match any values in names(setlist)");
      }
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
      return_items=return_items,
      sep=sep,
      ...);
   overlap_type <- attr(sv, "overlap_type");
   
   # numeric counts by set
   nCounts <- sapply(unique(sv$sets), function(i){
      sum(subset(sv, sets %in% i)$count)
   });
   names(nCounts) <- unique(sv$sets);
   # formatted numeric counts
   fCounts <- format(big.mark=big.mark,
      trim=TRUE,
      nCounts);
   # subgrouped counts (directional)
   if ("overlap" %in% overlap_type) {
      #gCounts <- list(NULL);
      gCounts <- lapply(jamba::nameVector(unique(sv$sets)), function(i){
         NULL
      });
      gbase_signs <- NULL;
   } else {
      gCounts <- lapply(jamba::nameVector(unique(sv$sets)), function(i){
         j <- subset(sv, sets %in% i);
         jamba::nameVector(j$count, j$overlap_label)
      });
      gbase_signs_l <- lapply(jamba::nameVector(unique(sv$sets)), function(i){
         j <- subset(sv, sets %in% i);
         jamba::nameVector(j$count, paste0(j$sets, "|", j[[overlap_type]]))
      });
      gbase_signs <- unlist(unname(gbase_signs_l));
   }
   
   # get Venn circles
   venn_sp <- get_venn_shapes(counts=nCounts,
      proportional=proportional,
      circle_nudge=circle_nudge,
      sep=sep,
      ...);
   
   # optionally rotate the shapes
   if (length(rotate_degrees) > 0 && rotate_degrees != 0) {
      venn_sp <- rescale_sp(sp=venn_sp,
         share_center=TRUE,
         rotate_degrees=rotate_degrees);
   }

   # convert to venn overlap polygons
   venn_spdf <- find_vennpoly_overlaps(venn_sp,
      venn_counts=nCounts,
      venn_colors=set_color,
      sep=sep,
      ...); # removed ...
   
   # generate labels from nCounts and gCounts
   nlabel_df <- data.frame(label=names(nCounts),
      venn_counts=nCounts,
      stringsAsFactors=FALSE,
      check.names=FALSE);
   nlabel_df <- jamba::mergeAllXY(data.frame(venn_spdf), nlabel_df);
   nlabel_df <- nlabel_df[match(names(nCounts), nlabel_df$label),,drop=FALSE];
   nlabel_df$color <- jamba::rmNA(nlabel_df$color,
      naValue="#FFFFFFFF");
   nlabel_df$venn_color <- jamba::rmNA(nlabel_df$venn_color,
      naValue="#FFFFFFFF");
   # repair x_label and y_label stored as list
   nlabel_df$x_label <- unlist(jamba::rmNULL(nlabel_df$x_label, nullValue=NA))
   nlabel_df$y_label <- unlist(jamba::rmNULL(nlabel_df$y_label, nullValue=NA))
   
   # Now add the main count labels
   if ("main" %in% show_set) {
      venn_text <- ifelse(grepl(sep, fixed=TRUE, x=nlabel_df$label),
         jamba::formatInt(
            jamba::rmNA(naValue=0,
               nlabel_df$venn_counts)),
         paste0("**",
            nlabel_df$label,
            "**<br>\n",
            jamba::formatInt(jamba::rmNA(naValue=0, nlabel_df$venn_counts))));
   } else if ("all" %in% show_set) {
      venn_text <- paste0("**",
         nlabel_df$label,
         "**<br>\n",
         jamba::formatInt(jamba::rmNA(naValue=0, nlabel_df$venn_counts)));
   } else {
      venn_text <- jamba::formatInt(
         jamba::rmNA(naValue=0,
            nlabel_df$venn_counts));
   }
   x_main <- nlabel_df$x_label;
   y_main <- nlabel_df$y_label;
   vjust_main <- rep(0.5, length(x_main));
   halign_main <- rep(0.5, length(x_main));
   #hjust_main <- ifelse(nchar(up_text) == 0 & nchar(dn_text) == 0, 0.5, 1);
   hjust_main <- rep(1, length(x_main));
   if ("overlap" %in% overlap_type) {
      hjust_main <- rep(0.5, length(x_main));
   }
   
   ## Labels for each overlap
   gbase_labels <- curate_venn_labels(
      names(unlist(unname(gCounts))),
      unicode=unicode,
      "sign",
      curate_df=curate_df,
      ...);
   gbase_colors <- curate_venn_labels(
      names(unlist(unname(gCounts))),
      unicode=unicode,
      "color",
      curate_df=curate_df,
      ...);
   gcount_labels <- sapply(seq_along(unlist(gCounts)), function(i){
      ilabel <- paste0(
         gbase_labels[i],
         ": ",
         format(trim=TRUE,
            big.mark=big.mark,
            unlist(gCounts)[i]));
   });
   ## order labels again?
   gdf <- jamba::mixedSortDF(data.frame(
      group=rep(seq_along(gCounts), lengths(gCounts)),
      gbase_labels=gbase_labels,
      label=rep(names(gCounts), lengths(gCounts)),
      index=seq_along(gbase_labels),
      stringsAsFactors=FALSE,
      check.names=FALSE), byCols=c(1, 2));
   gbase_labels <- gbase_labels[gdf$index];
   gbase_colors <- gbase_colors[gdf$index];
   gcount_labels <- gcount_labels[gdf$index];
   gcount_sorted <- unlist(gCounts)[gdf$index];
   gbase_signs <- gbase_signs[gdf$index];

   gCounts_len <- lengths(gCounts);

   x_signed <- rep(x_main, gCounts_len);
   y_signed <- rep(y_main, gCounts_len);
   vjust_signed <- unname(unlist(lapply(gCounts_len, function(i){
      iseq <- seq_len(i) - 1;
      iseq - mean(iseq);
   }))) + 0.5;
   hjust_signed <- rep(0, length(x_signed));
   halign_signed <- rep(0, length(x_signed));
   
   if (alpha_by_counts) {
      poly_alpha <- jamba::normScale(sqrt(nlabel_df$venn_counts),
         low=0,
         from=0,
         to=1);
   }
   
   ## label_style
   label_fill_main <- rep(NA, length(x_main));
   label_border_main <- rep(NA, length(x_main));
   label_color_main <- jamba::setTextContrastColor(
      jamba::alpha2col(nlabel_df$color,
      alpha=poly_alpha));
   label_fill_signed <- rep(NA, length(x_signed));
   label_border_signed <- rep(NA, length(x_signed));
   label_color_signed <- jamba::setTextContrastColor(
      jamba::alpha2col(rep(nlabel_df$color, gCounts_len),
         alpha=poly_alpha));
   
   ## replace or remove? 
   # venndir_label_style() performs this function
   if (grepl("_box", label_style)) {
      label_border_main <- jamba::alpha2col(alpha=0.8,
         jamba::makeColorDarker(nlabel_df$color,
            darkFactor=1.2));
      label_border_signed <- rep(label_border_main,
         gCounts_len);
   }
   if (grepl("fill", label_style)) {
      label_fill_main <- nlabel_df$color;
      label_color_main <- jamba::setTextContrastColor(nlabel_df$color);
      label_fill_signed <- rep(label_fill_main,
         gCounts_len);
      label_color_signed <- rep(label_color_main,
         gCounts_len);
   }
   if (grepl("shaded", label_style)) {
      label_fill_main <- jamba::alpha2col(nlabel_df$color,
         alpha=0.5);
      label_color_main <- jamba::setTextContrastColor(
         jamba::alpha2col(nlabel_df$color,
            alpha=mean(c(poly_alpha, 0.9))));
      label_fill_signed <- rep(label_fill_main,
         gCounts_len);
      label_color_signed <- rep(label_color_main,
         gCounts_len);
   }
   if (grepl("lite", label_style)) {
      label_fill_main <- rep("#FFEEAABB", length(x_main));
      label_color_main <- rep("#000000", length(x_main));
      label_fill_signed <- rep(label_fill_main,
         gCounts_len);
      label_color_signed <- rep(label_color_main,
         gCounts_len);
   }

   ## Hide zero if show_zero=FALSE
   show_main <- (!is.na(nlabel_df$x_label));
   show_signed <- rep(show_main, gCounts_len);
   if (!show_zero && any(nlabel_df$venn_counts == 0)) {
      show_main <- (show_main & nlabel_df$venn_counts != 0);
   }
   show_signed <- (show_signed & unlist(gCounts) != 0)

   ## Update other polygon display attributes
   venn_spdf$alpha <- poly_alpha;
   venn_spdf$lwd <- rep(2, nrow(venn_spdf));
   venn_spdf$lty <- rep(1, nrow(venn_spdf));
   venn_spdf$border <- jamba::makeColorDarker(
      venn_spdf$color,
      darkFactor=1.2,
      sFactor=1.2);

   ## Prepare label data.frame
   label_n <- length(c(x_main, x_signed));
   label_df <- data.frame(
      x=c(x_main, x_signed),
      y=c(y_main, y_signed),
      text=unlist(c(venn_text, gcount_labels)),
      venn_counts=c(nCounts, gcount_sorted),
      overlap_set=c(nlabel_df$label,
         rep(nlabel_df$label, gCounts_len)),
      type=rep(c("main", "signed"),
         c(length(x_main), length(x_signed))),
      #show_label=c(show_main, show_signed),
      show_label=NA,
      vjust=c(vjust_main, vjust_signed),
      hjust=c(hjust_main, hjust_signed),
      halign=c(halign_main, halign_signed),
      rot=rep(0, label_n),
      color=unlist(c(label_color_main, gbase_colors)),
      fontsize=rep(c(14, 14) * font_cex,
         c(length(x_main), length(x_signed))),
      border=c(label_border_main, label_border_signed),
      lty=rep(1, label_n),
      lwd=rep(1, label_n),
      fill=c(label_fill_main, label_fill_signed),
      padding=rep(2, label_n),
      padding_unit=rep("pt", label_n),
      r=rep(2, label_n),
      r_unit=rep("pt", label_n),
      stringsAsFactors=FALSE,
      check.names=FALSE
   );
   if ("overlap" %in% overlap_type) {
      sv_match <- match(label_df$overlap_set, sv$sets);
      label_df$overlap_sign <- paste0(sv$sets, "|", sv$overlap)[sv_match];
   } else {
      label_df$overlap_sign <- c(rep("", length(x_main)),
         names(gbase_signs));
   }
   
   ## Check for missing signed labels, then we nudge main label to center
   for (i in unique(label_df$overlap_set)) {
      irows <- (label_df$overlap_set %in% i & 
            label_df$show_label %in% c(NA,TRUE));
      if (length(unique(label_df[irows,"type"])) == 1) {
         label_df[irows,"hjust"] <- 0.5;
      }
   }

   ## remove or replace with venndir_label_style()
   ## Adjust signed color
   g_update <- (label_df$type %in% "signed" &
      label_df$show_label %in% c(NA,TRUE) & 
      (!is.na(label_df$fill) |
         (label_df$overlap_set %in% venn_spdf$label)));
   if (any(g_update)) {
      g_update_match <- match(label_df$overlap_set[g_update],
         venn_spdf$label);
      g_update_color <- label_df$color[g_update];
      g_update_fill <- ifelse(is.na(label_df$fill[g_update]),
         jamba::alpha2col(
            venn_spdf$color[g_update_match],
            alpha=venn_spdf$alpha[g_update_match]),
         label_df$fill[g_update]);
      label_df$color[g_update] <- make_color_contrast(g_update_color, g_update_fill);
   }
   ## remove or replace
   ## this logic is inside render_venndir()
   if (!"overlap" %in% overlap_type && any(label_df$type %in% "signed")) {
      ## Adjust signed labels for contrast
      is_signed <- label_df$type %in% "signed";
      poly_match <- match(label_df$overlap_set, venn_spdf$label);
      poly_fill <- jamba::alpha2col(
         data.frame(venn_spdf)[poly_match,"color"],
         alpha=data.frame(venn_spdf)[poly_match,"alpha"]);
      label_fill <- ifelse(is.na(label_df$fill),
         poly_fill,
         label_df$fill);
      bg_fill <- ifelse(is.na(label_df$fill),
         "white",
         poly_fill);
      label_col1 <- label_df$color;
      label_col <- make_color_contrast(
         x=label_df$color,
         y=label_fill,
         bg=bg_fill,
         ...);
      label_df$color[is_signed] <- label_col[is_signed];
   }
   
   ## Optionally return items
   if (return_items) {
      sv_label <- paste0(sv$sets, "|", sv[[overlap_type]]);
      sv_match <- match(label_df$overlap_sign, sv_label);
      label_df$items <- I(sv$items[sv_match]);
   }
   
   ## Call render_venndir()
   gg <- NULL;
   if (!display_counts) {
      #label_df$show_label <- FALSE;
      label_df$display_counts <- FALSE;
   }
   if (do_plot) {
      if (any(c("gg", "base") %in% plot_style)) {
         gg <- render_venndir(venn_spdf=venn_spdf,
            label_df=label_df,
            show_label=show_label,
            show_items=show_items,
            show_zero=show_zero,
            display_counts=display_counts,
            label_style=label_style,
            plot_style=plot_style,
            max_items=max_items,
            ...);
      } else {
         gg <- ggrender_venndir(venn_spdf=venn_spdf,
            label_df=label_df,
            show_items=show_items,
            display_counts=display_counts,
            ...);
         print(gg);
      }
   }

   return(invisible(
      list(
         venn_spdf=venn_spdf,
         label_df=label_df,
         gg=gg)));
}
