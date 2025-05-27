

#' venndir label style
#' 
#' venndir label style
#' 
#' This internal function applies a label style to `venndir()` output,
#' overwriting the existing label style as necessary.
#' 
#' The label text color is adjusted to optimize visual contrast via
#' `make_color_contrast()`, which is useful when positioning
#' the label on top of dark or bright colors. If the label
#' is positioned outside the disgram, the text is assumed
#' to be on a white background, but a custom background color can
#' be defined using argument `bg`.
#' 
#' @family venndir internal
#'  
#' @param venndir_output `Venndir` object as returned by `venndir()`
#'     or `render_venndir()`.
#' @param show_labels `character` string to describe which count labels
#'    to display, and where. The presence of each letter enables each
#'    label, and UPPERCASE places the label outside the Venn diagram,
#'    while lowercase places the label inside.
#'    The default `"Ncs"` displays _N_ame (outside), _c_ount (inside),
#'    and _s_igned count (inside). When `overlap_type="overlap"` then
#'    the signed label is hidden by default.
#'    
#'    The label types are defined below:
#'    * _N_ame: "n" or "N" - the set name, by default it is displayed.
#'    * _O_verlap: "o" or "O" - the overlap name, by default it is hidden,
#'    because these labels can be very long, also the overlap should be
#'    evident in the Venn diagram already.
#'    * _c_ount: "c" or "C" - overlap count, independent of the sign
#'    * _p_ercentage: "p" or "P" - overlap percentage, by default hidden,
#'    but available as an option
#'    * _s_igned count: "s" or "S" - the signed overlap count, tabulated
#'    based upon `overlap_type` ("each", "concordant", "agreement", etc/)
#'    * _i_tems: "i" only, by default hidden. When enabled, item labels
#'    defined by `show_items` are spread across the specific Venn overlap
#'    region.
#' @param label_style `character` string indicating the style of label
#'    to display. The values `"basic","none","shaded","lite","fill"`
#'    style the label background fill, while presence of `"box"` in
#'    the string will draw a border around the label:
#'    `"basic"` or `"none"` uses no background fill,
#'    `"lite"` uses lite background fill,
#'    `"fill"` uses opaque fill with the overlap set color,
#'    `"shaded"` uses slightly transparent fill with overlap set color,
#'    `"box"` displays border around the label.
#' @param lite `character` color used when `label_preset` contains `"lite"`.
#' @param bg `character` color used as the background color of the
#'    figure, used with outside labels to determine whether the text
#'    should be light or dark for proper visual contrast.
#' @param label_preset DEPRECATED in favor of `show_labels`.
#' @param set,overlap,percent,count,signed,items DEPRECATED in favor
#'    of `show_labels`.
#' @param percent_delim `character` string used only when both count
#'    and percent labels are enabled, as a delimiter between the two
#'    labels. The default `"<br>"` causes a newline, so the count
#'    and percent values are on separate lines. Another suggestion is
#'    `": "` which separates the two values with semicolon on one line.
#' @param show_items `character` string for the item label content, used
#'    only when items are displayed.
#' @param max_items `numeric` maximum number of labels permitted when
#'    items are displayed. When there are too many items, the item label
#'    is suppressed.
#' @param inside_percent_threshold `numeric` size for each polygon below
#'    which labels are moved outside, for labels that would otherwise
#'    be displayed inside. Item labels are not affected by this setting.
#'    The threshold is calculated as a percent of the overall Venn diagram
#'    polygon area.
#' @param label_types `character` vector with one or more label types
#'    to be affected by this function. By default `"count"` and `"signed"`
#'    labels (all labels) are affected.
#' @param extra_styles `list` of two-element vectors, named by the type of
#'    label, default `list(percent=c("***", "***"))` will use `"***"`
#'    before and after each percentage label.
#'    No other label types are supported.
#' @param show_zero `logical` indicating whether to display zero `0`
#'    for empty overlaps for which the overlap polygon exists. Default FALSE
#'    hides the display of zeros.
#' @param sep `character` string used as delimiter between Venn set names.
#'    This value should generally not be changed.
#' @param useGrey `numeric` value used by `jamba::setTextContrastColor()`
#'    to define an appropriate contrasting color which retains some color
#'    saturation, default 15. Use `useGrey=0` would cause black or white
#'    labels with no color saturation.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to internal functions such
#'    as `make_color_contrast()`.
#' 
#' @examples
#' v <- venndir(make_venn_test(), do_plot=FALSE)
#' v2 <- venndir_label_style(v, label_style="shaded box")
#' plot(v2)
#' 
#' v3 <- venndir_label_style(v2, label_style="lite box")
#' plot(v3)
#' @export
venndir_label_style <- function
(venndir_output,
 show_labels=c("Nsc"),
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
 set=c("outside",
    "inside",
    "none"),
 overlap=c("none",
    "inside",
    "outside"),
 percent=c("none",
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
 percent_delim="<br>",
 show_items=NULL,
 max_items=3000,
 inside_percent_threshold=0,
 label_types=c("main", "signed"),
 extra_styles=list(percent=c("***", "***")),
 show_zero=TRUE,
 sep="&",
 useGrey=15,
 verbose=FALSE,
 ...)
{
   ## validate show_labels: NOCPSI
   # - Name, Overlap, Count, Percent, Sign, Item
   if (length(show_labels) == 0) {
      if ("show_labels" %in% names(attributes(venndir_output))) {
         show_labels <- attributes(venndir_output)$show_labels;
      } else {
         show_labels <- "";
      }
   }
   if (length(show_items) > 0) {
      metadata(venndir_output)$show_items <- show_items;
   } else if (length(metadata(venndir_output)$show_items) > 0) {
      show_items <- metadata(venndir_output)$show_items;
   } else {
      show_items <- "none";
      metadata(venndir_output)$show_items <- show_items;
   }
   use_nocpsi <- TRUE;
   if (use_nocpsi) {
      nocpsi <- c(N="set",
         O="overlap",
         C="count",
         P="percent",
         S="signed",
         I="items");
      for (i in names(nocpsi)) {
         if (any(grepl(tolower(i), show_labels))) {
            # enable inside label
            assign(nocpsi[i], "inside");
         } else if (any(grepl(toupper(i), show_labels))) {
            # enable outside label
            assign(nocpsi[i], "outside");
         } else {
            # disable this label
            assign(nocpsi[i], "none");
         }
      }
   } else {
      if ("none" %in% label_preset) {
         label_preset <- "main outside"
      }
   }
   # Todo: Validate label display.
   # signed - this check could be venndir()
   # - verify setlist contains signed data
   # 
   # set/overlap
   # - if overlap is not "none" then set="none"
   #
   # items
   # - currently items can only be "inside"
   # - if items=="inside"
   #    - verify items exist in venndir_output
   #    - verify item count <= max_items, otherwise items="none"
   # - if items=="inside", all other non-"none" labels must be "outside"
   
   ## Validate nocpsi
   if (use_nocpsi) {
   }
   
   # TODO:
   # check whether label coordinate overlaps the polygon
   # if yes then use contrasting text with combined background colors
   # if no then assume white background
   label_types <- match.arg(label_types,
      several.ok=TRUE);   
   
   # handle Venndir or JamPolygon input
   vo <- list();
   if ("Venndir" %in% class(venndir_output)) {
      if (verbose) {
         jamba::printDebug("venndir_label_style(): ",
            "Venndir input.");
      }
      vo <- venndir_output;
      venndir_output <- list();
      venndir_output$venn_spdf <- vo@jps@polygons;
      venndir_output$label_df <- vo@label_df;
   } else if ("list" %in% class(venndir_output) && "vo" %in% names(venndir_output)) {
      ## legacy list with "vo" as Venndir object
      # to be removed in future
      vo <- venndir_output$vo;
      venndir_output <- list();
      venndir_output$venn_spdf <- vo@jps@polygons;
      venndir_output$label_df <- vo@label_df;
   }
   if (length(vo) == 0) {
      stop("Input venndir_output was not recognized as 'Venndir' object.")
   }
   # apply label_style to label_df
   label_style <- rep(head(label_style, 1),
      length.out=nrow(venndir_output$label_df));

   # validate arguments
   label_preset <- match.arg(label_preset);
   if (!"none" %in% label_preset) {
      if (verbose) {
         jamba::printDebug("venndir_label_style(): ",
            "Applying label_preset: '", label_preset, "'");
      }
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
   }
   if (verbose) {
      jamba::printDebug("venndir_label_style(): ",
         "set: ", set,
         ", overlap: ", overlap,
         ", count: ", count,
         ", percent: ", percent,
         ", signed: ", signed,
         ", items: ", items);
   }
   
   ## Update venndir_output$label_df$text dependent upon percent
   use_count <- rep(count, length.out=nrow(venndir_output$label_df));
   use_percent <- rep(percent, length.out=nrow(venndir_output$label_df));
   sum_counts <- sum(
      subset(venndir_output$label_df,
         type %in% "main")$venn_counts, na.rm=TRUE)
   # jamba::printDebug("sum_counts:", sum_counts);# debug
   
   # optional workaround to display percent overlap
   use_percent_values <- (venndir_output$label_df$venn_counts /
         sum_counts * 100);
   use_percent_values <- ifelse(use_percent_values < 1 & use_percent_values > 0,
      # format(use_percent_values, digits=1),
      sapply(use_percent_values, format, digits=1),
      round(use_percent_values))
   pct1 <- "***";
   pct2 <- "***";
   if ("percent" %in% names(extra_styles)) {
      pcts <- rep(extra_styles[["percent"]], length.out=2);
      pct1 <- pcts[1];
      pct2 <- pcts[2];
   }
   use_text_df <- data.frame(
      count=ifelse(use_count %in% "none", "",
         jamba::formatInt(venndir_output$label_df$venn_counts, ...)),
      percent=ifelse(use_percent %in% "none", "",
         paste0(pct1, use_percent_values, "%", pct2)))
   # paste in order?
   use_text <- jamba::pasteByRow(use_text_df,
      sep=percent_delim);
   if (length(show_labels) > 0 && any(nchar(show_labels) > 0)) {
      use_text2 <- jamba::pasteByRow(use_text_df[, 2:1, drop=FALSE],
         sep=percent_delim);
      use_show_labels <- rep(show_labels,
         length.out=nrow(venndir_output$label_df));
      use_text <- ifelse(grepl("p.*c", ignore.case=TRUE, use_show_labels),
         use_text2,
         use_text)
   }
   # assign to "text" column only for label type="main"
   venndir_output$label_df$text <- ifelse(
      venndir_output$label_df$type %in% "main",
      use_text,
      venndir_output$label_df$text)
   
   # jamba::printDebug("use_count:");print(use_count);# debug
   # jamba::printDebug("use_percent:");print(use_percent);# debug
   # jamba::printDebug("use_text_df:");print(use_text_df);# debug
   # jamba::printDebug("use_text:");print(use_text);# debug
   # jamba::printDebug("label_df$venn_counts:");print(label_df$venn_counts);# debug
   apply_count <- ifelse(use_count %in% "none",
      ifelse(use_percent %in% "none",
         "none",
         use_percent),
      use_count)
   count <- apply_count;
      
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
   venn_spdf_df$rownum <- seq_len(nrow(venndir_output$venn_spdf));
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
   #
   jp_area <- area_JamPolygon(vo@jps);
   union_jp <- union_JamPolygon(vo@jps);
   total_jp_area <- area_JamPolygon(union_jp);
   sp_pct_area <- jp_area / total_jp_area * 100;

   poly_pct_area <- jamba::rmNA(sp_pct_area[sp_index],
      naValue=-1);
   if (length(inside_percent_threshold) == 0) {
      inside_percent_threshold <- c(0)
   }
   label_area_ok <- (poly_pct_area >= inside_percent_threshold);
   # if (verbose) jamba::printDebug("venndir_label_style(): ", "jp_area:", jp_area);# debug
   # if (verbose) jamba::printDebug("venndir_label_style(): ", "total_jp_area:", total_jp_area);# debug
   # if (verbose) jamba::printDebug("venndir_label_style(): ", "poly_pct_area:", poly_pct_area);# debug
   # if (verbose) jamba::printDebug("venndir_label_style(): ", "label_area_ok:", label_area_ok);# debug
   
   # we need the total counts per overlap_set in order to apply max_items
   main_label_df <- subset(venndir_output$label_df, type %in% "main");
   main_match <- match(venndir_output$label_df$overlap_set,
      main_label_df$overlap_set);
   venndir_output$label_df$main_venn_counts <- main_label_df$venn_counts[main_match];
   
   # 0.0.34.900 - confirm x,y coordinates when ref_polygon != overlap_set
   if ("ref_polygon" %in% colnames(venndir_output$label_df)) {
      need_xy_update <- (!is.na(venndir_output$label_df$ref_polygon) &
         venndir_output$label_df$overlap_set !=
            venndir_output$label_df$ref_polygon);
      if (any(need_xy_update)) {
         matchxy <- match(venndir_output$label_df$ref_polygon[need_xy_update],
            venndir_output$label_df$overlap_set);
         venndir_output$label_df[need_xy_update, c("x", "y")] <- (
            venndir_output$label_df[matchxy, c("x", "y")]);
      }
   }
   
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
         set_is_hidden <- (label_is_set &
               is.na(venndir_output$label_df$x) &
               is.na(venndir_output$label_df$ref_polygon) &
               label_has_shape);
         set_is_not_hidden <- (label_is_set &
               !set_is_hidden);
         venndir_output$label_df$set_is_hidden <- set_is_hidden;
         ## 0.0.34.900 - unsure if these variables are useful to store
         venndir_output$label_df$set_is_not_hidden <- set_is_not_hidden;
         if (any(set_is_hidden)) {
            set_hidden <- venndir_output$label_df$overlap_set[set_is_hidden];
            set_hidden_match <- match(set_hidden,
               venndir_output$venn_spdf$label);
            # jamba::printDebug("set_hidden_match:");print(set_hidden_match);# debug
            # jamba::printDebug("venn_polygons_df:");print(venndir_output$venn_spdf);# debug
            # 0.0.34.900 - change to use ref_polygon
            venndir_output$label_df[set_is_hidden, c("x", "y",
               "x_offset", "y_offset")] <- data.frame(
                  venndir_output$venn_spdf)[set_hidden_match, c("x_label",
                     "y_label", "x_offset", "y_offset")];
            venndir_output$label_df$overlap[set_is_hidden] <- "outside";
            # jamba::printDebug("venndir_label_style() hidden sets label_df:");print(venndir_output$label_df);# debug
            # stop("Stopping here");# debug
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
      # propagate item style for visible rows
      venndir_output$label_df$item_style <- ifelse(
         venndir_output$label_df$show_items %in% "none",
         "none",
         show_items)
      # jamba::printDebug("show_items:");print(show_items);# debug
      # jamba::printDebug("venndir_label_style(): ", "venndir_output$label_df:");print(venndir_output$label_df);# debug
      
      ######################################################
      # count labels
      # new logic
      venndir_output$label_df$count <- ifelse(
         venndir_output$label_df$show_items %in% "none",
         # inside can display count, no items are inside
         ifelse(
            venndir_output$label_df$type %in% "main",
            ifelse(
               # it must have non-zero counts, or we set show_zero=TRUE
               (venndir_output$label_df$venn_counts > 0 | show_zero) &
                  # overlap_set must equal ref_polygon so counts are only shown for the precise overlap
                  (venndir_output$label_df$overlap_set == venndir_output$label_df$ref_polygon),
               ifelse(
                  any(c("inside", "ifneeded", "detect") %in% count),
                  "inside",
                  count #"none"
               ),
               "none"
            ),
            ifelse(
               # it must have non-zero counts, or we set show_zero=TRUE
               (venndir_output$label_df$venn_counts > 0 | show_zero) &
                  # overlap_set must equal ref_polygon so counts are only shown for the precise overlap
                  (venndir_output$label_df$overlap_set == venndir_output$label_df$ref_polygon),
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
               # it must have non-zero counts, or we set show_zero=TRUE
               (venndir_output$label_df$venn_counts > 0 | show_zero) &
                  # overlap_set must equal ref_polygon so counts are only shown for the precise overlap
                  (venndir_output$label_df$overlap_set == venndir_output$label_df$ref_polygon),
               ifelse(
                  any(c("detect", "outside") %in% count),
                  "outside",
                  "none"
               ),
               "none"
            ),
            ifelse(
               # it must have non-zero counts, or we set show_zero=TRUE
               (venndir_output$label_df$venn_counts > 0 | show_zero) &
                  # overlap_set must equal ref_polygon so counts are only shown for the precise overlap
                  (venndir_output$label_df$overlap_set == venndir_output$label_df$ref_polygon),
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
      # jamba::printDebug("venndir_output$label_df:");print(venndir_output$label_df);
      # jamba::printDebug("venndir_output$venn_spdf:");print(venndir_output$venn_spdf);
      has_outside <- (venndir_output$label_df$overlap %in% "outside" |
            venndir_output$label_df$count %in% "outside");
      # jamba::printDebug("venndir_output$venn_spdf$x_outside[sp_index2[has_outside]]:");print(venndir_output$venn_spdf$x_outside[sp_index2[has_outside]]);
      if (any(has_outside)) {
         venndir_output$label_df$x_offset[has_outside] <- (venndir_output$venn_spdf$x_outside[sp_index2[has_outside]] - 
               venndir_output$label_df$x[has_outside]);
         venndir_output$label_df$y_offset[has_outside] <- (venndir_output$venn_spdf$y_outside[sp_index2[has_outside]] - 
               venndir_output$label_df$y[has_outside]);
      }
   }
   
   # toupdate
   toupdate <- venndir_output$label_df$type %in% label_types;
   
   # jamba::printDebug("venndir_output$label_df:");print(venndir_output$label_df);# debug
   # jamba::printDebug("venndir_output$label_df:");print(subset(venndir_output$label_df, ref_polygon %in% "set_B"));# debug
   # stop("stopping here");# debug
   
   # determine whether label coordinates overlap a polygon
   # xy_overlaps is NA when the point does not overlap a polygon,
   # and integer rownum when it overlaps a polygon
   xy_overlaps <- sapply(seq_len(nrow(venndir_output$label_df)), function(i){
      ixy <- cbind(
         sum(c(venndir_output$label_df$x[i],
            venndir_output$label_df$x_offset[i])),
         sum(c(venndir_output$label_df$y[i],
            venndir_output$label_df$y_offset[i])));
      # jamba::printDebug("row i:", i, ", ixy:");print(ixy);
      if (any(is.na(ixy))) {
         return(NA)
      }
      if (length(vo) > 0) {
         P <- list(x=ixy[,1], y=ixy[,2]);
         # use_jp <- vo@jps[vo@jps@polygons$type %in% "overlap", ];
         jpwhich <- which(vo@jps@polygons$type %in% "overlap")
         for (j in jpwhich) {
            test_jp <- vo@jps[j, ];
            # jamba::printDebug("P:");print(P);
            # jamba::printDebug("test_jp:");print(test_jp);
            # confirm there are polygon coordinates before testing
            if (length(jamba::rmNA(unlist(test_jp@polygons$x))) > 0 &&
                  1 %in% point_in_JamPolygon(x=P, jp=test_jp)) {
               return(j)
            }
         }
      } else if ("venn_jps" %in% names(venndir_output)) {
         # jamba::printDebug("testing point overlap with JamPolygon");
         P <- list(x=ixy[,1], y=ixy[,2]);
         # use_jp <- venndir_output$venn_jps[venndir_output$venn_jps@polygons$type %in% "overlap", ];
         spwhich <- which(venndir_output$venn_jps@polygons$type %in% "overlap")
         for (j in spwhich) {
            test_jp <- venndir_output$venn_jps[j, ];
            # jamba::printDebug("P:");print(P);
            # jamba::printDebug("test_jp:");print(test_jp);
            # confirm there are polygon coordinates before testing
            if (length(jamba::rmNA(unlist(test_jp@polygons$x))) > 0 &&
               1 %in% point_in_JamPolygon(x=P, jp=test_jp)) {
               return(j)
            }
         }
      }
      return(NA)
   });
   
   ## Update the input Venndir object in place
   vo@jps@polygons <- venndir_output$venn_spdf;
   
   # label_bg is the background color when the label is inside
   
   # jamba::printDebug("venndir_output$label_df:");print(head(venndir_output$label_df));
   # jamba::printDebug("venndir_output$venn_jps@polygons:");print(head(venndir_output$venn_jps@polygons));
   # jamba::printDebug("venndir_output$venn_spdf:");print(head(venndir_output$venn_spdf));
   # jamba::printDebug("xy_overlaps:");print(xy_overlaps);
   # jamba::printDebug("bg:");print(bg);
   if (length(vo) > 0) {
      label_bg <- ifelse(is.na(xy_overlaps),
         rep(bg, length.out=length(xy_overlaps)),
         jamba::alpha2col(
            vo@jps@polygons$fill[xy_overlaps],
            alpha=vo@jps@polygons$alpha[xy_overlaps])
      );
   } else if ("venn_jps" %in% names(venndir_output)) {
      ## Todo: Omit these sections in favor of "Venndir" input
      label_bg <- ifelse(is.na(xy_overlaps),
         rep(bg, length.out=length(xy_overlaps)),
         jamba::alpha2col(
            venndir_output$venn_jps@polygons$fill[xy_overlaps],
            alpha=venndir_output$venn_jps@polygons$alpha[xy_overlaps])
      );
   } else {
      label_bg <- ifelse(is.na(xy_overlaps),
         rep(bg, length.out=length(xy_overlaps)),
         jamba::alpha2col(
            venndir_output$venn_spdf$color[xy_overlaps],
            alpha=venndir_output$venn_spdf$alpha[xy_overlaps])
      );
   }
   
   # define color_sp_index
   color_sp_index <- jamba::unalpha(vo@jps@polygons$fill[sp_index]);
   # box is darker version of polygon color with alpha=0.8
   venndir_output$label_df$border[toupdate] <- ifelse(
      grepl("box", label_style),
      jamba::alpha2col(alpha=0.8,
         jamba::makeColorDarker(color_sp_index,
            darkFactor=1.5)),
      NA)[toupdate];
   
   # label background fill
   venndir_output$label_df$fill[toupdate] <- ifelse(
      grepl("fill", label_style),
      color_sp_index,
      ifelse(
         grepl("shaded", label_style),
         jamba::alpha2col(
            color_sp_index,
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
               jamba::alpha2col(color_sp_index,
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
                  bg=jamba::alpha2col(color_sp_index,
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
   vo@label_df <- venndir_output$label_df;
   
   # add attribute to help persist the default show_labels
   if (length(show_labels) > 0 && any(nchar(show_labels) > 0)) {
      attr(vo, "show_labels") <- show_labels;
   }
   
   return(vo);
}
