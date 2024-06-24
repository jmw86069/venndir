
#' Directional Venn diagram
#' 
#' Directional Venn diagram
#' 
#' @inheritParams signed_overlaps
#' @param sets `integer` index with optional subset of sets in `setlist`
#'    for the Venn diagram.
#'    This option is useful when defining consistent `set_colors` for
#'    all entries in `setlist`.
#' @param set_colors `character` vector of R colors, or `NULL` (default) to
#'    use default colors defined by `colorjam::rainbowJam()`.
#' @param setlist_labels `character` vector with optional custom labels
#'    to display in the Venn diagram. This option is intended when
#'    the `names(setlist)` are not suitable for display, but should
#'    still be maintained as the original names.
#' @param legend_labels `character` vector with optional custom labels
#'    to display in the Venn legend. This option is intended when
#'    the `names(setlist)` are not suitable for a legend, but should
#'    still be maintained as the original names.
#'    The legend labels are typically single-line entries and should
#'    have relatively short text length.
#' @param proportional `logical` (default FALSE) indicating whether
#'    to draw proportional Venn circles, also known as a Euler diagram.
#'    Proportional circles are not guaranteed to represent all possible
#'    overlaps. Proportional circles are determined by calling
#'    `eulerr::eulerr()`.
#'    Use `shape="ellipse"` for `eulerr()` to provide elliptical shapes.
#' @param show_labels `character` string to define the labels to display,
#'    and where they should be displayed.
#'    The definition uses a single letter to indicate each type of label
#'    to display, using UPPERCASE to display the label outside the Venn shape,
#'    and lowercase to display the label inside the Venn shape.
#'    The default `"Ncs"` displays _N_ame (outside), _c_ount (inside),
#'    and _s_igned count (inside).
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
#' @param return_items `logical` (default TRUE) indicating whether to
#'    return items in the overlap data. When `FALSE` item labels also
#'    cannot be displayed in the figure.
#'    The main reason not to return items is to conserve memory, for
#'    example if `setlist` is extremely large.
#' @param show_items `character` used to define the item label,
#'    only used when the `show_label` entry includes `"i"` which
#'    enables item display inside the Venn diagram.
#'    * `"item"`: shows only the item labels
#'    * `"sign"`: shows only the sign of each item
#'    * `"sign items"`: shows the sign and item together
#'    (or `"item sign"` will show the item, then the sign).
#' @param max_items `numeric` (default 3000) indicating the maximum number
#'    of item labels to display when enabled.
#' @param show_zero `logical` (default FALSE) indicating whether empty
#'    overlaps are labeled with count zero `0`. When `show_zero=TRUE` the
#'    count zero label is displayed, otherwise no count label is shown.
#' @param font_cex `numeric` vector recycled and applied in order:
#'    1. Set label
#'    2. Overlap count label
#'    3. Signed count label
#'    
#'    The default `c(1, 1, 0.8)` defines the signed count label slightly
#'    smaller than other labels.
#' @param poly_alpha `numeric` (default 0.8) value between 0 and 1, for
#'    alpha transparency of the polygon fill color.
#'    This value is ignored when `alpha_by_counts=TRUE`.
#'    * `poly_alpha=1` is completely opaque (no transparency)
#'    * `poly_alpha=0.8` is 80% opaque
#' @param alpha_by_counts `logical` indicating whether to define
#'    alpha transparency to Venn polygon fill based upon the counts
#'    contained in each polygon. When `TRUE` the `poly_alpha` is ignored.
#' @param label_style `character` string indicating the style for labels.
#'    Label color is adjusted based upon the determined background color,
#'    determined based upon the label fill color, and either the
#'    device background color, or Venn overlap fill color.
#'    There are pre-defined label styles.
#'    * `"basic"` no background shading
#'    * `"fill"` an opaque colored background
#'    * `"shaded"` a partially transparent colored background
#'    * `"lite"` a partially transparent lite background
#'    * `"box"` adds a dark border around the label region
#' @param label_preset `character` deprecated in favor of `show_labels`.
#'    This argument is passed to `venndir_label_style()`.
#' @param unicode `logical` (default TRUE) indicating whether to 
#'    display Unicode arrows for signed overlaps. Passed to
#'    `curate_venn_labels()`.
#'    Use `unicode=FALSE` if the signed label is not displayed properly.
#'    The most common causes: (1) the R console (terminal) is not configured
#'    to allow Unicode (UTF-8 or UTF-16) characters; (2) the display
#'    font does not contain Unicode characters in the font set.
#' @param big.mark `character` (default `","`) passed to `format()`
#'    to augment numeric labels.
#' @param curate_df `data.frame` or `NULL` passed to `curate_venn_labels()`,
#'    used to customize the formatting of signed overlaps.
#' @param venn_jp `NULL` or optional `JamPolygon` which contains one
#'    polygon for each `setlist`, intended to allow custom shapes to be
#'    used. Otherwise `get_venn_polygon_shapes()` is called.
#' @param inside_percent_threshold `numeric` (default 0) indicating the
#'    percent area that a Venn overlap region must contain in order
#'    for the count label to be displayed inside the region,
#'    otherwise the label is displayed outside the region.
#'    Values are expected to range from 0 to 100.
#' @param item_cex `numeric` value (default NULL) used to resize item labels.
#'    * When `item_cex` is a single value or `NULL`, auto-scaling is
#'    performed based upon the number of items in each overlap
#'    polygon, and the relative polygon areas.
#'    Any single `numeric` value for `item_cex` is multiplied by the
#'    auto-scaled value for each overlap region.
#'    * When two or more values are supplied as a vector, the
#'    values are recycled and applied across all Venn overlap regions,
#'    in the order they appear in `signed_overlaps()`.
#' @param item_style `character` string (default "text") indicating
#'    the style to display item labels when they are enabled.
#'    * `"default"` detects whether item labels contain `"<br>"` for newlines,
#'    and uses `"gridtext"` if that is the case, otherwise it uses `"text"`
#'    which is markedly faster.
#'    * `"text"` option is substantially faster, but does not allow
#'    markdown.
#'    * `"gridtext"`:  substantially slower for a large number of labels,
#'    but enables use of limited markdown by calling
#'    `gridtext::richtext_grob()`.
#'    Mostly useful for `venn_meme()`.
#' @param item_buffer `numeric` value (default -0.15) indicating the buffer
#'    adjustment applied to Venn overlap regions before arranging item
#'    labels. Passed to `label_fill_JamPolygon()` via `render_venndir()`.
#'    Negative values are recommended, so they shrink the region.
#' @param sign_count_delim `character` string used as a delimiter between
#'    the sign and counts, when `overlap_type` is not `"overlap"`.
#' @param padding `numeric` padding in units `"mm"` (default `c(3, 2)`)
#'    for overlap count, and signed overlap count labels, in order.
#' @param r `numeric` radius in units `"mm"` used for rounded
#'    rectangle corners for labels. Only visible when `label_preset`
#'    includes a background fill ("lite", "shaded", "fill"), or "box".
#' @param segment_distance `numeric` value indicating the distance
#'    between outside labels and the outer edge of the Venn diaram region.
#'    Larger values place labels farther away, while also shrinking the
#'    relative size of the Venn diagram.
#' @param do_plot `logical (default TRUE) indicating whether to render the
#'    plot, or return data without rendering the plot.
#'    When `do_plot=FALSE` the returned data can later be passed to
#'    `render_venndir()` to render the figure.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param debug `numeric` optional internal debug.
#' @param circle_nudge `list` of `numeric` x,y vectors. Not yet
#'    re-implemented after the version 0.0.30.900 update.
#' @param rotate_degrees `numeric` value in degrees, allowing rotation
#'    of the Venn diagram. Not yet re-implemented after version 0.0.30.900.
#' @param ... additional arguments are passed to `render_venndir()`.
#' 
#' @family venndir core
#' 
#' @returns `Venndir` object with slots:
#'    * `"jps"`: `JamPolygon` which contains each set polygon, and each
#'    overlap polygon defined for the Venn diagram.
#'    * `"label_df"`: `data.frame` which contains the coordinates for each
#'    Venn set, and Venn overlap label.
#'    * `"setlist"`: `list` as input to `venndir()`. This entry may be empty.
#' 
#' @examples
#' setlist <- make_venn_test(100, 3, do_signed=FALSE);
#' 
#' setlist <- make_venn_test(100, 3, do_signed=TRUE);
#' vo <- venndir(setlist)
#' jamba::sdim(vo);
#' 
#' # custom set labels
#' vo <- venndir(setlist,
#'    setlist_labels=paste("set", LETTERS[1:3]))
#' 
#' # custom set labels with Markdown custom colors
#' vo <- venndir(setlist,
#'    setlist_labels=paste0("Set <span style='color:blue'>", LETTERS[1:3], "</span>"))
#' 
#' # custom set and legend labels
#' vo <- venndir(setlist,
#'    setlist_labels=paste0("set<br>", LETTERS[1:3]),
#'    legend_labels=paste("Set", LETTERS[1:3]))
#' 
#' # custom set and legend labels
#' # proportional
#' # Set Name is inside with show_labels having lowercase "n"
#' vo <- venndir(setlist,
#'    proportional=TRUE,
#'    show_labels="ncs",
#'    label_style="lite box",
#'    setlist_labels=paste0("Set: ", LETTERS[1:3]),
#'    legend_labels=paste("Set", LETTERS[1:3]))
#' 
#' @export
venndir <- function
(setlist,
 overlap_type=c("detect",
    "concordance",
    "each",
    "overlap",
    "agreement"),
 sets=NULL,
 set_colors=NULL,
 setlist_labels=NULL,
 legend_labels=NULL,
 proportional=FALSE,
 show_labels="Ncs",
 return_items=TRUE,
 show_items=c(NA,
    "none",
    "sign item",
    "sign",
    "item"),
 max_items=3000,
 show_zero=FALSE,
 font_cex=c(1, 1, 0.8),
 # show_set=c("main", "all", "none"),
 show_label=NA,
 display_counts=TRUE,
 poly_alpha=0.8,
 alpha_by_counts=FALSE,
 label_style=c("basic",
    "fill",
    "shaded",
    "shaded_box",
    "lite",
    "lite_box"),
 label_preset="none",
 unicode=TRUE,
 big.mark=",",
 curate_df=NULL,
 venn_jp=NULL,
 inside_percent_threshold=0,
 item_cex=NULL,
 item_style=c("default",
    "text",
    "gridtext"),
 item_buffer=-0.15,
 sign_count_delim=": ",
 padding=c(3, 2),
 r=2,
 segment_distance=0.1,
 sep="&",
 do_plot=TRUE,
 verbose=FALSE,
 debug=0,
 circle_nudge=NULL,
 rotate_degrees=0,
 ...)
{
   # basic workflow:
   # - get signed_overlaps()
   # - use counts per Venn set to create Venn circles/ovals
   #   - proportional Euler, or not proportional Venn
   # - find polygon overlaps for the circles/ovals
   #   - identify any overlaps not represented (to label on the side)
   
   overlap_type <- match.arg(overlap_type);
   item_style <- match.arg(item_style);
   
   # show_set <- match.arg(show_set);
   show_set <- "main";
   if (length(padding) == 0) {
      padding <- c(3, 2);
   }
   padding <- rep(padding, length.out=2);

   label_style <- head(label_style, 1);
   show_items <- head(show_items, 1);
   if (length(show_items) == 0) {
      show_items <- NA;
   }
   if (!all(show_items %in% c("none","FALSE"))) {
      return_items <- TRUE;
   }
   
   # validate font_cex input
   if (length(font_cex) == 0) {
      font_cex <- c(1, 1, 0.8);
   }
   if (length(font_cex) == 2) {
      font_cex <- font_cex[c(1, 2, 2)];
   }
   if (length(font_cex) != 3) {
      font_cex <- rep(font_cex, length.out=3)
   }

   # optional subset of sets within setlist
   if (length(sets) == 0) {
      sets <- seq_along(setlist);
   } else if ("character" %in% class(sets)) {
      sets <- jamba::rmNA(match(sets, names(setlist)));
      if (length(sets) == 0) {
         stop("sets did not match any values in names(setlist)");
      }
   }
   
   # handle setlist names, labels, legend labels
   if (length(setlist_labels) == 0) {
      setlist_labels <- names(setlist);
   } else {
      if (length(setlist_labels) != length(setlist)) {
         stop("length(setlist_labels) must equal length(setlist).");
      }
   }
   names(setlist_labels) <- names(setlist);
   # handle setlist names, labels, legend labels
   if (length(legend_labels) == 0) {
      legend_labels <- names(setlist);
   } else {
      if (length(legend_labels) != length(setlist)) {
         stop("length(legend_labels) must equal length(setlist).");
      }
   }
   names(legend_labels) <- names(setlist);
   
   # define colors
   if (length(set_colors) == 0) {
      set_colors <- colorjam::rainbowJam(length(setlist),
         ...);
      set_color <- set_colors[sets];
   } else {
      set_color <- rep(set_colors,
         length.out=length(sets));
   }
   names(set_color) <- names(setlist)[sets];
   
   # get overlap data
   if (verbose) {
      jamba::printDebug("venndir(): ",
         "signed_overlaps()");
   }
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
      if (length(inside_percent_threshold) == 0) {
         inside_percent_threshold <- 0.5;
      }
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
      if (length(inside_percent_threshold) == 0) {
         inside_percent_threshold <- 5;
      }
   }
   
   # get Venn circles
   if (length(venn_jp) > 0) {
      if (verbose) {
         jamba::printDebug("venndir(): ",
            "User Venn shapes");
      }
      if (length(venn_jp) != length(setlist)) {
         stop("length(venn_jp) must equal length(setlist).");
      }
      if (any(area_JamPolygon(venn_jp) == 0)) {
         stop("Within venn_jp, every polygon must have non-zero area.");
      }
      rownames(venn_jp@polygons) <- paste0(names(setlist), "|set");
   } else if (length(venn_jp) == 0) {
      if (verbose) {
         jamba::printDebug("venndir(): ",
            "get_venn_polygon_shapes()");
      }
      # JamPolygon for each set
      venn_jp <- get_venn_polygon_shapes(counts=nCounts,
         proportional=proportional,
         sep=sep,
         circle_nudge=circle_nudge,
         rotate_degrees=rotate_degrees,
         return_type="JamPolygon",
         ...);
      rownames(venn_jp@polygons) <- paste0(names(venn_jp), "|set");
   }
      
   # Assign other attributes for consistency later on
   venn_jp@polygons$venn_name <- names(venn_jp);
   venn_jp@polygons$venn_counts <- NA;
   venn_jp@polygons$venn_items <- I(lapply(seq_len(length(venn_jp)),
      function(xi) character(0)));
   venn_jp@polygons$venn_color <- set_color[venn_jp@polygons$venn_name];
   border_dark_factor <- 1.1;
   venn_jp@polygons$border <- jamba::makeColorDarker(
      darkFactor=border_dark_factor,
      set_color[venn_jp@polygons$venn_name]);
   venn_jp@polygons$border.lwd <- 4;
   venn_jp@polygons$fill <- NA;
   venn_jp@polygons$label <- venn_jp@polygons$venn_name;
   venn_jp@polygons$label_x <- NA;
   venn_jp@polygons$label_y <- NA;
   venn_jp@polygons$type <- "set";
   venn_jp@polygons$innerborder <- NA;
   venn_jp@polygons$innerborder.lwd <- 0;
   

   # convert to venn overlap polygons
   if (verbose) {
      jamba::printDebug("venndir(): ",
         "find_venn_overlaps_JamPolygon()");
   }
   # returns data.frame:
   # label, x, y, color, venn_counts, venn_color
   venn_jpol <- find_venn_overlaps_JamPolygon(
      jp=venn_jp,
      venn_counts=nCounts,
      venn_colors=set_color,
      sep=sep,
      ...);
   # rownames(venn_jpol@polygons) <- names(venn_jpol);
   venn_jp@polygons$type <- "set";
   venn_jpol@polygons$type <- "overlap";
   # jamba::printDebug("venndir(): ", "venn_jpol:");print(venn_jpol);# debug
   
   # combine into one object
   venn_jps <- rbind2(venn_jp, venn_jpol);
   
   ## Add setlist_labels, legend_labels
   matchset <- match(venn_jps@polygons$venn_name, names(setlist_labels));
   venn_jps@polygons$venn_label <- setlist_labels[matchset];
   venn_jps@polygons$legend_label <- legend_labels[matchset];
   
   ## Todo:
   # - determine which overlap polygon can represent each set label
   #    - if set is fully inside another set, it must choose the least
   #      overlapping polygon overlap
   whichset <- (length(venn_jps@polygons$label) + 1) -
      match(unique(venn_jps@polygons$label),
         rev(venn_jps@polygons$label));
   # jamba::printDebug("venn_jps@polygons:");print(venn_jps@polygons);# debug
   
   # obtain outer label coordinates
   # consider adding sp_buffer=-0.1 here and relative=TRUE
   # which places segment inside polygon by 10% the polygon size
   # jamba::printDebug("whichset: ", whichset)
   # print(as.data.frame(venn_spdfs))
   # plot(venn_spdfs)
   
   ## Todo:
   # Consider making this section optional, in case there are too
   # many outside labels to determine, and
   # especially when no outside labels are required.
   if (TRUE) {
      # jamba::printDebug("whichset:", whichset);
      # print(venn_jps[whichset, ]);
      if (verbose) {
         jamba::printDebug("venndir(): ",
            "started label_outside_JamPolygon()");
      }
      ploxy <- label_outside_JamPolygon(jp=venn_jps[whichset, ],
         which_jp=seq_along(whichset),
         distance=segment_distance,
         verbose=verbose,
         buffer=-0.9,
         ...)
      if (verbose) {
         jamba::printDebug("venndir(): ",
            "completed label_outside_JamPolygon()");
      }
      venn_jps@polygons$x_offset <- 0;
      venn_jps@polygons$y_offset <- 0;
      ploxy_match <- match(venn_jps@polygons$label,
         venn_jps@polygons$label[whichset]);
      # jamba::printDebug("ploxy_match:", ploxy_match);
      ploxy_label_x <- sapply(ploxy, function(ixy){ixy["border", 1]});
      ploxy_label_y <- sapply(ploxy, function(ixy){ixy["border", 2]});
      ploxy_outside_x <- sapply(ploxy, function(ixy){ixy["label", 1]});
      ploxy_outside_y <- sapply(ploxy, function(ixy){ixy["label", 2]});
      # jamba::printDebug("ploxy_label_x:", ploxy_label_x);
      # jamba::printDebug("ploxy_outside_x:", ploxy_outside_x);
      
      # define other label coordinates
      venn_jps@polygons$x_label <- ploxy_label_x[ploxy_match];
      venn_jps@polygons$y_label <- ploxy_label_y[ploxy_match];
      venn_jps@polygons$x_outside <- ploxy_outside_x[ploxy_match];
      venn_jps@polygons$y_outside <- ploxy_outside_y[ploxy_match];
      venn_jps@polygons$x_offset <- venn_jps@polygons$x_outside - venn_jps@polygons$x_label;
      venn_jps@polygons$y_offset <- venn_jps@polygons$y_outside - venn_jps@polygons$y_label;
      
      # define label positioning relative to the coordinate point
      venn_jps@polygons$vjust <- 0.5;
      venn_jps@polygons$hjust <- 0.5;
      venn_jps@polygons$vjust[whichset] <- sapply(ploxy, function(ixy){
         ixy["label", "adjx"]
      });
      venn_jps@polygons$hjust[whichset] <- sapply(ploxy, function(ixy){
         ixy["label","adjy"]
      });
      # print(venn_jps);# debug
   }
   
   # show_set: whether to display each overlap label
   if ("main" %in% show_set) {
      # show only single-set labels (main set) but not each overlap label
      nsets <- lengths(strsplit(as.character(venn_jps@polygons$label),
         split=sep));
      dupelabel <- sapply(seq_along(venn_jps@polygons$label), function(i){
         venn_jps@polygons$label[i] %in% venn_jps@polygons$label[-i]
      });
      venn_jps@polygons$show_set <- FALSE;
      which_show <- (
         (nsets == 1 & venn_jps@polygons$type %in% "set" & !dupelabel) |
         (nsets == 1 & venn_jps@polygons$type %in% "overlap"));
      venn_jps@polygons$show_set[which_show] <- TRUE;
   } else if ("all" %in% show_set) {
      # show main set labels and all overlap labels
      venn_jps@polygons$show_set <- TRUE;
   } else {
      venn_jps@polygons$show_set <- FALSE;
   }
   

   # define main x,y label coordinates
   nlabel_df <- subset(venn_jps@polygons, type %in% "overlap")
   main_x <- unlist(nlabel_df$label_x);
   main_y <- unlist(nlabel_df$label_y);
   names(main_x) <- nlabel_df$name;
   names(main_y) <- nlabel_df$name;
   
   # venn text label
   venn_text <- jamba::formatInt(
      jamba::rmNA(naValue=0,
         nlabel_df$venn_counts));
   
   # label alignment
   main_vjust <- rep(0.5, length(main_x));
   main_halign <- rep(0.5, length(main_x));
   main_hjust <- rep(1, length(main_x));
   if ("overlap" %in% overlap_type) {
      main_hjust <- rep(0.5, length(main_x));
   }
   
   ## Labels for each overlap
   # - using gCounts from signed_overlaps() above
   gbase_labels <- curate_venn_labels(
      x=names(unlist(unname(gCounts))),
      unicode=unicode,
      type="sign",
      curate_df=curate_df,
      ...);
   gbase_colors <- curate_venn_labels(
      x=names(unlist(unname(gCounts))),
      unicode=unicode,
      type="color",
      curate_df=curate_df,
      ...);
   gcount_labels <- sapply(seq_along(unlist(gCounts)), function(i){
      ilabel <- paste0(
         gbase_labels[i],
         sign_count_delim,
         format(trim=TRUE,
            big.mark=big.mark,
            unlist(gCounts)[i]));
   });
   
   ## define and order labels for signed counts
   # Todo: consider using factor order to help sort sign in a controlled way
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
   #
   gCounts_len <- lengths(gCounts);
   
   # signed label positions
   signed_x <- rep(main_x, gCounts_len);
   signed_y <- rep(main_y, gCounts_len);
   signed_vjust <- unname(unlist(lapply(gCounts_len, function(i){
      iseq <- seq_len(i) - 1;
      iseq - mean(iseq);
   }))) + 0.5;
   signed_hjust <- rep(0, length(signed_x));
   signed_halign <- rep(0, length(signed_x));
   
   # # signed label positions
   # x_signed <- rep(x_main, gCounts_len);
   # y_signed <- rep(y_main, gCounts_len);
   # vjust_signed <- unname(unlist(lapply(gCounts_len, function(i){
   #    iseq <- seq_len(i) - 1;
   #    iseq - mean(iseq);
   # }))) + 0.5;
   # hjust_signed <- rep(0, length(x_signed));
   # halign_signed <- rep(0, length(x_signed));
   
   
   # venndir_label_style() assigns these values
   label_fill_main <- rep(NA, nrow(nlabel_df));
   label_border_main <- rep(NA, nrow(nlabel_df));
   label_color_main <- rep(NA, nrow(nlabel_df));
   label_fill_signed <- rep(label_fill_main, gCounts_len);
   label_border_signed <- rep(label_border_main, gCounts_len);
   label_color_signed <- rep(label_color_main, gCounts_len);

   ## Hide zero if show_zero=FALSE
   show_main <- (!is.na(nlabel_df$label_x));
   show_signed <- rep(show_main, gCounts_len);
   if (!show_zero && any(nlabel_df$venn_counts == 0)) {
      show_main <- (show_main & nlabel_df$venn_counts != 0);
   }
   show_signed <- (show_signed & unlist(gCounts) != 0)

   ## Update other polygon display attributes
   vset <- (venn_jps@polygons$type %in% "overlap")
   venn_jps@polygons$alpha <- ifelse(vset,
      poly_alpha,
      0);

   # define inner border
   venn_jps@polygons$innerborder.lwd <- 2;
   # venn_jps@polygons$innerborder.lty <- 1;
   border_dark_factor <- 1.1;
   border_s_factor <- 1.2;
   venn_jps@polygons$innerborder <- ifelse(vset,
      jamba::makeColorDarker(
         jamba::unalpha(venn_jps@polygons$venn_color),
         darkFactor=border_dark_factor,
         sFactor=border_s_factor),
      NA);

   # define outer border
   venn_jps@polygons$border.lwd <- 2;
   # venn_jps@polygons$border.lty <- 1;
   venn_jps@polygons$border <- ifelse(vset,
      NA,
      jamba::makeColorDarker(
         jamba::unalpha(venn_jps@polygons$venn_color),
         darkFactor=border_dark_factor,
         sFactor=border_s_factor))
   # define label font size
   venn_jps@polygons$fontsize <- 14 * head(font_cex, 1);
   
   # venn_jps@polygons$alpha <- poly_alpha;

   # optionally apply alpha by venn_counts
   if (alpha_by_counts) {
      venn_jps@polygons$alpha <- jamba::normScale(
         sqrt(
            jamba::rmNA(naValue=0, venn_jps@polygons$venn_counts)),
         low=0,
         from=0.05,
         to=1);
   }
   
   # adjust fill color with alpha
   venn_jps@polygons$fill <- ifelse(vset,
      jamba::alpha2col(
         x=venn_jps@polygons$venn_color,
         alpha=venn_jps@polygons$alpha),
      NA);

   ## Prepare label data.frame
   label_n <- length(c(main_x, signed_x));
   label_df <- data.frame(
      x=c(main_x, signed_x),
      y=c(main_y, signed_y),
      text=unlist(c(venn_text, gcount_labels)),
      venn_counts=c(nCounts, gcount_sorted),
      overlap_set=c(nlabel_df$label,
         rep(nlabel_df$label, gCounts_len)),
      type=rep(c("main", "signed"),
         c(length(main_x), length(signed_x))),
      x_offset=0,
      y_offset=0,
      show_label=NA,
      vjust=c(main_vjust, signed_vjust),
      hjust=c(main_hjust, signed_hjust),
      halign=c(main_halign, signed_halign),
      rot=rep(0, label_n),
      color=unlist(c(label_color_main, gbase_colors)),
      fontsize=rep(c(14 * font_cex[2], 14 * font_cex[3]),
         c(length(main_x), length(signed_x))),
      border=c(label_border_main, label_border_signed),
      lty=rep(1, label_n),
      lwd=rep(1, label_n),
      fill=c(label_fill_main, label_fill_signed),
      padding=rep(
         c(padding[1] * font_cex[2], padding[2] * font_cex[3] * 1),
         c(length(main_x), length(signed_x))),
      padding_unit=rep("pt", label_n),
      r=rep(
         c(padding[1] * font_cex[2], padding[1] * font_cex[3]),
         c(length(main_x), length(signed_x))),
      r_unit=rep("pt", label_n),
      stringsAsFactors=FALSE,
      check.names=FALSE
   );
   
   if ("overlap" %in% overlap_type) {
      sv_match <- match(label_df$overlap_set, sv$sets);
      label_df$overlap_sign <- paste0(sv$sets, "|", sv$overlap)[sv_match];
   } else {
      label_df$overlap_sign <- c(rep("", length(main_x)),
         names(gbase_signs));
   }

   ## Apply setlist_labels, legend_labels
   matchset <- match(label_df$overlap_set, venn_jps@polygons$venn_name)
   label_df$venn_label <- venn_jps@polygons$venn_label[matchset];
   # label_df$legend_label <- venn_jps@polygons$legend_label[matchset];
   
   ## Check for missing signed labels, then we nudge main label to center
   #
   # Todo: verify this works properly when all rows are always present
   #
   for (i in unique(label_df$overlap_set)) {
      irows <- (label_df$overlap_set %in% i & 
            label_df$show_label %in% c(NA, TRUE));
      if (length(unique(label_df[irows,"type"])) == 1) {
         label_df[irows,"hjust"] <- 0.5;
      }
   }
   
   ## Optionally return items
   if (TRUE %in% return_items) {
      sv_label <- paste0(sv$sets, "|", sv[[overlap_type]]);
      sv_match <- match(label_df$overlap_sign, sv_label);
      label_df$items <- I(sv$items[sv_match]);
   }

   # Create Venndir object
   vo <- new("Venndir",
      jps=venn_jps,
      label_df=label_df,
      setlist=setlist)
   
   ## venndir_label_style()
   #
   # Todo: use real x_outside
   #
   if (TRUE) {
      # jamba::printDebug("venn_jps@polygons (before venndir_label_style)");print(venn_jps@polygons);# debug
      # jamba::printDebug("label_df (before venndir_label_style)");print(label_df);# debug
      ## 0.0.32.900 - ensure show_items is non-empty when "i" or "item" defined
      if ((length(show_labels) > 0 && any(grepl("[iIsS]", show_labels))) ||
         (length(label_preset) > 0 && any(grepl("item", ignore.case=TRUE, label_preset)))) {
         if (any(show_items %in% c(NA, "none", "FALSE"))) {
            if ("overlap" %in% overlap_type) {
               show_items <- "item";
            } else {
               show_items <- "sign item";
            }
            # jamba::printDebug("venndir(): ", "show_items:", show_items);# debug
         }
      }
      
      ## Update Venndir object in place
      vo <- venndir_label_style(
         venndir_output=vo,
         show_labels=show_labels,
         show_items=show_items,
         label_preset=label_preset,
         label_style=label_style,
         show_zero=show_zero,
         max_items=max_items,
         inside_percent_threshold=inside_percent_threshold,
         verbose=verbose,
         ...);
      # vo@label_df <- vls$label_df;
      # venn_jps@polygons <- vls$venn_spdf;
      # jamba::printDebug("venn_jps@polygons (after venndir_label_style)");print(venn_jps@polygons);# debug
      # jamba::printDebug("label_df (after venndir_label_style)");print(label_df);# debug
   }
   # jamba::printDebug("venndir(): ", "vo@label_df:");print(vo@label_df);# debug
   # jamba::printDebug("venndir(): ", "venn_jps@polygons:");print(venn_jps@polygons);# debug
   
   ## 0.0.30.900 - assume this check already happened in venndir_label_style()
   # update show_items based upon max_items
   if (FALSE) {
      vo@label_df$show_items <- ifelse(
         # label_df$venn_counts > max_items &
         (lengths(vo@label_df$items) %in% c(0) |
          lengths(vo@label_df$items) > max_items) &
            !is.na(show_items) &
            !"none" %in% show_items,
         # label_df$show_items %in% "inside" &
         "none",
         show_items);
   }
   
   ## Call render_venndir()
   gg <- NULL;
   
   
   ## TODO: allow custom display of counts for each Venn overlap
   # For now, all or none, TRUE or FALSE.
   if (FALSE %in% display_counts) {
      #label_df$show_label <- FALSE;
      vo@label_df$display_counts <- FALSE;
   }
   retlist <- list(venn_jp=venn_jps,
      label_df=label_df);
   
   # # Create Venndir object
   # vo <- new("Venndir",
   #    jps=venn_jps,
   #    label_df=label_df,
   #    setlist=setlist)
   
   # Optionally plot
   if (do_plot) {
      gg <- render_venndir(
         venndir_output=vo,
         # venn_jp=venn_jps,
         # label_df=label_df,
         show_label=show_label,
         show_items=show_items,
         show_zero=show_zero,
         display_counts=display_counts,
         # label_preset=label_preset,
         # show_labels=show_labels,
         # label_style="custom",
         item_cex=item_cex,
         item_style=item_style,
         item_buffer=item_buffer,
         max_items=max_items,
         inside_percent_threshold=inside_percent_threshold,
         ...);
      #label_df <- gg@label_df;
      retlist$rv_label_df <- gg@label_df;
      retlist$gg <- gg;
   }
   # return Venndir object
   return(invisible(vo));
   
   # for debug
   return(invisible(list(
      # venn_jp=venn_jp, venn_jpol=venn_jpol,
      vo=vo,
      gg=gg,
      # venn_jps=venn_jps,
      nlabel_df=nlabel_df,
      label_df=label_df,
      nCounts=nCounts, gCounts=gCounts, gdf=gdf,
      glist=list(gbase_labels=gbase_labels,
         gbase_colors=gbase_colors,
         gcount_labels=gcount_labels,
         gcount_sorted=gcount_sorted,
         gbase_signs=gbase_signs,
         gCounts_len=gCounts_len,
         main_x=main_x,
         main_y=main_y)
   )));
   
   return(invisible(retlist));
}


