
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
#' @examples
#' setlist <- make_venn_test(100, 3, do_signed=FALSE);
#' 
#' setlist <- make_venn_test(100, 3, do_signed=TRUE);
#' vo <- venndir(setlist)
#' jamba::sdim(vo);
#' 
#' vobase <- venndir(setlist, debug=3)
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
 proportional=FALSE,
 show_labels="Ncs",
 return_items=TRUE,
 show_items=c(NA, "none", "sign item", "sign", "item"),
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
 item_style=c("text",
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
   if (verbose) {
      jamba::printDebug("venndir(): ",
         "define Venn shapes");
   }
   if (length(venn_jp) == 0) {
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
      
      # Assign other attributes for consistency later on
      rownames(venn_jp@polygons) <- paste0(names(venn_jp), "|set");
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
      
      # venn_sp <- get_venn_shapes(counts=nCounts,
      #    proportional=proportional,
      #    circle_nudge=circle_nudge,
      #    sep=sep,
      #    ...);
      
      # optionally rotate the shapes
      # if (length(rotate_degrees) > 0 && rotate_degrees != 0) {
      #    venn_sp <- rescale_sp(sp=venn_sp,
      #       share_center=TRUE,
      #       rotate_degrees=rotate_degrees);
      # }
   } else {
      # Todo: accept JamPolygon input
      #
      # TODO: Validate polyclip input as list of polyclip polygons
      stop("venn_jp input is not yet implemented.");
      if ("SpatialPolygonsDataFrame" %in% class(venn_sp)) {
         venn_sp_names <- rownames(data.frame(venn_sp));
         if (length(venn_sp_names) == 0) {
            if (nrow(venn_sp) == length(setlist)) {
               rownames(data.frame(venn_sp)) <- names(setlist);
            } else if (nrow(venn_sp) == length(nCounts)) {
               rownames(data.frame(venn_sp)) <- names(nCounts);
            } else {
               errmsg <- paste0("nrow(venn_sp)=",
                  nrow(venn_sp),
                  " must equal length(setlist)=",
                  length(setlist),
                  " or length(nCounts)=",
                  length(nCounts));
               stop(errmsg);
            }
         }
         vennmatch <- match(names(nCounts),
            rownames(data.frame(venn_sp)));
         venn_sp <- venn_sp[vennmatch,];
      } else {
         # SpatialPolygons
         venn_sp_names <- names(venn_sp);
         #print(venn_sp_names);
         if (length(venn_sp_names) == 0 || !any(names(setlist)[sets] %in% venn_sp_names)) {
            #print("Adding names to venn_sp");
            if (length(venn_sp) == length(setlist)) {
               for (i in seq_along(venn_sp@polygons)) {
                  venn_sp@polygons[[i]]@ID <- names(setlist)[i];
               }
            } else if (length(venn_sp) == length(sets)) {
               for (i in seq_along(venn_sp@polygons)) {
                  venn_sp@polygons[[i]]@ID <- names(setlist)[sets][i];
               }
            } else {
               errmsg <- paste0("length(venn_sp)=",
                  length(venn_sp),
                  " must equal length(setlist)=",
                  length(setlist),
                  " or length(nCounts)=",
                  length(nCounts));
               stop(errmsg);
            }
         }
         vennmatch <- match(names(setlist)[sets],
            names(venn_sp));
         if (FALSE) {
            print(names(nCounts));
            print(vennmatch);
         }
         venn_sp <- venn_sp[vennmatch];
      }
   }
   
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
   
   # combine into one object
   venn_jps <- rbind2(venn_jp, venn_jpol);
  
   ## return the owl to this point
   # return(invisible(list(venn_jp=venn_jp, venn_jpol=venn_jpol, nCounts=nCounts)));

   # combine venn_sp with venn_spdf
   #venn_sp <- sp::spChFIDs(venn_sp,
   #   paste0(names(venn_sp), "|set"));
   #   paste0(names(venn_sp), "|set"));
   if (FALSE) {
      venn_pcl_names <- jamba::nameVector(names(venn_jp));
      # venn_sp_names <- jamba::nameVector(names(venn_sp));
      venn_spdf1 <- sp::SpatialPolygonsDataFrame(
         sp::spChFIDs(venn_sp,
            names(venn_sp_names)),
         data=data.frame(
            check.names=FALSE,
            stringsAsFactors=FALSE,
            color="#00000000",
            label=venn_sp_names))
      venn_spdf$type <- "overlap";
      if (1 %in% debug) {
         venn_spdf1m <- sp::merge(venn_spdf1,
            venn_spdf,
            all.x=TRUE);
         return(list(venn_spdf=venn_spdf,
            venn_spdf1=venn_spdf1,
            venn_spdf1m=venn_spdf1m));
      }
      venn_spdf1 <- sp::merge(venn_spdf1,
         venn_spdf,
         all.x=TRUE);
      venn_spdf1$type <- "set";
      if (2 %in% debug) {
         return(list(venn_spdf=venn_spdf,
            venn_spdf1=venn_spdf1));
      }
      venn_spdf1 <- sp::spChFIDs(venn_spdf1,
         paste0(venn_spdf1$label, "|set"));
      venn_spdf1$color <- set_color[venn_spdf1$label];
      venn_spdf1$venn_color <- venn_spdf1$color;
      # merge set and overlap polygons
      venn_spdfs <- rbind(venn_spdf1[,colnames(data.frame(venn_spdf))],
         venn_spdf);
      if (3 %in% debug) {
         return(list(venn_spdf=venn_spdf,
            venn_spdf1=venn_spdf1,
            venn_spdfs=venn_spdfs));
      }
   }
   
   ## Todo:
   # - determine which overlap polygon can represent each set label
   #    - if set is fully inside another set, it must choose the least
   #      overlapping polygon overlap
   whichset <- (length(venn_jps@polygons$label) + 1) -
      match(unique(venn_jps@polygons$label),
         rev(venn_jps@polygons$label));
   
   # obtain outer label coordinates
   # consider adding sp_buffer=-0.1 here and relative=TRUE
   # which places segment inside polygon by 10% the polygon size
   # jamba::printDebug("whichset: ", whichset)
   # print(as.data.frame(venn_spdfs))
   # plot(venn_spdfs)
   
   ## Todo: Adapt below to handle class "JamPolygon"
   # which defines placement of labels outside the Venn diagram.
   if (TRUE) {
      # jamba::printDebug("whichset:", whichset);
      # print(venn_jps[whichset, ]);
      ploxy <- label_outside_JamPolygon(jp=venn_jps[whichset, ],
         which_jp=seq_along(whichset),
         distance=segment_distance,
         buffer=-0.9)
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
   
   ## Todo: verify this all can be skipped (below with nlabel_df):
   #
   # generate labels from nCounts and gCounts
   if (FALSE) {
      nlabel_df <- data.frame(label=names(nCounts),
         venn_counts=nCounts,
         stringsAsFactors=FALSE,
         check.names=FALSE);
      nlabel_df <- jamba::mergeAllXY(
         #as.data.frame(venn_spdf),
         subset(venn_jps@polygons, type %in% "overlap"),
         nlabel_df);
      # remove duplicate label rows
      nmatch <- jamba::rmNA(match(names(nCounts),
         nlabel_df$label));
      nlabel_df <- nlabel_df[nmatch, , drop=FALSE];
      nlabel_df$color <- jamba::rmNA(nlabel_df$color,
         naValue="#FFFFFFFF");
      nlabel_df$venn_color <- jamba::rmNA(nlabel_df$venn_color,
         naValue="#FFFFFFFF");
      # repair x_label and y_label stored as list
      nlabel_df$x_label <- unlist(jamba::rmNULL(nlabel_df$x_label, nullValue=NA))
      nlabel_df$y_label <- unlist(jamba::rmNULL(nlabel_df$y_label, nullValue=NA))
      
      # Now add the main count labels
      nlabel_df$show_set <- ifelse(
         grepl(sep, fixed=TRUE, x=nlabel_df$label) |
            is.na(nlabel_df$type),
         FALSE,
         TRUE);
      
      venn_text <- jamba::formatInt(
         jamba::rmNA(naValue=0,
            nlabel_df$venn_counts));
      
      x_main <- nlabel_df$x_label;
      y_main <- nlabel_df$y_label;
      vjust_main <- rep(0.5, length(x_main));
      halign_main <- rep(0.5, length(x_main));
      #hjust_main <- ifelse(nchar(up_text) == 0 & nchar(dn_text) == 0, 0.5, 1);
      hjust_main <- rep(1, length(x_main));
      if ("overlap" %in% overlap_type) {
         hjust_main <- rep(0.5, length(x_main));
      }
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
   
   venn_jps@polygons$alpha <- poly_alpha;
   # venn_spdf$lwd <- 2;
   # venn_spdf$lty <- 1;
   # venn_spdf$border <- jamba::makeColorDarker(
   #    venn_spdf$color,
   #    darkFactor=1.2,
   #    sFactor=1.2);
   # venn_spdf$fontsize <- 14 * font_cex[1];
   
   # optionally apply alpha by venn_counts
   if (alpha_by_counts) {
      venn_spdf$alpha <- jamba::normScale(
         sqrt(
            jamba::rmNA(naValue=0, venn_spdf$venn_counts)),
         low=0,
         from=0.02,
         to=1);
      venn_spdfs$alpha <- jamba::normScale(
         sqrt(
            jamba::rmNA(naValue=0, venn_spdfs$venn_counts)),
         low=0,
         from=0.02,
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
   if (return_items) {
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
   # Workaround: Ignore venndir_label_style() for "JamPolygon" for now.
   #
   # Todo: use real x_outside
   #
   if (TRUE) {
      # label_df$x_outside <- label_df$x;
      # label_df$y_outside <- label_df$y;
      # label_df$x_offset <- 0;
      # label_df$y_offset <- 0;
      # venn_jps@polygons$x_outside <- jamba::rmNA(naValue=0, unlist(venn_jps@polygons$label_x));
      # venn_jps@polygons$y_outside <- jamba::rmNA(naValue=0, unlist(venn_jps@polygons$label_y));
      # jamba::printDebug("venn_jps@polygons (before venndir_label_style)");print(venn_jps@polygons);# debug
      # jamba::printDebug("label_df (before venndir_label_style)");print(label_df);# debug
      vls <- venndir_label_style(
         venndir_output=vo,
         old_venndir_output=list(
            # venn_spdf=venn_spdfs,
            venn_spdf=venn_jps,
            label_df=label_df),
         show_labels=show_labels,
         show_items=show_items,
         label_preset=label_preset,
         label_style=label_style,
         show_zero=show_zero,
         max_items=max_items,
         inside_percent_threshold=inside_percent_threshold,
         verbose=verbose,
         ...);
      vo@label_df <- vls$label_df;
      venn_jps@polygons <- vls$venn_spdf;
      # jamba::printDebug("venn_jps@polygons (after venndir_label_style)");print(venn_jps@polygons);# debug
      # jamba::printDebug("label_df (after venndir_label_style)");print(label_df);# debug
   }

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
         label_preset=label_preset,
         show_labels=show_labels,
         label_style="custom",
         item_cex=item_cex,
         item_style=item_style,
         item_buffer=item_buffer,
         max_items=max_items,
         inside_percent_threshold=inside_percent_threshold,
         ...);
      #label_df <- gg$label_df;
      retlist$rv_label_df <- gg$label_df;
      retlist$gg <- gg;
   }
   # for debug
   return(invisible(list(
      # venn_jp=venn_jp, venn_jpol=venn_jpol,
      vo=vo,
      gg=gg,
      venn_jps=venn_jps,
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


