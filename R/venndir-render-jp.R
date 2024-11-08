
#' Render venndir output
#' 
#' Render venndir output
#' 
#' @family venndir core
#' 
#' @inheritParams venndir
#' @param venndir_output `Venndir` output from `venndir()`, or
#'    `list` with element `"vo"` as a `Venndir` object.
#' @param expand_fraction `numeric` value indicating how much to
#'    expand the figure range beyond the default calculated for
#'    the Venn diagram. Values above zero cause the Venn diagram
#'    to be slighly smaller.
#' @param font_cex `numeric` scalar to adjust font sizes.
#' @param item_cex `numeric` scalar applied to item labels in each overlap
#'    in order. When `length(item_cex) == 1` it is applied uniformly
#'    across all overlaps, otherwise it is recycled to the total
#'    number of overlaps.
#'    When provided, it is used instead of any adjustments to item label
#'    sizes based upon proportional area of each overlap.
#' @param item_cex_factor `numeric` value used to adjust pre-calculated
#'    item fontsizes. This value is used to adjust the item label sizes,
#'    which may also be adjusted proportional to the area of each overlap,
#'    in which case `item_cex_factor` is used to adjust those relative
#'    label sizes.
#' @param plot_warning `logical` indicating whether to include a warning
#'    when one or more non-zero overlap counts cannot be displayed
#'    in the figure.
#'    **Not yet re-implemented since version 0.0.30.900.**
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
#' @param template `character` (default "wide") describing the default
#'    layout for counts and signed counts. The value is stored in
#'    `venndir@metadata$template` for persistence.
#'    * `"wide"` - main counts on the left, right-justified; signed counts
#'    on the right, left-justified.
#'    * `"tall"` - main counts, center-justified; signed counts below main
#'    counts, center-justified.
#' @param adjust_center `logical` (default TRUE) used when labels are grouped,
#'    whether the group should be re-centered on the target point.
#'    Try `adjust_center=FALSE` if wide label groups are adjusted
#'    so that the count label is too far left.
#' @param draw_legend `logical` (default TRUE) indicating whether to draw
#'    a legend, calling `venndir_legender()`.
#' @param legend_x `character` passed to `venndir_legender()` to customize
#'    the position of the legend.
#' @param legend_font_cex `numeric` scalar to adjust the legend font size.
#' @param do_draw `logical` indicating whether to call `grid::grid.draw()`.
#'    The `grid` graphical objects are returned in attributes:
#'    "gtree", "grob_list", "viewport", and can be drawn separately.
#' @param do_newpage `logical` (default TRUE) indicating whether to call
#'    `grid::grid.newpage()`. This option allows the figure to be rendered
#'    inside an active display device, or active `grid::viewport`.
#'    Note: When `do_draw=FALSE`, it also forces `do_newpage=FALSE`.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to internal functions.
#' 
#' @returns `Venndir` object with attributes that contain underlying
#'    `grid` graphical objects (grobs):
#'    * `"gtree"`: a `grid::gTree` object suitable for drawing
#'    with `grid::grid.draw(attr(vo, "gtre"))`
#'    * `"grob_list"`: a `list` of `grid` object components used to build
#'    the complete diagram, they can be plotted individually, or
#'    assembled with `do.call(grid::gList, grob_list)`.
#'    The `grid::gList` can be assembled into a `gTree` with:
#'    `grid::grobTree(gList=do.call(grid::gList, grob_list)`
#'    * `"viewport"`: the `grid::viewport` that holds important context
#'    for the graphical objects, specifically the use of coordinate
#'    `grid::unit` measure `"snpc"`, which maintains a fixed aspect ratio.
#' 
#' @export
render_venndir <- function
(venndir_output=NULL,
 expand_fraction=0,
 font_cex=1,
 main=NULL,
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
 item_style=c("default",
    "text",
    "gridtext"),
 item_buffer=-0.15,
 group_labels=TRUE,
 template=NULL,
 adjust_center=FALSE,
 draw_legend=TRUE,
 legend_x="bottomright",
 legend_font_cex=1,
 show_label=NA,
 display_counts=TRUE,
 do_newpage=TRUE,
 do_draw=TRUE,
 draw_buffer=FALSE,
 verbose=FALSE,
 ...)
{
   # validate input
   if ("list" %in% class(venndir_output) && "vo" %in% names(venndir_output)) {
      venndir_output <- venndir_output$vo;
   }
   metadata <- list();
   if ("Venndir" %in% class(venndir_output)) {
      venn_jp <- venndir_output@jps;
      label_df <- venndir_output@label_df;
      setlist <- venndir_output@setlist;
      metadata <- tryCatch({
         venndir_output@metadata
      }, error=function(e){
         list()
      })
   } else {
      # legacy input
      if (length(venndir_output) > 0 && is.list(venndir_output)) {
         if (!any(c("jp", "label_df") %in% names(venndir_output))) {
            stop("List input must contain element names 'venn_jp' or 'label_df'.");
         }
         if (!inherits(venndir_output[["jp"]], "JamPolygon")) {
            stop("Element 'jp' must inherit from 'JamPolygon'.");
         }
         venn_jp <- venndir_output[["jp"]];
         if (!inherits(venndir_output[["label_df"]], "data.frame")) {
            stop("Element 'label_df' must inherit from 'data.frame'.");
         }
         label_df <- venndir_output[["label_df"]];
         setlist <- list();
         venndir_output <- new("Venndir",
            jps=venn_jp,
            label_df=label_df,
            setlist=list(),
            metadata=list())
      } else {
         stop("Input must be 'Venndir' or legacy list with 'jp' and 'label_df'")
      }
   }
   
   # validate other options
   if (length(template) > 0) {
      template <- match.arg(template,
         choices=c("tall", "wide"))
   } else {
      template <- tryCatch({
         metadata$template;
      }, error=function(e){
         NULL;
      })
      if (length(template) == 0) {
         template <- "wide"
      }
   }
   metadata$template <- template;
   if (length(main) > 0) {
      metadata$main <- main;
      # venndir_output@metadata$main <- main;
   } else {
      main <- tryCatch({
         venndir_output@metadata$main
      }, error=function(e){
         NULL;
      })
   }
   
   # jamba::printDebug("render_venndir() label_df:");print(label_df);# debug
   show_items <- head(setdiff(label_df$show_items, c(NA, "none")), 1);
   if (length(show_items) == 0) {
      show_items <- NA;
   }
   # jamba::printDebug("show_items: ", show_items);# debug
   # show_items <- head(show_items, 1);
   item_style <- match.arg(item_style);

   # validate other args
   if (length(expand_fraction) == 0) {
      expand_fraction <- 0;
   }

   # Apply label_style
   # - only if label_style is something other than "custom"
   # OR
   # - show_labels is something other than NULL or ""
   if (!"custom" %in% label_style ||
         (length(show_labels) > 0 && any(nchar(show_labels) > 0))) {
      venndir_output <- venndir_label_style(
         venndir_output=venndir_output,
         label_preset=label_preset,
         label_style=label_style,
         show_labels=show_labels,
         inside_percent_threshold=inside_percent_threshold,
         show_zero=show_zero,
         ...);
      venn_jp <- venndir_output@jps;
      label_df <- venndir_output@label_df;
      # jamba::printDebug("after venndir_label_style() label_df:");print(label_df);# debug
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
      
      # 0.0.34.900 - consider requiring non-zero venn_counts, for now leave as-is
      show_label <- (
         (label_df$overlap %in% c("outside", "inside") |
            label_df$count %in% c("outside", "inside")))
         # & label_df$venn_counts > 0);
      
      # 0.0.34.900 -  experiment by adding show_label
      label_df$show_label <- show_label;
      
      # 0.0.34.900 -  experiment by adding poly_ref_name
      if (!"ref_polygon" %in% colnames(label_df)) {
         matchjps <- match(label_df$overlap_set, rownames(venn_jp@polygons));
         label_df$ref_polygon <- venn_jp@polygons$ref_polygon[matchjps];
      }
      
      # jamba::printDebug("label_df:");print(label_df);# debug

      # warn about hidden non-zero labels
      warn_rows <- (
         (label_df$x %in% NA |
               label_df$y %in% NA) &
            !label_df$venn_counts %in% c(NA, 0) &
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
         # jamba::printDebug("show offset data.frame summary:");print(data.frame(label_df.rownames=rownames(label_df), show_label=show_label, label_df.overlap=label_df$overlap, label_df.count=label_df$count, label_df.x=label_df$x, label_df.x_offset=label_df$x_offset, label_outside=label_outside, has_offset=has_offset));# debug
         #
         # Todo: Deal with has_offset, for now set to FALSE
         # jamba::printDebug("label_outside:");print(table(label_outside));
         # jamba::printDebug("has_offset:");print(table(has_offset));
         #
         # Handle labels outside
         if (any(show_label & has_offset)) {
            use_offset <- (show_label & has_offset);
            # use_offset <- (show_label & has_offset & label_df$venn_counts > 0);
            # jamba::printDebug("use_offset rows in label_df:");print(subset(label_df, use_offset));# debug
            offset_sets <- label_df$overlap_set[use_offset];
            # jamba::printDebug("offset_sets: ");print(offset_sets);# debug
            offset_ref_polygon <- venn_jp@polygons$ref_polygon[match(offset_sets, rownames(venn_jp@polygons))]
            names(offset_ref_polygon) <- offset_sets;
            # jamba::printDebug("offset_ref_polygon: ");print(offset_ref_polygon);# debug
            sp_index <- match(offset_ref_polygon, rownames(venn_jp@polygons))
            names(sp_index) <- offset_sets;
            # jamba::printDebug("sp_index: ");print(sp_index);# debug
            
            # 0.0.20.900 - fix order of preferred polygon labels
            #sp_index <- match(offset_sets, venn_spdf$label);
            # sp_index <- (length(venn_spdf$label) + 1 - 
            #       match(offset_sets, 
            #          rev(venn_spdf$label)));
            ## 0.0.32.900 - subset polygons for non-empty coordinates
            polygon_nonempty <- venn_jp@polygons$is_empty %in% 0;
            # polygon_nonempty <- sapply(venn_jp@polygons$x, function(ix1){
            #    length(jamba::rmNA(unlist(ix1))) > 0
            # });
            use_jp <- venn_jp[which(polygon_nonempty), ];
            use_polygons <- use_jp@polygons;
            use_polygons$rownum <- seq_len(nrow(use_polygons));
            use_polygons$nsets <- lengths(strsplit(use_polygons$name, "&"));
            use_polygons <- jamba::mixedSortDF(use_polygons, byCols=c("type", "nsets"));
            ## define best available polygon to label
            
            sp_index0 <- sapply(offset_sets, function(iset1){
               # jamba::printDebug("paste0(\"(^|&)\", iset1, \"($|&)\"):");print(paste0("(^|&)", iset1, "($|&)"));# debug
               kset1 <- head(grep(paste0("(^|&)", iset1, "($|&)"),
                  use_polygons$label), 1)
               iset2 <- use_polygons$rownum[kset1];
               # } else {
               #    iset2 <- head(grep(paste0("(^|&)", iset1, "($|&)"),
               #       venn_jp@polygons$label), 1)
               # }
               iset2
            })

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
               # 0.0.34.900 - use modified sp_index, which might use incorrect colors?
               poly_color=venn_jp@polygons$fill[sp_index],
               poly_border=venn_jp@polygons$border[sp_index]);
               # poly_color=use_jp@polygons$fill[sp_index],
               # poly_border=use_jp@polygons$border[sp_index]);
            # jamba::printDebug("test_xy:");print(test_xy);# debug
            # sp_list <- lapply(sp_index, function(i){
            #    venn_spdf[i,]});
            jp_list <- lapply(sp_index, function(i){
               # use_jp[i, ]
               # 0.0.34.900 - use modified sp_index
               venn_jp[i, ]
            });
            # jamba::printDebug("render_venndir(): ", "test_xy:");print(test_xy);# debug
            # jamba::printDebug("render_venndir(): ", "jp_list:");print(jp_list);# debug
            new_xy <- label_segment_JamPolygon(
               x0=test_xy$x0,
               y0=test_xy$y0,
               x1=test_xy$x1,
               y1=test_xy$y1,
               jp=jp_list,
               buffer=test_xy$segment_buffer,
               verbose=verbose,
               ...);
            rownames(new_xy) <- names(sp_index);
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
                  sc2 <- gsub("^$", "#00000000", jamba::rmNA(sc1));
                  sc2a <- jamba::col2alpha(sc2);
                  sc2 <- sc2[sc2a > 0];
                  sc2 <- jamba::alpha2col(alpha=(1 + jamba::col2alpha(sc2))/2,
                     sc2);
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
                     venn_jp@polygons$innerborder.lwd[test_xy$sp_index[has_segment]],
                     nullValue=2), each=2),
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
      # jamba::printDebug("items_dfs:");print(items_dfs);# debug

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
            # jamba::printDebug("dio:", dio);# debug
            if (grepl("sign", dio)) {
               labels <- paste(labels, prefixes);
               # jamba::printDebug("prefixes:", head(prefixes));# debug
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
            verbose=verbose,
            ...);
         if (length(lpf) == 0) {
            return(lpf)
         }
         # add overlap label to items_df
         lpf$items_df$overlap_set <- head(items_df1$overlap_set, 1);
         # jamba::printDebug("lpf$items_df:");print(lpf$items_df);
         lpf;
      });
      # combine item label into one data.frame
      itemlabels_df <- jamba::rbindList(lapply(
         jamba::rmNULL(itemlabels_list), function(i1){
         i1$items_df;
      }));
      itemlabels_jp <- NULL;
   }
   
   gdf <- NULL;
   if (any(show_label)) {
      # generate data.frame of label coordinates
      # 0.0.34.900 - hide label unless explicitly shown
      show_overlap_outside <- (label_df$overlap %in% "outside" & !is.na(label_df$x) & show_label)
      show_overlap_inside <- (label_df$overlap %in% "inside" & !is.na(label_df$x) & show_label)
      # show_overlap_outside <- (label_df$overlap %in% "outside" & !is.na(label_df$x))
      # show_overlap_inside <- (label_df$overlap %in% "inside" & !is.na(label_df$x))
      show_count_outside <- (label_df$count %in% "outside" & !is.na(label_df$x))
      show_count_inside <- (label_df$count %in% "inside" & !is.na(label_df$x));
      if (!"venn_label" %in% colnames(label_df)) {
         label_df$venn_label <- label_df$overlap_set;
      }
      overlap_set <- paste0("**", label_df$overlap_set, "**");
      venn_label <- ifelse(!label_df$venn_label %in% c(NA, ""),
         paste0("**", label_df$venn_label, "**"),
         "")
      # 0.0.41.900 - also convert \n to <br>
      if (any(grepl("[\n\r]", venn_label))) {
         venn_label <- gsub("\n", "<br>",
            gsub("^[\n]+|[\n]+$", "",
            gsub("\n\n", "\n",
            gsub("[\r\n]+|<br>", "\n", venn_label))))
      }
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
         label_df_rowname=c(
            rownames(label_df)[show_overlap_outside],
            rownames(label_df)[show_overlap_inside],
            rownames(label_df)[show_count_outside],
            rownames(label_df)[show_count_inside]),
         overlap_set=c(
            label_df$overlap_set[show_overlap_outside],
            label_df$overlap_set[show_overlap_inside],
            label_df$overlap_set[show_count_outside],
            label_df$overlap_set[show_count_inside]),
         ref_polygon=c(
            label_df$ref_polygon[show_overlap_outside],
            label_df$ref_polygon[show_overlap_inside],
            label_df$ref_polygon[show_count_outside],
            label_df$ref_polygon[show_count_inside]),
         text=c(
            venn_label[show_overlap_outside],
            venn_label[show_overlap_inside],
            label_df$text[show_count_outside],
            label_df$text[show_count_inside]),
         # text=c(
         #    overlap_set[show_overlap_outside],
         #    overlap_set[show_overlap_inside],
         #    label_df$text[show_count_outside],
         #    label_df$text[show_count_inside]),
         type=c(
            label_df$type[show_overlap_outside],
            label_df$type[show_overlap_inside],
            label_df$type[show_count_outside],
            label_df$type[show_count_inside]),
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
            (label_df$halign[show_count_outside] > 0) * 1,
            (label_df$halign[show_count_inside] > 0) * 1),
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
      if (TRUE) {
         # assume signed labels were already adjusted, but not main labels
         # Todo: adjust them all, consistently, in venndir_label_style()
         new_label_col <- ifelse(gdf$type %in% "main",
            make_color_contrast(x=gdf$label_col,
               # L_threshold=63,
               y=gdf$final_fill,
               ...),
            gdf$label_col);
         gdf$label_col <- new_label_col;
      }
      ## update all labels
      # jamba::printDebug("gdf:");print(gdf);# debug
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
      do_draw=FALSE, # experimental
      do_pop_viewport=TRUE,
      ...);
      # do_pop_viewport=FALSE);
   # on.exit(grid::popViewport());
   # adjx,adjy are functions to transform x,y into grid "snpc" coordinates
   adjx <- attr(jp, "adjx");
   adjy <- attr(jp, "adjy");
   jp_viewport <- attr(jp, "viewport");
   # jp_gTree has the polygon grobs
   jp_gTree <- attr(jp, "grob_tree");
   jp_grobList <- list()
   jp_grobList$jps <- jp_gTree;

   ############################################
   # Plot title
   if (length(main) > 0 && any(nchar(main) > 0)) {
      main <- gsub("\n", "<br>", main);
      main <- jamba::cPaste(main, sep="<br>\n");
      nlines <- length(strsplit(main, "<br>")[[1]])
      #
      main_grob <- gridtext::richtext_grob(
         text=main,
         x=0.5,
         y=grid::unit(1, "snpc") - grid::unit(0.75, "char"),
         default.units="snpc",
         gp=grid::gpar(
            # col=itemlabels_df$color,
            fontsize=16 * font_cex[1]),
         r=grid::unit(c(0, 0, 0, 0), "pt"),
         padding=grid::unit(c(0, 0, 0, 0), "pt"),
         margin=grid::unit(c(0, 0, 0, 0), "pt"),
         vp=jp_viewport,
         hjust=0.5,
         vjust=1);
      jp_grobList$main_title <- main_grob;
   }
   
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
      if ("default" %in% item_style) {
         # auto-detect
         item_style <- "text";
         # check for <br>, <span>, <sup>, <sub>, or *text* format
         gridtext_check <- "<br>|<span|[*][^*]+[*]|<sup>|<sub>";
         if (jamba::igrepHas(gridtext_check, itemlabels_df$text)) {
            item_style <- "gridtext";
         }
      }
      if ("text" %in% item_style) {
         # jamba::printDebug("itemlabels_df:");print(itemlabels_df);# debug
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
      } else if ("gridtext" %in% item_style) {
         # jamba::printDebug("itemlabels_df:");print(itemlabels_df);# debug
         text_grob <- gridtext::richtext_grob(
            text=itemlabels_df$text,
            x=adjx(itemlabels_df$x),
            y=adjy(itemlabels_df$y),
            rot=jamba::rmNULL(nullValue=0, itemlabels_df$rot),
            # check.overlap=FALSE,
            default.units="snpc",
            gp=grid::gpar(
               col=itemlabels_df$color,
               fontsize=itemlabels_df$fontsize),
            r=grid::unit(c(0, 0, 0, 0), "pt"),
            padding=grid::unit(c(0, 0, 0, 0), "pt"),
            margin=grid::unit(c(0, 0, 0, 0), "pt"),
            vp=jp_viewport,
            hjust=0.5,
            vjust=0.5);
      }
      jp_grobList$item_labels <- text_grob;
      # grid::grid.draw(text_grob);
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
      # jamba::printDebug("gdf:");print(gdf);# debug
      # confirm gdf is not empty - (but why would it be empty?)
      if (nrow(gdf) > 0) {
         # 0.0.36.900 - use a list
         gdf$roworder <- seq_len(nrow(gdf));
         # jamba::printDebug("head(gdf, 10):");print(head(gdf, 10));# debug
         gdf_list <- split(gdf,
            jamba::pasteByRow(gdf[, c("padding", "r"), drop=FALSE]));
         ## Push viewport in case that helps grob size estimates
         # grid::pushViewport(jp_viewport);
         g_labels_list <- lapply(gdf_list, function(igdf){
            ## 0.0.39.900 - fix for inconsistent whitespace width
            ## - seems to occur only with ": " and on certain output devices
            igdf$text <- gsub(": ", ":", igdf$text);
            # jamba::printDebug("igdf$text");print(igdf$text);# debug
            g_labels <- gridtext::richtext_grob(
               text=igdf$text,
               x=adjx(igdf$x),
               y=adjy(igdf$y),
               default.units="snpc",
               vjust=igdf$vjust,
               hjust=igdf$hjust,
               halign=igdf$halign,
               # valign=igdf$valign,
               valign=0.5,
               rot=igdf$rot,
               padding=grid::unit(igdf$padding,
                  igdf$padding_unit),
               r=grid::unit(igdf$r,
                  igdf$r_unit),
               vp=jp_viewport,
               gp=grid::gpar(
                  fontfamily=fontfamily,
                  col=igdf$label_col,
                  fontsize=igdf$fontsize
               ),
               box_gp=grid::gpar(
                  col=if(group_labels){NA}else{igdf$border_col},
                  fill=if(group_labels){NA}else{igdf$box_fill},
                  # col=igdf$border_col,
                  # fill=igdf$box_fill,
                  lty=igdf$box_lty,
                  lwd=igdf$box_lwd)
            );
            # re-assign custom childNames to the grobs
            new_childNames <- paste0(
               igdf$overlap_set, ":",
               igdf$type, ":",
               gsub("(count|overlap)_", "\\1:", igdf$location), ":",
               igdf$roworder, ":",
               igdf$label_df_rowname);
            templist <- lapply(grid::childNames(g_labels), function(iname){
               grid::getGrob(g_labels, iname);
            })
            names(templist) <- new_childNames;
            # jamba::printDebug("names(templist):");print(names(templist));# debug
            templist
         });
         # grid::popViewport();
         # assemble g_labels_list into gTree
         g_labels_gTree <- grid::grobTree(
            vp=jp_viewport,
            do.call(grid::gList,
               unlist(unname(g_labels_list), recursive=FALSE)),
            name="labels");
         # jamba::printDebug("names(grid::childNames(g_labels_gTree)):");print(names(grid::childNames(g_labels_gTree)));# debug
         g_labels_groupdf <- data.frame(check.names=FALSE,
            jamba::rbindList(strsplit(names(grid::childNames(g_labels_gTree)), ":"),
               newColnames=c("overlap_set",
                  "counttype",
                  "type",
                  "location",
                  "roworder",
                  "label_df_rowname",
                  paste0("extra", seq_len(100)))))
         # jamba::printDebug("render_venndir(): ", "g_labels_groupdf:");print(g_labels_groupdf);# debug
         g_labels_groupdf$roworder <- as.numeric(g_labels_groupdf$roworder);
         g_labels_groupdf$name <- names(grid::childNames(g_labels_gTree));
         g_labels_groupdf$childName <- grid::childNames(g_labels_gTree);
         g_labels_groupdf <- jamba::mixedSortDF(g_labels_groupdf,
            byCols=c(1, 4, -3, 2, 5, 6));
         # jp_grobList$g_labels_list <- g_labels_list;
         # jp_grobList$g_labels_groupdf <- g_labels_groupdf;
         # jp_grobList$g_labels_gTree <- g_labels_gTree;
         # jamba::printDebug("g_labels_groupdf:");print(g_labels_groupdf);# debug
         ## Strategy: group labels
         if (TRUE %in% group_labels) {
            # jamba::printDebug("unique(segment_df):");print(unique(segment_df));# debug
            # grid::pushViewport(attr(jp, "viewport"));
            grouped_labels_list <- tryCatch({
               dgg <- draw_gridtext_groups(
                  g_labels=g_labels_gTree,
                  gdf=gdf[g_labels_groupdf$roworder, , drop=FALSE],
                  groupdf=g_labels_groupdf,
                  segment_df=unique(segment_df),
                  adjust_center=adjust_center,
                  adjx_fn=adjx,
                  adjy_fn=adjy,
                  do_draw=FALSE,
                  template=template,
                  verbose=verbose)
               # dgg$g_labels;
               dgg
            }, error=function(e){
               print(e);
               g_labels;
            });
            # grid::popViewport();
            # jp_grobList$grouped_labels_list <- grouped_labels_list;
            # roundedRect around grouped labels
            g_labels_grobs <- grouped_labels_list$grobs;
            # adjusted positions for grouped labels
            g_labels <- grouped_labels_list$g_labels;
            
            ## do not draw here
            # grid::pushViewport(attr(jp, "viewport"));
            ## draw roundedrect outlines
            # lapply(g_labels_grobs, grid::grid.draw);
            ## draw re-positioned grouped labels
            # grid::grid.draw(g_labels);
            # grid::popViewport();
            
            ## Todo: Consider combining label_borders and labels
            jp_grobList$label_borders <- grid::grobTree(
               do.call(grid::gList, g_labels_grobs),
               vp=jp_viewport,
               name="label_borders");
            jp_grobList$labels <- grid::grobTree(
               g_labels,
               vp=jp_viewport,
               name="labels");
         } else {
            ## do not draw here
            # grid::pushViewport(attr(jp, "viewport"))
            # grid::grid.draw(g_labels_gTree);
            # grid::popViewport();
            # jp_grobList$labels <- g_labels_gTree;
         }
      }
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
      ## do not draw here
      # grid::grid.draw(segments_grob);
      jp_grobList$segments <- grid::grobTree(
         vp=jp_viewport,
         name="segments",
         grid::gList(segments_grob));
   }
   
   ## grobs drawn
   # text_grob
   # g_labels
   # segments_grob

   # prepare new Venndir object
   vo_new <- new("Venndir",
      jps=venn_jp,
      label_df=label_df,
      setlist=setlist,
      metadata=metadata)

   # venndir legender
   if (TRUE %in% draw_legend) {
      ## Todo: Consider returning grobs from this function also
      legend_grob <- venndir_legender(
         venndir_output=vo_new,
         x=legend_x,
         font_cex=legend_font_cex,
         draw_legend=FALSE,
         vp=jp_viewport,
         ...)
      jp_grobList$legend <- legend_grob;
   }

   ## Assemble into gTree
   venndir_gtree <- grid::grobTree(
      do.call(grid::gList, jp_grobList),
      vp=jp_viewport,
      name="venndir_gTree")
   attr(vo_new, "gtree") <- venndir_gtree;
   attr(vo_new, "grob_list") <- jp_grobList;
   attr(vo_new, "viewport") <- jp_viewport;
   
   ## Draw the rest of the owl
   if (TRUE %in% do_draw) {
      if (TRUE %in% do_newpage) {
         grid::grid.newpage();
      }
      grid::grid.draw(venndir_gtree);
   }

   ## Todo: Consider displaying message on the plot.
   ## Perhaps the number of hidden overlap labels?
   # warning in case not all overlaps can be displayed
   if (length(warning_label) > 0) {
      jamba::printDebug("warning_label exists");
   }
   # return Venndir object
   return(invisible(vo_new));
}
