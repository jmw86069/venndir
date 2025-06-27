
#' Render venndir output, Venndir generic plot()
#' 
#' Render venndir output, generic plot function for a Venndir object
#' 
#' 
#' ## About fonts and unicode symbols
#' 
#' R handles unicode symbols for each graphics device somewhat independently.
#' Creating a PNG uses a different set of fonts than creating a PDF.
#' Not all fonts used for PNG are compatible with PDF, and vice versa.
#' (It is not solely an R problem.)
#' 
#' For the most part, users should not have to care about these details...
#' 
#' How this affects venndir:
#' 
#' Not every font registered in R supports extended unicode characters
#' `upArrow` and `downArrow`, which causes it to draw blank boxes like `[]`.
#' The quick options:
#' 
#' * Try a different `fontfamily` that may support the characters of interest.
#' * Try a different grob type, see `text_grob_type="marquee"` below.
#' * Install `extrafont`, which may be useful for installing consistent
#' fonts to be used with PNG and PDF output formats.
#' 
#' 
#' ## Additional custom options
#' 
#' Several options are passed through `...` (ellipses) to internal
#' functions, documented below:
#' 
#' * '`L_threshold`' is passed to `make_color_contrast()` to control
#' the Luminance at which text is either dark or light. If dark text
#' is shown on a dark background, set `L_threshold=55` or below the default 65.
#' 
#' * '`text_grob_type`' is passed to `assemble_venndir_labels()` to control
#' the type of `grid` graphical object (grob).
#' 
#'    * `text_grob_type="marquee"` (default) uses `marquee::marquee_grob()`.
#'    Its rendering of Unicode is outstanding, and is the future of R font
#'    rendering. It is not 100% compatible with all device outputs.
#'    Its main feature is using `systemfonts` to substitute missing glyphs
#'    to use another font, for example it always finds Unicode
#'    upArrow/downArrow and other Unicode symbols.
#'    It also handles CommonMark markdown, images, bullets, but not HTML.
#'    * `text_grob_type="textGrob"` uses `grid::textGrob()`, which is the
#'    grid default. It does not handle markdown syntax, and no special handling
#'    of Unicode characters. It is the fastest rendering option.
#'    * `text_grob_type="richtext_grob"` uses `gridtext::richtext_grob()`,
#'    which requires the `gridtext` package. This approach will be removed,
#'    and is kept as a fallback option until marquee is more fully tested.
#'    It recognizes markdown, and some limited HTML.
#' 
#' * '`fontfamilies`' is passed to `assemble_venndir_label()`, as a `list`
#' with three named elements: `"overlap"`, `"count"`, `"signed"`
#' 
#'    * It allows a custom font to be used for each type of label.
#'    It may be useful to use a light font or narrow font for signed labels,
#'    for example.
#'    * The custom font is also recognized by `venndir_legender()` for
#'    consistency, for example a custom count font is used for count labels
#'    in the legend.
#' 
#' * `outerborder`,`outerborder.lwd`,`innerborder`,`innerborder.lwd`,
#' `border`,`border.lwd`
#' 
#'    * These arguments are passed to `plot.JamPolygon()`
#'    and override internal values when defined.
#'    They can produce interesting effects.
#'    * The innerborder is drawn only on the inside edge of each polygon.
#'    * The outerborder is drawn only on the outside edge of each polygon.
#'    * The border is drawn on the border edge itself - and for Venndir
#'    objects by default is typically not drawn.
#' 
#'    * Use white borders: `outerborder=NA, innerborder="white"`
#'    * Use wide internal border, thin white line:
#'    `outerborder=NA, innerborder.lwd=4, border="white", border.lwd=1`
#' 
#' @family venndir core
#' 
#' @inheritParams venndir
#' @param venndir_output `Venndir` output from `venndir()`, or
#'    `list` with element `"vo"` as a `Venndir` object.
#' @param expand_fraction `numeric` value, default `NULL` to use automated
#'    settings. It defines the figure margin fraction on each side in order
#'    clockwise from the bottom:
#'    bottom, left, top, right.
#'    
#'    The default behavior:
#'    * When `expand_fraction` is defined in the call to `render_venndir()`
#'    or `plot()`, or is defined in the metadata of the `Venndir` object,
#'    it is used as-is with no further adjustments. The function argument
#'    takes priority if defined.
#'    * If `expand_fraction` is not defined, and is not present in
#'    `Venndir` metadata, it is adjusted as described below.
#'    * The final, adjusted `expand_fraction` is stored in the
#'    `Venndir` object metadata returned by this function.
#'    
#'    Adjustments, when applied:
#'    * When the legend will be drawn, due to `draw_legend=TRUE`, and
#'    the position `legend_x` includes 'top' or 'bottom', the corresponding
#'    margin is increased `0.2` to reduce overlap between figure and legend.
#'    For example, `legend_x='bottom'` adds `c(0.2, 0, 0, 0)`, and
#'    `legend_x='top'` adds `c(0, 0, 0.2, 0)`.
#'    * When `main` title is defined, the top margin is increased
#'    `0.07` for each line of text.
#' @param font_cex `numeric` scalar to adjust font sizes.
#' @param item_cex `numeric` default NULL, used to define baseline font size
#'    (single value), or exact font `cex` values (multiple values).
#'    * When a single value is provided, each set of items is used to
#'    define a font scaling, based upon the relative area of the
#'    overlap polygon to the max item polygon area, and the number of
#'    items in each polygon. These values are multiplied by `item_cex`
#'    to produce the final adjustment.
#'    These values are multiplied by `item_cex_factor`.
#'    * When multiple values are provided with names, the names are
#'    matched with overlap names: `venndir_output@label_df$overlap_set`,
#'    and applied accordingly. Any missing values retain the pre-existing
#'    value by default.
#'    * When multiple values are provided without names, the length is
#'    matched to the number of polygons in `venndir_output@jps@polygons`
#'    with non-zero and non-NA venn_counts; or the number of
#'    unique polygons in `venndir_output@jps@polygons$venn_name`; or
#'    the number of unique overlaps in `venndir_output@label_df$overlap_set`.
#'    If a length match is found, those values are assigned to
#'    `names(item_cex)`.
#'    There is no further adjustment by polygon area, nor number of labels.
#'    These values are multiplied by `item_cex_factor`.
#' @param item_cex_factor `numeric`, default 1, used to adjust the
#'    `item_cex` values overall. It is intended to be a single value,
#'    and is multiplied by `item_cex` as it is defined for each set of items.
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
#' @param draw_legend `logical` (default NULL) indicating whether to draw
#'    a legend, calling `venndir_legender()`.
#'    When `NULL` it uses `metadata(venndir_output)$draw_legend` when defined,
#'    otherwise defaults to `TRUE`.
#' @param legend_x `character` passed to `venndir_legender()` to customize
#'    the position of the legend.
#' @param legend_font_cex `numeric` scalar to adjust the legend font size.
#' @param draw_footnotes `logical` (default NULL) indicating whether to draw
#'    footnotes if they exist in the 'metadata' slot, calling
#'    `render_venndir_footnotes()`.
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
 expand_fraction=NULL,
 font_cex=1,
 main=NULL,
 item_cex=NULL,
 item_cex_factor=1,
 plot_warning=FALSE,
 show_labels=NULL,
 show_items=NULL,
 item_degrees=NULL,
 max_items=100,
 show_zero=TRUE,
 show_segments=NULL,
 segment_buffer=NULL,
 label_preset=c("none"),
 label_style=c("custom",
    "basic",
    "fill",
    "shaded",
    "shaded_box",
    "lite",
    "lite_box"),
 fontfamily=NULL,
 inside_percent_threshold=0,
 item_style=NULL,
 item_buffer=NULL,
 group_labels=TRUE,
 template=NULL,
 adjust_center=FALSE,
 draw_legend=NULL,
 legend_x="bottomright",
 legend_font_cex=NULL,
 draw_footnotes=NULL,
 show_label=NA,
 display_counts=TRUE,
 do_newpage=TRUE,
 do_draw=TRUE,
 draw_buffer=FALSE,
 verbose=FALSE,
 debug=FALSE,
 ...)
{
   # validate input
   if ("list" %in% class(venndir_output) && "vo" %in% names(venndir_output)) {
      venndir_output <- venndir_output$vo;
   }
   
   # ignore debug="gridtext" when gridtext is not installed
   if ("gridtext" %in% debug &&
         !requireNamespace("gridtext", quietly=TRUE)) {
      debug <- setdiff(debug, "gridtext");
   }
   metadata <- list();
   if ("Venndir" %in% class(venndir_output)) {
      venn_jp <- venndir_output@jps;
      label_df <- venndir_output@label_df;
      setlist <- venndir_output@setlist;
      metadata <- tryCatch({
         metadata(venndir_output)
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
   
   ## validate other options in metadata
   # template
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
   # sign_count_delim
   if ("sign_count_delim" %in% names(metadata)) {
      sign_count_delim <- metadata$sign_count_delim;
   } else {
      sign_count_delim <- " ";
   }
   # draw_legend
   if (length(draw_legend) > 0) {
      draw_legend <- head(as.logical(draw_legend), 1);
   } else {
      if ("draw_legend" %in% names(metadata)) {
         if (length(metadata$draw_legend) == 0) {
            draw_legend <- TRUE
         } else {
            draw_legend <- head(as.logical(metadata$draw_legend), 1);
         }
      } else {
         draw_legend <- TRUE;
      }
   }
   # draw_footnotes
   if (length(draw_footnotes) > 0) {
      draw_footnotes <- head(as.logical(draw_footnotes), 1);
   } else {
      if ("draw_footnotes" %in% names(metadata)) {
         if (length(metadata$draw_footnotes) == 0) {
            draw_footnotes <- TRUE
         } else {
            draw_footnotes <- head(as.logical(metadata$draw_footnotes), 1);
         }
      } else {
         draw_footnotes <- TRUE;
      }
   }
   # legend_font_cex
   if (length(legend_font_cex) > 0) {
      metadata$legend_font_cex <- legend_font_cex;
   } else {
      legend_font_cex <- metadata$legend_font_cex;
   }
   # item_cex
   if (length(item_cex) > 0) {
      metadata$item_cex <- item_cex;
   } else {
      item_cex <- metadata$item_cex;
   }
   # item_style
   if (length(item_style) > 0) {
      metadata$item_style <- item_style;
   } else {
      item_style <- metadata$item_style;
   }
   # item_degrees
   if (length(item_degrees) > 0) {
      metadata$item_degrees <- item_degrees;
   } else {
      item_degrees <- metadata$item_degrees;
      if (length(item_degrees) == 0) {
         item_degrees <- 0;
         metadata$item_degrees <- item_degrees;
      }
   }
   # item_buffer
   if (length(item_buffer) == 0) {
      if ("item_buffer" %in% names(metadata)) {
         item_buffer <- metadata$item_buffer;
      } else {
         item_buffer <- -0.15;
         metadata$item_buffer <- item_buffer;
      }
   } else {
      metadata$item_buffer <- item_buffer;
   }
   
   # fontfamily
   if (length(fontfamily) == 0 || all(fontfamily %in% c(NA, ""))) {
      if ("fontfamily" %in% names(metadata)) {
         fontfamily <- metadata$fontfamily;
      } else {
         # default "sans" to avoid font not being present for all devices
         fontfamily <- "sans";
         metadata$fontfamily <- fontfamily;
      }
   } else {
      metadata$fontfamily <- fontfamily;
   }
   # show_items
   if (length(show_items) > 0) {
      metadata$show_items <- show_items;
   } else {
      show_items <- metadata$show_items;
   }
   
   show_items <- head(setdiff(label_df$show_items, c(NA, "none")), 1);
   if (length(show_items) == 0) {
      show_items <- NA;
   }
   # jamba::printDebug("show_items: ", show_items);# debug
   # show_items <- head(show_items, 1);
   item_style <- match.arg(item_style,
      choices=c("default", "marquee", "text", "gridtext"));
   # if 'gridtext' is chosen but not installed, use 'default' as fallback
   if ("gridtext" %in% item_style &&
         !requireNamespace("gridtext", quietly=TRUE)) {
      item_style <- "default";
   }

   # validate other args
   if (length(expand_fraction) == 0) {
      if ("expand_fraction" %in% names(metadata(venndir_output))) {
         expand_fraction <- metadata(venndir_output)$expand_fraction;
      }
   }
   if (length(expand_fraction) == 0) {
      expand_fraction <- c(-0.05, 0, -0.06, 0)
      if (TRUE %in% draw_legend) {
         if (grepl("bottom", legend_x)) {
            expand_fraction <- expand_fraction + c(0.20, 0, 0, 0)
         } else if (grepl("top", legend_x)) {
            expand_fraction <- expand_fraction + c(0, 0, 0.20, 0)
         }
      }
      if (length(main) > 0 && nchar(main) > 0) {
         main_nlines <- length(unlist(strsplit(main, "(\n|<br>)")))
         expand_fraction <- expand_fraction + c(0, 0, main_nlines * 0.07, 0);
      }
   } else {
      expand_fraction <- rep(expand_fraction, length.out=4);
   }
   # Store back into Venndir
   metadata$expand_fraction <- expand_fraction;

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
         max_items=max_items,
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
   
   # segment_buffer
   if (length(segment_buffer) == 0) {
      if ("segment_buffer" %in% names(metadata)) {
         segment_buffer <- metadata$segment_buffer;
      }
      if (length(segment_buffer) == 0) {
         segment_buffer <- -0.1;
      }
   }
   # Process labels
   warn_df <- data.frame(overlap_set="A", venn_counts=0)[0, , drop=FALSE];
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
      if (TRUE %in% verbose) {
         jamba::printDebug("render_venndir): ",
            "Processing labels.");
      }
      # auto-scale item_cex based upon number of items and polygon area
      if (length(item_cex) == 0) {
         item_cex <- 1;
      }
      if (length(item_cex) == 1) {
         if (is.na(item_cex)) {
            item_cex <- 1;
         }
         # recipe to calculate item_cex
         if (TRUE %in% verbose) {
            jamba::printDebug("render_venndir): ",
               "Defining item_cex.");
         }
         item_cex <- tryCatch({
            poly_rows <- which(!is.na(venn_jp@polygons$venn_counts) &
                  venn_jp@polygons$venn_counts >= 0);

            # area adjustment is sqrt() of the fraction of max area
            so_areas <- area_JamPolygon(venn_jp[poly_rows, ]);
            names(so_areas) <- rownames(venn_jp@polygons)[poly_rows];
            so_area_fracmax <- so_areas / max(so_areas, na.rm=TRUE)
            
            # so_area_adj <- sqrt(so_area_fracmax);
            so_area_adj <- (so_area_fracmax)^(1/2.5);
            
            # count adjustment - 4 / cube root of count
            # so it gets smaller with more labels, slightly slower then sqrt()
            so_counts <- venn_jp@polygons$venn_counts[poly_rows];
            names(so_counts) <- rownames(venn_jp@polygons)[poly_rows];
            n_per_area_fracmax <- so_counts / so_area_fracmax;
            # adjfn() is a wrapper for cube root transform
            # adjfn <- function(x)sqrt(x)
            adjfn <- function(x)x^(1/3)
            so_cex <- (3 / adjfn(so_counts))

            # update here in case area fails, it will use this crude item_cex
            new_item_cex <- item_cex * so_cex * so_area_adj * item_cex_factor;
            ## apply noise floor - limits the maximum practical item_cex
            floor_item_cex <- jamba::noiseFloor(new_item_cex,
               ceiling=50,
               floor=0.2)
            if (verbose > 1 || TRUE %in% debug) {
               jamba::printDebug("item label size calculations:");
               print(data.frame(so_areas, so_area_fracmax, so_area_adj,
                  so_counts, n_per_area_fracmax, so_cex,
                  item_cex, new_item_cex, floor_item_cex));# debug
            }
            item_cex <- floor_item_cex;

            if (FALSE) {
               ## Previous calculations
               # take median of the larger area polygons
               so_big <- median(so_areas[so_areas / max(so_areas) >= 0.5])
               so_areas_cex <- sqrt(so_areas) / sqrt(so_big);
               
               # weight the effect by number of item labels
               # so that 2 labels would not be scaled as much as 10
               so_areas_wt <- (1 + 0.5) / (so_counts + 0.5)
               so_areas_cex_wt <- so_areas_wt + (1 - so_areas_wt) * so_areas_cex
               # adjust the crude scaling by the relative polygon area
               item_cex <- item_cex * so_areas_cex_wt;
               # jamba::printDebug("so_areas_cex_wt (relative area adjust): ", so_areas_cex_wt);# debug
               # jamba::printDebug("item_cex (relative area adjusted): ", item_cex);# debug
               # for debugging, the data.frame can be printed
               #print(data.frame(so_counts, so_cex, so_areas_cex, so_areas_wt, so_areas_cex_wt, item_cex));
            }
            item_cex;
         }, error=function(e){
            if (TRUE %in% verbose || TRUE %in% debug) {
               jamba::printDebug("Error during item_cex calculations:");
               print(e);
            }
            item_cex;
         });
      } else {
         item_cex <- item_cex * item_cex_factor;
      }
      if (length(item_cex) == 0 || all(is.na(item_cex))) {
         item_cex <- 1;
      }
      if (length(names(item_cex)) == 0) {
         uvnames1 <- unique(subset(venn_jp@polygons, !is.na(venn_counts) &
               venn_counts > 0)$venn_name);
         uvnames <- unique(venn_jp@polygons$venn_name);
         uonames <- unique(label_df$overlap_set);
         if (length(item_cex) == length(uvnames1)) {
            names(item_cex) <- uvnames1;
         } else if (length(item_cex) == length(uvnames)) {
            names(item_cex) <- uvnames;
         } else if (length(item_cex) == length(uonames)) {
            names(item_cex) <- uonames;
         } else {
            item_cex <- rep(item_cex, length.out=length(uonames));
            names(item_cex) <- uonames;
         }
      }
      metadata$item_cex <- item_cex;
      # jamba::printDebug("item_cex: ", item_cex);# debug
      
      ## Fill any missing optional colnames with defaults
      label_df_defaults <- list(
         type="main",
         show_label=NA,
         show_items=NA,
         item_degrees=item_degrees,
         rot=0,
         color="black",
         fontsize=14,
         item_cex=head(item_cex, 1),
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
      if (TRUE %in% verbose) {
         jamba::printDebug("render_venndir): ",
            "Applying label_df defaults.");
      }
      for (i in label_df_add) {
         # if (length(label_df_defaults[[i]]) > 0) {
            label_df[[i]] <- rep(label_df_defaults[[i]],
               length.out=nrow(label_df));
         # }
      }
      
      # manually apply item_cex
      if (length(names(item_cex)) > 0) {
         match_i <- match(label_df$overlap_set, names(item_cex))
         label_df$item_cex <- ifelse(!is.na(match_i),
            item_cex[match_i],
            label_df$item_cex)
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
      
      # 0.0.51.900 - silence "agreement/concordance" for single set overlaps
      if ("curate_df" %in% names(venndir_output@metadata)) {
         curate_df <- venndir_output@metadata[["curate_df"]];
      } else {
         if ("unicode" %in% names(venndir_output@metadata)) {
            unicode <- venndir_output@metadata[["unicode"]];
         } else {
            # detect any unicode chars with integer value > 127
            unicode <- any(sapply(label_df$text, function(i){
               any(utf8ToInt(i) > 127)
            }))
         }
         curate_df <- get_venndir_curate_df(unicode=unicode,
            ...)
      }
      # Use "hide_singlet" if already defined in label_df
      # Todo: Display hidden singlet when only showing signed values?
      if (!"hide_singlet" %in% colnames(label_df)) {
         label_df$hide_singlet <- FALSE;
         if (inherits(curate_df, "data.frame") > 0 &&
               "hide_singlet" %in% colnames(curate_df) &&
               any(curate_df$hide_singlet %in% TRUE)) {
            hide_vals <- subset(curate_df, hide_singlet %in% TRUE)$from;
            hide_grep <- paste0(paste0("^(", hide_vals, ")$"), collapse="|");
            hiderows <- (label_df$nsets %in% 1 &
               grepl(hide_grep,
                  gsub("^.+[|]", "", label_df$overlap_sign)));
            # hide singlets matching the appropriate pattern
            if (any(hiderows)) {
               label_df[hiderows, "hide_singlet"] <- TRUE
               show_label[hiderows] <- FALSE;
            }
         }
      }
      # 0.0.34.900 -  experiment by adding show_label
      label_df$show_label <- show_label;
      
      # 0.0.51.900 - re-apply "none" visibility for show_label=FALSE
      if (any(label_df$show_label %in% FALSE)) {
         noshow <- (label_df$show_label %in% FALSE);
         icols <- intersect(c("count"), colnames(label_df));
         for (icol in icols) {
            label_df[noshow, icol] <- "none";
         }
      }
      if (any(TRUE %in% label_df$hide_singlet)) {
         if ("item_style" %in% colnames(label_df)) {
            signitem <- grepl("sign.*item|item.*sign", label_df$item_style);
            nssi <- (label_df$hide_singlet %in% TRUE & signitem);
            if (any(nssi)) {
               label_df[nssi, "item_style"] <- gsub("sign", "",
                  label_df[nssi, "item_style"]);
            }
         }
      }

      # 0.0.34.900 -  experiment by adding poly_ref_name
      if (!"ref_polygon" %in% colnames(label_df)) {
         matchjps <- match(label_df$overlap_set, rownames(venn_jp@polygons));
         label_df$ref_polygon <- venn_jp@polygons$ref_polygon[matchjps];
      }
      
      # prepare line segments for labels outside their respective polygons
      g_labels <- NULL;
      segment_df <- NULL;
      if (any(show_label %in% TRUE)) {
         if (TRUE %in% verbose) {
            jamba::printDebug("render_venndir): ",
               "Showing labels.");
         }
         # jamba::printDebug("show_label");
         # jamba::printDebug("table(label_df$overlap):");print(table(label_df$overlap));
         label_outside <- (label_df$overlap %in% "outside" |
               label_df$count %in% "outside");
         
         # Determine if any offset labels require line segment
         has_offset <- label_outside &
            (!label_df$x_offset %in% c(0, NA) |
             !label_df$y_offset %in% c(0, NA));

         # Todo: Deal with has_offset, for now set to FALSE
         # jamba::printDebug("label_outside:");print(table(label_outside));
         # jamba::printDebug("has_offset:");print(table(has_offset));
         #
         # Handle labels outside
         # - Currently associates singlet labels together with the set name.
         # - Dr. Theofilatos suggested not doing this association.
         if (any(show_label & has_offset)) {
            use_offset <- (show_label & has_offset);
            # use_offset <- (show_label & has_offset & label_df$venn_counts > 0);
            offset_sets <- label_df$overlap_set[use_offset];
            # jamba::printDebug("offset_sets: ");print(offset_sets);# debug
            offset_ref_polygon <- venn_jp@polygons$ref_polygon[
               match(offset_sets, rownames(venn_jp@polygons))];
            names(offset_ref_polygon) <- offset_sets;
            # jamba::printDebug("offset_ref_polygon: ");print(offset_ref_polygon);# debug
            sp_index <- match(offset_ref_polygon, rownames(venn_jp@polygons))
            names(sp_index) <- offset_sets;
            # jamba::printDebug("sp_index: ");print(sp_index);# debug
            
            ## 0.0.32.900 - subset polygons for non-empty coordinates
            polygon_nonempty <- venn_jp@polygons$is_empty %in% 0;
            use_jp <- venn_jp[which(polygon_nonempty), ];
            use_polygons <- use_jp@polygons;
            use_polygons$rownum <- seq_len(nrow(use_polygons));
            use_polygons$nsets <- lengths(strsplit(use_polygons$name, "&"));
            use_polygons <- jamba::mixedSortDF(use_polygons,
               byCols=c("type", "nsets"));
            ## define best available polygon to label
            
            sp_index0 <- sapply(offset_sets, function(iset1){
               # 0.0.56.900 - add escapes to support inline styles "{.style text}"
               esc_iset1 <- gsub("([\\^*])", "\\\\\\1", iset1)
               esc_iset1 <- gsub("([(){}.+$])", "[\\1]", esc_iset1)
               # jamba::printDebug("paste0(\"(^|&)\", iset1, \"($|&)\"):");print(paste0("(^|&)", iset1, "($|&)"));# debug
               # jamba::printDebug("paste0(\"(^|&)\", esc_iset1, \"($|&)\"):");print(paste0("(^|&)", esc_iset1, "($|&)"));# debug
               kset1 <- head(grep(paste0("(^|&)", esc_iset1, "($|&)"),
                  use_polygons$label), 1)
               iset2 <- use_polygons$rownum[kset1];
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
      if (TRUE %in% verbose) {
         jamba::printDebug("render_venndir): ",
            "Showing items.");
      }
      items_dfs <- subset(label_df, !label_df$show_items %in% c("none", NA));
      uos <- unique(as.character(items_dfs$overlap_set));
      if (length(names(item_buffer)) > 0 &&
            all(uos %in% names(item_buffer))) {
         item_buffer <- item_buffer[uos];
      } else {
         item_buffer <- rep(item_buffer,
            length.out=length(uos));
         names(item_buffer) <- uos;
      }
      # jamba::printDebug("unique(items_dfs$overlap_set): ", unique(items_dfs$overlap_set));
      # jamba::printDebug("middle(items_dfs, 20): ");print(jamba::middle(items_dfs, 20));# debug
      items_dfs <- split(items_dfs,
         factor(items_dfs$overlap_set,
            levels=unique(items_dfs$overlap_set)));
      # jamba::printDebug("sdim(items_dfs):");print(jamba::sdim(items_dfs));# debug
      # jamba::printDebug("items_dfs:");print(items_dfs);# debug

      #for (items_df1 in items_dfs) {
      itemlabels_list <- lapply(items_dfs, function(items_df1){
         items_df1$rownum <- seq_len(nrow(items_df1));
         # jamba::printDebug("middle(items_df1, 20): ");# debug
         # print(jamba::middle(items_df1, 20));# debug
         useos <- as.character(head(items_df1$overlap_set, 1))
         use_item_buffer <- item_buffer[useos]
         items_list <- items_df1$items;
         items_list <- items_list[lengths(items_list) > 0];
         if (length(items_list) > 0) {
            ## 0.0.42.900 and previous
            # items <- unname(unlist(jamba::mixedSorts(items_list)));
            ## 0.0.43.900
            # items <- unname(unlist((items_list)));
            ## 0.0.44.900 - expect items to be ordered factor or character
            items <- unname(unlist(jamba::mixedSorts(items_list)));
         } else {
            return(NULL)
         }
         color1 <- rep(items_df1$color, lengths(items_df1$items));
         vis <- which(venn_jp@polygons$label %in% items_df1$overlap_set);
         # vi <- tail(venn_jp@polygons$label %in% items_df1$overlap_set, 1);
         vi <- venn_jp@polygons$label %in% tail(items_df1$overlap_set, 1);
         vdf <- venn_jp@polygons[vi, , drop=FALSE];
         if (nchar(sign_count_delim) > 0) {
            use_sign_count_delim <- paste(collapse="",
               paste0("[",
                  gsub("[\\^]", "\\\\^",
                     strsplit(sign_count_delim, "")[[1]]),
                  "]"))
            use_pattern <- paste0("(", use_sign_count_delim, "|[ :]*)[0-9,]+$")
         } else {
            use_pattern <- paste0("([ :]*)[0-9,]+$")
         }
         prefixes <- rep(
            # gsub("[ :]*[0-9,]+$", "", items_df1$text),
            gsub(use_pattern, "", items_df1$text),
            lengths(items_df1$items));
         labels <- NULL;
         # note currently uses the same show_items format per polygon
         # not for each row in items_dfs, so it is not possible to
         # use different show_items format for up-up and down-down within
         # the same polygon
         
         # Todo: change "item_style" colname to "item_label"?
         # because item_style is used for something else
         use_show_items <- tail(items_df1$item_style, 1);
         show_items_order <- strsplit(use_show_items, "[- _.]")[[1]];
         for (dio in show_items_order) {
            # jamba::printDebug("dio:", dio);# debug
            if (grepl("sign", dio)) {
               if (length(labels) > 0) {
                  labels <- paste0(labels, sign_count_delim, prefixes);
               } else {
                  labels <- prefixes;
               }
               # jamba::printDebug("prefixes:", head(prefixes));# debug
            } else if (grepl("item", dio)) {
               if (length(labels) > 0) {
                  labels <- paste0(labels, sign_count_delim, items);
               } else {
                  labels <- items;
               }
            }
         }
         # 0.0.45.900 - remove this line, should not be needed and may be
         # useful to permit leading and trailing spaces
         # labels <- gsub("^[ ]+|[ ]+$", "", labels);
         bg <- jamba::alpha2col(vdf$color, vdf$alpha)
         color <- make_color_contrast(color1,
            y=bg,
            ...);
         # define item coordinates
         lpf <- label_fill_JamPolygon(jp=venn_jp[tail(vis, 1), ],
            ref_jp=venn_jp,
            color=color,
            cex=head(items_df1$item_cex, 1),
            draw_points=FALSE,
            labels=labels,
            plot_style="none",
            draw_labels=FALSE,
            degrees=items_df1$item_degrees[1],
            buffer=use_item_buffer,
            seed=123,
            verbose=verbose,
            ...);
         if (length(lpf) == 0) {
            return(lpf)
         }
         # add overlap label to items_df
         lpf$items_df$overlap_set <- head(items_df1$overlap_set, 1);
         # lpf$items_df$rownum <- rep(items_df1$rownum, lengths(items_df1$items));
         # jamba::printDebug("lpf$items_df:");print(head(lpf$items_df, 10));# debug
         lpf;
      });
      if (TRUE %in% verbose) {
         jamba::printDebug("render_venndir): ",
            "Defined item label positions.");
         print(jamba::sdim(itemlabels_list));# debug
      }
      # combine item label into one data.frame
      itemlabels_df <- jamba::rbindList(lapply(
         names(jamba::rmNULL(itemlabels_list)), function(i1name){
            itemlabels_list[[i1name]]$items_df;
         }));
      # jamba::printDebug("ssdim(itemlabels_list):");print(jamba::ssdim(itemlabels_list));# debug
      # jamba::printDebug("dim(itemlabels_df):");print(dim(itemlabels_df));# debug
      # jamba::printDebug("middle(itemlabels_df):");print(jamba::middle(itemlabels_df));# debug
      # jamba::printDebug(unique(itemlabels_df$color), unique(itemlabels_df$color));# debug
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
      if (any(grepl("[\n\r]|<br>", venn_label))) {
         # venn_label <- gsub("\n", "<br>",
         #    gsub("^[\n]+|[\n]+$", "",
         #    gsub("\n\n", "\n",
         #    gsub("[\r\n]+|<br>", "\n", venn_label))))
         # 0.0.47.900
         venn_label <- gsub("\n", "<br>",
            gsub("^[\n]+|[\n]+$", "", # remove leading/trailing newline
               gsub("[\n]+", "\n",    # limit to one newline at a time
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
      # jamba::printDebug("label_df pre-gdf:");print(label_df);# debug
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
      # fix halign for one-column or two-column alignment
      # gdf$halign <- 0.5;

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
      ksplit <- split(seq_along(bg_alpha), max_bg_alpha);
      bg_alpha_adj <- rep(0, length(bg_alpha));
      for (kset in ksplit) {
         bg_alpha_adj[kset] <- jamba::noiseFloor(bg_alpha[kset],
            ceiling=unique(max_bg_alpha[kset]))
      }
      bg_fill_adj <- jamba::alpha2col(bg_fill, alpha=bg_alpha_adj);
      canvas_alpha <- max_bg_alpha - bg_alpha_adj;
      canvas_adj <- jamba::alpha2col(
         rep("white", length(canvas_alpha)),
         alpha=canvas_alpha)
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
      if (TRUE %in% verbose) {
         jamba::printDebug("render_venndir): ",
            "Defined gdf.");
      }
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
      do_pop_viewport=TRUE, # do_pop_viewport,do_viewport,do_newpage FALSE due to do_draw=FALSE
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
   if (TRUE %in% verbose) {
      jamba::printDebug("render_venndir): ",
         "Created jps grobs.");
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
         y=itemlabels_df$fill,
         ...)
      itemlabels_df$color <- new_item_color;
      # jamba::printDebug("middle(itemlabels_df):");print(jamba::middle(itemlabels_df));
      #
      if ("gridtext" %in% item_style) {
         # auto-detect
         item_style <- "text";
         # check for <br>, <span>, <sup>, <sub>, or *text* format
         gridtext_check <- "<br>|<span|[*][^*]+[*]|<sup>|<sub>";
         if (jamba::igrepHas(gridtext_check, itemlabels_df$text)) {
            item_style <- "gridtext";
         }
      }
      if (any(c("default", "marquee") %in% item_style) ||
            "marquee" %in% debug) {
         ## use marquee by default
         ## substitute style for each label
         # '{.color label}'
         ## 0.0.56.900 - recognize tags to prevent styling:
         # '![Caption](image)'
         # new_item_text <- paste0("{",
         #    itemlabels_df$color, " ",
         #    itemlabels_df$text, "}")
         # print("items marquee_grob");# debug
         # define styles
         min_fontsize <- min(itemlabels_df$fontsize);
         item_mstyle <- marquee::classic_style(
            base_size=min_fontsize,
            lineheight=0.9,
            body_font=fontfamily,
            align="center")
         ## placeholder styling below:
            # outline="white", outline_width=2,
            # background="palegoldenrod",
            # border="black", border_size=marquee::trbl(1, 1, 1, 1), border_radius=3,
            # padding=marquee::trbl(1, 3, 1, 3),
         if (length(venndir_output@metadata[["marquee_styles"]]) > 0) {
            item_mstyle <- combine_marquee_styles(
               mss=item_mstyle,
               msl=venndir_output@metadata[["marquee_styles"]],
               ...)
         }
         
         rel_fontsize <- round(itemlabels_df$fontsize / min_fontsize, digits=2);
         rel_fontsizeu <- jamba::nameVector(unique(rel_fontsize));
         
         ## 0.0.56.900 - no need to add rel.1 - also avoids re-styling
         ## when jitter_cex=0
         add_rel_sizes <- setdiff(rel_fontsize, 1.0);
         for (irel in add_rel_sizes) {
            item_mstyle <- marquee::modify_style(
               x=item_mstyle,
               paste0("rel.", irel),
               size=marquee::relative(irel))
         }
         
         ## 0.0.56.900 - recognize image tag fields
         text_is_img <- grepl("^[!][[].*][(].+[)]$", itemlabels_df$text);

         ## In progress: handle multi-line entries, each line on its own span
         add_inline_color <- TRUE;
         if (add_inline_color) {
            use_text <- itemlabels_df$text;
            
            # replace HTML <br> with newline "\n"
            use_text <- gsub('<br>|<br/>', "\n", use_text)
            
            # replace newline "\n" with placeholder "~!!~"
            # use_text <- gsub("\n\n", "\n~!!~", use_text)
            use_text <- gsub("\n", "~!!~", use_text)
            
            ## enforce "  \n" at line ends to force marquee line breaks
            # use_text <- gsub("([^ \n]|^)\n", "\\1  \n", use_text)
            
            ## split multi-line entries into lines to apply span to each
            use_texts <- NULL;
            k <- seq_along(use_text);
            use_text_multi <- grepl("~!!~", use_text);
            if (any(use_text_multi)) {
               # split by line, yadda yadda
               use_texts <- strsplit(use_text, "~!!~")
               use_text <- unlist(use_texts);
               k <- rep(k, lengths(use_texts));
            }
            new_item_text <- paste0(
               "{", paste0(".rel.", rel_fontsize[k]), " ",
               "{", itemlabels_df$color[k], " ",
               use_text,
               "}", "}")
            if (length(use_texts) > 0) {
               new_item_text <- unname(jamba::cPaste(sep="  \n",
                  split(new_item_text, k)))
            }
         } else {
            new_item_text <- paste0(
               "{", paste0(".rel.", rel_fontsize), " ",
               gsub("([^ \n]|^)\n", "\\1  \n",
                  gsub('<br>|<br/>', '  \n', itemlabels_df$text)),
               "}"
               )
         }
         ## consider reverting images so they are not sized relative to text,
         ## except that seems to break viewport during rendering.
         if (FALSE && any(text_is_img)) {
            new_item_text[text_is_img] <- paste0("{.body ", as.character(
               itemlabels_df$text[text_is_img]), "}");
         }
         # jamba::printDebug("new_item_text:");print(new_item_text);# debug
         
         text_grob <- marquee::marquee_grob(
            text=new_item_text,
            force_body_margin=TRUE,
            width=NA,
            ignore_html=TRUE,
            name="venndir_items",
            x=adjx(itemlabels_df$x),
            y=adjy(itemlabels_df$y),
            default.units="snpc",
            angle=jamba::rmNULL(nullValue=0, itemlabels_df$rot),
            style=item_mstyle,
            vp=jp_viewport,
            hjust="center-ink",
            vjust="center-ink");
      } else if ("text" %in% item_style) {
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
               fontfamily=fontfamily,
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
               fontfamily=fontfamily,
               fontsize=itemlabels_df$fontsize),
            r=grid::unit(0, "pt"),
            padding=grid::unit(c(0, 0, 0, 0), "pt"),
            margin=grid::unit(c(0, 0, 0, 0), "pt"),
            vp=jp_viewport,
            hjust=0.5,
            vjust=0.5);
      }
      if (TRUE %in% verbose) {
         jamba::printDebug("render_venndir): ",
            "Completed item label grobs.");
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
         
         # split gdf rows by groups of labels
         if (FALSE) {
            # pre-0.0.54.900 - use overlap_set for grouping labels
            # note that if overlap label is ever shown itself, we cannot combine
            new_childNames <- paste0(
               gdf$overlap_set, ":",
               # gdf$ref_polygon, ":",
               gdf$type, ":",
               gsub("(count|overlap)_", "\\1:", gdf$location), ":",
               gdf$roworder, ":",
               gdf$label_df_rowname);
            # 0.0.54.900 - use ref_polygon for grouping labels
            new_childNames <- paste0(
               # gdf$overlap_set, ":",
               gdf$ref_polygon, ":",
               gsub("^.*(count|overlap)_", "", gdf$location), ":")
         }
         # 0.0.55.900 - use ref_polygon,x,y coords for grouping
         new_childNames <- paste0(
            gdf$ref_polygon, ":",
            round(gdf$x, digits=3), ":", round(gdf$y, digits=3), ":",
            gsub("^.*(count|overlap)_", "", gdf$location), ":")
         gdf$label_group <- new_childNames;
         # jamba::printDebug("head(gdf, 20):");print(head(gdf, 20));# debug
         
         # 0.0.55.900 - intermediate check for consistent coordinates
         # for (icn in unique(new_childNames)) {
         #    igdf_rows <- which(new_childNames %in% icn)
         #    # ensure all x,y coords are identical per label group?
         #    gdf[igdf_rows, "x"] <- head(gdf[igdf_rows, "x"], 1)
         #    gdf[igdf_rows, "y"] <- head(gdf[igdf_rows, "y"], 1)
         # }

         gdf_list <- split(gdf,
            factor(new_childNames, levels=unique(new_childNames)));

         # make segment_df$match_group so it uses consistent grouping logic
         if (length(segment_df) > 0) {
            segment_k <- rep(seq(from=1, to=nrow(segment_df), by=2), each=2);
            segment_df$match_group <- paste0(segment_df$group, ":",
               round(segment_df$x[segment_k], digits=3), ":",
               round(segment_df$y[segment_k], digits=3), ":")
            # jamba::printDebug("segment_df:");print(segment_df);# debug
         }

         # default args
         avlf <- formals(assemble_venndir_label);
         default_fontcolors <- eval(avlf$fontcolors)
         default_fontsizes <- eval(avlf$fontsizes)
         default_fontfamilies <- eval(avlf$fontfamilies)
         # iterate label groups
         g_labels_list <- lapply(jamba::nameVectorN(gdf_list), function(igdfname){
            igdf <- gdf_list[[igdfname]];
            # optionally change to Markdown format
            # igdf$text <- gsub("<br>\n|<br>", "\n\n", igdf$text);
            # 0.0.47.900
            igdf$text <- gsub("<br>\n|<br>", "\n", igdf$text);
            # optionally change to textGrob format
            igdf$text <- gsub("<br>\n|<br>", "\n", igdf$text);
            # remove markdown bold
            igdf$text <- gsub("[*]+([^*]+)[*]+", "\\1", igdf$text);
            
            # get each label component
            use_fontcolors <- default_fontcolors;
            use_fontsizes <- default_fontsizes;
            use_fontfamilies <- default_fontfamilies;
            signed_df <- subset(igdf, type %in% "signed" & grepl("count", location));
            signed_labels <- signed_df$text;
            if (nrow(signed_df) > 0) {
               use_fontcolors$signed <- signed_df$label_col;
               use_fontsizes$signed <- signed_df$fontsize
            }
            count_df <- subset(igdf, type %in% "main" & grepl("count", location));
            count_labels <- count_df$text;
            if (nrow(count_df) > 0) {
               use_fontcolors$count <- count_df$label_col;
               count_labels <- unname(unlist(strsplit(count_labels, "\n")));
               use_count_fontsize <- rep(count_df$fontsize,
                  length.out=length(count_labels)) * c(1,
                     rep(0.7, length.out=length(count_labels) - 1));
               use_fontsizes$count <- use_count_fontsize;
            }
            overlap_df <- subset(igdf, type %in% "main" & grepl("overlap", location));
            overlap_labels <- overlap_df$text;
            if (nrow(overlap_df) > 0) {
               use_fontcolors$overlap <- overlap_df$label_col;
               use_fontsizes$overlap <- overlap_df$fontsize;
               overlap_labels <- unname(unlist(strsplit(overlap_labels, "\n")));
            }
            
            # do some checking for segment angle, then define just properly
            use_just <- c("center", "center")
            if (any(grepl(":outside:", igdfname))) {
               # determine segment angle, use ref_polygon for fully
               # internal sets that draw segment to internal overlap polygon
               igdfname_ref <- head(igdf$ref_polygon, 1);
               # pre-0.0.55.900
               # isegment_df <- unique(subset(segment_df,
               #    group %in% gsub(":.+", "", igdfname_ref)));
               # 0.0.55.900
               isegment_df <- unique(subset(segment_df,
                  match_group %in% gsub(":outside:", ":", igdfname)));
               if (nrow(isegment_df) == 2) {
                  line_degree <- jamba::rad2deg(
                     atan2(y=diff(isegment_df$y[1+c(1,0)]),
                        x=diff(isegment_df$x[1+c(1,0)])))
                  use_adj <- degrees_to_adj(line_degree, ...);
                  use_just <- as.vector(use_adj[1, ]);
                  if (FALSE) {
                     use_just <- as.character(use_adj);
                     use_just[1] <- ifelse(use_adj[1] == 0.5, "center",
                        ifelse(use_adj[1] == 1, "right", "left"))
                     use_just[2] <- ifelse(use_adj[2] == 0.5, "center",
                        ifelse(use_adj[2] == 1, "top", "bottom"))
                  }
               }
               
            }
            
            # assemble_venndir_label
            g_label <- assemble_venndir_label(
               x=grid::unit(adjx(head(igdf$x, 1)), "snpc"),
               y=grid::unit(adjy(head(igdf$y, 1)), "snpc"),
               overlap_labels=overlap_labels,
               count_labels=count_labels,
               signed_labels=signed_labels,
               # debug="overlap",
               just=use_just,
               fontfamily=fontfamily,
               fontcolors=use_fontcolors,
               fontsizes=use_fontsizes,
               frame_fill=head(igdf$box_fill, 1),
               frame_border=head(igdf$border_col, 1),
               template=template,
               marquee_styles=venndir_output@metadata[["marquee_styles"]],
               ...
            )
            if ("frameGrob" %in% debug) {
               jamba::printDebug("signed_labels: ", signed_labels);# debug
               jamba::printDebug("count_labels: ", count_labels);# debug
               jamba::printDebug("overlap_labels: ", overlap_labels);# debug
               stop("Stopping for debug.");# debug
            }
            g_label
         })
         # assemble into gTree label object
         g_labels_gTree <- grid::grobTree(
            vp=jp_viewport,
            do.call(grid::gList,
               g_labels_list),
            name="labels");
         # return grobs
         jp_grobList$labels <- g_labels_gTree;
      }
   }
   
   # segments
   if (length(show_segments) == 0) {
      if ("show_segments" %in% names(metadata)) {
         show_segments <- metadata$show_segments;
      }
      if (length(show_segments) == 0) {
         show_segments <- TRUE
      }
   }
   if (TRUE %in% show_segments && length(segment_df) > 0) {
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
      
      # make segment somewhat darker
      segment_wide$color <- jamba::makeColorDarker(segment_wide$color,
         darkFactor=1.2)
      
      # create segments grob
      segments_grob <- grid::segmentsGrob(
         x0=adjx(segment_wide$x0),
         x1=adjx(segment_wide$x1),
         y0=adjy(segment_wide$y0),
         y1=adjy(segment_wide$y1),
         default.units="snpc",
         gp=grid::gpar(col=segment_wide$color,
            lwd=ceiling(segment_wide$lwd)),
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

   # print warning when some overlap counts are hidden
   # todo: consider displaying message on the plot
   warn_df <- metadata(vo_new)$warn_df;
   if (length(warn_df) > 0 &&
         inherits(warn_df, "data.frame") &&
         nrow(warn_df) > 0) {
      nhid <- nrow(warn_df);
      if (TRUE %in% plot_warning) {
         jamba::printDebug("Warning: ",
            nhid, " overlap count",
            ifelse(nhid > 1, "s are", " is"),
            " hidden.");
      }
   }

   ########################################   
   # venndir legender
   if (TRUE %in% draw_legend) {
      ## Todo: Consider returning grobs from this function also
      ## 0.0.46.900 - try viewport using full device width
      # legend_vp <- grid::viewport(
      #    width=grid::unit(1, "npc"),
      #    height=grid::unit(1, "npc"),
      #    xscale=c(0, 1),
      #    yscale=c(0, 1));
      
      legend_grob <- venndir_legender(
         venndir_output=vo_new,
         x=legend_x,
         font_cex=legend_font_cex,
         fontfamily=fontfamily,
         draw_legend=FALSE,
         # vp=legend_vp,
         vp=jp_viewport,
         ...)
      jp_grobList$legend <- legend_grob;
   }

   ########################################   
   # venndir footnotes
   if (TRUE %in% draw_footnotes) {
      footnote_grob <- render_venndir_footnotes(
         venndir_output=vo_new,
         draw_footnote=FALSE,
         vp=jp_viewport,
         fontfamily=fontfamily,
         ...)
      if (length(footnote_grob) > 0) {
         jp_grobList$footnote <- footnote_grob;
      }
   }
   
   ############################################
   # Plot title
   if (length(main) > 0 && any(nchar(main) > 0)) {
      # main <- gsub("\n", "<br>", main);
      main <- gsub("<br>", "  \n", main);
      main <- jamba::cPaste(main, sep="  \n");
      nlines <- length(strsplit(main, "\n")[[1]])
      #
      if ("gridtext" %in% debug) {
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
      } else if (jamba::check_pkg_installed("marquee")) {
         main_style <- marquee::classic_style(
            base_size=16 * font_cex[1],
            body_font=fontfamily,
            # weight="bold",
            align="center"
         );
         # optional user-defined inline styles
         marquee_styles <- venndir_output@metadata[["marquee_styles"]];
         if (length(marquee_styles) > 0) {
            main_style <- combine_marquee_styles(
               mss=main_style,
               msl=marquee_styles,
               ...)
         }
         main_grob <- marquee::marquee_grob(
            text=main,
            ignore_html=TRUE,
            name="venndir_main",
            x=0.5,
            # width=NA, # necessary if border,fill are applied, also devoid
            y=grid::unit(1, "snpc") - grid::unit(0.75, "char"),
            default.units="snpc",
            style=main_style,
            vp=jp_viewport,
            hjust="center",
            vjust="top");
      }
      jp_grobList$main_title <- main_grob;
   }
   
   ############################################
   ## Assemble into gTree
   venndir_gtree <- grid::grobTree(
      do.call(grid::gList, jp_grobList),
      vp=jp_viewport,
      name="venndir_gTree")
   attr(vo_new, "gtree") <- venndir_gtree;
   attr(vo_new, "grob_list") <- jp_grobList;
   attr(vo_new, "viewport") <- jp_viewport;
   attr(vo_new, "adjx") <- adjx;
   attr(vo_new, "adjy") <- adjy;
   
   ## Draw the rest of the owl
   if (TRUE %in% do_draw) {
      if (TRUE %in% do_newpage) {
         grid::grid.newpage();
      }
      grid::grid.draw(venndir_gtree);
   }
   # return Venndir object
   return(invisible(vo_new));
}
