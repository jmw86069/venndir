
#' Directional Venn diagram
#' 
#' Directional Venn diagram
#' 
#' This function takes 'setlist' `list` as input, produces a `Venndir` object
#' and plots the data by default.
#' 
#' When the input 'setlist' is a `list` of `character` vectors, it will
#' produce basic Venn overlap counts.
#' 
#' When the input 'setlist' is a 'list' of `numeric` vectors, the vector
#' element names are used as items, and the values are considered the
#' directionality, or "sign". The overlaps are tabulated and delineated
#' by the 'overlap_type' requested:
#' 
#' * overlap_type="detect" - by default it will use "concordance" when
#' the input data contains directionality.
#' * overlap_type="concordance" - counts are organized as up/up or down/down,
#' or "discordant".
#' * overlap_type="each" - counts are organized by each combination of up/down
#' for each overlap.
#' * overlap_type="overlap" - counts are organized without using sign.
#' * overlap_type="agreement" - counts are organized by "agreement" (up/up, 
#' or down/down), or "disagreement" (up/down, down/up).
#' 
#' ## Label options
#' 
#' * The argument 'show_labels' is used to define which labels are displayed.
#' * Labels are enabled using a single letter, defined below.
#' 
#'    * UPPERCASE places the label outside.
#'    * lowercase places the label inside the Venn diagram.
#'    * Note that some labels cannot be placed outside (item labels).
#'    Similarly, when item labels are enabled, counts cannot be displayed
#'    inside, and must be outside or hidden.
#' 
#' * `N` - set _N_ame
#' * `c` - _c_ount for each overlap
#' * `s` - _s_igned count for each overlap
#' * `p` - _p_ercent total items represented in each overlap
#' * `i` - _i_tem labels for those items within each overlap
#' 
#' ## Item labels
#' 
#' * When item labels are enabled, the placement is defined by
#' `label_fill_JamPolygon()`, which uses an offset method, essentially
#' filling rows of labels left-to-right, alternating higher/lower
#' across each row.
#' * Items are sorted by sign if present, then by label.
#' 
#'    * To control the order that the signs are sorted, see
#'    `curate_venn_labels()` and argument `curate_df` to define
#'    custom order for each sign.
#'    * For `venn_meme()` item labels, they are displayed in the same order
#'    as provided.
#' 
#' * Item label font size is adjusted by default for each overlap polygon,
#' proportional to the available area relative to the total Venn area.
#' * Item label font sizes can be customized using `item_cex`.
#' 
#'    * A single value will be applied to the auto-scaling font sizes,
#'    adjusting all fonts consistently.
#'    * Multiple values will be recycled across the total number of overlap
#'    regions, applying font size to each region as drawn in order.
#' 
#' * Items can be rendered using `marquee::marquee_grob()` (default) or
#' `grid::grid.text()`.
#' 
#'    * The default marquee interprets items as markdown, which enables
#'    text styling, line wrap, and potentially embedded images. Mostly,
#'    marquee offers the best support for Unicode arrows using whichever
#'    font is requested.  
#'    Note that items use commonmark syntax, so to force line wrap, one
#'    must end a line with two spaces, then newline, for example:
#'    `"one line[space][space]\nsecond line"`
#'    * The potential benefit of `grid::grid.text()` is speed, and that
#'    it displays items exactly as provided with no markdown interpretation.
#'    This function will not display all Unicode characters for all fonts,
#'    due to inconsistencies in how R fonts are supported.
#'    (It's a long history.)
#' 
#' @inheritParams signed_overlaps
#' @param sets `integer` index with optional subset of sets in `setlist`
#'    for the Venn diagram.
#'    This option is useful when defining consistent `set_colors` for
#'    all entries in `setlist`.
#' @param set_colors `character` vector of R colors, or default `NULL` to
#'    use categorical colors defined by `colorjam::rainbowJam()`.
#'    It will generate colors for every element in `setlist` even when
#'    a subset is defined with `sets`.
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
#' @param draw_legend `logical` passed to `render_venndir()`, and stored
#'    in the `Venndir` metadata.
#' @param legend_font_cex `numeric` scalar, default 1, used to adjust
#'    the relative size of fonts with `venndir_legender()` when
#'    `draw_legend=TRUE`. This value is stored in `metadata` for persistence.
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
#' @param main `character` string used as a plot title, default NULL
#'    will render no title. When provided, it is rendered using
#'    `gridtext::richtext_grob()` which enables some Markdown-style
#'    formatting. The title is stored in `venndir@metadata$main`
#'    for persistence.
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
#'    The default `c(1, 1, 0.7)` defines the signed count label slightly
#'    smaller than other labels.
#'    * When one value is provided, it is multiplied by `c(1, 1, 0.7)` to
#'    adjust font sizes altogether, keeping relative sizes.
#'    * When two values are provided, they are multiplied by `c(1, 1, 0.7)`
#'    using the second value twice.
#'    * When three values are provided, they are used as-is without change.
#' @param fontfamily `character` string to define the fontfamily.
#'    Default is "sans" because it should get mapped to a supported font
#'    for each graphics device.
#'    The `fontfamily` must match a recognized font for the given output
#'    device, and this font must be capable of producing UTF-8 / Unicode
#'    characters, in order to print up arrow and down arrow.
#'    You may review `systemfonts::system_fonts()` for a listing of fonts
#'    recognized by `ragg` devices, which seems to have the best
#'    overall font capabilities.
#'    When it does not work, either use `unicode=FALSE`, or check the
#'    output from `Sys.getlocale()` to ensure the setting is capable
#'    of using UTF-8 (for example "C" may not be sufficient).
#'    Using the package `ragg` appears to be more consistently successful
#'    for rasterized output than base R output, for example:
#'    `ragg::agg_png()`, `ragg::agg_tiff()`, `ragg::agg_jpeg()`
#'    produce substantially higher quality output, and with more successful
#'    usage of system fonts, than `png()`, `tiff()`, and `jpeg()`.
#'    Similarly, for PDF output, consider `cairo_pdf()` or
#'    `Cairo::CairoPDF()` instead of using `pdf()`.
#' @param poly_alpha `numeric` (default 0.6) value between 0 and 1, for
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
#' @param template `character` (default "wide") describing the default
#'    layout for counts and signed counts. The value is stored in
#'    `venndir@metadata$template` for persistence.
#'    * `"wide"` - main counts on the left, right-justified; signed counts
#'    on the right, left-justified.
#'    This option is preferred for small numbers, and less-crowded diagrams.
#'    * `"tall"` - main counts, center-justified; signed counts below main
#'    counts, center-justified.
#'    This option is recommended for large numbers (where there are 1000
#'    or more items in a single overlap region), or for crowded diagrams.
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
#' @param item_cex `numeric` default 1, used to define baseline font size
#'    (single value), or exact font `cex` values (multiple values).
#'    * When a single value is provided, each set of items is used to
#'    define a font scaling, based upon the relative area of the
#'    overlap polygon to the max item polygon area, and the number of
#'    items in each polygon. These values are multiplied by `item_cex`
#'    to produce the final adjustment.
#'    These values are multiplied by `item_cex_factor`.
#'    * When multiple values are provided, they are recycled to the
#'    number of polygons that contain items, and applied in order.
#'    There is no further adjustment by polygon area, nor number of labels.
#'    These values are multiplied by `item_cex_factor`.
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
#' @param center `numeric` coordinates relative to the plot bounding box,
#'    default `c(0, 0)` uses a center point in the middle (x=0)
#'    and slightly down (y=-0.15) from the plot center.
#'    It is used to place labels outside the diagram.
#'    In short, labels are placed by drawing a line from this center point,
#'    outward through the Venn overlap region to be labeled. The
#'    label is positioned outside the polygon region by `segment_distance`.
#'    The default `c(0, -0.15)` ensures that labels tend to be at the
#'    top of the plot, and not on the left/right side of the plot.
#'    This argument is passed along to `label_outside_JamPolygon()`.
#' @param segment_distance `numeric` value, default 0.05, the distance
#'    between outside labels and the outer edge of the Venn diaram region,
#'    relative to the size of the Venn polygons.
#'    The default `0.05` is approximately a 5% buffer outside.
#'    Note that when labels are placed outside (using `show_labels`)
#'    the outside label coordinates are used to define the plot
#'    range, which causes the Venn diagram itself to shrink accordingly.
#' @param do_plot `logical (default TRUE) indicating whether to generate the
#'    the figure.
#'    * When `do_plot=TRUE` it calls `render_venndir()` to create `grid`
#'    objects to be displayed. Arguments in `...` are passed to
#'    `render_venndir()`: To hide display, use `do_draw=FALSE`.
#'    To prevent calling `grid::grid.newpage()` so the plot can
#'    be drawn inside another active display device, use `do_newpage=FALSE`.
#'    * When `do_plot=FALSE` the returned `Venndir` object can be passed to
#'    `render_venndir()` to render the figure. Same points are valid
#'    regarding `do_draw` and `do_newpage`, which are arguments
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
#'    When `do_plot=TRUE` this function also calls `render_venndir()`,
#'    and returns the `grid` graphical objects (grobs) in the attributes:
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
 draw_legend=TRUE,
 legend_signed=NULL,
 legend_font_cex=1,
 proportional=FALSE,
 show_labels="Ncs",
 main=NULL,
 return_items=TRUE,
 show_items=c(NA,
    "none",
    "sign item",
    "sign",
    "item"),
 max_items=3000,
 show_zero=FALSE,
 font_cex=c(1, 1, 0.7),
 fontfamily="Arial",
 # show_set=c("main", "all", "none"),
 show_label=NA,
 display_counts=TRUE,
 poly_alpha=0.6,
 alpha_by_counts=FALSE,
 label_style=c("basic",
    "fill",
    "shaded",
    "shaded_box",
    "lite",
    "lite_box"),
 label_preset="none",
 template=c("wide",
    "tall"),
 unicode=TRUE,
 big.mark=",",
 curate_df=NULL,
 venn_jp=NULL,
 inside_percent_threshold=0,
 item_cex=1,
 item_style=c("default",
    "marquee",
    "text",
    "gridtext"),
 item_buffer=-0.15,
 item_degrees=0,
 sign_count_delim=" ",
 padding=c(3, 2),
 r=2,
 center=c(0, 0),
 segment_distance=0.05,
 segment_buffer=-0.1,
 show_segments=TRUE,
 sep="&",
 do_plot=TRUE,
 verbose=FALSE,
 debug=0,
 circle_nudge=NULL,
 lwd=0.3,
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
   template <- match.arg(template);
   
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
      font_cex <- c(1, 1, 0.7);
   }
   if (length(font_cex) > 3) {
      font_cex <- head(font_cex, 3);
   }
   if (length(font_cex) == 1) {
      font_cex <- c(1, 1, 0.7) * font_cex[1];
   }
   if (length(font_cex) == 2) {
      font_cex <- c(1, 1, 0.7) * font_cex[c(1, 2, 2)];
   }
   # global adjustment to the default condition
   font_cex <- font_cex * 1.2;

   # accept incidence matrix input
   if (inherits(setlist, "matrix")) {
      setlist <- im_value2list(setlist);
   }
   
   # Validate names(setlist)
   if (length(names(setlist)) == 0) {
      names(setlist) <- as.character(seq_along(setlist));
   }
   # check for NA or duplicated names(setlist)
   if (any(is.na(names(setlist))) || any(duplicated(names(setlist)))) {
      names(setlist) <- jamba::makeNames(
         jamba::rmNA(names(setlist),
            naValue="set"));
      if (any(duplicated(names(setlist)))) {
         names(setlist) <- jamba::makeNames(names(setlist),
            renameFirst=FALSE);
      }
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
   
   # 0.0.38.900 - ensure internal names do not contain "bad characters"
   # such as ":"
   if (any(grepl(":", names(setlist)))) {
      names(setlist) <- gsub(":", ".", names(setlist));
      names(setlist_labels) <- names(setlist);
      names(legend_labels) <- names(setlist);
   }
   
   
   # define colors
   if (length(set_colors) == 0) {
      set_colors <- colorjam::rainbowJam(length(setlist),
         ...);
      names(set_colors) <- names(setlist);
   } else if (length(set_colors) == length(setlist)) {
      if (!all(names(setlist) %in% names(set_colors))) {
         names(set_colors) <- names(setlist);
      }
   } else if (length(set_colors) == length(sets)) {
      names(set_colors) <- names(setlist)[sets];
   } else {
      set_colors <- rep(set_colors,
         length.out=length(setlist));
      names(set_colors) <- names(setlist);
   }

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
   }
      
   # Assign other attributes for consistency later on
   venn_jp@polygons$venn_name <- names(venn_jp);
   venn_jp@polygons$venn_counts <- NA;
   venn_jp@polygons$venn_items <- I(lapply(seq_len(length(venn_jp)),
      function(xi) character(0)));
   venn_jp@polygons$venn_color <- set_colors[venn_jp@polygons$venn_name];
   border_dark_factor <- 1.1;
   
   venn_jp@polygons$outerborder <- jamba::alpha2col(
      alpha=poly_alpha,
      jamba::makeColorDarker(
         darkFactor=border_dark_factor,
         set_colors[venn_jp@polygons$venn_name]));
   venn_jp@polygons$outerborder.lwd <- lwd;
   
   venn_jp@polygons$border <- jamba::alpha2col(
      alpha=poly_alpha,
      jamba::makeColorDarker(
         darkFactor=border_dark_factor,
         set_colors[venn_jp@polygons$venn_name]));
   venn_jp@polygons$border.lwd <- lwd/2;
   
   venn_jp@polygons$innerborder <- NA;
   venn_jp@polygons$innerborder.lwd <- 0;
   
   venn_jp@polygons$fill <- NA;
   venn_jp@polygons$label <- venn_jp@polygons$venn_name;
   venn_jp@polygons$label_x <- NA;
   venn_jp@polygons$label_y <- NA;
   venn_jp@polygons$type <- "set";
   

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
      # venn_colors=set_colors[names(venn_jp)],
      venn_colors=jamba::alpha2col(set_colors[names(venn_jp)], alpha=poly_alpha),
      sep=sep,
      ...);
   # rownames(venn_jpol@polygons) <- names(venn_jpol);
   venn_jp@polygons$type <- "set";
   # update JamPolygon names to indicate full set
   names(venn_jp) <- paste0(names(venn_jp), "|set");
   rownames(venn_jp@polygons) <- names(venn_jp);

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
   
   ## Determine which polygon should be used when drawing each label
   #
   ## Previous logic below, only matched identical set and not subsets
   # whichset1 <- (length(venn_jps@polygons$label) + 1) -
   #    match(unique(venn_jps@polygons$label),
   #       rev(venn_jps@polygons$label));
   #
   ## Updated logic prioritizes non-empty polygon, with fewest overlaps
   venn_jps@polygons$is_empty <- sapply(venn_jps@polygons$x, function(ix1){
      length(unique(jamba::rmNA(ix1))) == 0
   }) * 1;
   whichset <- sapply(unique(venn_jps@polygons$label), function(ilab){
      ilabdf <- subset(venn_jps@polygons,
         # non-overlap single set can point to overlap if necessary
         (!grepl("&", ilab) & grepl(paste0("(^|&)", ilab, "($|&)"), label)) |
         # however a multi-set overlap must only point to itself
         ilab == name)
      if (nrow(ilabdf) == 0) {
         return(NA)
      }
      ilabdf$num_sets <- lengths(strsplit(ilabdf$venn_name, "&"));
      ilabdf <- jamba::mixedSortDF(ilabdf,
         byCols=c("is_empty", "type", "num_sets"));
      ilabdf <- subset(ilabdf, is_empty %in% 0);
      if (nrow(ilabdf) == 0) {
         return(NA)
      }
      head(match(rownames(ilabdf), rownames(venn_jps@polygons)), 1)
   })
   ## For convenience downstream, store the preferred reference polygon
   ## for each row, otherwise we have to calculate it again and want
   ## the result to be consistent.
   matchwhichset <- match(venn_jps@polygons$label, names(whichset));
   venn_jps@polygons$ref_polygon_num <- whichset[matchwhichset];
   venn_jps@polygons$ref_polygon <- rownames(venn_jps@polygons)[
      venn_jps@polygons$ref_polygon_num];
   
   ## Manually enforce type="overlap" must be the same ref_polygon
   # - this approach was incorrect, it must be resolved in render_venndir()
   # overlap_rows <- (venn_jps@polygons$type %in% "overlap")
   # venn_jps@polygons$ref_polygon[overlap_rows] <- (
   #    venn_jps@polygons$venn_name)[overlap_rows]
   
   use_whichset <- whichset[!is.na(whichset)];
   # jamba::printDebug("whichset:");print(whichset);# debug
   # jamba::printDebug("use_whichset:");print(use_whichset);# debug
   # stop("stopping here");
   
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
      # ploxy is named by names(get_outside_labels)
      # contains outer label position, inner buffer line segment position
      get_outside_labels1 <- which(lengths(venn_jps@polygons$x) > 0);
      get_outside_labels <- (match(venn_jps@polygons$ref_polygon,
         names(venn_jps)))[get_outside_labels1];
      names(get_outside_labels) <- rownames(venn_jps@polygons)[
         get_outside_labels1];
      # jamba::printDebug("get_outside_labels:");print(get_outside_labels);# debug
      
      # special case correction for center
      if (length(sets) == 2 &&
            # any(c(TRUE, FALSE) %in% proportional) &&
            all(center == 0)) {
         center <- c(0, -0.2);
      }
      
      use_jps <- venn_jps[get_outside_labels];
      names(use_jps) <- names(get_outside_labels);
      ploxy <- label_outside_JamPolygon(
         # jp=venn_jps[get_outside_labels],
         jp=use_jps,
         distance=segment_distance,
         center=center,
         # center_method="bbox",
         verbose=verbose,
         # do_plot=TRUE,
         # buffer=-0.6,
         ...)
      # jamba::printDebug("names(ploxy):");print(names(ploxy));# debug
      # jamba::printDebug("(ploxy)[c(1, 3, 5)]:");print((ploxy)[c(1, 3, 5)]);# debug
      kc <- c("name", "venn_name", "label",
         "label_x", "label_y", "x_label", "y_label",
         "x_outside", "y_outside",
         "is_empty", "ref_polygon_num", "ref_polygon")
      kc1 <- intersect(kc, colnames(venn_jps@polygons))
      # jamba::printDebug("venn_jps@polygons[, kc1]:");print(venn_jps@polygons[, kc1]);# debug
      # names(ploxy) <- names(use_whichset);
      if (verbose) {
         jamba::printDebug("venndir(): ",
            "completed label_outside_JamPolygon()");
      }
      # Reminder:
      # label_x,label_y is used by JamPolygon as internal label position
      # x_label,y_label is not used? it is expected in venndir_label_style()
      # outside_x,outside_y is the outside label actual coordinate
      # x_offset,y_offset is the offset from label_x,label_y for outside labels
      #
      # could probably add
      # segment_x,segment_y to prevent having to re-calculate the line segment?
      venn_jps@polygons$x_offset <- NA_integer_;
      venn_jps@polygons$y_offset <- NA_integer_;
      
      # define outside position
      # ploxy_match <- match(venn_jps@polygons$label, names(ploxy));
      ploxy_outside_x <- sapply(ploxy, function(ixy){ixy["label", "x"]});
      ploxy_outside_y <- sapply(ploxy, function(ixy){ixy["label", "y"]});
      ploxy_segment_x <- sapply(ploxy, function(ixy){ixy["border", "x"]});
      ploxy_segment_y <- sapply(ploxy, function(ixy){ixy["border", "y"]});
      match_outside <- match(names(ploxy), names(venn_jps));
      # jamba::printDebug("venn_jps@polygons:");print(venn_jps@polygons);# debug
      # jamba::printDebug("names(venn_jps):");print(names(venn_jps));# debug
      # jamba::printDebug("match_outside:");print(match_outside);# debug

      if (!"x_outside" %in% colnames(venn_jps@polygons)) {
         venn_jps@polygons$x_outside <- NA_integer_;
      }
      if (!"y_outside" %in% colnames(venn_jps@polygons)) {
         venn_jps@polygons$y_outside <- NA_integer_;
      }
      venn_jps@polygons$x_outside[match_outside] <- ploxy_outside_x;
      venn_jps@polygons$y_outside[match_outside] <- ploxy_outside_y;
      
      if (!"segment_x" %in% colnames(venn_jps@polygons)) {
         venn_jps@polygons$segment_x <- NA_integer_;
      }
      if (!"segment_y" %in% colnames(venn_jps@polygons)) {
         venn_jps@polygons$segment_y <- NA_integer_;
      }
      venn_jps@polygons$segment_x[match_outside] <- ploxy_segment_x;
      venn_jps@polygons$segment_y[match_outside] <- ploxy_segment_y;
      
      if (!"x_label" %in% colnames(venn_jps@polygons)) {
         venn_jps@polygons$x_label <- NA_integer_;
      }
      if (!"y_label" %in% colnames(venn_jps@polygons)) {
         venn_jps@polygons$y_label <- NA_integer_;
      }
      venn_jps@polygons$x_label[match_outside] <- ploxy_segment_x;
      venn_jps@polygons$y_label[match_outside] <- ploxy_segment_y;
      
      # re-define inside label position using ref_polygon
      ref_match <- match(venn_jps@polygons$ref_polygon, venn_jps@polygons$name)
      # jamba::printDebug("ref_match:");print(ref_match);# debug
      venn_jps@polygons$label_x <- venn_jps@polygons$label_x[ref_match];
      venn_jps@polygons$label_y <- venn_jps@polygons$label_y[ref_match];

      # calculate offset by (outside-inside)
      venn_jps@polygons$x_offset <- venn_jps@polygons$x_outside - venn_jps@polygons$label_x;
      venn_jps@polygons$y_offset <- venn_jps@polygons$y_outside - venn_jps@polygons$label_y;
      
      # I don't think we need any of this junk
      if (FALSE) {
         ploxy_match <- match(venn_jps@polygons$ref_polygon, names(ploxy));
         # jamba::printDebug("whichset:");print(whichset);# debug
         # jamba::printDebug("ploxy:");print(ploxy);# debug
         # jamba::printDebug("venn_jps@polygons:");print(venn_jps@polygons);# debug
         # jamba::printDebug("ploxy_match:");print(ploxy_match);# debug
         
         ploxy_label_x <- sapply(ploxy, function(ixy){ixy["border", "x"]});
         ploxy_label_y <- sapply(ploxy, function(ixy){ixy["border", "y"]});
         # jamba::printDebug("ploxy_label_x:");print(ploxy_label_x);# debug
         # jamba::printDebug("ploxy_outside_x:", ploxy_outside_x);# debug
         
         # define other label coordinates
         #
         venn_jps@polygons$x_label <- ploxy_label_x[venn_jps@polygons$ref_polygon];
         venn_jps@polygons$y_label <- ploxy_label_y[venn_jps@polygons$ref_polygon];
         # venn_jps@polygons$x_label <- ploxy_label_x[ploxy_match];
         # venn_jps@polygons$y_label <- ploxy_label_y[ploxy_match];
         
         #
         venn_jps@polygons$x_outside <- ploxy_outside_x[ploxy_match];
         venn_jps@polygons$y_outside <- ploxy_outside_y[ploxy_match];
         # jamba::printDebug("venn_jps@polygons[, kc]:");print(venn_jps@polygons[, kc]);# debug
         venn_jps@polygons$x_offset <- venn_jps@polygons$x_outside - venn_jps@polygons$x_label;
         venn_jps@polygons$y_offset <- venn_jps@polygons$y_outside - venn_jps@polygons$y_label;
      }
      
      # define label positioning relative to the coordinate point
      ## 0.0.47.900 - vjust/hjust no longer used
      # venn_jps@polygons$vjust <- 0.5;
      # venn_jps@polygons$hjust <- 0.5;
      # venn_jps@polygons$vjust[jamba::rmNA(whichset)] <- sapply(ploxy, function(ixy){
      #    ixy["label", "adjx"]
      # });
      # venn_jps@polygons$hjust[jamba::rmNA(whichset)] <- sapply(ploxy, function(ixy){
      #    ixy["label", "adjy"]
      # });
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
   # main_vjust <- rep(0.5, length(main_x));
   # main_halign <- rep(0.5, length(main_x));
   # main_hjust <- rep(1, length(main_x));
   # if ("overlap" %in% overlap_type) {
   #    main_hjust <- rep(0.5, length(main_x));
   # }
   
   ## Labels for each overlap
   # - using gCounts from signed_overlaps() above
   if (length(curate_df) == 0) {
      curate_df <- get_venndir_curate_df(unicode=unicode,
         ...)
   }
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
   if (length(sign_count_delim) == 0) {
      sign_count_delim <- "";
   }
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
   venn_jps@polygons$innerborder.lwd <- lwd;
   # venn_jps@polygons$innerborder.lty <- 1;
   border_dark_factor <- 1.1;
   border_s_factor <- 1.2;
   venn_jps@polygons$innerborder <- ifelse(vset,
      jamba::alpha2col(alpha=1,
         jamba::makeColorDarker(
            jamba::unalpha(venn_jps@polygons$venn_color),
            darkFactor=border_dark_factor,
            sFactor=border_s_factor)),
      NA);

   # define outer border
   venn_jps@polygons$border.lwd <- lwd/2;
   # option to re-use outer border as border
   # venn_jps@polygons$border <- ifelse(vset,
   #    NA,
   #    venn_jps@polygons$outerborder);
   venn_jps@polygons$border <- NA;
   # venn_jps@polygons$border.lty <- 3;
   venn_jps@polygons$outerborder.lwd <- lwd;
   venn_jps@polygons$outerborder <- ifelse(vset,
      NA,
      jamba::alpha2col(alpha=1,
         jamba::makeColorDarker(
            jamba::unalpha(venn_jps@polygons$venn_color),
            darkFactor=border_dark_factor,
            sFactor=border_s_factor)))
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
      # vjust=c(main_vjust, signed_vjust),
      # hjust=c(main_hjust, signed_hjust),
      # halign=c(main_halign, signed_halign),
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
         c(padding[1] * font_cex[2], padding[2] * font_cex[3] * 1),
         c(length(main_x), length(signed_x))),
      r_unit=rep("pt", label_n),
      stringsAsFactors=FALSE,
      check.names=FALSE
   );
   # jamba::printDebug("venndir() label_df:");print(label_df);# debug
   # stop("Stopping here");# debug
   
   if ("overlap" %in% overlap_type) {
      sv_match <- match(label_df$overlap_set, sv$sets);
      label_df$overlap_sign <- paste0(sv$sets, "|", sv$overlap)[sv_match];
   } else {
      label_df$overlap_sign <- c(rep("", length(main_x)),
         names(gbase_signs));
   }

   # 0.0.34.900 -  experiment by adding poly_ref_name
   matchjps <- match(label_df$overlap_set, rownames(venn_jps@polygons));
   label_df$ref_polygon <- venn_jps@polygons$ref_polygon[matchjps];

   ## Apply setlist_labels, legend_labels
   matchset <- match(label_df$overlap_set, venn_jps@polygons$venn_name)
   label_df$venn_label <- venn_jps@polygons$venn_label[matchset];
   # label_df$legend_label <- venn_jps@polygons$legend_label[matchset];
   
   ## Check for missing signed labels, then we nudge main label to center
   #
   # Todo: verify this works properly when all rows are always present
   #
   ## 0.0.47.900 - no longer used
   # for (i in unique(label_df$overlap_set)) {
   #    irows <- (label_df$overlap_set %in% i & 
   #          label_df$show_label %in% c(NA, TRUE));
   #    if (length(unique(label_df[irows,"type"])) == 1) {
   #       label_df[irows,"hjust"] <- 0.5;
   #    }
   # }
   
   ## Optionally return items
   if (TRUE %in% return_items) {
      sv_label <- paste0(sv$sets, "|", sv[[overlap_type]]);
      sv_match <- match(label_df$overlap_sign, sv_label);
      label_df$items <- I(sv$items[sv_match]);
   }

   ## Create metadata options
   if (length(fontfamily) == 0) {
      fontfamily <- "";
   }
   metadata <- list(
      main=main,
      overlap_type=overlap_type,
      show_items=show_items,
      template=template,
      show_segments=show_segments,
      fontfamily=fontfamily,
      draw_legend=draw_legend,
      legend_signed=legend_signed,
      legend_font_cex=legend_font_cex,
      sign_count_delim=sign_count_delim,
      item_buffer=item_buffer,
      item_cex=item_cex,
      item_style=item_style,
      item_degrees=item_degrees,
      unicode=unicode,
      curate_df=curate_df,
      segment_buffer=segment_buffer)
   
   # Create Venndir object
   vo <- new("Venndir",
      jps=venn_jps,
      label_df=label_df,
      setlist=setlist,
      metadata=metadata)

   # Add warning_list to vo
   vo <- get_venndir_label_warning_list(vo);
   # Consider printing the warning here
   if (FALSE) {
      warn_df <- metadata(vo)$warn_df;
      if (length(warn_df) > 0 &&
            inherits(warn_df, "data.frame") &&
            nrow(warn_df) > 0) {
         jamba::printDebug("warn_df:");
         print(warn_df);# debug
      }
   }

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
            metadata(vo)$show_items <- show_items;
            # jamba::printDebug("venndir(): ", "show_items:", show_items);# debug
         }
      }
      
      ## Update Venndir object in place
      # jamba::printDebug("venndir() before venndir_label_style() vo@label_df:");print(vo@label_df);# debug
      vo <- venndir_label_style(
         venndir_output=vo,
         show_labels=show_labels,
         show_items=metadata(vo)$show_items,
         label_preset=label_preset,
         label_style=label_style,
         show_zero=show_zero,
         max_items=max_items,
         inside_percent_threshold=inside_percent_threshold,
         verbose=verbose,
         ...);
      # jamba::printDebug("venndir() after venndir_label_style() vo@label_df:");print(vo@label_df);# debug
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
         # item_cex=item_cex,
         # item_style=item_style,
         # item_buffer=item_buffer,
         max_items=max_items,
         inside_percent_threshold=inside_percent_threshold,
         ...);
      #label_df <- gg@label_df;
      # retlist$rv_label_df <- gg@label_df;
      # retlist$gg <- gg;
      if ("gtree" %in% names(attributes(gg))) {
         attr(vo, "gtree") <- attr(gg, "gtree")
      }
      if ("grob_list" %in% names(attributes(gg))) {
         attr(vo, "grob_list") <- attr(gg, "grob_list")
      }
      if ("viewport" %in% names(attributes(gg))) {
         attr(vo, "viewport") <- attr(gg, "viewport")
      }
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

#' Internal function to define warning label for missing Venn overlaps
#' 
#' Internal function to define warning label for missing Venn overlaps
#' 
#' @returns `data.frame` when input is `data.frame`, or `Venndir` with
#'    metadata entry `"warn_df"` with input `Venndir`.
#'    The `data.frame` has columns 'overlap_set' and 'venn_counts',
#'    for each Venn overlap that is not displayed, and the Venn count
#'    for each overlap.
#' 
#' @noRd
get_venndir_label_warning_list <- function
(label_df,
 ...)
{
   #
   # warn about hidden non-zero labels
   # - venn_counts are not zero; label_type is "main" AND EITHER:
   #    - x,y are NA
   #    OR
   #    - overlap_set != ref_polygon;
   vo <- NULL;
   if (inherits(label_df, "Venndir")) {
      vo <- label_df;
      label_df <- vo@label_df;
   }
   
   warn_rows <- (
      (
         (label_df$x %in% NA | label_df$y %in% NA) |
         (!label_df$overlap_set == label_df$ref_polygon)
      ) &
         !label_df$venn_counts %in% c(NA, 0) &
         label_df$type %in% "main");

   #
   warn_df <- data.frame(overlap_set="A", venn_counts=0)[0, , drop=FALSE];
   warning_text <- NULL;
   warning_label <- NULL;
   if (any(warn_rows)) {
      warn_df <- data.frame(check.names=FALSE,
         overlap_set=label_df$overlap_set[warn_rows],
         venn_counts=label_df$venn_counts[warn_rows])
   }
   if (length(vo) > 0) {
      metadata(vo)$warn_df <- warn_df;
      return(vo);
   }
   return(warn_df);
}

