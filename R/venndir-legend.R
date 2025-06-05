
#' Add legend to venndir figure
#' 
#' Add legend to venndir figure
#' 
#' Note this function is experimental and is under active development. The
#' implementation and arguments may change in future.
#'  
#' Limitations: Currently this function relies upon having the `setlist`
#' used to produce the `venndir()` output, and the `venndir()` output.
#' In future, the `setlist` could be derived from the `venndir()` output
#' object directly. That step likely needs a new function.
#' 
#' When using arguments `legend_style="grid"` and `draw_legend=FALSE` the
#' `grid` `grob` object is returned, so that it can be manipulated
#' as needed before rendering. Note that in this case, the viewport
#' will have already been defined and stored into `legend_grob$vp`
#' with x position `legend_grob$vp$x` and y position `legend_grob$vp$y`.
#' Total legend width is: `sum(legend_grob$widths)`, and
#' total legend height is: `sum(legend_grob$heights)`.
#' 
#' Todo:
#' 
#' * Consider bottom-justifying text in each cell, left-justifying text
#' labels, and right-justifying numeric values.
#' 
#' @family venndir core
#' 
#' @param venndir_output `Venndir` object, default NULL, or one of:
#'    * `Venndir` object, also used to define `set_colors` when applicable
#'    * `list` representing `setlist`
#'    * `data.frame` output from `signed_overlaps()`
#' @param setlist `list`, default NULL, used when `venndir_output` is NULL
#'    * `list` representing `setlist`
#'    * `data.frame` with output from `signed_overlaps()`, with columns
#'    `"sets"`, `"overlap"`, `"count"`. It may include `"each"` which is
#'    required to display signed counts with `legend_signed=TRUE`.
#' @param x `character` string indicating the position of the legend,
#'    as passed to `graphics::legend()` when `legend_style="base"`.
#'    Not relevant when `legend_style="data.frame"`.
#' @param set_colors `character` optional vector of R colors, whose names
#'    should match `names(setlist)`. When not supplied, colors are inferred
#'    from `venndir_output`, and when that is not supplied, colors are
#'    defined using `colorjam::rainbowJam()`.
#' @param legend_style `character` string, default `"grid"` with the
#'    legend style, with these options:
#'    * `"grid"` generates a grid `grob` using `gridExtra::grid.table()`.
#'    * `"data.frame"` generates the legend `data.frame`.
#' @param unicode `logical` default TRUE, whether to use unicode arrows
#'    for signed counts, used when `legend_signed=TRUE`.
#'    This argument is passed to `curate_venn_labels()`.
#' @param legend_headers `character` vector with headers to use,
#'    named by the default heading. To display no column header,
#'    set `legend_headers=NULL` or `legend_headers=FALSE`.
#'    Recognized names:
#'    * "Set" - the header for the set names, default "Set".
#'    * "Label" - when `labels` or `legend_labels` are defined, the
#'    header "Label" can be customized.
#'    * "Size" - the header for the set sizes, default "Size".
#'    * "Sign" - the header for the set signs, default "Sign".
#'    * "Percentage" - the header for the set size percentage overall,
#'    default "Percentage".
#' @param keep_newlines `logical` indicating whether to keep newlines
#'    (line breaks) in the set labels used in the Venn legend.
#' @param legend_total `logical` default TRUE, whether to include the
#'    total unique items in the legend.
#'    When `FALSE` it also sets `legend_percentage=FALSE`.
#' @param legend_percentage `logical` default NULL, whether to include
#'    the percentage of total items represented in each set.
#'    This option is only used when `legend_total=TRUE`.
#'    * When set to `NULL` it will display percentage only when present
#'    as a text label, as when `show_labels` include `"p"` or `"P"`.
#' @param legend_signed `logical` default NULL, whether to include
#'    signed counts in the legend.
#'    * When set to `NULL` it displays signed counts only when available,
#'    which requires: `overlap_type` is not "overlap", and
#'    `legend_df` column "type" has entries with "sign".
#' @param combine_signed `logical` default TRUE, whether to combine multiple
#'    signed counts into one column (TRUE), or keep separate columns
#'    (FALSE). Somewhat experimental.
#'    This option is only used when signed counts are included.
#' @param combine_size `logical` default TRUE, whether to combine
#'    counts in the "Size" column with signed counts.
#'    When `TRUE`, this option also left-aligns the "Size" column.
#' @param legend_color_style `character` string to customize how colors are
#'    used, `c("fill", "border")` are default:
#'    * `"fill"`: will use the Venn fill color
#'    * `"border"`: will use the Venn border color
#'    * `"nofill"`: will remove the Venn fill color
#'    * `"noborder"`: will remove the Venn border color
#'    * `"greyfill"`: will use `"F4F4f4"` grey fill color, matching the Total.
#'    * `"greyborder"`: will use `"#999999"` border color, matching the Total.
#'    * `"blackborder"`: will use `"#000000"` border color, matching the Total.
#' @param box.lwd `numeric` used to define the box line width,
#'    as passed to `graphics::legend()` when `legend_style="base"`.
#' @param item_type `character` string used as a suffix to the counts
#'    displayed in the legend. Use `item_type=""` or `item_type=NULL`
#'    to hide this suffix.
#' @param header_color,header_bg,header_border `character` R colors
#'    to define colors for particular elements:
#'    * `header_color` defines the text color of the column headers.
#'    To hide the column header, currently use `header_color="white"`,
#'    matching whatever color is used for `header_bg`.
#'    * `header_bg` defines the background fill color for the column headers.
#'    * `header_border` defines the border color for the header, which
#'    is also applied to all cells in the table. Note that the line is
#'    drawn with line width `lwd`.
#' @param lwd `numeric`, default 1, cell border line width used
#'    in the legend table, when `legend_style="grid"`.
#' @param x_inset,y_inset `grid::unit`, default 2 lines each, used
#'    when `legend_style="grid"` to position the legend relative to `x`,
#'    useful when positioning the legend near the edge `x="bottomleft"`.
#'    The inset controls distance from the plot edge, along the x- and y-axes,
#'    respectively. For example `x_inset=grid::unit(2, "lines")` will
#'    place the legend table 2 character lines (which are defined by
#'    line height for typical character size) away from the left or
#'    right edge of the plot. Any valid `grid::unit` can be used.
#'    Note that the metric is "plot edge", and the plot is fixed 1:1 aspect,
#'    so for wide plot devices a negative `x_inset` may be used to nudge
#'    the legend outside the typical plot boundary.
#' @param font_cex `numeric` adjustment to default font sizes. The default
#'    font size with `legend_style="grid"` uses a 12 point font, so to adjust
#'    to a specific font size like 8 points, use: `font_cex=8/12`
#' @param fontfamily `character` used as the default font for all labels,
#'    and is used in `fontfamilies` by default. However `fontfamilies`
#'    takes priority when defined.
#' @param fontfamilies `list` with named elements
#'    to allow a custom font for each element of the legend, consistent
#'    with usage by `assemble_venndir_label()`.
#'    * `"overlap"`: used for set labels and column headers,
#'    and column headers are currently always bold font.
#'    * `"count"`: used for main counts in "Size", and percentage in
#'    "Percentage" when displayed.
#'    * `"signed"`: used for signed counts, when shown and when
#'    `combine_size=FALSE`, otherwise the signed labels are combined
#'    with count labels and they re-use the same font as `"count"` above.
#' @param table_theme `list` default NULL, optional theme as described in
#'    `gridExtra::tableGrob()`, and `gridExtra::ttheme_default()`.
#'    When supplied, the `font_cex` argument is ignored. 
#'    The `list` components include:
#'    * `base_size` - default font size
#'    * `base_colour` - default font color
#'    * `base_family` - default font family
#'    * `parse` - `logical` whether to parse plotmath expressions
#'    * `padding` - `grid::unit()` for horizontal and vertical padding
#'    within each cell
#' @param draw_legend `logical`, default TRUE, whether to draw the resulting
#'    legend. When `FALSE` the data is returned based upon `legend_style`,
#'    when `legend_style="grid"` it returns a grid `grob`, otherwise
#'    it returns `data.frame`.
#' @param alias `character`, default NULL, optional set aliases,
#'    intended to add a new first column to the legend with these
#'    aliases. Useful to display a short label in the legend table,
#'    while also supplying a longer label via `names(setlist)`
#'    or `labels`.
#'    This argument differs from `labels` in that `labels` replaces
#'    the `names(setlist)` in the legend table with `labels`.
#'    However, `alias` adds a new column to the table as column 1.
#' @param labels `character` default NULL, optional custom labels to
#'    display in place of `names(setlist)` in the legend table.
#'    Note: `names(labels)` must also match `names(setlist)`.
#'    This argument differs from `labels` in that `labels` replaces
#'    the `names(setlist)` in the legend table with `labels`.
#'    However, `alias` adds a new column to the table as column 1.
#' @param legend_padding `numeric` or `grid::unit`, default 5, padding for
#'    each table cell. This value is also adjusted by `font_cex`.
#'    This value is only used when `table_theme` is not
#'    provided.
#' @param set_suffix `character` string (default `""`) used as optional
#'    suffix, and previously used `":"` but was changed to `""`.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to internal functions.
#'    Notably `curate_venn_labels()` may accept the `curate_df` argument,
#'    to allow custom labeling.
#' 
#' @return `data.frame` with legend information is returned invisibly,
#'    unless using `legend_style="grid", draw_legend=FALSE` in which case the
#'    legend `grob` object is returned which can then be manipulated
#'    before rendering.
#' 
#' @examples
#' setlist <- make_venn_test(100, 3, do_signed=TRUE);
#' # by default the legend is shown
#' vo <- venndir(setlist)
#' 
#' # move to different corner
#' vo <- venndir(setlist, legend_x="bottomleft")
#' 
#' # add alias column
#' vo <- venndir(setlist,
#'    legend_labels=setNames(paste0("Items detected in ", LETTERS[1:3]),
#'       names(setlist)),
#'    alias=setNames(LETTERS[1:3], names(setlist)))
#' 
#' # turn off the default legend
#' vo <- venndir(setlist, draw_legend=FALSE)
#' venndir_legender(setlist=setlist,
#'    venndir_output=vo,
#'    x="bottomleft")
#' 
#' # test multi-line labels
#' names(setlist) <- c("Group B-<br>Group A",
#'    "Group C-<br>\nGroup B",
#'    "Group C-<br>\nGroup A")
#' vo <- venndir(setlist,
#'    draw_legend=FALSE,
#'    font_cex=1.3,
#'    fontfamily="Arial",
#'    expand_fraction=c(-0.1, 0.4, -0.1, 0.1),
#'    show_segments=FALSE)
#' venndir_legender(setlist=setlist,
#'    venndir_output=vo,
#'    font_cex=1.3,
#'    x="bottomleft")
#'    
#' # some common customizations
#' vo <- venndir(setlist,
#'    draw_legend=FALSE,
#'    expand_fraction=c(0.2, 0.2, 0.2, 0.2)*3,
#'    font_cex=0.7,
#'    fontfamily="Arial")
#' venndir_legender(vo,
#'    legend_headers=c(Set="Comparison",
#'       Size="Counts (Signed Counts)"),
#'    legend_color_style=c("nofill", "blackborder"),
#'    font_cex=0.8,
#'    x="bottomleft")
#' venndir_legender(vo,
#'    combine_size=FALSE,
#'    legend_headers=c(Set="Comparison",
#'       Size="Counts", Sign="(Signed Counts)"),
#'    legend_color_style=c("nofill", "blackborder"),
#'    font_cex=0.8,
#'    x="bottomright")
#' venndir_legender(vo,
#'    combine_size=FALSE,
#'    combine_sign=FALSE,
#'    legend_headers=c(Set="Comparison",
#'       Size="Counts", Sign="(Signed Counts)"),
#'    legend_color_style=c("nofill", "blackborder"),
#'    font_cex=0.8,
#'    x="topright")
#' venndir_legender(vo,
#'    legend_percent=TRUE,
#'    legend_headers=c(Set="Comparison",
#'    Percentage="Pct.",
#'       Size="Counts", Sign="(Signed Counts)"),
#'    legend_color_style=c("nofill", "blackborder"),
#'    font_cex=0.8,
#'    x="topleft")
#' 
#' # Example showing how to render the legend_grob
#' # which may offer some flexibility.
#' vo <- venndir(setlist,
#'    draw_legend=FALSE)
#' legend_grob <- venndir_legender(setlist=setlist,
#'    venndir_output=vo,
#'    draw_legend=FALSE,
#'    header_color="white",
#'    x="bottomleft")
#' grid::grid.draw(legend_grob)
#' 
#' # custom grid table theme
#' vo <- venndir(setlist,
#'    show_segments=FALSE,
#'    draw_legend=FALSE)
#' legend_grob <- venndir_legender(setlist=setlist,
#'    venndir_output=vo,
#'    headers=FALSE,
#'    x="bottomright",
#'    table_theme=gridExtra::ttheme_default(base_size=11,
#'       base_family="sans",
#'       padding=grid::unit(c(2, 2), "mm")))
#' 
#' # optional expanded labels, and subset setlist
#' setlist <- make_venn_test(100, 5, do_signed=TRUE);
#' vo <- venndir(setlist,
#'    sets=c(4, 1, 2),
#'    show_segments=FALSE,
#'    draw_legend=FALSE)
#' venndir_legender(venndir_output=vo,
#'    font_cex=1,
#'    setlist=setlist,
#'    labels=jamba::nameVector(
#'       paste0("This is set ", LETTERS[1:5]),
#'       names(setlist)))
#' 
#' @export
venndir_legender <- function
(venndir_output=NULL, 
 setlist=NULL,
 x="bottomright", 
 set_colors=NULL,
 legend_style=c("grid",
    "data.frame"),
 unicode=TRUE,
 legend_headers=c(Set="Set",
    Size="Size",
    Percentage="Percentage",
    Sign="Sign"),
 sign_count_delim=NULL,
 keep_newlines=FALSE,
 legend_total=TRUE,
 legend_percentage=NULL,
 legend_signed=NULL,
 combine_signed=TRUE,
 combine_size=TRUE,
 legend_color_style=c("fill",
    "border"),
 box.lwd=0,
 item_type="",
 header_color="#000000",
 header_bg="#FFFFFF00",
 header_border="#FFFFFF00",
 lwd=1,
 x_inset=grid::unit(2, "lines"),
 y_inset=grid::unit(2, "lines"),
 font_cex=1,
 fontfamily="Arial",
 fontfamilies=list(signed=fontfamily,
    count=fontfamily,
    overlap=fontfamily),
 poly_alpha=0.8,
 table_theme=NULL,
 draw_legend=TRUE,
 alias=NULL,
 labels=NULL,
 legend_padding=2,
 set_suffix="",
 vp=NULL,
 fg_fun=marquee_text_grob,
 verbose=FALSE,
 ...)
{
   legend_style <- match.arg(legend_style);

   legend_headers_default <- eval(formals(venndir_legender)$legend_headers);
   if (length(legend_headers) == 0 || FALSE %in% legend_headers) {
      legend_headers <- legend_headers_default;
      legend_headers[] <- "";
   }
   if (!all(names(legend_headers_default) %in% names(legend_headers))) {
      add_names <- setdiff(names(legend_headers_default),
         names(legend_headers));
      legend_headers[add_names] <- legend_headers_default[add_names];
   }
   
   # handle input data
   if (length(venndir_output) > 0) {
      # recognize first argument venndir_output as list or data.frame
      # by assigning to setlist
      if (inherits(venndir_output, c("list", "data.frame"))) {
         if (length(setlist) == 0) {
            setlist <- venndir_output;
            venndir_output <- NULL;
         } else {
            # consider warning that venndir_output is ignored
            if (verbose) {
               # Todo: use cli::
               jamba::printDebug("venndir_legender(): ",
                  "venndir_output is ", c("list", "data.frame"),
                  " and ", "setlist", " is provided, so ",
                  "venndir_output", " is ignored.")
            }
            venndir_output <- NULL;
         }
      } else if (inherits(venndir_output, "Venndir")) {
         if (length(setlist) > 0) {
            # consider warning that it will be overwritten
         }
         setlist <- venndir_output@setlist;
         if (length(legend_signed) == 0) {
            if ("legend_signed" %in% names(metadata(venndir_output))) {
               legend_signed <- metadata(venndir_output)$legend_signed;
            }
         }
      } else {
         stop(paste0("When supplied, venndir_output must be ",
            "'Venndir', or 'list', or 'data.frame'"));
      }
   } else if (length(setlist) == 0) {
      stop("Either venndir_output or setlist are required as input.")
   }
   
   # handle setlist as signed_overlaps() output, convert to setlist
   if (inherits(setlist, "data.frame")) {
      if (!all(c("sets", "overlap", "count") %in% colnames(setlist))) {
         stop(paste0("Input as a data.frame must contain colnames: ",
            jamba::cPaste(c("'sets'", "'overlap'", "'count'")),
            ", optionally 'each'."));
      }
      so <- setlist;
      if (all(c("each", "overlap_label") %in% colnames(so))) {
         # for 'each' we can infer all signed overlaps
         # otherwise we must handle it as unsigned
         sol <- lapply(split(so, so$sets), function(idf){
            jamba::nameVector(idf$count,
               gsub(" ", "_", idf$overlap_label))
         })
         # convert to setlist
         setlist <- signed_counts2setlist(sol)
         # define default legend_signed
         if (length(legend_signed) == 0) {
            legend_signed <- TRUE;
         }
      } else {
         # without 'each' column, we must handle it as unsigned counts
         soc <- lapply(split(so, so$sets), function(idf){
            sum(idf$count)
         })
         setlist <- counts2setlist(soc)
         # define default legend_signed
         if (length(legend_signed) == 0) {
            legend_signed <- FALSE;
         }
      }
   }

   # detect default legend_signed
   if (length(legend_signed) == 0) {
      if (length(venndir_output) > 0) {
         # by default, follow what is shown in the venndir_output
         if ("overlap" %in% metadata(venndir_output)$overlap_type) {
            legend_signed <- FALSE;
         } else {
            legend_signed <- ("signed" %in% venndir_output@label_df$type);
         }
      } else {
         # observe setlist, look for signed data with multiple unique values
         is_signed <- any(unlist(lapply(setlist, function(i){
            if (length(names(i)) == 0) {
               FALSE
            } else {
               length(setdiff(i, c("", "0", NA))) > 1
            }
         })))
         if (TRUE %in% is_signed) {
            legend_signed <- TRUE;
         }
      }
   }
   # turn off legend_signed when there are no signed counts?
   # for now, skip this logic
   # if (length(venndir_output) > 0 &&
   #       TRUE %in% legend_signed &&
   #       !"signed" %in% venndir_output@label_df$type) {
   #    legend_signed <- FALSE
   # }
   
   # define empty sign_count_delim
   if (length(sign_count_delim) == 0) {
      if (length(venndir_output) > 0) {
         if ("sign_count_delim" %in% names(metadata(venndir_output))) {
            sign_count_delim <- metadata(venndir_output)$sign_count_delim;
         }
      }
      if (length(sign_count_delim) == 0) {
         sign_count_delim <- " ";
      }
   }

   # detect legend_percentage
   if (length(legend_percentage) == 0) {
      legend_percentage <- FALSE;
      # crudely detect whether percentages are included as labels
      if (length(venndir_output) > 0 &&
         any(grepl("[%][* ]*$", venndir_output@label_df$text))) {
         legend_percentage <- TRUE;
      }
   }
   
   # make sure setlist matches Venn displayed sets
   if (length(labels) > 0) {
      legend_labels <- labels;
      labels <- NULL;
   } else {
      legend_labels <- NULL;
      if (length(venndir_output) > 0) {
         use_setlist_names <- intersect(names(setlist),
            venndir_output@label_df$overlap_set)
         if (length(use_setlist_names) == 0) {
            stop("label_df$overlap_set does not match names(setlist)");
         }
         setlist <- setlist[use_setlist_names];
         if ("legend_label" %in% colnames(venndir_output@jps@polygons)) {
            matchset <- match(names(setlist),
               venndir_output@jps@polygons$venn_name)
            if (any(is.na(matchset))) {
               stop("Venndir 'venn_name' does not match names(setlist).");
               # Todo: recover by ignoring legend_labels
            } else {
               legend_labels <- jamba::nameVector(
                  venndir_output@jps@polygons$legend_label[matchset],
                  names(setlist));
            }
         }
      }
   }
   if (length(legend_labels) == 0) {
      legend_labels <- jamba::nameVector(names(setlist));
   }

   # custom function to replace newline legend_style with \n
   fix_setlist_names <- function
   (setlist,
    use_newline="\n",
    keep_newlines=FALSE,
    use_labels=legend_labels)
   {
      if (FALSE %in% keep_newlines) {
         use_newline <- " ";
      }
      # 0.0.33.900 - use custom labels
      names(setlist) <- use_labels[names(setlist)];
      # apply additional logic
      names(setlist) <- gsub(paste0("[", use_newline, "]+"),
         use_newline,
         gsub("<br>|\n",
            use_newline,
            names(setlist)));
      return(setlist)
   }
   
   # construct setlist if not provided
   total_items <- NULL;
   if (length(setlist) == 0) {
      if (length(venndir_output) == 0) {
         stop("Either setlist or venndir_output must be provided.");
      }
      setlist <- setlist(venndir_output);
   }
   
   # Quick function to update legend colnames
   adjust_legend_headers <- function
   (gridlegend_df_use,
    legend_headers,
    cell_padding=c("", ""),
    ...)
   {
      # repair slight hack with trailing whitespace
      # used for column justification
      colnames(gridlegend_df_use) <- gsub("^Size ", "Size",
         colnames(gridlegend_df_use));
      # rename only when legend_headers contains something to change
      if (!all(names(legend_headers) == legend_headers)) {
         switch_from <- intersect(colnames(gridlegend_df_use),
            names(legend_headers))
         if (length(switch_from) > 0) {
            switch_to <- legend_headers[switch_from];
            gridlegend_df_use <- jamba::renameColumn(gridlegend_df_use,
               from=switch_from,
               to=switch_to,
               check.names=FALSE)
         }
      }
      # add padding
      colnames(gridlegend_df_use) <- paste0(
         cell_padding[[1]],
         colnames(gridlegend_df_use),
         cell_padding[[2]])
      gridlegend_df_use
   }
   
   # pick out venn colors if available
   vodf_color <- NULL;
   vodf_border <- NULL;
   vodf_lwd <- NULL;
   if (length(set_colors) > 0) {
      if (!all(names(setlist) %in% names(set_colors))) {
         stop("Not all names(setlist) were defined in names(set_colors).")
      }
      vodf_color <- set_colors[names(setlist)];
      vodf_border <- jamba::makeColorDarker(jamba::unalpha(vodf_color),
         darkFactor=1.2)
      vodf_lwd <- rep(0.5, length(vodf_color));
      names(vodf_lwd) <- names(vodf_color);
   } else if (length(venndir_output) > 0) {
      vodf_all <- venndir_output@jps@polygons;
      use_label <- head(intersect(c("label", "name"),
         colnames(vodf_all)), 1);
      
      vodf <- subset(vodf_all, type %in% "set");
      vodf_color <- jamba::nameVector(
         vodf$venn_color,
         vodf[[use_label]]);
      if ("alpha" %in% colnames(vodf)) {
         vodf_lwd <- jamba::nameVector(
            vodf$border.lwd,
            vodf[[use_label]]);
      } else {
         vodf_lwd <- rep(1, length(vodf[[use_label]]))
         names(vodf_lwd) <- vodf[[use_label]];
      }
      if ("alpha" %in% colnames(vodf)) {
         vodf_ol <- subset(vodf_all,
            type %in% "overlap" &
               label %in% names(vodf_color))
         vo_match <- match(vodf[[use_label]], vodf_ol[[use_label]]);
         vo_alpha <- ifelse(is.na(vo_match),
            1,
            vodf_ol$alpha[vo_match])
         vodf_color <- jamba::alpha2col(vodf_color,
            alpha=vo_alpha)
      } else {
         # poly_alpha
         vodf_color <- jamba::alpha2col(vodf_color,
            alpha=poly_alpha)
      }
      if ("border" %in% colnames(vodf)) {
         vodf_ol <- subset(vodf_all,
            type %in% "overlap" &
               label %in% names(vodf_color))
         # vo_match <- match(vodf[[use_label]], vodf_ol[[use_label]]);
         vo_match <- match(vodf[[use_label]], vodf[[use_label]]);
         vodf_border <- ifelse(is.na(vo_match),
            header_border,
            ifelse(!is.na(vodf$outerborder[vo_match]),
               vodf$outerborder[vo_match],
               ifelse(!is.na(vodf$innerborder[vo_match]),
                  vodf$innerborder[vo_match],
                  vodf$border[vo_match])));
         # vodf_ol$border[vo_match])
         names(vodf_border) <- vodf[[use_label]];
      }
   } else {
      vodf_color <- jamba::nameVector(
         rep("#FFFFFF", length(setlist)),
         names(setlist))
      vodf_border <- jamba::makeColorDarker(jamba::unalpha(vodf_color),
         darkFactor=1.2)
      vodf_lwd <- rep(0.5, length(vodf_color));
      names(vodf_lwd) <- names(vodf_color);
   }
   
   # optionally include total unique items
   if (TRUE %in% legend_total) {
      if (length(venndir_output) > 0) {
         # venndir_legender(setlist=setlist, venndir_output=vo, x="bottomright")
         total_items <- sum(vodf_all$venn_counts, na.rm=TRUE);
      } else {
         sv <- signed_overlaps(setlist,
            overlap_type="overlap",
            return_items=FALSE,
            ...);
         total_items <- sum(sv$count, na.rm=TRUE);
      }
   }
   
   # optionally subset setlist by vodf_color
   if (length(vodf_color) > 0 &&
         !all(names(setlist) %in% names(vodf_color))) {
      keep_sets <- intersect(names(vodf_color),
         names(setlist));
      setlist <- setlist[keep_sets]
      vodf_color <- vodf_color[keep_sets]
   }

   # fix names by converting newline from HTML <br> to \n
   # Note: This step must occur after subsetting by names(setlist)
   setlist <- fix_setlist_names(setlist,
      use_newline="\n",
      keep_newlines=keep_newlines)
   if (length(alias) > 0) {
      alias <- fix_setlist_names(alias,
         use_newline="\n",
         keep_newlines=keep_newlines)
   }
   vodf_color <- fix_setlist_names(vodf_color,
      use_newline="\n",
      keep_newlines=keep_newlines)
   vodf_border <- fix_setlist_names(vodf_border,
      use_newline="\n",
      keep_newlines=keep_newlines)
   vodf_lwd <- fix_setlist_names(vodf_lwd,
      use_newline="\n",
      keep_newlines=keep_newlines)
   
   # legend data.frame
   legend_df <- data.frame(check.names=FALSE,
      stringsAsFactors=FALSE,
      legend=paste0(names(setlist), ": ",
         jamba::formatInt(lengths(setlist)),
         " ",
         item_type),
      color=vodf_color[names(setlist)],
      border=vodf_border[names(setlist)],
      lwd=vodf_lwd[names(setlist)])
   
   # optionally include total unique items
   gridlegend_df <- data.frame(check.names=FALSE,
      stringsAsFactors=FALSE,
      set=names(setlist),
      size=lengths(setlist))

   # optional signed counts
   gcdf1 <- NULL;
   gcdf2 <- NULL;
   if (TRUE %in% legend_signed) {
      sv <- jamba::rbindList(lapply(seq_along(setlist), function(isetlist){
         if (length(unlist(setlist[isetlist])) == 0) {
            return(NULL)
         }
         idf <- signed_overlaps(setlist[isetlist],
            overlap_type="each",
            return_items=FALSE);
         idf[, setdiff(colnames(idf), names(setlist[isetlist])), drop=FALSE]
      }))
      # total_items <- sum(sv$count, na.rm=TRUE);
      gCounts <- lapply(jamba::nameVector(unique(sv$sets)), function(i){
         j <- subset(sv, sets %in% i);
         jamba::nameVector(j$count, j$overlap_label)
      });
      
      gbase_labels <- curate_venn_labels(
         x=names(unlist(unname(gCounts))),
         unicode=unicode,
         type="sign",
         ...)
      gbase_colors <- curate_venn_labels(
         x=names(unlist(unname(gCounts))),
         unicode=unicode,
         type="color",
         ...)
         # curate_df=curate_df);
      big.mark <- ",";
      use_sign_count_delim <- sub(" ", "", sign_count_delim);
      gcount_labels <- sapply(seq_along(unlist(gCounts)), function(i){
         ilabel <- paste0(
            gbase_labels[i],
            use_sign_count_delim,
            format(trim=TRUE,
               big.mark=big.mark,
               unlist(gCounts)[i]));
      });
      gcount_df <- data.frame(check.names=FALSE,
         set_name=rep(names(gCounts), lengths(gCounts)),
         count=unlist(unname(gCounts)),
         label=gbase_labels,
         count_label=gcount_labels,
         count_label_color=gbase_colors)
      gcdf <- data.frame(check.names=FALSE,
         jamba::rbindList(lapply(split(gcount_df, gcount_df$set_name), function(idf){
            unname(unlist(
               strsplit(
                  jamba::pasteByRow(
                     idf[, c("count_label_color", "count_label"), drop=FALSE],
                     sep="!!"), "!!")))
         })))
      gcdf1 <- gcdf[, seq(from=1, to=ncol(gcdf), by=2), drop=FALSE];
      # each count in its own column, mostly so it can be colorized
      gcdf2 <- gcdf[, seq(from=2, to=ncol(gcdf), by=2), drop=FALSE];
      # replace with "(# up, # dn)" format
      gcolnames1 <- c(
         # " (Sign)",
         "Sign",
         sapply(seq_len(ncol(gcdf2)), function(k1){
            paste(rep(" ", k1), collapse="")}));
      gcolnames <- tail(gcolnames1, -1)
      colnames(gcdf2) <- gcolnames
      colnames(gcdf1) <- colnames(gcdf2);
      if (TRUE %in% combine_signed) {
         gcdf2 <- data.frame(check.names=FALSE,
            `Sign`=paste0("(", jamba::pasteByRow(gcdf2, sep=", "), ")  "))
         # colnames(gcdf2) <- gcolnames[1]
      } else if (TRUE %in% combine_size) {
         colnames(gcdf2) <- jamba::makeNames(rep("Sign", ncol(gcdf2)));
      }
      rownames(gcdf2) <- rownames(gcdf1);
   }
   
   # optional labels
   if (length(labels) > 0) {
      if (!all(names(setlist) %in% names(labels))) {
         stop("names(labels) must contain all names(setlist)")
      }
      legend_df$legend <- paste0(
         names(setlist), " - ",
         labels[names(setlist)], ": ",
         jamba::formatInt(lengths(setlist)),
         " ",
         item_type)
      gridlegend_df <- data.frame(check.names=FALSE,
         stringsAsFactors=FALSE,
         set=names(setlist),
         label=labels[names(setlist)],
         size=lengths(setlist))
      rownames(gridlegend_df) <- names(setlist)
   }
   
   # optional alias
   if (length(alias) > 0) {
      if (!all(names(setlist) %in% names(alias))) {
         stop("names(alias) must contain all names(setlist)")
      }
      legend_df$legend <- paste0(
         alias[names(setlist)], " - ",
         names(setlist), set_suffix, " ",
         jamba::formatInt(lengths(setlist)),
         " ",
         item_type)
      gridlegend_df$label <- gridlegend_df$set;
      gridlegend_df$set <- alias[names(setlist)];
      gridlegend_df <- gridlegend_df[, c("set", "label", "size"), drop=FALSE];
      rownames(gridlegend_df) <- names(setlist)
      # gridlegend_df <- data.frame(check.names=FALSE,
      #    stringsAsFactors=FALSE,
      #    set=alias[names(setlist)],
      #    label=names(setlist),
      #    size=lengths(setlist))
   }
   
   # optionally include total item count
   if (TRUE %in% legend_total) {
      gridlegend_df2 <- data.frame(check.names=FALSE,
         stringsAsFactors=FALSE,
         set=paste0("Total", ifelse(nchar(item_type) > 0, " ", ""),
            item_type),
         label="",
         size=total_items);
      if (!"label" %in% colnames(gridlegend_df)) {
         gridlegend_df2$label <- NULL;
      }
      rownames(gridlegend_df2) <- "Total";
      gridlegend_df <- rbind(gridlegend_df, gridlegend_df2);
      # confirm no duplicate rownames, and do no rename first instance
      rownames(gridlegend_df) <- jamba::makeNames(
         rownames(gridlegend_df), renameFirst=FALSE);
      #
      legend_df2 <- data.frame(legend="Total",
         # color="grey95",
         # border="grey95",
         color="#F4F4F4",
         border="#999999",
         lwd=1);
      legend_df <- rbind(legend_df, legend_df2);
   }

   if ("nofill" %in% legend_color_style) {
      legend_df$color[] <- "#FFFFFF00";
   }
   if ("greyfill" %in% legend_color_style) {
      legend_df$color[] <- "#F4F4F4";
   }
   if ("noborder" %in% legend_color_style) {
      legend_df$border[] <- "#FFFFFF00";
      # legend_df$border[] <- legend_df$color[];
   }
   if ("greyborder" %in% legend_color_style) {
      legend_df$border[] <- "#999999";
   }
   if ("blackborder" %in% legend_color_style) {
      legend_df$border[] <- "#000000";
   }
   legend_df$lwd <- 1;
   
   # optionally add percentage
   if (TRUE %in% legend_percentage && TRUE %in% legend_total) {
      pct_total <- round(
         1000 * gridlegend_df$size / tail(gridlegend_df$size, 1)) / 10;
      gcdf2p <- data.frame(check.names=FALSE,
         `Percentage`=paste(format(pct_total, digits=3, trim=TRUE), "%"))
      # rownames(gcdf2p) <- gridlegend_df$set;
      rownames(gcdf2p) <- rownames(gridlegend_df);
      if (TRUE %in% legend_signed) {
         matchgc <- match(rownames(gcdf2p), rownames(gcdf2))
         gcdf2 <- data.frame(check.names=FALSE,
            gcdf2[matchgc, , drop=FALSE],
            gcdf2p)
         rownames(gcdf2) <- rownames(gcdf2p);
      } else {
         gcdf2 <- gcdf2p;
      }
   }
   
   # add signed/percentage columns to legend
   if (TRUE %in% legend_signed || TRUE %in% legend_percentage) {
      matchgc <- match(gridlegend_df$set, rownames(gcdf2));
      add_df <- gcdf2[match(rownames(gridlegend_df), rownames(gcdf2)), , drop=FALSE];
      add_df[is.na(add_df)] <- ""
      addc_df <- gcdf1[match(rownames(gridlegend_df), rownames(gcdf2)), , drop=FALSE];
      addc_df[is.na(addc_df)] <- ""
      gridlegend_df[, colnames(add_df)] <- add_df
   }
   
   # render legend
   if (any(c("grid", "data.frame") %in% legend_style)) {
      # prepare graphical object
      gridlegend_df_use <- gridlegend_df;
      gridlegend_df_use$set <- paste0(gridlegend_df$set, set_suffix);
      if (length(item_type) > 0 && nchar(item_type) > 0) {
         gridlegend_df_use$size <- paste0(
            jamba::formatInt(gridlegend_df$size),
            " ",
            item_type)
      } else {
         gridlegend_df_use$size <- paste0(
            jamba::formatInt(gridlegend_df$size))
      }
      if (length(header_color) == 0 || nchar(header_color) == 0) {
         header_color <- "#00000000"
      }
      
      ## optionally combine Size with Sign
      # - Note: Include percentage made the final string confusing,
      #   since percentage is not in parentheses
      # if (TRUE %in% legend_signed || TRUE %in% legend_percentage) {
      if (TRUE %in% combine_size && TRUE %in% legend_signed) {
         signcol <- grep("Sign", value=TRUE, colnames(gridlegend_df_use));
         # signcol <- grep("Sign|Percentage", value=TRUE,
         #    colnames(gridlegend_df_use));
         gridlegend_df_use$size <- sub("  [(]", " (",
            jamba::pasteByRow(
               gridlegend_df_use[, c("size", signcol), drop=FALSE], sep=" "));
         keepcols <- setdiff(colnames(gridlegend_df_use), signcol)
         gridlegend_df_use <- gridlegend_df_use[, keepcols, drop=FALSE];
         gridlegend_df_use <- jamba::renameColumn(gridlegend_df_use,
            from="size", to="Size ")
      }
      
      if ("data.frame" %in% legend_style) {
         colnames(gridlegend_df_use) <- jamba::ucfirst(
            colnames(gridlegend_df_use));
         rownames(gridlegend_df_use) <- seq_len(
            nrow(gridlegend_df_use));
         # Bonus points: Apply column justification
         use_justify <- ifelse(
            grepl("Size$|Percent", colnames(gridlegend_df_use)),
            "right", "left")
         justify_colnames <- TRUE;
         # Adjust colnames by legend_headers
         gridlegend_df_use <- adjust_legend_headers(gridlegend_df_use,
            legend_headers=legend_headers,
            cell_padding=c("", ""))
         # Bonus points: Apply column justification
         for (inum in seq_len(ncol(gridlegend_df_use))) {
            if (TRUE %in% justify_colnames) {
               usex <- c(colnames(gridlegend_df_use)[inum],
                  gridlegend_df_use[[inum]])
               gridlegend_df_use[[inum]] <- tail(format(usex,
                  justify=use_justify[[inum]]), -1);
               colnames(gridlegend_df_use)[inum] <- head(format(usex,
                  justify=use_justify[[inum]]), 1);
            } else {
               gridlegend_df_use[[inum]] <- format(gridlegend_df_use[[inum]],
                  justify=use_justify[[inum]])
            }
         }
         return(gridlegend_df_use)
      }
      
      ## table theme
      # - note the ugly hack to create whitespace before/after the
      #   column text, and colnames() text
      # - this hack is due to `gridExtra::tableGrob()` using `padding`
      #   to define slightly wider text box, but not applying that padding
      #   to the label position. So using `hjust=1` causes the text to
      #   be placed at the border of the extended padded range, the very edge.
      colnames(gridlegend_df_use) <- jamba::ucfirst(
         colnames(gridlegend_df_use));
      
      # add leading,trailing whitespace to all columns
      cell_padding <- c("  ", " ");
      for (icol in colnames(gridlegend_df_use)) {
         # data.frame cells
         gridlegend_df_use[[icol]] <- paste0(cell_padding[[1]],
            gridlegend_df_use[[icol]],
            cell_padding[[2]]);
      }
      use_padding <- grid::unit(c(1, 1) * font_cex, "mm");
      if (length(table_theme) == 0) {
         use_hjust1 <- ifelse(
            grepl("Size$|Percent", colnames(gridlegend_df_use)), 1, 0)
         use_hjust <- rep(use_hjust1,
            each=nrow(gridlegend_df_use))
         # adjust legend_padding by font_cex, convert to mm
         if (length(legend_padding) == 0) {
            legend_padding <- formals(venndir_legender)$legend_padding;
         }
         if (is.numeric(legend_padding)) {
            legend_padding <- rep(legend_padding, length.out=2);
            legend_padding <- legend_padding * font_cex;
            use_padding <- grid::unit(legend_padding, "mm");
         } else if (grid::is.unit(legend_padding)) {
            use_padding <- legend_padding;
         }

         table_theme <- gridExtra::ttheme_default(
            base_size=12 * font_cex,
            base_family=fontfamilies$overlap,
            core=list(
               fg_params=list(
                  hjust=use_hjust,
                  x=use_hjust),
               padding=use_padding),
            colhead=list(
               fg_params=list(
                  hjust=use_hjust1,
                  x=use_hjust1),
               padding=use_padding))
            # colhead=list(fg_params=list(hjust=use_hjust, x=use_hjust))
            # colhead=list(fg_params=list(hjust=1, x=0.95)))
      }
      
      # Final updates to colnames
      gridlegend_df_use <- adjust_legend_headers(gridlegend_df_use,
         legend_headers=legend_headers,
         cell_padding=cell_padding)

      # create the tableGrob
      if (length(fg_fun) > 0 && inherits(fg_fun, "function")) {
         table_theme$core$fg_fun <- fg_fun;
         table_theme$colhead$fg_fun <- fg_fun;
      }
      # legend_grob <- jamba::call_fn_ellipsis(gridExtra::tableGrob,
      legend_grob <- gridExtra::tableGrob(
         rows=NULL,
         theme=table_theme,
         vp=vp,
         d=gridlegend_df_use)

      # optionally color each row
      if (length(vodf_color) > 0) {
         # custom function to find matching cell
         find_cell <- function(table, row, col, name="core-fg"){
            ldf <- table$layout
            which(ldf$t == row &
                  ldf$l == col &
                  ldf$name == name)
         }
         # enforce consistent row heights
         # otherwise the Total without arrows can be shorter than others
         rowheights <- legend_grob$heights;
         maxrowheight <- max(rowheights);
         newheights <- do.call(grid::unit.c,
            rep(list(maxrowheight), length(rowheights)));
         legend_grob$heights <- newheights;
         # custom function to update existing gpar values
         update_gpar_values <- function
         (gp,
          gp_list,
          ...)
         {
            for (iname in names(gp_list)) {
               gp[[iname]] <- gp_list[[iname]]
            }
            return(gp)
         }
         
         # adjust the top row
         for (icol in seq_len(ncol(gridlegend_df_use))) {
            # header background fill
            ind <- find_cell(legend_grob, 1, icol, "colhead-bg")
            use_hjust <- 0;
            if (icol == ncol(gridlegend_df_use)) {
               # right-align
               use_hjust <- 1;
            }
            if (length(ind) > 0) {
               # manually adjust some attributes
               legend_grob$grobs[ind][[1]][["gp"]] <- update_gpar_values(
                  gp=legend_grob$grobs[ind][[1]][["gp"]],
                  gp_list=list(fill=header_bg,
                     col=header_border,
                     lwd=lwd))
            }
            # header foreground style
            ind <- find_cell(legend_grob, 1, icol, "colhead-fg")
            if (length(ind) > 0) {
               if (FALSE %in% "textGrob") {
                  # textGrob only
                  legend_grob$grobs[ind][[1]][["gp"]] <- update_gpar_values(
                     gp=legend_grob$grobs[ind][[1]][["gp"]],
                     gp_list=list(
                        col=make_color_contrast(header_color, header_bg)))
               } else if (TRUE) {
                  # marquee grob only
                  # adjust vertical alignment for marquee
                  # legend_grob$grobs[ind][[1]][["y"]] <- grid::unit(0.12, "npc")
                  # adjust based upon use_padding
                  legend_grob$grobs[ind][[1]][["y"]] <- use_padding[1] * 1/2;
                  # font color
                  legend_grob$grobs[ind][[1]][["text"]][["color"]] <- rep(
                     make_color_contrast(header_color, header_bg),
                     length.out=length(
                        legend_grob$grobs[ind][[1]][["text"]][["color"]]));
                  legend_grob$grobs[ind][[1]][["shape"]][["col"]] <- rep(
                     make_color_contrast(header_color, header_bg),
                     length.out=length(
                        legend_grob$grobs[ind][[1]][["shape"]][["col"]]));
               }
            }
         }
         # iterate each row
         # Todo: Consider resizing columns with alternate fonts
         for (irow in seq_len(nrow(legend_df))) {
            for (icol in seq_len(ncol(gridlegend_df_use))) {
               icolname <- colnames(gridlegend_df_use)[icol];
               
               # iterate foreground
               ind <- find_cell(legend_grob, irow + 1, icol, "core-fg")
               for (ind0 in unique(ind)) {
                  use_fontfamily <- fontfamilies$overlap;
                  if (any(grepl("Size|Percent", icolname))) {
                     use_fontfamily <- fontfamilies$count;
                  } else if (any(grepl("Sign|^[ ]*$", icolname))) {
                     use_fontfamily <- fontfamilies$signed;
                  }
                  
                  # manually adjust some attributes
                  if (FALSE %in% "textGrob") {
                     # textGrob type only
                     legend_grob$grobs[ind0][[1]][["gp"]] <- update_gpar_values(
                        gp=legend_grob$grobs[ind0][[1]][["gp"]],
                        gp_list=list(
                           fontfamily=use_fontfamily,
                           col=jamba::setTextContrastColor(
                              legend_df$color[irow])))
                  } else if (TRUE) {
                     # marquee object
                     # adjust vertical alignment for marquee
                     # legend_grob$grobs[ind0][[1]][["y"]] <- grid::unit(0.12, "npc")
                     # adjust based upon use_padding
                     legend_grob$grobs[ind0][[1]][["y"]] <- use_padding[1] * 1/2;
                     # font color
                     legend_grob$grobs[ind0][[1]][["text"]][["color"]] <- rep(
                        jamba::setTextContrastColor(legend_df$color[irow]),
                        length.out=length(
                           legend_grob$grobs[ind0][[1]][["text"]][["color"]]));
                     # fontfamily
                     legend_grob$grobs[ind0][[1]][["text"]][["family"]] <- rep(
                        use_fontfamily,
                        length.out=length(
                           legend_grob$grobs[ind0][[1]][["text"]][["family"]]));
                     # shape font color
                     legend_grob$grobs[ind0][[1]][["shape"]][["col"]] <- rep(
                        jamba::setTextContrastColor(legend_df$color[irow]),
                        length.out=length(
                           legend_grob$grobs[ind0][[1]][["shape"]][["col"]]));
                  }
                  # legend_grob$grobs[ind0][[1]][["gp"]] <- grid::gpar(
                  #    # fontface="bold",
                  #    col=jamba::setTextContrastColor(legend_df$color[irow]))
               }
               
               # iterate background
               ind <- find_cell(legend_grob, irow + 1, icol, "core-bg")
               for (ind0 in unique(ind)) {
                  if (verbose) {
                     jamba::printDebug('legend_grob$grobs[ind0][[1]][["gp"]]',' (before):');
                     print(legend_grob$grobs[ind0][[1]][["gp"]])
                  }
                  legend_grob$grobs[ind0][[1]][["gp"]] <- update_gpar_values(
                     gp=legend_grob$grobs[ind0][[1]][["gp"]],
                     gp_list=list(
                        fill=legend_df$color[irow],
                        col=legend_df$border[irow],
                        lwd=legend_df$lwd[irow]))
                  if (verbose) {
                     jamba::printDebug('legend_grob$grobs[ind0][[1]][["gp"]]',' (after):');
                     print(legend_grob$grobs[ind0][[1]][["gp"]])
                  }
               }
            }
         }
      }
      
      # position the object
      legend_width <- sum(legend_grob$widths)
      legend_height <- sum(legend_grob$heights)
      legend_x <- 0.5;
      if (grepl("left", x)) {
         legend_x <- 0.5 * legend_width + x_inset
      } else if (grepl("right", x)) {
         legend_x <- grid::unit(1, "npc") - 0.5 * legend_width - x_inset
      }
      legend_y <- 0.5;
      if (grepl("top", x)) {
         legend_y <- grid::unit(1, "npc") - 0.5 * legend_height - y_inset
      } else if (grepl("bottom", x)) {
         legend_y <- 0.5 * legend_height + y_inset
      }
      legend_grob$vp <- grid::viewport(
         x=legend_x,
         y=legend_y)
      # draw the object
      if (TRUE %in% draw_legend) {
         grid::grid.draw(legend_grob)
      } else {
         return(legend_grob);
      }
   }
   return(invisible(legend_df))
}
