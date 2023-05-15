
#' Add optional legend to venndir figures (developmental)
#' 
#' Add optional legend to venndir figures (developmental). This function
#' is being evaluated and developed, and will very likely change
#' again in an upcoming release.
#' 
#' Note this function is experimental and is under active development. The
#' implementation and arguments may change in future.
#' 
#' Limitations: Currently this function relies upon having the `setlist`
#' used to produce the `venndir()` output, and the `venndir()` output.
#' In future, the `setlist` could be derived from the `venndir()` output
#' object directly. That step likely needs a new function.
#' 
#' When using arguments `style="grid"` and `draw_legend=FALSE` the
#' `grid` `grob` object is returned, so that it can be manipulated
#' as needed before rendering. Note that in this case, the viewport
#' will have already been defined and stored into `legend_grob$vp`
#' with x position `legend_grob$vp$x` and y position `legend_grob$vp$y`.
#' Total legend width is: `sum(legend_grob$widths)`, and
#' total legend height is: `sum(legend_grob$heights)`.
#' 
#' Todo:
#' 
#' * Consider bottom-justifying text in each cell, so there is consistent
#' alignment of labels that end in ":" with the numerical value.
#' This adjustment would only affect multi-line labels.
#' 
#' @family venndir utility
#' 
#' @param setlist `list` used as input to `venndir()`, which is used
#'    only to derive the number of elements.
#' @param x `character` string indicating the position of the legend,
#'    as passed to `graphics::legend()` when `style="base"`.
#' @param venndir_out `list` object returned by `venndir()`, which is used
#'    to generate the legend counts. When supplied, the `set_colors` are
#'    also defined by this object.
#' @param set_colors `character` optional vector of R colors, whose names
#'    should match `names(setlist)`. When not supplied, colors are inferred
#'    from `venndir_out`, and when that is not supplied, colors are
#'    defined using `colorjam::rainbowJam()`.
#' @param keep_newlines `logical` indicating whether to keep newlines
#'    (line breaks) in the set labels used in the Venn legend.
#' @param box.lwd `numeric` used to define the box line width,
#'    as passed to `graphics::legend()` when `style="base"`.
#' @param style `character` string indicating the style of legend:
#'    * `"base"` uses `graphics::legend()`
#'    * `"grid"` uses `gridExtra::grid.table()`
#'    * `"data.frame"` only returns the venndir legend `data.frame`.
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
#' @param lwd `numeric` value for line width used for the cell border
#'    in the legend table, when `style="grid"`.
#' @param x_inset,y_inset `grid::unit` object that defines the inset
#'    distance away from each plot edge, along the x-axis and y-axis,
#'    respectively. For example `x_inset=grid::unit(2, "lines")` will
#'    place the legend table 2 character lines (which are defined by
#'    line height for typical character size) away from the left or
#'    right edge of the plot. Any valid `grid::unit` can be used, however
#'    using `"lines"` units provides some consistent width rather
#'    than using `"npc"` in terms of fraction of overall plot width
#'    or height.
#' @param font_cex `numeric` adjustment to default font sizes. The default
#'    font size with `style="grid"` uses a 12 point font, so to adjust
#'    to a specific font size like 8 points, use: `font_cex=8/12`
#' @param table_theme `list` of theme parameters as described in
#'    `gridExtra::tableGrob()`, and `gridExtra::ttheme_default()`.
#'    When supplied, the `font_cex` argument is ignored. 
#'    The `list` components include:
#'    * `base_size` - default font size
#'    * `base_colour` - default font color
#'    * `base_family` - default font family
#'    * `parse` - `logical` whether to parse plotmath expressions
#'    * `padding` - `grid::unit()` for horizontal and vertical padding
#'    within each cell
#' @param draw_legend `logical` indicating whether to draw the resulting
#'    legend. Because this function is in development, the `style="grid"`
#'    seems to be preferred, however it may be useful to return the
#'    `grob` object for manipulation before rendering inside the active
#'    plot. In that case, use arguments: `style="grid", draw_legend=FALSE`.
#' @param alias `character` vector with optional set aliases, experimental
#'    feature to allow referring to entries in `setlist` by shorter aliases.
#'    Note that `names(alias)` must also match `names(setlist)`.
#'    Note: The argument `labels` is more likely to be useful, since
#'    the original call to `venndir()` should use `setlist` with aliased
#'    names, and expanded labels would be provided separately.
#' @param labels `character` vector with optional set labels, experimental
#'    feature to supply a longer label associated with each entry in `setlist`.
#'    Note that `names(labels)` must also match `names(setlist)`.
#'    Note: The most useful workflow would be to assign short aliases to
#'    `names(setlist)` when calling `venndir()`, then use the original long
#'    label names as argument `labels` here.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are passed to subsequent functions.
#' 
#' @return `data.frame` with legend information is returned invisibly,
#'    unless using `style="grid", draw_legend=FALSE` in which case the
#'    legend `grob` object is returned which can then be manipulated
#'    before rendering.
#' 
#' @examples
#' setlist <- make_venn_test(100, 3, do_signed=TRUE);
#' vo <- venndir(setlist)
#' 
#' venndir_legender(setlist=setlist,
#'    venndir_out=vo,
#'    x="bottomleft")
#' 
#' # base legend bottom-right
#' venndir_legender(setlist=setlist,
#'    venndir_out=vo,
#'    style="base",
#'    x="bottomright")
#' 
#' vo <- venndir(setlist,
#'    show_segments=FALSE,
#'    plot_style="gg")
#' venndir_legender(setlist=setlist,
#'    venndir_out=vo,
#'    style="grid",
#'    x="bottomleft")
#' 
#' # test multi-line labels
#' names(setlist) <- c("Group A-<br>Group B",
#'    "Group B-<br>\nGroup C",
#'    "Dex_KO-\nVeh_WT")
#' vo <- venndir(setlist,
#'    show_segments=FALSE,
#'    plot_style="gg")
#' venndir_legender(setlist=setlist,
#'    venndir_out=vo,
#'    style="grid",
#'    x="bottomleft")
#' 
#' # Same as above, showing how to render the legend_grob.
#' # This method also "hides" the column headers.
#' vo <- venndir(setlist,
#'    show_segments=FALSE,
#'    font_cex=c(1, 1, 0.5, 0.5),
#'    plot_style="gg")
#' legend_grob <- venndir_legender(setlist=setlist,
#'    venndir_out=vo,
#'    style="grid",
#'    draw_legend=FALSE,
#'    header_color="white",
#'    x="bottomright")
#' grid::grid.draw(legend_grob)
#' 
#' # custom grid table theme
#' vo <- venndir(setlist,
#'    show_segments=FALSE,
#'    plot_style="gg")
#' legend_grob <- venndir_legender(setlist=setlist,
#'    venndir_out=vo,
#'    headers=FALSE,
#'    style="grid",
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
#'    plot_style="gg")
#' venndir_legender(venndir_out=vo,
#'    font_cex=0.8,
#'    setlist=setlist,
#'    labels=jamba::nameVector(
#'       paste0("This is set ", LETTERS[1:5]),
#'       names(setlist)))
#' 
#' # Venn with no border, and more transparent colors
#' vo124 <- venndir(setlist, sets=c(1, 2, 4), poly_alpha=0.4, do_plot=FALSE)
#' vo124$venn_spdf$lwd <- 0.1
#' render_venndir(vo124)
#' venndir_legender(setlist=setlist, venndir_out=vo124)
#' 
#' @export
venndir_legender <- function
(setlist,
 x="bottomleft", 
 venndir_out=NULL, 
 set_colors=NULL,
 keep_newlines=FALSE,
 box.lwd=0, 
 style=c("grid",
    "base",
    "data.frame"),
 item_type="",
 header_color="#000000",
 header_bg="#FFFFFF",
 header_border="#FFFFFF",
 lwd=1,
 x_inset=grid::unit(2, "lines"),
 y_inset=grid::unit(2, "lines"),
 font_cex=1,
 table_theme=NULL,
 draw_legend=TRUE,
 alias=NULL,
 labels=NULL,
 verbose=FALSE,
 ...)
{
   style <- match.arg(style);

   # custom function to replace newline style with \n
   fix_setlist_names <- function
   (setlist,
    use_newline="\n",
    keep_newlines=FALSE)
   {
      if (FALSE %in% keep_newlines) {
         use_newline <- " ";
      }
      names(setlist) <- gsub(paste0("[", use_newline, "]+"),
         use_newline,
         gsub("<br>|\n",
            use_newline,
            names(setlist)));
      return(setlist)
   }
   
   # theme
   if (length(table_theme) == 0) {
      table_theme <- gridExtra::ttheme_default(base_size=12 * font_cex)
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
   } else if (length(venndir_out) > 0) {
      vodf <- data.frame(venndir_out$venn_spdf);
      vodf <- subset(vodf, type %in% "set");
      vodf_color <- jamba::nameVector(
         vodf$venn_color,
         vodf$label);
      if ("alpha" %in% colnames(data.frame(venndir_out$venn_spdf))) {
         vodf_lwd <- jamba::nameVector(
            vodf$lwd,
            vodf$label);
      } else {
         vodf_lwd <- rep(1, length(vodf$label))
         names(vodf_lwd) <- vodf$label;
      }
      if ("alpha" %in% colnames(data.frame(venndir_out$venn_spdf))) {
         vodf_ol <- subset(data.frame(venndir_out$venn_spdf),
            type %in% "overlap" &
               label %in% names(vodf_color))
         vo_match <- match(vodf$label, vodf_ol$label);
         vo_alpha <- ifelse(is.na(vo_match),
            1,
            vodf_ol$alpha[vo_match])
         vodf_color <- jamba::alpha2col(vodf_color,
            alpha=vo_alpha)
      }
      if ("border" %in% colnames(data.frame(venndir_out$venn_spdf))) {
         vodf_ol <- subset(data.frame(venndir_out$venn_spdf),
            type %in% "overlap" &
               label %in% names(vodf_color))
         vo_match <- match(vodf$label, vodf_ol$label);
         vodf_border <- ifelse(is.na(vo_match),
            header_border,
            vodf_ol$border[vo_match])
         names(vodf_border) <- vodf$label;
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
   gridlegend_df <- data.frame(check.names=FALSE,
      stringsAsFactors=FALSE,
      set=names(setlist),
      size=lengths(setlist))

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
   }
   
   # optional alias
   if (length(alias) > 0) {
      if (!all(names(setlist) %in% names(alias))) {
         stop("names(alias) must contain all names(setlist)")
      }
      legend_df$legend <- paste0(
         alias[names(setlist)], " - ",
         names(setlist), ": ",
         jamba::formatInt(lengths(setlist)),
         " ",
         item_type)
      gridlegend_df <- data.frame(check.names=FALSE,
         stringsAsFactors=FALSE,
         set=alias[names(setlist)],
         label=names(setlist),
         size=lengths(setlist))
   }
   
   # render legend
   if ("base" %in% style && TRUE %in% draw_legend) {
      opar <- par("xpd"=TRUE);
      on.exit(par(opar))
      legend(x=x,
         box.lwd=box.lwd,
         inset=c(-0.05, 0),
         col=NA,
         pch=22,
         pt.bg=legend_df$color,
         pt.cex=2,
         border=NULL,
         legend=legend_df$legend,
         ...);
   } else if ("grid" %in% style) {
      # prepare graphical object
      gridlegend_df_use <- gridlegend_df;
      gridlegend_df_use$set <- paste0(gridlegend_df$set, ":");
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
      legend_grob <- gridExtra::tableGrob(
         rows=NULL,
         theme=table_theme,
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
            ind <- find_cell(legend_grob, 1, icol, "colhead-bg")
            if (length(ind) > 0) {
               legend_grob$grobs[ind][[1]][["gp"]] <- update_gpar_values(
                  gp=legend_grob$grobs[ind][[1]][["gp"]],
                  gp_list=list(fill=header_bg,
                     col=header_border,
                     lwd=lwd))
               # legend_grob$grobs[ind][[1]][["gp"]] <- grid::gpar(
               #    fill=header_bg,
               #    col=header_border,
               #    lwd=lwd)
               # legend_grob$grobs[ind][[1]][["gp"]] <- gpar_vlist;
            }
            ind <- find_cell(legend_grob, 1, icol, "colhead-fg")
            if (length(ind) > 0) {
               legend_grob$grobs[ind][[1]][["gp"]] <- update_gpar_values(
                  gp=legend_grob$grobs[ind][[1]][["gp"]],
                  gp_list=list(
                     col=header_color))
               # legend_grob$grobs[ind][[1]][["gp"]] <- grid::gpar(
               #    col=header_color)
            }
         }
         # iterate each row
         for (irow in seq_len(nrow(legend_df))) {
            for (icol in seq_len(ncol(gridlegend_df_use))) {
               ind <- find_cell(legend_grob, irow + 1, icol, "core-fg")
               for (ind0 in unique(ind)) {
                  legend_grob$grobs[ind0][[1]][["gp"]] <- update_gpar_values(
                     gp=legend_grob$grobs[ind0][[1]][["gp"]],
                     gp_list=list(
                        # fontface="bold",
                        col=jamba::setTextContrastColor(legend_df$color[irow])))
                  # legend_grob$grobs[ind0][[1]][["gp"]] <- grid::gpar(
                  #    # fontface="bold",
                  #    col=jamba::setTextContrastColor(legend_df$color[irow]))
               }
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
