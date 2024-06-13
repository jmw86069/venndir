
#' Convert venndir output to data.frame
#' 
#' Convert venndir output to data.frame or kable format
#' for visual review.
#' 
#' ## Todo:
#' 
#' * Accept input from `signed_overlaps()` for purely table summary.
#' 
#' @family venndir utility
#' 
#' @returns object of class `"data.frame"` or `"knitr_kable"` based upon
#'    argument `return_type`.
#' 
#' @param venndir_out `list` output from `venndir()`
#' @param return_type `character` string indicating how to format output:
#'    * `"kable"`: returns an object `"knitr_kable"` suitable for printing
#'    as HTML or LaTeX. This output is recommended for RMarkdown documents.
#'    * `"data.frame"`: returns a `data.frame` object without colorization.
#' @param trim_blanks `logical` whether to remove rows with 0 items.
#' @param wrap_width `numeric` maximum width for item text before it
#'    is word-wrapped. To disable word-wrapping use `wrap_width=Inf`.
#' @param colorize_headers `logical` indicating whether to color the
#'    background of each header by the `set_colors`.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' setlist <- venndir::make_venn_test(100, 3);
#' venndir_out <- venndir::venndir(setlist, overlap_type="each")
#' kdf <- venndir_to_df(venndir_out)
#' kdf
#' 
#' kdf <- venndir_to_df(venndir_out, return_type="data.frame")
#' kdf
#' 
#' @export
venndir_to_df <- function
(venndir_out,
 return_type=c("kable",
    "data.frame"),
 trim_blanks=TRUE,
 wrap_width=80,
 colorize_headers=TRUE,
 set_colors=NULL,
 ...)
{
   # validate arguments
   return_type <- match.arg(return_type);
   # label_df
   if (!"Venndir" %in% class(venndir_out) && "vo" %in% names(venndir_out)) {
      venndir_out <- venndir_out$vo;
   }
   label_df <- venndir_out@label_df;
   # encode factor to help sort properly
   label_df$overlap_set <- factor(label_df$overlap_set,
      levels=unique(label_df$overlap_set))
   label_df <- jamba::mixedSortDF(label_df,
      byCols=c("overlap_set", "text"))
   
   # spdf
   if (length(set_colors) == 0) {
      set_colors <- jamba::nameVector(
         subset(venndir_out@jps@polygons,
            type %in% "overlap")[, c("venn_color", "label")]);
   }
   
   # label text color
   text_color <- farver::raise_channel(
      farver::cap_channel(
         label_df$color,
         space="hcl",
         channel="l",
         value=45),
      space="hcl",
      channel="c",
      value=70)
   
   # display_colnames
   display_colnames <- c("overlap_set",
      "text",
      "overlap_sign",
      "items");
   df <- label_df[, display_colnames, drop=FALSE];
   
   # wrap item labels
   item_label <- jamba::cPasteSU(lapply(df$items, c), sep=", ")
   item_labels <- lapply(item_label, function(i){
      strwrap(i,
         width=wrap_width)
   })
   row_seq <- rep(seq_len(nrow(df)),
      lengths(item_labels));
   dftall <- df[row_seq, , drop=FALSE]
   dftall$items <- unname(unlist(item_labels));
   dftall$text_color <- rep(text_color,
      lengths(item_labels));
   # dftall
   
   if (TRUE %in% trim_blanks) {
      blank_lines <- (nchar(dftall$overlap_sign) > 0 &
            nchar(dftall$items) == 0)
      if (any(blank_lines)) {
         dftall <- subset(dftall, !blank_lines);
      }
   }
   
   # repair set names with "<br>"
   if (any(grepl("<br.*>|\n", ignore.case=TRUE, dftall$overlap_set))) {
      dftall$overlap_set <- gsub("[ ]+", " ",
         gsub("<br[ /]*>|\n", " ",
            ignore.case=TRUE,
            dftall$overlap_set))
      names(set_colors) <- gsub("[ ]+", " ",
         gsub("<br[ /]*>|\n", " ",
            ignore.case=TRUE,
            names(set_colors)))
   }
   if (any(grepl("<br.*>|\n", ignore.case=TRUE, dftall$overlap_sign))) {
      dftall$overlap_sign <- gsub("[ ]+", " ",
         gsub("<br[ /]*>|\n", " ",
            ignore.case=TRUE,
            dftall$overlap_sign))
   }
   
   if ("data.frame" %in% return_type) {
      return(dftall[, 1:4])
   }

   # split by set, then subset by set/direction
   kdftall <- jamba::kable_coloring(
      dftall[, "items", drop=FALSE],
      col.names=NULL,
      row.names=FALSE);
   kdftall <- kableExtra::column_spec(kdftall,
      column=1,
      color=jamba::unalpha(dftall$text_color))
   for (igroup in as.character(unique(dftall$overlap_set))) {
      from_to <- which(dftall$overlap_set %in% igroup);
      bg_color <- set_colors[igroup];
      fg_color <- jamba::setTextContrastColor(bg_color);
      kdftall <- kableExtra::pack_rows(
         kdftall,
         group_label=igroup,
         hline_before=TRUE,
         label_row_css=paste0(
            "background-color: ", bg_color, ";",
            "color: ", fg_color, ";"),
         color=fg_color, background=bg_color,
         start_row=min(from_to), end_row=max(from_to))
   }
   row_group <- unname(jamba::pasteByRow(sep="!!!",
      dftall[, c("overlap_set", "text"), drop=FALSE]))
   for (igroup in unique(row_group)) {
      from_to <- which(row_group %in% igroup);
      igroup1 <- gsub("!!!.+", "", igroup)
      igroups <- strsplit(igroup, "!!!")[[1]]
      bg_color <- set_colors[igroup1];
      fg_color <- jamba::setTextContrastColor(bg_color);
      kdftall <- kableExtra::group_rows(
         kdftall,
         group_label=gsub("!!!", "   ", igroup),
         hline_before=TRUE,
         label_row_css=paste("border-bottom: 1px solid;",
            "border-bottom-color: #000;",
            "color:", fg_color, ";",
            "background-color:", bg_color, ";",
            "text-indent: 1.2em;"),
         color=fg_color, background=bg_color,
         start_row=min(from_to), end_row=max(from_to))
   }
   return(kdftall);

   # alternative method that only splits by set/direction
   kdftall <- jamba::kable_coloring(
      dftall[, -1, drop=FALSE],
      row.names=FALSE);
   row_group <- unname(jamba::pasteByRow(sep="!!!",
      dftall[, c("overlap_set", "text"), drop=FALSE]))
   for (igroup in unique(row_group)) {
      from_to <- which(row_group %in% igroup);
      igroup1 <- gsub("!!!.+", "", igroup)
      igroups <- strsplit(igroup, "!!!")[[1]]
      bg_color <- set_colors[igroup1];
      fg_color <- jamba::setTextContrastColor(bg_color);
      kdftall <- kableExtra::group_rows(
         kdftall,
         group_label=gsub("!!!", "   ", igroup),
         hline_before=TRUE,
         label_row_css=paste("border-bottom: 1px solid;",
            "border-bottom-color: #000;",
            "color:", fg_color, ";",
            "background-color:", bg_color, ";",
            "text-indent: 1.2em;"),
         color=fg_color, background=bg_color,
         indent=TRUE,
         start_row=min(from_to), end_row=max(from_to))
   }
         # label_row_css=paste0(
         #    "background-color: ", bg_color, ";",
         #    "color: ", fg_color, ";"),
   
}
