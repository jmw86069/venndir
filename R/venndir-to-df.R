
#' Convert venndir output to data.frame (experimental)
#' 
#' Convert venndir output to data.frame or kable format
#' for visual review, experimental.
#' 
#' ## Todo:
#' 
#' * Accept input from `signed_overlaps()` for purely table summary.
#' 
#' @family venndir internal
#' 
#' @returns object of class `"data.frame"` or `"knitr_kable"` based upon
#'    argument `return_type`.
#' 
#' @param venndir_out `list` output from `venndir()`
#' @param df_format `character` string, default "hits" with output format:
#'    * `"hits"` - returns a hit matrix, with the first column containing
#'    item labels, subsequent columns indicate "hits" with 0 or +1 or -1.
#'    * `"items"` - returns one column for each overlap, containing
#'    item labels in each corresponding column. This output does not
#'    include the sign.
#'    * `"wide"` - returns one row for each overlap_set/sign combination,
#'    and a column `"items"` which is a `list` of `character` vectors
#'    with the item labels. Rows may be split for "word wrapping"
#'    when there are too many items. This output is intended for
#'    `kable()` formatting with grouped rows, for example with argument
#'    `return_type="kable"`.
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
#' setlist <- venndir::make_venn_test(100, 3, do_signed=TRUE);
#' venndir_out <- venndir::venndir(setlist, overlap_type="each")
#' df <- venndir_to_df(venndir_out)
#' head(df, 10)
#' 
#' kdf <- venndir_to_df(venndir_out, return_type="kable")
#' kdf
#' 
#' df2 <- venndir_to_df(venndir_out, df_format="items")
#' head(df2, 10)
#' 
#' kdf2 <- venndir_to_df(venndir_out, df_format="items", return_type="kable")
#' kdf2
#' 
#' df3 <- venndir_to_df(venndir_out, df_format="wide", return_type="data.frame")
#' df3
#' 
#' kdf3 <- venndir_to_df(venndir_out, df_format="wide", return_type="kable")
#' kdf3
#' 
#' @export
venndir_to_df <- function
(venndir_out,
 df_format=c(
    "hits",
    "items",
    "wide"),
 return_type=c("data.frame",
    "kable"),
 trim_blanks=TRUE,
 wrap_width=80,
 colorize_headers=TRUE,
 set_colors=NULL,
 item_type="gene",
 add_counts=TRUE,
 verbose=FALSE,
 ...)
{
   # validate arguments
   df_format <- match.arg(df_format);
   return_type <- match.arg(return_type);

   # validate input
   if ("list" %in% class(venndir_out)) {
      if ("vo" %in% names(venndir_out)) {
         venndir_out <- venndir_out$vo;
      } else {
         stop("list input must contain Venndir object in 'vo'.")
      }
   }
   if ("data.frame" %in% class(venndir_out)) {
      if (!"items" %in% colnames(venndir_out)) {
         stop("data.frame input must contain 'items' column.");
      }
      # assume output is from textvenn()
      # label_df <- venndir_out;
      if (!"overlap_sign" %in% colnames(venndir_out)) {
         venndir_out[, "overlap_sign"] <- rownames(venndir_out);
      }
      label_df <- venndir_out;
      vennlist <- venndir_out$items;
      names(vennlist) <- rownames(venndir_out);
   } else if ("Venndir" %in% class(venndir_out)) {
      label_df <- venndir_out@label_df;
      vennlist <- label_df$items;
      names(vennlist) <- label_df$overlap_sign;
      vennlist <- vennlist[grepl("[|]", names(vennlist))];
   } else {
      stop(paste0("Input must be Venndir, ",
         "or data.frame output from textvenn() or signed_overlaps()"))
   }
   
   # Define missing set_colors
   if (length(set_colors) == 0) {
      if ("Venndir" %in% class(venndir_out)) {
         set_colors <- jamba::nameVector(
            subset(venndir_out@jps@polygons,
               type %in% "overlap")[, c("venn_color", "label")]);
      } else if ("color" %in% colnames(label_df)) {
         set_colors <- jamba::nameVector(renameFirst=FALSE,
            unique(label_df[, c("color", "sets")]));
      }
   }
   
   ## Hits format produces an incidence matrix of hits
   if ("hits" %in% df_format) {
      vln <- rep(names(vennlist),
         lengths(vennlist));
      vlv <- jamba::rbindList(lapply(strsplit(gsub("^.+[|]", "", vln), " "), as.numeric))
      rownames(vlv) <- unname(unlist(vennlist))
      colnames(vlv) <- strsplit(gsub("[|].+", "",
         head(unique(jamba::unvigrep("[|].*0", vln)), 1)), "&")[[1]]
      vlvdf <- data.frame(check.names=FALSE,
         item=rownames(vlv),
         vlv);
      colnames(vlvdf)[1] <- head(item_type, 1);
      if ("kable" %in% return_type) {
         kvlvdf <- jamba::kable_coloring(vlvdf,
            format.args=list(trim=TRUE, big.mark=","),
            format="html",
            row.names=FALSE,
            colorSub=c(set_colors,
               `-1`="dodgerblue",
               `1`="firebrick3"),
            ...)
         return(kvlvdf);
      }
      if ("data.frame" %in% return_type) {
         return(vlvdf);
      }
   }
   
   # Items format produces a table with items in each Venn overlap by column
   if ("items" %in% df_format) {
      vln <- names(vennlist);
      if (any(grepl("[|].*-1", vln))) {
         # goal: remove directionality for this purpose
         while(any(grepl("[|].*-1", vln))) {
            vln <- gsub("([|].*)-1", "\\11", vln);
         }
         vln <- factor(vln, levels=unique(vln))
         vennlist <- split(unname(unlist(vennlist)), rep(vln, lengths(vennlist)))
         # jamba::printDebug("sdim(vennlist):");print(jamba::sdim(vennlist));# debug
      }
      if (!length(vennlist) %in% c(3, 7)) {
         stop("This function only supports 2-way Venn without direction.")
      }
      #
      if (length(vennlist) == 3) {
         # 2-way Venn
         vennorder <- c(1, 3, 2)
      } else if (length(vennlist) == 7) {
         # 3-way Venn
         vennorder <- c(1, 4, 2, 6, 3, 5, 7);
      }
      vl <- vennlist[vennorder];
      vdf <- data.frame(do.call(cbind, lapply(vl, function(i){
         x <- rep("", max(lengths(vennlist)));
         x[seq_along(i)] <- i;
         x
      })))
      colnames(vdf) <- gsub("[|].+", "", names(vl));
      # Optionally append the number of items to each header
      if (TRUE %in% add_counts) {
         vcts <- sapply(vdf, function(i){
            sum(!i %in% "")
         })
         fromcols <- colnames(vdf);
         tocols <- paste0(fromcols,
            " (", vcts,
            " ", item_type,
            ifelse(vcts == 1, "", "s"),
            ")")
         colnames(vdf) <- tocols;
         k <- intersect(fromcols, names(set_colors));
         if (length(k) > 0) {
            ks <- set_colors[k];
            names(ks) <- tocols[match(k, fromcols)]
            set_colors[names(ks)] <- ks;
         }
      }
      if ("kable" %in% return_type) {
         kvdf <- jamba::kable_coloring(vdf,
            format.args=list(trim=TRUE, big.mark=","),
            format="html",
            row.names=FALSE,
            colorSub=c(set_colors,
               `-1`="dodgerblue",
               `1`="firebrick3"),
            ...)
         return(kvdf);
      }
      return(vdf)
   }
   
   ## Wide format, perhaps suitable for RMarkdown
   if ("wide" %in% df_format) {
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
         format.args=list(trim=TRUE, big.mark=","),
         format="html",
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
   }

   if (FALSE) {
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
   }
         # label_row_css=paste0(
         #    "background-color: ", bg_color, ";",
         #    "color: ", fg_color, ";"),
   
}
