
#' Curate Venn labels
#' 
#' Curate Venn labels
#' 
#' This venndir utility function is used to convert a basic
#' directional label such as `"0 1 0 -1"` suitable for display.
#' It can output either Unicode or non-Unicode text label,
#' or a corresponding color.
#' 
#' The input is defined in a `data.frame` obtained from
#' `get_venndir_curate_df()`, or using the user-provided `curate_df`.
#' 
#' Each sign returned by `signed_overlaps()`
#' is matched in the 'from' column, then replaced with values in the
#' 'sign' column, then colorized by the 'color' column.
#' 
#' Note that zeros `"0"` are typically removed before calling this function.
#'
#' The `curate_df` must contain these four colnames:
#' * `"from"` - regular expression patterns, which will be surrounded
#'    by `"^("` and `")$"` to ensure complete match.
#' * `"sign"` - `character` replacement for each value matched in `"from"`
#'    when `type="sign"`.
#' * `"color"` - `character` R color to assign to each value matched
#'    in `"from"`, when `type="color"`.
#' * `"hide_singlet"` - `logical` indicating whether signed singlet counts
#'    should be hidden, for example "agreement" has no meaning when
#'    only one set is involved.
#' 
#' Multiple signs are concatenated together, in the event the input overlap
#' has multiple values, for example `"1 1"` becomes `"^^"` with no
#' spacing.
#' 
#' To impose whitespace between sign characters, define `sign=c("^ ", "v ")`
#' to include whitespace. Any leading/trailing whitespace will be removed
#' afterwards.
#' 
#' @returns `vector` of labels or colors, based upon argument `type`,
#'    or when `type="all"` it returns `data.frame` with rows for each
#'    entry in `x`.
#' 
#' @family venndir advanced
#' 
#' @param x `vector` of overlap labels.
#' @param type `character` string, default 'type'.
#'    * `type="sign"` will curate `x` to sign
#'    * `type="color"` will curate `x` to color
#'    * `type="hide_singlet"` will indicate whether to hide singlet values,
#'    intended for values such as "agreement" which are not useful
#'    to display for singlet (single group) count values.
#'    * `type="all"` will return all the above as `data.frame`.
#' @param curate_df `data.frame` or `NULL` with optional curation 
#'    rules. The input is coerced to `data.frame` if necessary.
#'    The colnames are expected to include:
#'    * `"from"` - regular expression patterns
#'    * `"sign"` - replacement value when `type="sign"`
#'    * `"color"` - replacement R color when `type="color"`
#'    * `"hide_singlet"` - logical whether to display signed counts
#'    for singlet overlap sets.
#' @param unicode `logical` default TRUE, whether to use Unicode characters,
#'    passed to `get_venndir_curate_df`.
#' @param blend_preset `character` string passed as `preset` to
#'    `colorjam::blend_colors()` to define the color wheel used
#'    during color blending operations.
#' @param split `character` string used to split each "sign" in the input
#'    string `x`, assumed to be space character `" "`. This split is required
#'    to process replacement for each "sign" value without iteratively
#'    replacing values in `x` which can cause re-replacement of values
#'    which is not intended.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' venn_labels <- c("0 1 0 -1", "1 -1", "1 1 1", "mixed", "agreement", "1 1 0 0");
#' (curate_venn_labels(venn_labels, "sign"))
#' (curate_venn_labels(venn_labels, "sign", unicode=FALSE))
#' 
#' (curate_venn_labels(venn_labels, "color"))
#' 
#' (curate_venn_labels(venn_labels, "all"))
#' 
#' jamba::printDebug(as.list(curate_venn_labels(venn_labels, "sign")),
#'    collapse=", ",
#'    fgText=as.list(curate_venn_labels(venn_labels, "color")))
#' 
#' @export
curate_venn_labels <- function
(x,
 type=c("sign",
    "color",
    "hide_singlet",
    "all"),
 curate_df=NULL,
 unicode=TRUE,
 blend_preset="ryb",
 split=" ",
 ...)
{
   if (length(x) == 0) {
      return(x)
   }
   type <- match.arg(type);
   if ("all" %in% type) {
      #
      types <- jamba::nameVector(c("sign", "color", "hide_singlet"))
      out_df <- data.frame(lapply(types, function(itype){
         curate_venn_labels(x=x,
            type=itype,
            curate_df=curate_df,
            unicode=unicode,
            blend_preset=blend_preset,
            split=split,
            ...)
      }));
      return(out_df)
   }
   # 0.0.51.900 - move curate_df into get_venndir_curate_dr()
   if (inherits(curate_df, "data.frame") && nrow(curate_df) > 0) {
      # use curate_df
      if (!"color" %in% colnames(curate_df)) {
         curate_df$color <- "#000000";
      }
      if (!"hide_singlet" %in% colnames(curate_df)) {
         curate_df$hide_singlet <- FALSE;
      }
   } else {
      curate_df <- get_venndir_curate_df(unicode=unicode,
         ...)
   }

   # 0.0.27.900: process using positional matching
   # iterate each character from each value in x
   x_split <- jamba::rmNULL(strsplit(x, split=split),
      nullValue="")
   x_new <- lapply(x_split, function(ix){
      # iterate each character position
      ix_avail <- rep(TRUE, length.out=length(ix));
      for (i in seq_along(curate_df[,"from"])) {
         ifrom <- paste0("^(", curate_df[i, "from"], ")$");
         ix_match <- (grepl(ifrom, ix) & ix_avail);
         if (any(ix_match)) {
            ix[ix_match] <- gsub(ifrom,
               curate_df[i, type],
               ix[ix_match])
            ix_avail[ix_match] <- FALSE;
         }
      }
      ix
   })
   # combine entries
   if ("color" %in% type) {
      x <- jamba::cPaste(x_new, sep=" ")
   } else if ("sign" %in% type) {
      x <- gsub("^[ ]*|[ ]*$", "",
         jamba::cPaste(x_new, sep=""))
   } else if ("hide_singlet" %in% type) {
      x <- sapply(x_new, function(i) all(as.logical(i)));
      # x <- jamba::cPaste(x_new, sep=" ")
   }

   if ("color" %in% type) {
      x <- gsub("^[ ]+|[ ]+$", "",
         gsub("[ ]+", " ", x));
      # split into colors
      # replace non-colors with grey45
      x_colors <- jamba::rmNULL(nullValue="grey45",
         lapply(strsplit(x, " "), function(xc){
            xc[jamba::isColor(xc)]
         }))
      x <- jamba::rmNA(naValue="grey45",
         colorjam::blend_colors(x_colors,
            preset=blend_preset));
   }
   return(x);
}
