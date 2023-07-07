
#' Curate Venn labels
#' 
#' Curate Venn labels
#' 
#' This venndir utility function is used to convert a basic
#' directional label such as `"0 1 0 -1"` suitable for display.
#' It can output either Unicode or non-Unicode text label,
#' or a corresponding color.
#' 
#' The input currently recognizes directional labels such as
#' `"0 -1 1 0"` or the character labels `"agreement"`, `"concordant"`,
#' `"mixed"`. Note that zeros `"0"` are typically removed
#' before calling this function.
#'
#' The input vector `x` is split using `strsplit()` using whitespace
#' delimiter by default, then each full value is matched and replaced
#' using `"from"` in `curate_df`.
#'
#' When `curate_df` is not supplied, default values are generated
#' for `"from"` values: `"1"`, `"-1"`, `"0"`, `"concordant|agreement"`,
#' and `"mixed"`. Values are matched using regular expression `gsub()`
#' however the full string must match, therefore `from="1"` will only
#' match `"1"` and will not match `"-1"`.
#'
#' The `curate_df` must contain these three colnames:
#' * `"from"` - regular expression patterns, which will be surrounded
#'    by `"^("` and `")$"` to ensure complete match.
#' * `"sign"` - `character` replacement for each value matched in `"from"`
#'    when `type="sign"`.
#' * `"color"` - `character` R color to assign to each value matched
#'    in `"from"`, when `type="color"`.
#'
#' When two or more replacement values defined by `curate_df[,"sign"]` are
#' present in one entry in `x`, the values are concatenated together
#' with no whitespace. For example `"1 1"` becomes `"^^"` with no spacing.
#' To impose whitespace between characters, define `sign=c("^ ", "v ")`
#' to include whitespace. Any leading/trailing whitespace will be removed
#' afterwards.
#' 
#' @return `vector` of labels or colors, based upon argument `type`.
#' 
#' @family venndir utility
#' 
#' @param x `vector` of overlap labels.
#' @param type `character` string where `type="sign"` will curate
#'    `x` into directional sign, and `type="color"` will curate
#'    `x` into corresponding directional color.
#' @param curate_df `data.frame` or `NULL` with optional curation 
#'    rules. The input is coerced to `data.frame` if necessary.
#'    The colnames are expected to include:
#'    * `"from"` - regular expression patterns
#'    * `"sign"` - replacement value when `type="sign"`
#'    * `"color"` - replacement R color when `type="color"`
#' @param unicode `logical` indicating whether to use Unicode characters
#'    when `type="sign"`. Note this argument only affects the default
#'    values, it is not applied when using a custom `curate_df`.
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
#' options("warn"=-1); # make them stop
#' 
#' venn_labels <- c("0 1 0 -1", "1 -1", "1 1 1", "mixed", "agreement", "1 1 0 0");
#' (curate_venn_labels(venn_labels, "sign"))
#' (curate_venn_labels(venn_labels, "sign", unicode=FALSE))
#' 
#' (curate_venn_labels(venn_labels, "color"))
#' 
#' jamba::printDebug(as.list(curate_venn_labels(venn_labels, "sign")),
#'    collapse=", ",
#'    fgText=as.list(curate_venn_labels(venn_labels, "color")))
#' 
#' @export
curate_venn_labels <- function
(x,
 type=c("sign", "color"),
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
   if (length(curate_df) == 0) {
      if (2 %in% unicode) {
         curate_list <- list(
            c("-1", "\u2193", "dodgerblue3"),
            c("1", "\u2191", "firebrick"),
            c("concordant|agreement", "\u21F6", "dodgerblue3"),
            #c("[ ]*mixed", "\u2193\u2191", "grey45"));
            #c("[ ]*mixed", "\u21C6", "grey45"));
            c("mixed", "\u2194", "grey45"));
      } else if (1 %in% unicode) {
         curate_list <- list(
            c("-1", "\u2193", "dodgerblue3"),
            c("1", "\u2191", "firebrick"),
            c("0", "-", ""),
            c("concordant|agreement", "=", "dodgerblue3"),
            c("mixed", "X", "grey45"));
            #c("mixed", "\u21C6", "grey45"));
      } else {
         curate_list <- list(
            c("[ ]*-1", "v", "dodgerblue3"),
            c("[ ]*1", "^", "firebrick"), # somehow ^ is not supported
            c("[ ]*concordant|agreement", ">>>", "dodgerblue3"),
            c("[ ]*mixed", ">|<", "grey45"));
         curate_list <- list(
            c("-1", "v", "dodgerblue3"),
            c("1", "^", "firebrick"),
            c("0", "-", ""),
            c("concordant|agreement", ">>>", "dodgerblue3"),
            c("mixed", "X", "grey45"));
      }
      curate_df <- data.frame(check.names=FALSE,
         stringsAsFactors=FALSE,
         jamba::rbindList(curate_list))
      colnames(curate_df) <- c("from", "sign", "color");
   } else {
      if (!"data.frame" %in% class(curate_df)) {
         curate_df <- data.frame(check.names=FALSE,
            stringsAsFactors=FALSE,
            curate_df);
      }
      if (!"color" %in% colnames(curate_df)) {
         curate_df[,"color"] <- "#000000";
      }
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
   } else {
      x <- gsub("^[ ]*|[ ]*$", "",
         jamba::cPaste(x_new, sep=""))
   }
   
   # 0.0.26.900: process using gsub() which can re-replace certain character
   # for (i in seq_len(nrow(curate_df))) {
   #    x <- gsub(curate_df[i,"from"],
   #       curate_df[i,type],
   #       x);
   # }
   
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
