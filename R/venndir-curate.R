
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
#' @return `vector` of labels or colors, based upon argument `type`.
#' 
#' @family venndir utility
#' 
#' @param x `vector` of overlap labels.
#' @param type `character` string where `type="sign"` will curate
#'    `x` into directional sign, and `type="color"` will curate
#'    `x` into corresponding directional color.
#' @param curate_df `data.frame` or `NULL` with optional curation 
#'    rules. The colnames are expected to contain `c("from", "sign", "color")`.
#'    In this case, each row of `curate_df` is applied in order,
#'    and `gsub()` is called on each `"from"` and replaced using
#'    the value in column given by `type`.
#' @param unicode `logical` indicating whether to use Unicode characters
#'    when `type="sign"`. Note this argument only affects the default
#'    values, it is not applied when using a custom `curate_df`.
#' @param blend_preset `character` string passed as `preset` to
#'    `colorjam::blend_colors()` to define the color wheel used
#'    during color blending operations.
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
 ...)
{
   type <- match.arg(type);
   if (length(curate_df) == 0) {
      if (unicode) {
         curate_list <- list(
            c("[ ]*-1", "\u2193", "dodgerblue3"),
            c("[ ]*1", "\u2191", "firebrick"),
            c("[ ]*concordant|agreement", "\u21F6", "dodgerblue3"),
            c("[ ]*mixed", "\u2193\u2191", "grey45"));
            #c("[ ]*mixed", "\u21C6", "grey45"));
         curate_list <- list(
            c("-1", "\u2193", "dodgerblue3"),
            c("1", "\u2191", "firebrick"),
            c("0", "-", ""),
            #c("concordant|agreement", "\u21F6", "dodgerblue3"),
            c("concordant|agreement", "=", "dodgerblue3"),
            c("mixed", "X", "grey45"));
            #c("mixed", "\u2193\u2191", "grey45"));
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
      curate_df <- jamba::rbindList(curate_list)
      colnames(curate_df) <- c("from", "sign", "color");
   }
   for (i in seq_len(nrow(curate_df))) {
      x <- gsub(curate_df[i,"from"],
         curate_df[i,type],
         x);
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
   } else {
      x <- gsub(" ", "", x);
   }
   return(x);
}
