
#' Curate Venn labels
#' 
#' Curate Venn labels
#' 
#' @family venndir utility
#' 
#' @export
curate_venn_labels <- function
(x,
 type=c("sign", "color"),
 curate_df=NULL,
 unicode=TRUE,
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
            c("concordant|agreement", "\u21F6", "dodgerblue3"),
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
      x <- jamba::rmNA(naValue="grey45",
         colorjam::blend_colors(jamba::uniques(strsplit(x, " "))));
   } else {
      x <- gsub(" ", "", x);
   }
   return(invisible(x));
}
