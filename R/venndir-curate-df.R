
#' Define Venndir sign and symbol curation
#' 
#' Define Venndir 'curate_df' data for custom signs and symbols
#' 
#' This function defines the `data.frame` used for the argument
#' `curate_df` in `venndir()`. The purpose is to make it more
#' convenient to review and potentially modify the default values.
#' 
#' @family venndir signs
#' 
#' @returns `data.frame` with colnames:
#'    * 'from' - `character` sign to match
#'    * 'color' - `character` color associated with matched values
#'    * 'sign' - `character` replacement of the matched value in 'from'
#'    * 'hide_singlet' - `logical` indicating whether to hide the signed
#'    count value when the overlap is a singlet, for example "agreement"
#'    has no meaning when there is only one set involved.
#' 
#' @param unicode `logical` default TRUE, whether to use unicode output
#'    values. Optionally `unicode=2` will return an alternative set of
#'    Unicode values.
#' @param use_color `logical` default TRUE, whether to apply color,
#'    a convenient method to use dark grey.
#' @param curate_df `data.frame` with optional user-defined values,
#'    which are used in place of corresponding default values in the
#'    'from' column.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' # default
#' get_venndir_curate_df()
#' 
#' # alternate Unicode
#' get_venndir_curate_df(unicode=2)
#' 
#' # non-unicode
#' get_venndir_curate_df(unicode=FALSE)
#' 
#' # custom curate_df
#' my_curate_df <- data.frame(from="1", sign="^", color="red")
#' get_venndir_curate_df(unicode=TRUE, curate_df=my_curate_df)
#' @export
get_venndir_curate_df <- function
(unicode=TRUE,
 use_color=TRUE,
 curate_df=NULL,
 ...)
{
   #
   if (TRUE %in% unicode) {
      # Unicode standard
      curate_list1 <- list(
         c("-1",         "\u2193", "dodgerblue3", FALSE), # downArrow
         c("1",          "\u2191", "firebrick",   FALSE), # upArrow
         c("0",          "-",      "", FALSE),            # no sign
         c("concordant", "=",      "dodgerblue3", TRUE),  # equal sign
         c("agreement",  "=",      "dodgerblue3", TRUE),  # equal sign
         c("mixed",      "X",      "grey45",     FALSE)   # uppercase X
      );
   } else if (2 %in% unicode) {
      # Alternate unicode
      curate_list1 <- list(
         c("-1",         "\u2193", "dodgerblue3", FALSE), # downArrow
         c("1",          "\u2191", "firebrick",   FALSE), # upArrow
         c("0",          "-",      "",            FALSE), # no sign
         c("concordant", "\u2713", "dodgerblue3", TRUE),  # check mark
         c("agreement",  "\u2713", "dodgerblue3", TRUE),  # check mark
         c("mixed",      "\u2715", "grey45",     FALSE)   # X mark
      )
   } else {
      # Non-unicode
      curate_list1 <- list(
         c("-1",         "v", "dodgerblue3", FALSE), # downArrow
         c("1",          "^", "firebrick",   FALSE), # upArrow
         c("0",          "-", "",            FALSE), # no sign
         c("concordant", "=", "dodgerblue3", TRUE),  # equal sign
         c("agreement",  "=", "dodgerblue3", TRUE),  # equal sign
         c("mixed",      "X", "grey45",      FALSE)  # X mark
      )
   }
   # optionally remove color
   if (FALSE %in% use_color) {
      curate_df$color <- "grey30";
   }
   
   # convert to data.frame
   new_curate_df <- data.frame(check.names=FALSE,
      stringsAsFactors=FALSE,
      jamba::rbindList(curate_list1))
   colnames(new_curate_df) <- c("from",
      "sign",
      "color",
      "hide_singlet");
   # optionally use user-supplied curate_df
   if (inherits(curate_df, "data.frame") && nrow(curate_df) > 0) {
      if (!all(c("from", "sign", "color") %in% colnames(curate_df))) {
         stop("User-supplied curate_df must contain colnames: from,sign,color")
      }
      if (!"hide_singlet" %in% colnames(curate_df)) {
         curate_df$hide_singlet <- FALSE;
      }
      new_curate_df <- rbind(
         curate_df[, c("from", "sign", "color", "hide_singlet"), drop=FALSE],
         new_curate_df)
      # only use the first entry in case duplicated
      new_curate_df <- subset(new_curate_df, !duplicated(from));
   }
   return(new_curate_df)
}

