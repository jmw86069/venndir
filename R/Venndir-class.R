
#' Check Venndir object
#' 
#' Check Venndir object integrity
#' 
#' ## Todo:
#' 
#' * Write validation function.
#' * Consider whether to have `jp` (sets), `jps` (overlaps) in separate ojbects
#' 
#' @export
#' 
#' @param object `Venndir` object
#' 
#' @export
check_Venndir <- function
(object)
{
   # basic check to confirm all slot names exist
   # all(c("jp", "jps", "label_df", "setlist") %in% slotNames(object))
   is_valid <- (c("jps", "label_df", "setlist") %in% slotNames(object))
   # jamba::printDebug("is_valid:", is_valid);
   all(is_valid)
}

#' Venndir class
#' 
#' Venndir class
#' 
setClass("Venndir",
   slots=c(
      # jp="JamPolygon",
      jps="JamPolygon",
      label_df="data.frame",
      setlist="list"
   ),
   prototype=prototype(
      jps=NULL,
      label_df=data.frame(),
      setlist=list()
   ),
   validity=check_Venndir
);

# Todo:
# * print(), summary() functions
# * plot function as wrapper to render_venndir()
