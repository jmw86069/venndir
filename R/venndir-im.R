
#' Optimized list to incidence matrix
#' 
#' Optimized list to incidence matrix
#' 
#' This function rapidly converts a list of vectors into
#' an incidence matrix with value of `1` for each entry
#' (row) present in each input list (column).
#' 
#' Note that the rows in the output matrix are not sorted,
#' since this step can take several seconds when working with
#' a list whose vectors contain millions of rows.
#' 
#' @param setlist `list` of vectors
#' @param empty default single value used for empty/missing entries,
#'    the default `empty=0` uses zero for entries not present.
#'    Another alternative is `NA`.
#' @param do_sparse `logical` indicating whether to coerce the output
#'    to sparse matrix class `"ngCMatrix"` from the Matrix package.
#' @param ... additional arguments are ignored.
#' 
#' @export
list2im_opt <- function
(setlist,
 empty=0,
 do_sparse=TRUE,
 ...)
{
   setnamesunion <- Reduce("union", setlist);
   if (length(empty) == 0) {
      empty <- NA;
   } else {
      empty <- head(empty, 1);
   }
   setlistim <- do.call(cbind, lapply(setlist, function(i){
      i_match <- match(i, setnamesunion);
      j <- rep(empty,
         length(setnamesunion));
      j[i_match] <- 1;
      j;
   }))
   rownames(setlistim) <- setnamesunion;
   if (do_sparse && suppressPackageStartupMessages(require(Matrix))) {
      setlistim <- as(setlistim, "ngCMatrix");
   }
   return(setlistim);
}


#' Optimized list to signed incidence matrix
#' 
#' Optimized list to signed incidence matrix
#' 
#' This function converts a list of named vectors into
#' an incidence matrix with value for each entry
#' (row) present in each input list (column). The rows
#' are defined by the vector names, and values are
#' defined by the vector values.
#' 
#' Note that this function will store zero `0` when the input
#' vector value is zero. When this is not the desired behavior,
#' the argument `empty` can be used to distinguish missing data
#' from data that is zero, for example by setting `empty=NA`.
#' In this way a value of zero `0` indicates "present but zero",
#' and a value `NA` indicates "not present at all". This
#' distinction is helpful when comparing entities which are not
#' tested in each scenario. For example if "geneA" is present
#' and the value is `1` in one list; "geneA" is not tested in
#' the second list; therefore the absence of "geneA" of a non-zero
#' value in the second list is not counted as "non-overlapping"
#' because it was not possible for it to have a non-zero value.
#' 
#' @param setlist `list` of vectors
#' @param empty default single value used for empty/missing entries,
#'    the default `empty=0` uses zero for entries not present.
#'    Another alternative is `NA`.
#' @param do_sparse `logical` indicating whether to coerce the output
#'    to sparse matrix class `"ngCMatrix"` from the Matrix package.
#' @param coerce_sign `logical` indicating whether to coerce numeric
#'    vector values to the sign. When `coerce_sign=FALSE` the vector
#'    values are stored directly. When `coerce_sign=TRUE` the signs of
#'    the vector values are stored.
#' @param ... additional arguments are ignored.
#' 
#' @export
list2im_signed <- function
(setlist,
 empty=0,
 do_sparse=TRUE,
 force_sign=FALSE,
 ...)
{
   setnames <- lapply(setlist, names);
   setnamesunion <- Reduce("union", setnames);
   if (length(empty) == 0) {
      empty <- NA;
   } else {
      empty <- head(empty, 1);
   }
   setlistim <- do.call(cbind, lapply(setlist, function(i){
      i_match <- match(names(i), setnamesunion);
      j <- rep(empty, length(setnamesunion));
      if (force_sign) {
         j[i_match] <- sign(i);
      } else {
         j[i_match] <- i;
      }
      j;
   }))
   rownames(setlistim) <- setnamesunion;
   if (!is.character(setlistim[1,1]) &&
         do_sparse &&
         suppressPackageStartupMessages(require(Matrix))) {
      setlistim <- as(setlistim, "dgCMatrix");
   }
   return(setlistim);
}
