
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
#' @family venndir conversion
#' 
#' @return `Matrix` object with class `"ngCMatrix"` that contains
#'    contains `logical` values. When `do_sparse=FALSE` the returned
#'    object is `matrix` with values `c(0, 1)`.
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
#' @family venndir conversion
#' 
#' @return `Matrix` object that contains signed direction encoded
#'    as `c(-1, 0, 1)` values.
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
#' @examples
#' setlist <- make_venn_test(100, 3, do_signed=TRUE)
#' imv <- list2im_value(setlist);
#' print(head(imv));
#' 
#' # convert back to list
#' im_value2list(imv);
#' 
#' # make a simple character vector list
#' setlistv <- lapply(setlist, function(i){
#'    j <- letters[i+3];
#'    names(j) <- names(i);
#'    j;
#' })
#' imv <- list2im_value(setlistv);
#' print(head(imv));
#' im_value2list(imv);
#' 
#' @export
list2im_value <- function
(setlist,
 empty=NULL,
 do_sparse=TRUE,
 force_sign=FALSE,
 ...)
{
   setnames <- lapply(setlist, names);
   setnamesunion <- Reduce("union", setnames);
   
   # define empty when not defined
   if (length(empty) == 0) {
      #empty <- NA;
   } else {
      empty <- head(empty, 1);
   }
   setlistim <- do.call(cbind, lapply(setlist, function(i){
      i_match <- match(names(i), setnamesunion);
      j <- rep(NA, length(setnamesunion));
      if (force_sign) {
         j[i_match] <- sign(i);
      } else {
         j[i_match] <- i;
      }
      if (any(is.na(j)) && length(empty) == 0) {
         if (is.character(j)) {
            j[is.na(j)] <- "";
         } else {
            j[is.na(j)] <- 0;
         }
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

#' Value incidence matrix to list
#' 
#' Value incidence matrix to list
#' 
#' This function is the reciprocal to `list2im_value()`.
#' 
#' A value incidence matrix is a matrix whose non-empty values
#' are retained in each item vector, where items are stored as
#' vector names.
#' 
#' This function is most commonly used with signed values
#' `c(-1, 0, 1)`, to indicate direction where `1` is `up`,
#' `-1` is `down`, and `0` is `not changed`. In this case `0`
#' which is considered empty by default.
#' 
#' @return `list` whose names are set names derived from `colnames(x)`,
#'    and where each `vector` is named using `rownames(x)` and the
#'    values from `x`, for non-empty values.
#' 
#' @family venndir conversion
#' 
#' @examples
#' setlist <- make_venn_test(100, 3, do_signed=TRUE)
#' ims <- list2im_value(setlist);
#' print(head(ims));
#' 
#' # convert back to list
#' im_value2list(ims);
#' 
#' # make a simple character vector list
#' setlistv <- lapply(setlist, function(i){
#'    j <- letters[i+3];
#'    names(j) <- names(i);
#'    j;
#' })
#' imv <- list2im_value(setlistv);
#' print(head(imv));
#' im_value2list(imv);
#'
#' @export
im_value2list <- function
(x,
 empty=c(NA, "", 0),
 ...)
{
   # the reciprocal of list2im_value()
   x_rows <- rownames(x);
   x_cols <- colnames(x);
   l <- lapply(jamba::nameVector(x_cols), function(i){
      has_value <- (!x[,i] %in% empty);
      jamba::nameVector(x[has_value,i], x_rows[has_value], makeNamesFunc=c);
   });
   return(l);
}


#' Incidence matrix to list
#' 
#' Incidence matrix to list
#' 
#' This function is the reciprocal to `list2im()`. This function
#' will also convert a signed incidence matrix to a normal
#' list, removing the directional sign.
#' 
#' @family venndir conversion
#' 
#' @examples
#' setlist <- make_venn_test(100, 3, do_signed=TRUE)
#' ims <- list2im_value(setlist);
#' print(head(ims));
#' 
#' # convert back to list
#' im_value2list(ims);
#' im2list(ims);
#'
#' @export
im2list <- function
(x,
 empty=c(NA, "", 0, FALSE),
 ...)
{
   # the reciprocal of list2im()
   x_rows <- rownames(x);
   x_cols <- colnames(x);
   l <- lapply(jamba::nameVector(x_cols), function(i){
      i_empty <- as(empty, class(x[,i]));
      has_value <- (!x[,i] %in% i_empty);
      x_rows[has_value];
   });
   return(l);
}
