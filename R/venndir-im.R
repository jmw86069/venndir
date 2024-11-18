
#' Optimized list to incidence matrix
#' 
#' Optimized conversion of list to incidence matrix
#' 
#' This function rapidly converts a list of vectors into
#' an incidence matrix whose rownames are items, and colnames
#' are the names of the input list. The default output 
#' `do_sparse=TRUE` returns a `logical` matrix class `ngCMatrix`
#' from the `Matrix` package. When `do_sparse=FALSE` the
#' output is a `matrix` class with `numeric` values `0` and `1`.
#' 
#' Note that the rows in the output matrix are not sorted,
#' since the step of sorting item names may take several
#' seconds when working with a list whose vectors contain
#' millions of items. For sorted rows, the best remedy is
#' to run this function, the re-order rownames afterward.
#' 
#' @family venndir conversion
#' 
#' @return `matrix` object with value `c(0, 1)` when `do_sparse=FALSE`
#'    (default), or when `do_sparse=TRUE`, it returns a `Matrix` object
#'    class `"CsparseMatrix"` with `logical` values, only when
#'    `Matrix` is available.
#' 
#' @param setlist `list` of vectors
#' @param empty default single value used for empty/missing entries,
#'    the default `empty=0` uses zero for entries not present.
#'    Another alternative is `NA`.
#'    Providing a `character` value will convert the output to
#'    a `character` matrix, be warned.
#' @param do_sparse `logical` indicating whether to coerce the output
#'    to sparse matrix class `"CsparseMatrix"` from the Matrix package.
#'    The default is `FALSE` as of version 0.0.33.900, since the
#'    most common use case requires a regular matrix.
#'    For extremely large data, consider using a sparse matrix.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' setlist <- list(A=c("one", "two", "three"),
#'    b=c("two", "one", "four", "five"));
#' list2im_opt(setlist);
#' 
#' list2im_opt(setlist, do_sparse=TRUE);
#' 
#' @importFrom methods as
#' 
#' @export
list2im_opt <- function
(setlist,
 empty=0,
 do_sparse=FALSE,
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
   if (TRUE %in% do_sparse &&
         requireNamespace("Matrix", quietly=TRUE)) {
      setlistim <- as(
         as(
            as(setlistim,
               "nMatrix"),
            "generalMatrix"),
         "CsparseMatrix");
   }
   return(setlistim);
}


#' Convert list to a value incidence matrix
#' 
#' Optimized conversion of list to a value incidence matrix
#' 
#' This function converts a list of named vectors into
#' an incidence matrix with value for each entry
#' (row) present in each input list (column). This output
#' is called a "value incidence matrix" because the
#' value itself is included in the matrix as opposed to
#' a true incidence matrix that represents only `TRUE`
#' or `FALSE` (`1` or `0`) at each position.
#' 
#' The rownames of the output matrix represent items,
#' encoded by the vector names. The colnames of the output
#' matrix represent the list names.
#' 
#' The default value in the output matrix is `0` for
#' a numeric matrix, and `""` for a character matrix,
#' based on the input vector classes.
#' 
#' To override this behavior, use the argument `empty`.
#' For example, it may be useful to encode missing entries
#' as `NA`, which means "this entry was not observed", and
#' store true values of `0` to indicate "this entry was observed
#' and its value was `0` zero". In this case use `empty=NA`.
#' 
#' This behavior can be useful with gene expression data,
#' when a particular gene may not be observed in all data sets.
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
#'    The default is `FALSE`.
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
#' # example showing empty=NA
#' setlist2 <- make_venn_test(20, 3, sizes=c(12, 14, 8), do_signed=TRUE)
#' setlist2 <- lapply(setlist2, function(i){
#'    i[] <- sample(c(-1, 0, 1),
#'       replace=TRUE,
#'       size=length(i));
#'    i
#' })
#' imv2 <- list2im_value(setlist2, empty=NA);
#' imv2;
#' 
#' # to convert back to list, define empty=NA so 0 is not considered as empty
#' im_value2list(imv2, empty=NA);
#' 
#' # make a simple character vector list
#' setlistv <- lapply(setlist, function(i){
#'    j <- letters[i+3];
#'    names(j) <- names(i);
#'    j;
#' })
#' imv <- list2im_value(setlistv);
#' print(head(imv));
#' 
#' # convert back to list of character vectors
#' im_value2list(imv);
#' 
#' @export
list2im_value <- function
(setlist,
 empty=NULL,
 do_sparse=FALSE,
 force_sign=FALSE,
 ...)
{
   setnames <- lapply(setlist, names);
   setnamesunion <- Reduce("union", setnames);
   
   # check for any character or factor input
   # so the resulting im will use consistent empty values
   setlist_hascharacter <- any(sapply(setlist, function(i){
      is.character(i) | is.factor(i)
   }))
   
   # define empty when not defined
   if (length(empty) == 0) {
      if (TRUE %in% setlist_hascharacter) {
         empty <- ""
      } else {
         empty <- 0
      }
   } else {
      empty <- head(empty, 1);
   }
   
   setlistim <- do.call(cbind, lapply(setlist, function(i){
      i_match <- match(names(i), setnamesunion);
      j <- rep(NA, length(setnamesunion));
      if (TRUE %in% force_sign) {
         if (!is.numeric(i)) {
            stop("force_sign=TRUE but data is not numeric.")
         }
         j[i_match] <- sign(i);
      } else {
         if (is.factor(i)) {
            warning(paste0("list2im_value()",
               " coerced some input values from factor to character."))
            j[i_match] <- as.character(i);
         } else {
            j[i_match] <- i;
         }
      }
      if (any(is.na(j))) {
         j[is.na(j)] <- empty;
      }
      j;
   }))
   rownames(setlistim) <- setnamesunion;
   if (!is.character(setlistim[1,1]) &&
         TRUE %in% do_sparse &&
         requireNamespace("Matrix", quietly=TRUE)) {
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
#' @param x `matrix` where non-empty values indicate presence of each
#'    element (row) in each set (column).
#' @param empty `list` of values recognized as empty. Each item is
#'    co-erced to the class in columns of `x`.
#' @param ... additional arguments are ignored.
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
 empty=list(NA, "", 0, FALSE),
 ...)
{
   # the reciprocal of list2im()
   x_rows <- rownames(x);
   x_cols <- colnames(x);
   l <- lapply(jamba::nameVector(x_cols), function(i){
      i_empty <- unlist(lapply(empty, function(k){
         as(k, class(x[, i]))
      }))
      # i_empty <- as(empty, class(x[,i]));
      has_value <- (!x[,i] %in% i_empty);
      x_rows[has_value];
   });
   return(l);
}
