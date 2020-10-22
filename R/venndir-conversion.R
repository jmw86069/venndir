
## TODO:
## setlist_from_signed_combinations

#' venndir conversion from overlap counts to setlist
#' 
#' venndir conversion from overlap counts to setlist
#' 
#' This function takes input in the form of named `vector`
#' with overlap counts, which is the input to similar Venn
#' functions in `eulerr::euler()`, and is called "expressionInput"
#' when used with `upsetr::fromExpression()`.
#' 
#' @return `list` where the list names are the names of each set,
#'    and values of each list element is a vector of items.
#'    The items are artificial labels used for convenience.
#' 
#' @family venndir conversion
#' 
#' @examples
#' # example from eulerr::euler()
#' combo <- c(A = 2,
#'    B = 2,
#'    C = 2,
#'    "A&B" = 2,
#'    "A&C" = 1,
#'    "B&C" = 1,
#'    "A&B&C" = 4)
#' setlist <- counts2setlist(combo)
#' setlist;
#' 
#' # Venn diagram
#' venndir(setlist)
#' 
#' # Proportional Venn (Euler) diagram
#' venndir(setlist, proportional=TRUE)
#' 
#' @export
counts2setlist <- function
(x,
 sep="&",
 ...)
{
   ## input is assumed to be numeric/integer vector whose
   ## names represent Venn overlap combinations
   ## All missing combinations are assumed to be zero.
   combo_value_list <- lapply(names(x), function(i){
      paste0(rep(i, x[[i]]), "_", seq_len(x[[i]]))
   });
   combo_sets <- strsplit(names(x), split=sep);
   combo_im <- list2im_opt(combo_sets);
   set_names <- rownames(combo_im);
   set_values <- lapply(jamba::nameVector(set_names), function(i){
      j <- which(combo_im[i,]);
      #jamba::printDebug("i:", i, ", j:", j);
      unname(unlist(combo_value_list[j]));
   })
   return(set_values);
}

#' venndir conversion from overlap list to setlist
#' 
#' venndir conversion from overlap list to setlist
#' 
#' This function takes input in the form of named `list`
#' of `vectors`. The list names represent set overlaps.
#' The vectors each contain items contained in that
#' overlap. This content is similar to the input to
#' `counts2setlist()` except in that case the input
#' only contains the number of items per overlap,
#' and not the items. In this case each overlap contains
#' the vector of items also.
#' 
#' @return `list` where the list names are the names of each set,
#'    and values of each list element is a vector of items.
#' 
#' @family venndir conversion
#' 
#' @examples
#' # example from eulerr::euler()
#' av_overlap_list <- list(
#'    AV="BATMAN<br>VILLAIN",
#'    ML="MOUNTAIN<br>LION",
#'    T="TODDLER",
#'    `AV&ML`="Wants to<br>kill you",
#'    `AV&T`="Goes off on<br>a lot of rambling<br>monologues",
#'    `ML&T`="Resists<br>taking a bath",
#'    `AV&ML&T`="Impossible<br>to reason with"
#' )
#' setlist <- overlaplist2setlist(av_overlap_list);
#' 
#' # Venn diagram
#' venndir(setlist,
#'    show_items="item",
#'    item_angle=0,
#'    item_cex=rep(c(2, 1.6, 1.35, 1.5), c(3, 1, 2, 1)))
#' 
#' # Proportional Venn (Euler) diagram
#' venndir(setlist, proportional=TRUE)
#' 
#' @export
overlaplist2setlist <- function
(x,
 sep="&",
 ...)
{
   combo_sets <- strsplit(names(x), split=sep);
   combo_im <- list2im_opt(combo_sets);
   set_names <- rownames(combo_im);
   set_values <- lapply(jamba::nameVector(set_names), function(i){
      j <- which(combo_im[i,]);
      #jamba::printDebug("i:", i, ", j:", j);
      unname(unlist(x[j]));
   })
   return(set_values);
}
