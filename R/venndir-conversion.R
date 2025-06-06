
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
      paste0(rep(i, x[[i]]),
         rep("_", x[[i]]),
         seq_len(x[[i]]))
   });
   combo_sets <- strsplit(names(x),
      fixed=TRUE,
      split=sep);
   combo_im <- list2im_opt(combo_sets,
      do_sparse=TRUE);
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
#' Since version 0.0.44.900, this function also accepts nested
#' list format, as returned by `overlaplist()` for a `Venndir`
#' object, when the `overlap_type` is something other than
#' `"overlap"`.
#' 
#' Note that when converting data with `overlap_type` `"concordance"`
#' and `"agreement"` it will be slightly lossy.
#' * `overlap_type="concordance"` encodes mismatches as `"mixed"`,
#' instead of the original sign for each set.
#' * `overlap_type="agreement"` encodes as `"agreement"` and `"mixed"`
#' instead of the original sign for each set.
#' 
#' Ideally, use `setlist()` to obtain to actual input setlist from
#' the `Venndir` object.
#' 
#' @return `list` where the list names are the names of each set,
#'    and values of each list element is a vector of items.
#' 
#' @family venndir conversion
#' 
#' @examples
#' # example from venn_meme()
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
#'    item_cex_factor=0.7,
#'    show_labels="i")
#' 
#' # Proportional Venn (Euler) diagram
#' venndir(setlist,
#'    proportional=TRUE,
#'    item_cex_factor=0.7,
#'    show_labels="i")
#' 
#' # test interconversion: setlist to venndir to overlaplist to setlist
#' setlist2 <- make_venn_test(100, 3, do_signed=TRUE);
#' vo <- venndir(setlist2, do_plot=FALSE, overlap_type="each")
#' ollist <- overlaplist(vo)
#' setlist2b <- overlaplist2setlist(ollist)
#' 
#' # all should be TRUE:
#' sapply(names(setlist2b), function(i){
#'    all(setlist2[[i]] == setlist2b[[i]][names(setlist2[[i]])])
#' })
#' 
#' @export
overlaplist2setlist <- function
(x,
 sep="&",
 ...)
{
   x_names <- names(x)
   names(x_names) <- names(x);

   ## check nested list
   if (inherits(x[[1]], c("list", "AsIs"))) {
      x_new <- jamba::rbindList(lapply(x_names, function(x_name){
         x_sets <- unlist(strsplit(x_name, "&"))
         x_signs <- jamba::nameVector(names(x[[x_name]]));
         jamba::rbindList(lapply(x_signs, function(x_sign){
            x_signvals1 <- strsplit(x_sign, "[_ ]")[[1]];
            x_signvals <- rep(
               x_signvals1[!x_signvals1 %in% c("0", "")],
               length.out=length(x_sets))
            ivals <- x[[x_name]][[x_sign]];
            jamba::rbindList(lapply(jamba::nameVector(seq_along(x_sets), x_sets), function(iset){
               data.frame(set=x_sets[iset],
                  name=ivals,
                  value=rep(x_signvals[iset], length(ivals)))
            }))
         }))
      }))
      x_new_list <- lapply(split(x_new, x_new$set), function(idf){
         jamba::nameVector(idf$value, idf$name)
      })
      return(x_new_list)
   }
   
   combo_sets <- strsplit(x_names,
      fixed=TRUE,
      split=sep);
   combo_im <- list2im_opt(combo_sets,
      do_sparse=TRUE);
   set_names <- rownames(combo_im);
   set_values <- lapply(jamba::nameVector(set_names), function(i){
      j <- which(combo_im[i,]);
      #jamba::printDebug("i:", i, ", j:", j);
      unname(unlist(x[j]));
   })
   return(set_values);
}

#' venndir conversion from signed overlap counts to setlist
#' 
#' venndir conversion from signed overlap counts to setlist
#' 
#' This function takes input in the form of a `list`
#' whose names are set overlap labels, for example `"set_A"`,
#' or `"set_A&set_B"`.
#' 
#' Each list element is an `integer` vector whose names
#' are value overlaps, for example `"1"`, or `"-1_1"`,
#' and whose `integer` values contain the overlap counts.
#' 
#' 
#' 
#' @return `list` where the list names are the names of each set,
#'    and values of each list element is a vector of items.
#'    The items are artificial labels used for convenience.
#' 
#' @family venndir conversion
#' 
#' @examples
#' x <- list(
#'    "set_A"=c(
#'       "1"=80,
#'       "-1"=95
#'    ),
#'    "set_B"=c(
#'       "1"=15,
#'       "-1"=30
#'    ),
#'    "set_A&set_B"=c(
#'       "1_1"=100,
#'       "-1_-1"=125,
#'       "1_-1"=3,
#'       "-1_1"=4
#'    )
#' )
#' setlist <- signed_counts2setlist(x)
#' # default Venn diagram
#' vo <- venndir(setlist, overlap_type="each")
#' # show counts,percent outside, and sign for each item inside
#' venndir(setlist,
#'    overlap_type="each",
#'    show_labels="NCSPi",
#'    show_items="sign",
#'    item_degrees=10)
#' 
#' # show counts inside without set label (which is in the legend)
#' vo <- venndir(setlist,
#'    show_labels="cs",
#'    proportional=TRUE,
#'    label_style="fill_box")
#' 
#' # demonstrate interconversion
#' setlist2 <- make_venn_test(100, 3, do_signed=TRUE)
#' vo <- venndir(setlist2, do_plot=FALSE, overlap_type="each")
#' venndir(signed_counts2setlist(signed_counts(vo)), overlap_type="each")
#' 
#' @export
signed_counts2setlist <- function
(x,
 sep="&",
 value_sep="_",
 ...)
{
   ## input is assumed to be a list named by set overlap
   ## where each list element is a list named by value
   ## overlap
   combo_value_list <- unlist(recursive=FALSE, lapply(names(x), function(i){
      j <- x[[i]];
      unlist(recursive=FALSE, lapply(names(j), function(kname){
         k <- j[[kname]];
         inames <- strsplit(i, split=sep)[[1]];
         knames <- strsplit(kname, split=value_sep)[[1]];
         valuenames <- paste0(
            rep(i, k),
            "_",
            kname,
            "_",
            seq_len(k))
         unlist(recursive=FALSE,
            lapply(seq_along(inames), function(m){
               v <- list(jamba::nameVector(
                  rep(knames[m], length(valuenames)),
                  valuenames))
               names(v) <- inames[m];
               v
            })
         )
      }))
   }));
   combo_value_list
   set_names <- unique(names(combo_value_list));
   set_values <- lapply(jamba::nameVector(set_names), function(i){
      unlist(unname(combo_value_list[(names(combo_value_list) %in% i)]))
   })
   return(set_values);
}

