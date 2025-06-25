
#' Generate setlist data for testing
#' 
#' Generate setlist data for testing
#' 
#' This function generates data to use as test input to
#' Venn diagram functions. It can generate sets of items,
#' or signed sets (integer values `-1`, `1`) named by
#' item.
#' 
#' This function defines a range of set sizes, using `min_size`
#' and `max_size`, with roughly square-root sequence of sizes
#' between these two extremes.
#' 
#' Note that the universe size represents the total available
#' items, but not necessarily the total number of items
#' represented by the sets. For example, if `n_items=1000000`,
#' `max_size=500` and `n_sets=3` then the maximum number of
#' items actually represented is `1500`.
#' 
#' The universe can be defined using optional argument `items`,
#' which takes priority over `n_items`.
#' 
#' The specific size of each set can be defined with optional
#' argument `sizes`, which takes priority over `min_size`, and
#' `max_size`.
#' 
#' @family venndir support
#' 
#' @return `list` of items, either as a list of item vectors,
#' or when `do_signed=TRUE` the list of vectors, where vector
#' names contain the items, and vector values are signed values
#' from `c(-1, 1)`.
#' 
#' @param n_items `integer` total number of items available
#'    to all sets, also known as the universe size.
#' @param n_sets `integer` number of sets that contain items.
#' @param do_signed `logical` indicating whether to return signed
#'    sets, which indicate directionality with `-1` or `1` values,
#'    named by the items.
#' @param concordance `numeric` between -1 and 1, used when `do_signed=TRUE`.
#'    This value imposes an approximate amount of concordance between
#'    random pairs of sets, using the concordance equation:
#'    `concordance = (agree - disagree) / (agree + disgree)` where
#'    `(agree + disagree) = n`. This equation approximates
#'    the number of items that agree as:
#'    `agree = ceiling((concordance * n + n) / 2)`.
#' @param min_size `integer` minimum range of items that may
#'    be contained in each set.
#' @param max_size `integer` maximum range of items that may
#'    be contained in each set.
#' @param items `vector` or `NULL` that contains the universe
#'    of items. When `items` is defined, `n_items` is ignored.
#' @param sizes `vector` of `integer` values, or `NULL`, indicating
#'    the size of each set. When `sizes` is defined, `min_size` and
#'    `max_size` is ignored. When `sizes` is defined, `names(sizes)`
#'    are used as names for each set.
#' @param seed `numeric` or `NULL` used with `set.seed()` for data
#'    reproducibility. When `seed=NULL` then `set.seed()` is not called.
#' @param item_prefix `character` string used as prefix for item names,
#'    default `"item_"`.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' ## basic setlist without signed direction
#' setlist <- make_venn_test(n_items=100,
#'    n_sets=3,
#'    min_size=5,
#'    max_size=25)
#' set_im <- list2im_opt(setlist);
#' table(jamba::pasteByRow(as.matrix(set_im)*1))
#' 
#' ## basic setlist with signed direction
#' setlist <- make_venn_test(n_items=100,
#'    n_sets=3,
#'    do_signed=TRUE)
#' jamba::sdim(setlist);
#' 
#' ## some example overlap summaries
#' sv1 <- signed_overlaps(setlist=setlist, "overlap")
#' sv1
#' 
#' ## Familiar named overlap counts
#' jamba::nameVector(sv1[,c("count","sets")])
#' 
#' ## directional count table for each combination
#' sv2 <- signed_overlaps(setlist=setlist, "each")
#' sv2
#' 
#' ## directional count table for agreement or mixed
#' sv3 <- signed_overlaps(setlist=setlist, "agreement")
#' sv3
#' 
#' ## signed incidence matrix
#' imv <- list2im_value(setlist)
#' dim(imv)
#' head(imv)
#' 
#' ## text venn diagram
#' textvenn(setlist, overlap_type="overlap")
#' 
#' ## text venn diagram with signed direction
#' textvenn(setlist, overlap_type="each")
#' 
#' @export
make_venn_test <- function
(n_items=200,
 n_sets=3,
 do_signed=FALSE,
 concordance=0.5,
 min_size=ceiling(n_items / 50),
 max_size=ceiling(n_items / 2),
 items=NULL,
 sizes=NULL,
 set_names=NULL,
 seed=123,
 item_prefix="item_",
 ...)
{
   # create set items
   if (length(seed) > 0) {
      set.seed(head(seed, 1))
   }
   if (length(items) == 0) {
      items <- paste0(item_prefix,
         jamba::padInteger(seq_len(n_items)));
   } else {
      n_items <- length(items);
   }
   if (length(sizes) > 0) {
      sizes <- rep(sizes,
         length.out=n_sets);
   }
   if (length(set_names) == 0) {
      #
   }

   # define set names
   if (length(set_names) == 0) {
      if (length(names(sizes)) == 0) {
         set_names <- paste0("set_",
            jamba::colNum2excelName(seq_len(n_sets)));
      } else {
         set_names <- names(sizes);
      }
   }
   if (any(set_names %in% c("", NA)) || any(duplicated(set_names))) {
      set_names <- jamba::makeNames(
         jamba::rmNA(naValue="set", set_names),
         suffix="_")
   }

   # define sizes upfront so they are reproducible by seed
   if (length(sizes) == 0) {
      sizes <- lapply(jamba::nameVector(set_names), function(i){
         sample_seq <- unique(ceiling(
            2^seq(from=log2(min_size),
               to=log2(max_size),
               length.out=80)));
         n <- sample(sample_seq, 1);
      });
   } else {
      names(sizes) <- set_names;
   }
   
   # define set_list items so they are reproducible by seed
   # and not affected by do_signed=TRUE
   set_list <- lapply(jamba::nameVector(set_names), function(i){
      # choose random entries using sizes[i] length
      j <- sample(items, size=sizes[[i]]);
   });
   
   # define sign
   if (do_signed) {
      #set_list <- lapply(set_list, function(j){
      if (length(concordance) > 0) {
         concordance <- jamba::noiseFloor(concordance,
            minimum=-1,
            ceiling=1);
         concordance <- rep(concordance, length.out=length(set_names));
         names(concordance) <- set_names;
      }
      ## Randomize the sign
      set_list <- lapply(jamba::nameVector(set_names), function(i){
         j <- set_list[[i]];
         k <- sample(c(-1, 1),
            replace=TRUE,
            size=length(j));
         jamba::nameVector(k, j);
      });
      ## When concordance is defined, force overlaps to have fixed concordance
      ## kruskalConcordance =  (Agree - Disagree)  / (Agree + Disagree)
      ##  kruskalConcordance * (Agree + (n-Agree))  =  (Agree - (n-Agree))
      ##  kruskalConcordance * n                    =  (2*Agree - n)
      ##  kruskalConcordance * n + n                =  2*Agree
      ## (kruskalConcordance * n + n) / 2           =  Agree
      ##
      ## kruskalConcordance * (Agree + Disagree) = (Agree - Disagree)
      if (length(concordance) > 0 && length(set_names) > 1) {
         #set_list <- lapply(jamba::nameVector(set_names), function(i){
         for (i in set_names) {
            i23 <- setdiff(set_names, i);
            i2 <- sample(i23, 1);
            j <- set_list[[i]];
            j2 <- set_list[[i2]];
            jj2 <- j2[intersect(names(j), names(j2))];
            n12 <- length(jj2);
            KC <- concordance[[i]];
            if (n12 > 0) {
               KC_agree <- ceiling((KC * n12 + n12) / 2);
               jj2a <- head(names(jj2),  KC_agree);
               jj2d <- tail(names(jj2),  -KC_agree);
               jj2s <- rep(c(1, -1), c(length(jj2a), length(jj2d)));
               j[c(jj2a, jj2d)] <- j2[c(jj2a, jj2d)] * jj2s;
            }
            set_list[[i]] <- j;
         }
      }
   }
   names(set_list) <- set_names;
   
   return(set_list);
}
