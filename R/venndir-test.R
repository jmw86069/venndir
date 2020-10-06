
#' Generate test sets for venndir
#' 
#' Generate test sets for venndir
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
#' @family venndir utility
#' 
#' @return `list` of items, either as a list of item vectors,
#' or when `do_signed=TRUE` the list of vectors, where vector
#' names contain the items, and vector values are signed values
#' from `c(-1, 1)`.
#' 
#' @param n_items `integer` total number of items available
#'    to all sets, also known as the universe size.
#' @param n_sets `integer` number of sets that contain items.
#' @param min_size `integer` minimum range of items that may
#'    be contained in each set.
#' @param max_size `integer` maximum range of items that may
#'    be contained in each set.
#' @param do_signed `logical` indicating whether to return signed
#'    sets, which indicate directionality with `-1` or `1` values,
#'    named by the items.
#' @param items `vector` or `NULL` that contains the universe
#'    of items. When `items` is defined, `n_items` is ignored.
#' @param sizes `vector` of `integer` values, or `NULL`, indicating
#'    the size of each set. When `sizes` is defined, `min_size` and
#'    `max_size` is ignored. When `sizes` is defined, `names(sizes)`
#'    are used as names for each set.
#' @param seed `numeric` or `NULL` used with `set.seed()` for data
#'    reproducibility. When `seed=NULL` then `set.seed()` is not called.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' set_list <- make_venn_test(n_items=100,
#'    n_sets=3,
#'    min_size=5,
#'    max_size=25)
#' set_im <- list2im_opt(set_list);
#' pryr::object_size(set_im)
#' pryr::object_size(as.matrix(set_im)*1)
#' head(set_im)
#' table(jamba::pasteByRow(as.matrix(set_im)*1))
#' 
#' set_list <- make_venn_test()
#' jamba::sdim(set_list);
#' sv1 <- signed_overlaps(setlist=set_list, "overlap")
#' sv2 <- signed_overlaps(setlist=set_list, "each")
#' sv3 <- signed_overlaps(setlist=set_list, "concordant")
#' 
#' set_im <- list2im_opt(set_list)
#' dim(set_im)
#' head(set_im)
#' 
#' set_lists <- make_venn_test(n_items=100, do_signed=TRUE)
#' jamba::sdim(set_lists);
#' set_ims <- list2im_signed(set_lists, do_sparse=TRUE);
#' dim(set_ims);
#' head(set_ims);
#' 
#' 
#' svs1 <- signed_overlaps(setlist=set_lists, "overlap")
#' svs2 <- signed_overlaps(setlist=set_lists, "each")
#' svs3 <- signed_overlaps(setlist=set_lists, "concordance")
#' svs4 <- signed_overlaps(setlist=set_lists, "concordant")
#' 
#' sv <- signed_overlaps(set_lists, "overlap")
#' jamba::nameVector(sv[,c("count","overlap_set")])
#' 
#' sv_each <- slicejam::signed_venn(set_lists, "each")
#' jamba::ssdim(sv_each)
#' sv_each_counts_df <- jamba::rbindList(lapply(names(sv_each$Venn_Counts), function(i){
#'    idf <- sv_each$Venn_Counts[[i]];
#'    idf$set <- i;
#'    idf$sign <- rownames(idf);
#'    idf$nz_sign <- gsub("[ ]+", " ",
#'       gsub("0", "", idf$sign));
#'    idf[,c("set","sign","count","nz_sign"),drop=FALSE];
#' }))
#' head(sv_each_counts_df,20)
#' 
#' sv_concordance <- slicejam::signed_venn(set_lists, "concordance")
#' jamba::ssdim(sv_concordance)
#' sv_concordance$Venn_Counts
#' 
#' sv_concordant <- slicejam::signed_venn(set_lists, "concordant")
#' jamba::ssdim(sv_concordant)
#' sv_concordant$Venn_Counts
#' 
#' @export
make_venn_test <- function
(n_items=1000000,
 n_sets=4,
 min_size=ceiling(n_items / 1000),
 max_size=ceiling(n_items / 2),
 do_signed=FALSE,
 items=NULL,
 sizes=NULL,
 seed=123,
 ...)
{
   # create set items
   if (length(seed) > 0) {
      set.seed(head(seed, 1))
   }
   if (length(items) == 0) {
      items <- paste0("peak_",
         jamba::padInteger(seq_len(n_items)));
   } else {
      n_items <- length(items);
   }
   if (length(sizes) > 0) {
      sizes <- rep(sizes, length.out=n_items);
   }

   # define set names
   if (length(names(sizes)) == 0) {
      set_names <- paste0("set_",
         jamba::colNum2excelName(seq_len(n_sets)));
   } else {
      # make sure names are unique
      names(sizes) <- jamba::makeNames(names(sizes));
      set_names <- names(sizes);
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
   }
   
   # define set_list items so they are reproducible by seed
   # and not affected by do_signed=TRUE
   set_list <- lapply(jamba::nameVector(set_names), function(i){
      # choose random entries using sizes[i] length
      j <- sample(items, size=sizes[[i]]);
   });
   
   # define sign
   if (do_signed) {
      set_list <- lapply(set_list, function(j){
         k <- sample(c(-1, 1),
            replace=TRUE,
            size=length(j));
         jamba::nameVector(k, j);
      });
   }
   
   return(set_list);
}
