
# Driving motivation:
# - take probe-level hit list or hit matrix
# - collapse into gene-level hit list or matrix

#' Collapse incidence matrix using row groups
#' 
#' Collapse incidence matrix using row groups, for example when
#' converting probe-level, transcript-level, peptide-level data
#' to gene-level data.
#' 
#' This function is a simple converted for incidence matrix data,
#' taking the "majority-hit" for each row grouping. The most common
#' scenario is to group rows by gene, in order to summarize the
#' observed changes at gene level, when the original data may
#' contain multiple possible measurements for each gene.
#' 
#' The default logic assumes that *any* observed statistical hit for
#' a gene is sufficient evidence to implicate that gene as a "hit",
#' even if other potential measurements for the same gene did not
#' meet the statistical criteria used, as relevant to the platform
#' technology.
#' 
#' @returns `numeric` matrix
#' 
#' @family venndir conversion
#' 
#' @param im `numeric` matrix with columns for each set
#' @param row_groups `character` or `factor` with row groupings.
#' @param logic `character` logic to use, default 'majority-hit'.
#'    * `"majority-hit"`: uses the majority winner among non-zero values.
#'    * `"majority"`: uses the majority winner including non-zero and zero.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' im <- cbind(A=c(-1, -1, 0, 1, 1, 1, -1, 0, 0, 1, 1, 0),
#'    B=c(-1, -1, -1, 1, 1, 0, -1, 0, 0, 1, 1, 1),
#'    C=c(-1, -1, -1, 1, 1, 0, -1, 0, 0, 0, 0, 0));
#' row_groups <- rep(c("a", "b", "c"), c(6, 3, 3))
#' 
#' # default logic returns the majority non-zero value when present
#' new_im <- collapse_im(im, row_groups)
#' new_im
#' 
#' # majority logic will prioritize "0" when it is the majority
#' # (not recommended for most gene-based data)
#' new_im2 <- collapse_im(im, row_groups, logic="majority")
#' new_im2
#' 
#' # more detail
#' imdf <- data.frame(im, row_groups,
#'    new_im[match(row_groups, rownames(new_im)), ])
#' split(imdf, imdf$row_groups)
#' @export
collapse_im <- function
(im,
 row_groups=NULL,
 logic=c("majority-hit",
    "majority"),
 verbose=FALSE,
 ...)
{
   #
   logic <- match.arg(logic);
   
   if (length(row_groups) != nrow(im)) {
      stop("length(row_groups) must eqal nrow(im)");
   }
   # if no row_groups are duplicated, return data unchanged
   if (!any(duplicated(row_groups))) {
      return(im)
   }
   
   if (!inherits(row_groups, "factor")) {
      row_groups <- factor(row_groups,
         levels=unique(row_groups))
   }
   if (length(rownames(im)) == 0) {
      rownames(im) <- as.character(seq_len(nrow(im)));
   }
   if (any(duplicated(rownames(im)))) {
      rownames(im) <- jamba::makeNames(rownames(im));
   }
   im_split <- split(rownames(im), row_groups)
   
   if ("majority-hit" %in% logic) {
      use_fn <- function(x){
         xin <- (x %in% c(NA, 0));
         if (all(xin)) {
            return(0)
         }
         # negative values first
         # x[!xin][order(abs(x[!xin]), x[!xin])]
         # use first occurring
         as.numeric(head(names(
            jamba::tcount(x[!xin][order(-abs(x[!xin]))], nameSortFunc=c)
            ), 1))
      }
   } else if ("majority" %in% logic) {
      use_fn <- function(x){
         xin <- (x %in% c(NA, 0));
         if (all(xin)) {
            return(0)
         }
         x1 <- factor(x, levels=jamba::provigrep(c("1", "."), unique(x)))
         as.numeric(head(names(sort(table(x1), decreasing=TRUE)), 1))
      }
   }
   new_im1 <- (lapply(names(im_split), function(imname){
      imrows <- im_split[[imname]];
      #
      if (length(imrows) == 1) {
         im1 <- im[imrows, , drop=FALSE];
         rownames(im1) <- imname;
         return(im1);
      }
      if (TRUE %in% verbose) {
         jamba::printDebug("input im rows:");
         print(im[imrows, , drop=FALSE]);
      }
      im1 <- apply(im[imrows, , drop=FALSE], 2, simplify=FALSE, use_fn);
      if (TRUE %in% verbose) {
         jamba::printDebug("output im row:");
         print(do.call(cbind, im1));
      }
      im1 <- do.call(cbind, im1);
      rownames(im1) <- imname;
      im1
   }));
   new_im <- do.call(rbind, new_im1);
   new_im
}
