
#' Text Venn diagram
#' 
#' Text Venn diagram
#' 
#' This function is a very simple method to print a Venn diagram
#' using text, intended to be displayed using mono-spaced font
#' on an R console.
#' 
#' @return `data.frame` returned using `invisible()`, from the
#'    output of `signed_overlaps()`.
#' 
#' @family venndir core
#' 
#' @param setlist `list` of item vectors; `list` of vectors named by item;
#'    incidence `matrix` with values `c(0, 1)` or `c(FALSE, TRUE)`, or
#'    `c(-1, 0, 1)`.
#' @param sets `integer` `vector` as index to `setlist`, used to
#'    pull out a subset of the list elements. This subset is useful
#'    because the set colors are defined for the full `setlist`,
#'    which allows the subset of colors to be consistent for each set.
#' @param set_colors `NULL` or `character` `vector` that contains
#'    R-compatible colors. When `set_colors` is `NULL`, categorical
#'    colors are defined using `colorjam::rainbowJam()`. When
#'    `set_colors` is defined, the values are recycled to the total
#'    number of sets represented by `setlist`.
#' @param spacing,padding `integer` values indicating the character
#'    spacing and padding around labels printed to the console output.
#' @param inverse_title,inverse_counts `logical` indicating whether
#'    to inverse the color, when `inverse_title=TRUE` then each Venn
#'    set is printed on colored background, when `inverse_title=FALSE`,
#'    each set is printed with colored text with no background color.
#' @param return_items `logical` passed to `signed_overlaps()` to
#'    indicate whether to include items with each Venn set. This data
#'    is returned using `invisible()`, and is relevant only when
#'    storing the return value.
#' @param unicode `logical` passed to `curate_venn_labels()`
#'    indicating whether the directional label can include special
#'    Unicode characters.
#' @param big.mark `character` passed to `format()` for numeric labels.
#' @param verbose `logical` indicating whether to print verbose output.
#' 
#' @examples
#' setlist <- make_venn_test(n_items=100, do_signed=TRUE)
#' 
#' # two-way Venn by default shows concordance
#' textvenn(setlist, sets=c(1,2))
#' 
#' # without signed directionality use overlap_type="overlap"
#' textvenn(setlist, sets=c(1,2), overlap_type="overlap")
#' 
#' # three-way Venn showing each signed directionality
#' textvenn(setlist, sets=c(1,2,3), overlap_type="each")
#' 
#' # larger number of items
#' setlist <- make_venn_test(n_items=1000000, sizes=200000, do_signed=TRUE)
#' # text Venn with directionality
#' textvenn(setlist, sets=c(1,2,3), "agreement")
#' 
#' # basic text Venn with directionality
#' textvenn(setlist, sets=c(1,2,3), "each")
#' 
#' @export
textvenn <- function
(setlist,
 sets=seq_along(setlist),
 overlap_type=c("concordance", "overlap", "each", "agreement"),
 set_colors=NULL,
 spacing=5,
 padding=1,
 inverse_title=TRUE,
 inverse_counts=FALSE,
 color_by_counts=TRUE,
 return_items=FALSE,
 unicode=TRUE,
 big.mark=",",
 verbose=FALSE,
 ...)
{
   n <- length(sets);
   if (n > 3) {
      stop("The textvenn() function currently only supports 2 or 3 sets.");
   }
   overlap_type <- match.arg(overlap_type);
   
   # define colors
   if (length(set_colors) == 0) {
      set_colors <- colorjam::rainbowJam(length(setlist),
         ...);
      set_color <- set_colors[sets];
   } else {
      set_color <- rep(set_colors,
         length.out=n);
   }

   #c("^(-1.* 1.*|1.* -1.*)$", "\u21C6"),
   if (unicode) {
      curate_list <- list(
         c("[ ]*-1", "\u2193", "dodgerblue"),
         c("[ ]*1", "\u2191", "firebrick"),
         c("[ ]*concordant", "\u21F6", "dodgerblue"),
         c("[ ]*mixed", "\u21C6", "firebrick"));
   } else {
      curate_list <- list(
         c("[ ]*-1", "v", "dodgerblue"),
         c("[ ]*1", "^", "firebrick"),
         c("[ ]*concordant", ">>>", "dodgerblue"),
         c("[ ]*mixed", ">|<", "grey45"));
   }
   curate_df <- jamba::rbindList(curate_list)
   curate_labels <- function(x, curate_df){
      for (i in seq_len(nrow(curate_df))) {
         x <- gsub(curate_df[i,1],
            curate_df[i,2],
            x);
      }
      x;
   }
   curate_colors <- function(x, curate_df){
      for (i in seq_len(nrow(curate_df))) {
         x <- gsub(curate_df[i,1],
            curate_df[i,3],
            x);
      }
      x;
   }
   
   # get overlap data
   sv <- signed_overlaps(setlist[sets],
      overlap_type=overlap_type,
      return_items=return_items);
   #sv <- signed_overlaps(list(A=letters[1:10], B=LETTERS[1:20]), "overlap");
   #sv
   
   # numeric counts
   nCounts <- sapply(unique(sv$sets), function(i){
      sum(subset(sv, sets %in% i)$count)
   });
   # formatted numeric counts
   fCounts <- format(big.mark=big.mark,
      trim=TRUE,
      nCounts);
   # grouped counts (directional)
   gCounts <- lapply(jamba::nameVector(unique(sv$sets)), function(i){
      j <- subset(sv, sets %in% i);
      jamba::nameVector(j$count, j$overlap_label)
   });
   
   ctNchar <- max(nchar(fCounts));
   nameNchar <- nchar(names(setlist));
   spacer <- paste(rep(" ", length.out=spacing), collapse="");
   
   if (n == 2) {
      ## 2-way Venn, one central number
      vCol <- set_color;
      vCol12 <- colorjam::blend_colors(vCol);
      
      ## Create matrix for labels
      set_colnums <- c(1,5,3)*2 - 1;
      set_rownums <- c(1, 1, 1);
      venn_nrow <- max(c(2, lengths(gCounts)));
      venn_m <- matrix(ncol=10,
         nrow=venn_nrow,
         data=" ");
      venn_m[cbind(set_rownums, set_colnums)] <- names(fCounts);
      venn_m[cbind(set_rownums + 1, set_colnums)] <- fCounts;

      # matrix for label colors
      header_colors <- c(vCol, vCol12);
      if (color_by_counts) {
         count_colors <- colorjam::vals2colorLevels(sqrt(nCounts),
            divergent=FALSE,
            col="Reds",
            trimRamp=c(8, 1),
            lens=2,
            baseline=0);
      } else {
         if (inverse_counts) {
            count_colors <- header_colors;
         } else {
            count_colors <- c("darkorange3");
         }
      }
      venn_c <- matrix(ncol=10,
         nrow=venn_nrow,
         data=NA);
      venn_c[1,set_colnums] <- header_colors;
      venn_c[2,set_colnums] <- count_colors;

      # matrix indicating what colors to invert
      venn_i <- matrix(ncol=10,
         nrow=venn_nrow,
         data=FALSE);
      if (inverse_title) {
         venn_i[1,set_colnums] <- TRUE;
      }
      if (inverse_counts) {
         venn_i[2,set_colnums] <- TRUE;
      }

      ## signed counts
      if (any(lengths(gCounts) > 1)) {
         if (color_by_counts) {
            gcount_colors <- colorjam::vals2colorLevels(sqrt(unlist(gCounts)),
               divergent=FALSE,
               col="Reds",
               trimRamp=c(8, 1),
               lens=2,
               baseline=0);
         } else {
            if (inverse_counts) {
               gcount_colors <- rep(header_colors, lengths(gCounts));
            } else {
               gcount_colors <- "darkorange3";
            }
         }
         seq_colnums <- rep(set_colnums + 1, lengths(gCounts));
         seq_rownums <- rep(set_rownums + 1, lengths(gCounts)) +
            unlist(lapply(gCounts, seq_along)) - 2;
         gbase_labels <- curate_venn_labels(
            names(unlist(unname(gCounts))),
            type="sign",
            unicode=unicode);
         gbase_colors <- curate_venn_labels(
            names(unlist(unname(gCounts))),
            "color");
         gcount_labels <- sapply(seq_along(unlist(gCounts)), function(i){
            ilabel <- paste0(
               gbase_labels[i],
               ": ",
               format(trim=TRUE,
                  big.mark=big.mark,
                  unlist(gCounts)[i]));
         });
         ## order labels again?
         gdf <- jamba::mixedSortDF(data.frame(
            group=rep(seq_along(gCounts), lengths(gCounts)),
            label=gbase_labels,
            index=seq_along(gbase_labels)), byCols=c(1, 2))
         gbase_labels <- gbase_labels[gdf$index];
         gbase_colors <- gbase_colors[gdf$index];
         gcount_labels <- gcount_labels[gdf$index];
         for (i in seq_along(gcount_labels)) {
            venn_m[seq_rownums[i],seq_colnums[i]] <- gcount_labels[[i]];
            venn_c[seq_rownums[i],seq_colnums[i]] <- gbase_colors[[i]];
            if (inverse_counts) {
               venn_i[seq_rownums[i],seq_colnums[i]] <- TRUE;
            }
         }
      }
      
      # print this colorized text table
      print_color_df(df=venn_m,
         dfcolor=venn_c,
         dfinvert=venn_i,
         padding=padding,
         ...);
      return(invisible(sv));

   } else if (n == 3) {
      ## 3-way Venn
      vCol <- set_color;
      vCol12 <- colorjam::blend_colors(vCol[1:2]);
      vCol23 <- colorjam::blend_colors(vCol[2:3]);
      vCol13 <- colorjam::blend_colors(vCol[c(1,3)]);
      vCol123 <- colorjam::blend_colors(vCol[c(1:3)]);

      ## Create matrix for labels
      set_colnums <- c(1,5,3, 3,2,4, 3) * 2 - 1;
      set_colnums <- c(1, 10, 5, 5, 3, 8, 5);
      venn_ncol <- 11;
      set_rownums <- c(3,3,12, 1,8,8, 6);
      venn_m <- matrix(ncol=venn_ncol, nrow=13, data=" ");
      venn_m[cbind(set_rownums, set_colnums)] <- names(fCounts);
      venn_m[cbind(set_rownums + 1, set_colnums)] <- fCounts;

      # matrix for label colors
      header_colors <- c(vCol, vCol12, vCol13, vCol23, vCol123);
      if (color_by_counts) {
         count_colors <- colorjam::vals2colorLevels(sqrt(nCounts),
            divergent=FALSE,
            col="Reds",
            trimRamp=c(8, 1),
            lens=2,
            baseline=0);
      } else {
         if (inverse_counts) {
            count_colors <- header_colors;
         } else {
            count_colors <- "darkorange3";
         }
      }
      venn_c <- matrix(ncol=venn_ncol, nrow=13, data=NA);
      venn_c[cbind(set_rownums, set_colnums)] <- header_colors;
      venn_c[cbind(set_rownums + 1, set_colnums)] <- count_colors;

      # matrix indicating what colors to invert
      venn_i <- matrix(ncol=venn_ncol, nrow=13, data=FALSE);
      if (inverse_title) {
         venn_i[cbind(set_rownums, set_colnums)] <- TRUE;
      }
      if (inverse_counts) {
         venn_i[cbind(set_rownums + 1, set_colnums)] <- TRUE;
      }
      
      ## signed counts
      if (any(lengths(gCounts) > 1)) {
         if (color_by_counts) {
            gcount_colors <- colorjam::vals2colorLevels(sqrt(unlist(gCounts)),
               divergent=FALSE,
               col="Reds",
               trimRamp=c(8, 1),
               lens=2,
               baseline=0);
         } else {
            if (inverse_counts) {
               gcount_colors <- rep(header_colors, lengths(gCounts));
            } else {
               gcount_colors <- "darkorange3";
            }
         }
         seq_colnums <- rep(set_colnums + 1, lengths(gCounts)) +
            unlist(lapply(gCounts, function(a){
               floor((seq_along(a) - 1) / 4)
            }));
         #seq_colnums <- rep(set_colnums + 1, lengths(gCounts));
         seq_rownums <- rep(set_rownums + 1, lengths(gCounts)) +
            unlist(lapply(gCounts, function(a){
               (seq_along(a) - 1) %% 4
            })) - 1;
         #seq_rownums <- rep(set_rownums + 1, lengths(gCounts)) +
         #2   unlist(lapply(gCounts, seq_along)) - 2;
         gbase_labels <- curate_venn_labels(
            names(unlist(unname(gCounts))),
            type="sign",
            unicode=unicode);
         gbase_colors <- curate_venn_labels(
            names(unlist(unname(gCounts))),
            "color");
         gcount_labels <- sapply(seq_along(unlist(gCounts)), function(i){
            ilabel <- paste0(
               gbase_labels[i],
               ": ",
               format(trim=TRUE,
                  big.mark=",",
                  unlist(gCounts)[i]));
         });
         ## order labels again?
         gdf <- jamba::mixedSortDF(data.frame(
            group=rep(seq_along(gCounts), lengths(gCounts)),
            label=gbase_labels,
            index=seq_along(gbase_labels)), byCols=c(1, 2))
         gbase_labels <- gbase_labels[gdf$index];
         gbase_colors <- gbase_colors[gdf$index];
         gcount_labels <- gcount_labels[gdf$index];
         for (i in seq_along(gcount_labels)) {
            venn_m[seq_rownums[i],seq_colnums[i]] <- gcount_labels[[i]];
            venn_c[seq_rownums[i],seq_colnums[i]] <- gbase_colors[[i]];
            if (inverse_counts) {
               venn_i[seq_rownums[i],seq_colnums[i]] <- TRUE;
            }
         }
      }
      
      ## print color data.frame
      print_color_df(venn_m,
         venn_c,
         venn_i,
         padding=padding,
         ...);
      return(invisible(sv));
   }
   invisible(sv);
}
