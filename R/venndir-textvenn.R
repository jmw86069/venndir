
#' Text Venn diagram
#' 
#' Text Venn diagram
#' 
#' This function is a very simple method to print a Venn diagram
#' using text, intended to be displayed using mono-spaced font
#' on an R console.
#' 
#' @export
textvenn <- function
(setlist,
 sets=seq_along(setlist),
 set_colors=NULL,
 spacing=5,
 padding=2,
 useBgColor=TRUE,
 countsUseBgColor=FALSE,
 return_items=FALSE,
 verbose=FALSE,
 ...)
{
   n <- length(sets);
   if (n > 3) {
      stop("The textvenn() function currently only supports 2 or 3 sets.");
   }
   
   # define colors
   if (length(set_colors) == 0) {
      set_colors <- colorjam::rainbowJam(length(setlist),
         ...);
      set_color <- set_colors[sets];
   } else {
      set_color <- rep(set_colors,
         length.out=n);
   }

   # get overlap data
   sv <- signed_overlaps(setlist[sets],
      "overlap",
      return_items=return_items);
   #sv <- signed_overlaps(list(A=letters[1:10], B=LETTERS[1:20]), "overlap");
   #sv
   
   fCounts <- format(sv$count,
      big.mark=",",
      trim=TRUE);
   ctNchar <- max(nchar(fCounts));
   nameNchar <- nchar(names(setlist));
   spacer <- paste(rep(" ", length.out=spacing), collapse="");
   
   if (n == 2) {
      ## 2-way Venn, one central number
      vCol <- set_color;
      rad2deg <- jamba::rad2deg;
      vCol12 <- colorjam::blend_colors(vCol);
      
      ## Create matrix for labels
      set_colnums <- c(1,5,3);
      venn_m <- matrix(ncol=5, nrow=2, data=" ");
      venn_m[1,set_colnums] <- sv$sets;
      venn_m[2,set_colnums] <- fCounts;
      
      # matrix for label colors
      count_colors <- colorjam::vals2colorLevels(sqrt(sv$count),
         divergent=FALSE,
         col="Reds",
         trimRamp=c(8, 1),
         lens=2,
         baseline=0);
      venn_c <- matrix(ncol=5, nrow=2, data=NA);
      venn_c[1,set_colnums] <- c(vCol, vCol12);
      venn_c[2,set_colnums] <- count_colors;
      
      # matrix indicating what colors to invert
      venn_i <- matrix(ncol=5, nrow=2, data=FALSE);
      if (useBgColor) {
         venn_i[1,set_colnums] <- TRUE;
      }
      if (countsUseBgColor) {
         venn_i[2,set_colnums] <- TRUE;
      }
      
      # print this colorized text table
      print_color_df(df=venn_m,
         dfcolor=venn_c,
         dfinvert=venn_i,
         padding=padding);
      return(invisible(sv));

   } else if (n == 3) {
      ## 3-way Venn
      vCol <- set_color;
      vCol12 <- colorjam::blend_colors(vCol[1:2]);
      vCol23 <- colorjam::blend_colors(vCol[2:3]);
      vCol13 <- colorjam::blend_colors(vCol[c(1,3)]);
      vCol123 <- colorjam::blend_colors(vCol[c(1:3)]);
      counts <- jamba::nameVector(sv$count, sv$sets);
      
      ## Create matrix for labels
      set_colnums <- c(1,5,3, 3,2,4, 3);
      #set_rownums <- c(1,1,11, 3,8,8, 6);
      set_rownums <- c(3,3,12, 1,8,8, 6);
      venn_m <- matrix(ncol=5, nrow=13, data=" ");
      venn_m[cbind(set_rownums, set_colnums)] <- sv$sets;
      venn_m[cbind(set_rownums + 1, set_colnums)] <- format(counts,
         big.mark=",",
         trim=TRUE);

      # matrix for label colors
      count_colors <- colorjam::vals2colorLevels(sqrt(counts),
         divergent=FALSE,
         col="Reds",
         trimRamp=c(8, 1),
         lens=2,
         baseline=0);
      header_colors <- c(vCol, vCol12, vCol13, vCol23, vCol123);
      venn_c <- matrix(ncol=5, nrow=13, data=NA);
      venn_c[cbind(set_rownums, set_colnums)] <- header_colors;
      venn_c[cbind(set_rownums + 1, set_colnums)] <- count_colors;

      # matrix indicating what colors to invert
      venn_i <- matrix(ncol=5, nrow=13, data=FALSE);
      if (useBgColor) {
         venn_i[cbind(set_rownums, set_colnums)] <- TRUE;
      }
      if (countsUseBgColor) {
         venn_i[cbind(set_rownums + 1, set_colnums)] <- TRUE;
      }
      print_color_df(venn_m,
         venn_c,
         venn_i,
         padding=padding);
      return(invisible(sv));
   }
   invisible(sv);
}
