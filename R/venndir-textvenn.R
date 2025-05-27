
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
#' @param overlap_type `character` passed to `signed_overlaps()`, default
#'    'detect' will show 'concordance' when provided signed list, otherwise
#'    it shows 'overlap' with only the counts.
#' @param template `character` string, default 'wide' indicatiing the
#'    placement of signed counts.
#'    * 'wide' - places signed counts beside total overlap counts
#'    * 'tall' - places signed counts below total overlap counts
#' @param draw_legend `logical` default FALSE, whether to print a text
#'    legend table below the text venn output.
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
#' @param return_items `logical` default TRUE, whether to return items
#'    in the `data.frame` in column `"items"`.
#'    This argument is passed to `signed_overlaps()`.
#' @param unicode `logical` passed to `curate_venn_labels()`
#'    indicating whether the directional label can include special
#'    Unicode characters.
#' @param big.mark `character` passed to `format()` for numeric labels.
#' @param blend_preset `character` string passed as `preset` to
#'    `colorjam::blend_colors()` to define the color wheel used
#'    during color blending operations.
#' @param curate_df `data.frame` or `NULL` passed to `curate_venn_labels()`.
#' @param lightMode `logical` default `jamba::checkLightMode()` checks
#'    whether the console has a light background, and therefore needs
#'    to have darker text. This check is incomplete, it assumes
#'    RStudio has a light background, and everything else is dark.
#'    To override consistently, set the option below, or add to `.Rprofile`:
#'    * `options("jam.lightMode"=TRUE)` will force lightMode=TRUE, for
#'    light background and darker text.
#'    * `options("jam.lightMode"=FALSE)` will force lightMode=FALSE, for
#'    dark background and lighter text.
#'    
#' @param verbose `logical` indicating whether to print verbose output.
#' 
#' @examples
#' # for this purpose, set lightMode=TRUE to ensure darker text
#' withr::with_options(list(jam.lightMode=TRUE), {
#' 
#' # generate test data
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
#' setlist <- make_venn_test(n_items=1000000, n_sets=4, sizes=200000, do_signed=TRUE)
#' # text Venn with directionality
#' textvenn(setlist, sets=c(1,4), "agreement")
#' 
#' # basic text Venn with directionality
#' textvenn(setlist, sets=c(3,4), "each")
#' 
#' # simple way to show legend
#' venndir_legender(textvenn(setlist, sets=c(1,2,4), overlap_type="overlap"),
#'    combine_size=FALSE,
#'    legend_style="data.frame")
#' 
#' })
#' @export
textvenn <- function
(setlist,
 sets=seq_along(setlist),
 overlap_type=c("detect",
    "concordance",
    "overlap",
    "each",
    "agreement"),
 template=c("wide",
    "tall"),
 draw_legend=FALSE,
 set_colors=NULL,
 spacing=5,
 padding=1,
 inverse_title=TRUE,
 inverse_counts=FALSE,
 color_by_counts=TRUE,
 return_items=TRUE,
 unicode=TRUE,
 big.mark=",",
 sep="&",
 blend_preset="ryb",
 curate_df=NULL,
 lightMode=jamba::checkLightMode(),
 debug=NULL,
 verbose=FALSE,
 ...)
{
   n <- length(sets);
   if (n > 3) {
      stop("The textvenn() function currently only supports 2 or 3 sets.");
   }
   overlap_type <- match.arg(overlap_type);
   template <- match.arg(template);
   
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
      overlap_type=overlap_type,
      return_items=return_items,
      sep=sep);

   # custom function for debug output
   handle_textvenn_debug <- function(outdf=NULL, debug="data.frames") {
      df <- data.frame(outdf$df)
      dfcolor <- data.frame(outdf$dfcolor);
      use_color_sub <- jamba::rmNULL(
         lapply(jamba::nameVectorN(dfcolor), function(i){
            k <- !is.na(dfcolor[,i]);
            jamba::nameVector(subset(
               data.frame(a=dfcolor[k, i], b=df[k, i]),
               !duplicated(b)))
         }))
      use_align <- ifelse(sapply(colnames(df), function(i){
         any(grepl(":", df[, i]))
      }), "l", "r")
      if ("html" %in% debug) {
         kbl <- jamba::kable_coloring(df,
            align=use_align,
            col.names=rep("", ncol(df)),
            extra_css="white-space: nowrap; border-top: none;",
            colorSub=use_color_sub,
            border_left=FALSE) %>%
            kableExtra::kable_styling(bootstrap_options=c("condensed"),
               full_width=FALSE)
         return(kbl)
      }
      return(outdf)
   }
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
   # number of groups per count
   gGCounts <- sapply(jamba::nameVector(unique(sv$sets)), function(i){
      j <- subset(sv, sets %in% i);
      j$num_sets
   });

   ctNchar <- max(nchar(fCounts));
   nameNchar <- nchar(names(setlist));
   spacer <- paste(rep(" ", length.out=spacing), collapse="");

   if (n == 2) {
      ## 2-way Venn, one central number
      vCol <- set_color;
      vCol12 <- colorjam::blend_colors(vCol,
         preset=blend_preset,
         ...);
      
      ## Create matrix for labels
      if ("wide" %in% template) {
         set_colnums <- c(1, 5, 3)*2 - 1;
         set_rownums <- c(1, 1, 1);
         venn_nrow <- max(c(2, lengths(gCounts)));
         venn_m <- matrix(ncol=10,
            nrow=venn_nrow,
            data=" ");
         venn_m[cbind(set_rownums, set_colnums)] <- names(fCounts);
         venn_m[cbind(set_rownums + 1, set_colnums)] <- fCounts;
      } else if ("tall" %in% template) {
         set_colnums <- c(1, 5, 3);
         set_rownums <- c(1, 1, 1);
         venn_nrow <- max(c(2, lengths(gCounts) + 2));
         venn_m <- matrix(ncol=10,
            nrow=venn_nrow,
            data=" ");
         venn_m[cbind(set_rownums, set_colnums)] <- names(fCounts);
         venn_m[cbind(set_rownums + 1, set_colnums)] <- fCounts;
      }

      # matrix for label colors
      header_colors <- c(vCol, vCol12);
      names(header_colors) <- unique(sv$sets);
      if (!"color" %in% colnames(sv)) {
         sv$color <- header_colors[sv$sets]
      }
      if (color_by_counts) {
         count_color_fn <- colorjam::col_linear_xf(
            x=max(sqrt(nCounts)),
            floor=max(c(0, min(sqrt(nCounts) - 1))),
            lens=0,
            ...);
         count_colors <- count_color_fn(sqrt(nCounts));
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
      venn_c[1, set_colnums] <- header_colors;
      venn_c[2, set_colnums] <- count_colors;

      # matrix indicating what colors to invert
      venn_i <- matrix(ncol=10,
         nrow=venn_nrow,
         data=FALSE);
      if (inverse_title) {
         venn_i[1, set_colnums] <- TRUE;
      }
      if (inverse_counts) {
         venn_i[2, set_colnums] <- TRUE;
      }

      ## signed counts
      if (any(lengths(gCounts) > 1)) {
         if (color_by_counts) {
            count_color_fn <- colorjam::col_linear_xf(
               x=max(sqrt(unlist(gCounts))),
               floor=max(c(0, min(sqrt(unlist(gCounts)) - 1))),
               lens=0,
               ...);
            gcount_colors <- count_color_fn(sqrt(unlist(gCounts)))
         } else {
            if (inverse_counts) {
               gcount_colors <- rep(header_colors, lengths(gCounts));
            } else {
               gcount_colors <- "darkorange3";
            }
         }
         if ("wide" %in% template) {
            seq_colnums <- rep(set_colnums + 1, lengths(gCounts));
            seq_rownums <- rep(set_rownums + 1, lengths(gCounts)) +
               unlist(lapply(gCounts, seq_along)) - 2;
         } else {
            seq_colnums <- rep(set_colnums, lengths(gCounts));
            seq_rownums <- rep(set_rownums + 2, lengths(gCounts)) +
               unlist(lapply(gCounts, seq_along)) - 1;
         }
         names(seq_colnums) <- names(seq_rownums);
         gbase_df <- curate_venn_labels(
            names(unlist(unname(gCounts))),
            type="all",
            unicode=unicode,
            curate_df=curate_df,
            ...);
         gbase_labels <- gbase_df$sign;
         gbase_colors <- gbase_df$color;
         gbase_hide <- (gbase_df$hide_singlet & unlist(gGCounts) == 1);
         gcount_labels <- sapply(seq_along(unlist(gCounts)), function(i){
            ilabel <- paste0(
               gbase_labels[i],
               ": ",
               format(trim=TRUE,
                  big.mark=big.mark,
                  unlist(gCounts)[i]));
            ifelse(gbase_hide[i], "", ilabel)
         });
         ## order labels again?
         gdf <- jamba::mixedSortDF(data.frame(
            group=rep(seq_along(gCounts), lengths(gCounts)),
            label=gbase_labels,
            index=seq_along(gbase_labels),
            stringsAsFactors=FALSE), byCols=c(1, 2))
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
      outdf <- print_color_df(df=venn_m,
         dfcolor=venn_c,
         dfinvert=venn_i,
         padding=padding,
         lightMode=lightMode,
         debug=debug,
         ...);
      if (any(c("html", "data.frames") %in% debug)) {
         return(handle_textvenn_debug(outdf, debug))
      }
      # return(invisible(sv));

   } else if (n == 3) {
      ## 3-way Venn
      vCol <- set_color;
      vCol12 <- colorjam::blend_colors(vCol[1:2],
         preset=blend_preset,
         ...);
      vCol23 <- colorjam::blend_colors(vCol[2:3],
         preset=blend_preset,
         ...);
      vCol13 <- colorjam::blend_colors(vCol[c(1,3)],
         preset=blend_preset,
         ...);
      vCol123 <- colorjam::blend_colors(vCol[c(1:3)],
         preset=blend_preset,
         ...);

      ## Create matrix for labels
      if ("wide" %in% template) {
         venn_ncol <- 11;
         venn_nrow <- 13;
         set_colnums <- c(1, 10,  5, 5, 3, 8, 5);
         set_rownums <- c(3,  3, 12, 1, 8, 8, 6);
         venn_m <- matrix(ncol=venn_ncol, nrow=venn_nrow, data=" ");
         venn_m[cbind(set_rownums, set_colnums)] <- names(fCounts);
         venn_m[cbind(set_rownums + 1, set_colnums)] <- fCounts;
      } else if ("tall" %in% template) {
         mid1 <- lengths(gCounts)[4];
         mid2 <- lengths(gCounts)[7];
         mid3 <- lengths(gCounts)[3];
         venn_ncol <- 11;
         venn_nrow <- 8 + mid1 + mid2 + mid3;
         set_colnums <- c(1, 10, 5, 5, 3, 8, 5);
         set_rownums <- c(3, 3,
            7 + mid1 + mid2,
            1,
            5 + mid1, 5 + mid1,
            4 + mid1);
         venn_m <- matrix(ncol=venn_ncol, nrow=venn_nrow, data=" ");
         venn_m[cbind(set_rownums, set_colnums)] <- names(fCounts);
         venn_m[cbind(set_rownums + 1, set_colnums)] <- fCounts;
      }

      # matrix for label colors
      header_colors <- c(vCol, vCol12, vCol13, vCol23, vCol123);
      names(header_colors) <- unique(sv$sets);
      if (!"color" %in% colnames(sv)) {
         sv$color <- header_colors[sv$sets]
      }
      if (color_by_counts) {
         count_color_fn <- colorjam::col_linear_xf(
            x=max(sqrt(nCounts)),
            floor=max(c(0, min(sqrt(nCounts) - 1))),
            lens=0,
            ...);
         count_colors <- count_color_fn(sqrt(nCounts));
      } else {
         if (inverse_counts) {
            count_colors <- header_colors;
         } else {
            count_colors <- "darkorange3";
         }
      }
      venn_c <- matrix(ncol=venn_ncol, nrow=venn_nrow, data=NA);
      venn_c[cbind(set_rownums, set_colnums)] <- header_colors;
      venn_c[cbind(set_rownums + 1, set_colnums)] <- count_colors;

      # matrix indicating what colors to invert
      venn_i <- matrix(ncol=venn_ncol, nrow=venn_nrow, data=FALSE);
      if (inverse_title) {
         venn_i[cbind(set_rownums, set_colnums)] <- TRUE;
      }
      if (inverse_counts) {
         venn_i[cbind(set_rownums + 1, set_colnums)] <- TRUE;
      }
      
      ## signed counts
      if (any(lengths(gCounts) > 1)) {
         if (color_by_counts) {
            count_color_fn <- colorjam::col_linear_xf(
               x=max(sqrt(unlist(gCounts))),
               floor=max(c(0, min(sqrt(unlist(gCounts)) - 1))),
               lens=0,
               ...);
            gcount_colors <- count_color_fn(sqrt(unlist(gCounts)))
         } else {
            if (inverse_counts) {
               gcount_colors <- rep(header_colors, lengths(gCounts));
            } else {
               gcount_colors <- "darkorange3";
            }
         }
         if ("wide" %in% template) {
            seq_colnums <- rep(set_colnums + 1, lengths(gCounts)) +
               unlist(lapply(gCounts, function(a){
                  floor((seq_along(a) - 1) / 4)
               }));
            seq_rownums <- rep(set_rownums + 1, lengths(gCounts)) +
               unlist(lapply(gCounts, function(a){
                  (seq_along(a) - 1) %% 4
               })) - 1;
         } else {
            seq_colnums <- rep(set_colnums, lengths(gCounts));
            seq_rownums <- rep(set_rownums + 2, lengths(gCounts)) +
               unlist(lapply(gCounts, seq_along)) - 1;
            names(seq_rownums) <- jamba::makeNames(suffix="",
               rep(names(gCounts), lengths(gCounts)));
            names(seq_colnums) <- names(seq_rownums);
         }
         gbase_df <- curate_venn_labels(
            names(unlist(unname(gCounts))),
            type="all",
            unicode=unicode,
            curate_df=curate_df,
            ...);
         gbase_labels <- gbase_df$sign;
         gbase_colors <- gbase_df$color;
         gbase_hide <- (gbase_df$hide_singlet & unlist(gGCounts) == 1);
         gcount_labels <- sapply(seq_along(unlist(gCounts)), function(i){
            ilabel <- paste0(
               gbase_labels[i],
               ": ",
               format(trim=TRUE,
                  big.mark=",",
                  unlist(gCounts)[i]));
            ifelse(gbase_hide[i], "", ilabel)
         });
         ## order labels again?
         gdf <- jamba::mixedSortDF(data.frame(
            group=rep(seq_along(gCounts), lengths(gCounts)),
            label=gbase_labels,
            index=seq_along(gbase_labels),
            stringsAsFactors=FALSE), byCols=c(1, 2))
         gbase_labels <- gbase_labels[gdf$index];
         gbase_colors <- gbase_colors[gdf$index];
         gcount_labels <- gcount_labels[gdf$index];
         for (i in seq_along(gcount_labels)) {
            venn_m[seq_rownums[i], seq_colnums[i]] <- gcount_labels[[i]];
            venn_c[seq_rownums[i], seq_colnums[i]] <- gbase_colors[[i]];
            if (inverse_counts) {
               venn_i[seq_rownums[i],seq_colnums[i]] <- TRUE;
            }
         }
      }
      
      ## print color data.frame
      outdf <- print_color_df(venn_m,
         venn_c,
         venn_i,
         padding=padding,
         lightMode=lightMode,
         debug=debug,
         ...);
      if (any(c("html", "data.frames") %in% debug)) {
         return(handle_textvenn_debug(outdf, debug))
      }
      # return(invisible(sv));
   }
   if (TRUE %in% draw_legend) {
      svl <- venndir_legender(sv,
         legend_style="data.frame",
         ...)
      print(svl);
      #
   }
   invisible(sv);
}
