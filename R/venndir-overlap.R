
#' Signed overlaps
#' 
#' Signed overlaps
#' 
#' This function is the core function to summarize overlaps
#' including directionality (signed).
#' 
#' The directional counts can be summarized in slightly different
#' ways, defined by the argument `overlap_type`:
#' 
#' * `overlap_type="each"` - this option returns all possible
#' directions individually counted.
#' * `overlap_type="concordance"` - this option returns the counts
#' for each consistent direction, for example `"up-up-up"` would
#' be counted, and `"down-down-down"` would be counted, but any
#' mixture of `"up"` and `"down"` would be summarized and counted
#' as `"mixed"`. For 3-way overlaps, there are 8 possible directions,
#' the labels are difficult to place in the Venn diagram, and are not
#' altogether meaningful. Note that this option is the default
#' for `venndir()`.
#' * `overlap_type="overlap"` - this option only summarizes overlaps
#' without regard to direction. This option returns standard Venn
#' overlap counts.
#' * `overlap_type="agreement"` - this option groups all directions
#' that agree and returns them as `"concordant"`, all others are
#' returned as `"mixed"`.
#' 
#' @family venndir overlaps
#' 
#' @param setlist `list` of named vectors, whose names represent
#'    set items, and whose values represent direction using 
#'    values `c(-1, 0, 1)`.
#' @param overlap_type `character` value indicating the type of
#'    overlap logic: `"each"` records each combination of
#'    signs; `"overlap"` disregards the sign and returns any match
#'    item overlap; `"concordance"` represents counts for full
#'    agreement, or `"mixed"` for any inconsistent overlapping
#'    direction; `"agreement"` represents full agreement in direction
#'    as `"agreement"`, and `"mixed"` for any inconsistent
#'    direction.
#' @param return_items `logical` indicating whether to return
#'    the items within each overlap set.
#' @param return_item_labels `logical` indicating whether to return
#'    the directional label associated with each item. A directional
#'    label combines the direction from `setlist` by item.
#' @param sep `character` used as a delimiter between set names,
#'    the default is `"&"`.
#' @param trim_label `logical` indicating whether to trim the
#'    directional label, for example instead of returning `"0 1 -1"`
#'    it will return `"1 -1"` because the overlap name already
#'    indicates the sets involved.
#' @param include_blanks `logical` indicating whether each set overlap
#'    should be represented at least once even when no items are
#'    present in the overlap. When `include_blanks=TRUE` is useful
#'    in that it guarantees all possible combinations of overlaps
#'    are represented consistently in the output.
#' @param ... additional arguments are passed to `list2imsigned()`.
#' 
#' @export
signed_overlaps <- function
(setlist,
 overlap_type=c("each",
    "overlap",
    "concordance",
    "agreement"),
 return_items=FALSE,
 return_item_labels=return_items,
 sep="&",
 trim_label=TRUE,
 include_blanks=TRUE,
 ...)
{
   ##
   overlap_type <- match.arg(overlap_type);
   
   ## 1sec
   # convert setlist to signed incidence matrix
   #svims <- list2im_signed(setlist, ...);
   if (inherits(setlist, "Matrix") || inherits(setlist, "matrix")) {
      svims <- setlist;
   } else {
      setlist <- lapply(setlist, function(i){
         if (length(names(i)) == 0) {
            if (is.numeric(i)) {
               warning("signed_overlaps(): input list contains vector with un-named numeric values.");
            }
            i <- jamba::nameVector(rep(1, length(i)), i);
         }
         i
      });
      svims <- list2im_signed(setlist);
   }
   
   ## 0.02sec
   # convert to overlap vector (signed)
   # 0 0 1, 1 1 0, 1 1 1, etc.
   if (is.character(svims[1,1])) {
      # if data is stored as character, change NA to ""
      svimsl <- (svims != "") * 1
      svimsl[is.na(svims)] <- 0;
      svimss <- do.call(paste, lapply(seq_len(ncol(svims)), function(i){
         jamba::rmNA(naValue="", svims[,i]);
      }))
   } else {
      # if data is stored as numeric, change NA to "0"
      svimsl <- (svims != 0) * 1
      svimsl[is.na(svims)] <- 0;
      svimss <- do.call(paste, lapply(seq_len(ncol(svims)), function(i){
         jamba::rmNA(naValue="0", svims[,i]);
      }))
   }
   
   ## 1.2sec
   # convert to overlap vector (un-signed)
   # 0 0 1, 1 1 0, 1 1 1, etc.
   svimsv <- do.call(paste, lapply(seq_len(ncol(svimsl)), function(i){svimsl[,i]}))
   
   ## concordance
   ## 0.02sec
   # for observed overlap vectors, determine which are concordant
   # (should be concordant* since up-up and down-down both get assigned TRUE)
   svimssu <- unique(svimss);
   svimssu_concordance <- jamba::nameVector(sapply(strsplit(svimssu, " "), function(i){
      j <- setdiff(i, c("","0"))
      length(unique(j)) == 1;
   }), svimssu);
   
   # alternate approach, split using overlap_type upfront, avoid data.table
   svims_split_names <- sapply(jamba::nameVector(unique(svimsv)), function(i){
      paste(collapse=sep,
         colnames(svims)[strsplit(i, " ")[[1]] %in% "1"])
   });
   svims_split_name_ct <- sapply(strsplit(jamba::nameVectorN(svims_split_names), " "), function(i){
      sum(as.numeric(i))
   })
   if ("overlap" %in% overlap_type) {
      svimsv_olt <- paste(svims_split_names[svimsv],
         sep="|",
         svimsv);
   } else if ("concordance" %in% overlap_type) {
      svimsv_olt <- paste(svims_split_names[svimsv],
         sep="|",
         ifelse(svimssu_concordance[svimss], svimss, "mixed"));
   } else if ("agreement" %in% overlap_type) {
      svimsv_olt <- paste(svims_split_names[svimsv],
         sep="|",
         ifelse(svimssu_concordance[svimss], "agreement", "mixed"));
   } else {
      svimsv_olt <- paste(svims_split_names[svimsv],
         sep="|",
         svimss);
   }
   
   # split by observed directions within each overlap set
   ## 0.07 sec
   svims_split <- split(svimss, svimsv_olt);

   # Create labels for each split
   svims_df <- data.frame(jamba::rbindList(strsplit(names(svims_split), "[|]")),
      check.names=FALSE);
   rownames(svims_df) <- names(svims_split);
   colnames(svims_df) <- c("sets", overlap_type);
   svims_df$overlap <- names(svims_split_names)[match(svims_df$sets, svims_split_names)];
   svims_df$num_sets <- svims_split_name_ct[svims_df$overlap];
   svims_df$count <- lengths(svims_split[names(svims_split)]);

   # add one column per setlist name
   sldf <- data.frame(jamba::rbindList(strsplit(svims_df$overlap, " ")));
   colnames(sldf) <- colnames(svims);
   svims_df[,colnames(svims)] <- sldf;
   
   # optionally include blank entries where no overlaps are present
   if (include_blanks) {
      blank_df <- make_venn_combn_df(colnames(svims));
      blank_df_num <- rowSums(blank_df);
      blank_df$sets <- rownames(blank_df);
      blank_df$count <- 0;
      blank_df$overlap <- jamba::pasteByRow(blank_df[,colnames(svims),drop=FALSE], sep=" ");
      blank_df$num_sets <- blank_df_num;
      rownames(blank_df) <- paste0(blank_df$sets, "|", blank_df$overlap);
      if ("agreement" %in% overlap_type) {
         blank_df[[overlap_type]] <- "concordant";
      } else {
         blank_df[[overlap_type]] <- blank_df$overlap;
      }
      if (any(!blank_df$overlap %in% svims_df$overlap)) {
         missing_overlap <- setdiff(blank_df$overlap, svims_df$overlap);
         add_df <- subset(blank_df, overlap %in% missing_overlap)[,colnames(svims_df),drop=FALSE];
         svims_df <- rbind(svims_df, add_df);
      }
   }

   # order rows by number of overlaps, then by set
   svims_df2 <- jamba::mixedSortDF(svims_df, byCols=c("num_sets", paste0("-", names(setlist))))
   svims_split_names2 <- jamba::nameVector(svims_df2[,c("sets", "overlap")]);
   svims_split_names3 <- jamba::nameVector(svims_df2[,c("overlap", "sets")]);
   
   # optionally clean up the label
   if (trim_label) {
      svims_df2[,"overlap_label"] <- gsub("^[ ]+|[ ]+$", "",
         gsub("[ ]+", " ",
         gsub("0", "",
         svims_df2[,overlap_type])));
   }
   
   # optionally include items
   if (return_items) {
      svitems_split <- split(rownames(svims), svimsv_olt);
      imatch <- match(rownames(svims_df2), names(svitems_split));
      svims_df2$items <- I(svitems_split[imatch]);
   }
   return(svims_df2);

   # 0.08sec
   # assemble data.frame with relevant counts
   svims_split_count_dfs <- lapply(svims_split_names2, function(iname){
      olname <- svims_split_names3[iname];
      i <- svims_split[[paste0(iname, "|", olname)]];
      idf <- data.frame(count=as(table(i), "matrix"), check.names=FALSE);
      idf$each <- rownames(idf);
      idf$concordance <- ifelse(svimssu_concordance[idf$each], idf$each, "mixed");
      idf$agreement <- ifelse(svimssu_concordance[idf$each], "concordant", "mixed");
      idf$overlap <- olname;
      idf$overlap_set <- iname;
      idf;
   });
   # combine all rows
   svims_split_count_df <- jamba::rbindList(svims_split_count_dfs);
   
   # group using overlap_type unless "each" where it returns everything
   if (!"each" %in% overlap_type) {
      svims_split_count_df$overlap_set <- factor(svims_split_count_df$overlap_set,
         levels=unique(svims_split_count_df$overlap_set));
      grp_colnames <- unique(c("overlap_set",
         "overlap",
         overlap_type,
         "count"));
      svims_split_count_df_g <- shrink_df(svims_split_count_df[,grp_colnames],
         by=c("overlap_set", overlap_type),
         num_func=sum);
   } else {
      svims_split_count_df_g <- svims_split_count_df;
   }
   rownames(svims_split_count_df_g) <- jamba::pasteByRow(svims_split_count_df_g[,c("overlap_set", overlap_type)],
      sep=" ");
   
   # optionally include items
   if (return_items) {
      if ("overlap" %in% overlap_type) {
         svims_split <- paste(svims_split_names2[svimsv],
            svimsv);
      } else if ("concordance" %in% overlap_type) {
         svims_split <- paste(svims_split_names2[svimsv],
            ifelse(svimssu_concordance[svimss],
               svimss,
               "mixed"));
      } else if ("agreement" %in% overlap_type) {
         svims_split <- paste(svims_split_names2[svimsv],
            ifelse(svimssu_concordance[svimss],
               "concordant",
               "mixed"));
      } else {
         svims_split <- paste(svims_split_names2[svimsv],
            svimss);
      }
      svims_items <- split(rownames(svims),
         svims_split);
      ol_id <- jamba::pasteByRow(svims_split_count_df_g[,c("overlap_set", overlap_type)],
         sep=" ");
      svims_items2 <- svims_items[ol_id];
      svims_split_count_df_g$items <- I(svims_items2);
   }

   return(svims_split_count_df_g);
}

#' Make full Venn combination data.frame
#' 
#' Make full Venn combination data.frame
#' 
#' This function returns a `data.frame` with all possible
#' combinations of set overlaps for the number of sets
#' provided.
#' 
#' @param x either character vector of set names, or integer number
#'    of sets.
#' @param include_zero logical indicating whether to include one
#'    row with all zeros, in the event of counting factors where
#'    some factor levels may not be present in any sets.
#' @param sep character separator used when combining set names into
#'    a single label.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' make_venn_combn_df(3);
#' make_venn_combn_df(3, include_zero=TRUE);
#' 
#' make_venn_combn_df(letters[1:3]);
#' 
#' @export
make_venn_combn_df <- function
(x,
 include_zero=FALSE,
 sep="&",
 ...)
{
   if (is.numeric(x) && length(x) == 1 && round(x) == x) {
      x <- paste0("set_", seq_len(x));
   }
   setdf <- data.frame(check.names=FALSE,
      as.list(
         jamba::nameVector(
            rep(0, length(x)),
            x)));
   setdfs <- jamba::rbindList(lapply(seq_along(x), function(i){
      combn_m <- t(combn(x, m=i));
      rownames(combn_m) <- unname(jamba::pasteByRow(combn_m, sep=sep));
      jamba::rbindList(lapply(rownames(combn_m), function(k){
         j <- combn_m[k,];
         setdf1 <- setdf;
         setdf1[,j] <- 1;
         rownames(setdf1) <- k;
         setdf1;
      }))
   }));
   if (include_zero) {
      rownames(setdf) <- "none";
      setdfs <- rbind(setdf, setdfs);
   }
   return(setdfs);
}

