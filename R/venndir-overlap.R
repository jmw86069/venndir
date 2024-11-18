
#' Signed overlaps
#' 
#' Calculate signed, directional overlaps across sets
#' 
#' This function is the core function to summarize overlaps
#' that include signed directionality. It is intended for
#' situations where two sets may share items, but where the
#' signed direction associated with those items may or may
#' not also be shared.
#' 
#' One motivating example is with biological data, where
#' a subset of genes, proteins, or regions of genome, may be
#' regulated up or down, and this direction is relevant
#' to understanding the biological process. Two experiments
#' may identify similar genes, proteins, or regions of
#' genome, but they may not regulate them in the same
#' direction. This function is intended to help summarize
#' item overlaps alongside the directionality of each item.
#' 
#' The directional counts can be summarized in slightly different
#' ways, defined by the argument `overlap_type`:
#' 
#' * `overlap_type="detect"` - default behavior: each vector in `setlist`
#'    is handled independently:
#'     * a vector with no names will use the vector
#'     values as items after converting them to `character`;
#'     * a named vector with `character` or `factor` values
#'     will will use the vector names as items,
#'     and character values as item values;
#'     * a named vector with `numeric` or `integer` values
#'     will use vector names as items, and will convert
#'     numeric values to `sign()`.
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
#' Note that `overlap_type="agreement"` and `overlap_type="concordance"`
#' will not convert `numeric` values to `sign()`, so if the input
#' contains `numeric` values such as `1.2435` they should probably be
#' converted to `sign()` before calling `signed_overlaps()`, for example:
#' `signed_overlaps(lapply(setlist, sign))`
#' 
#' @family venndir core
#' 
#' @param setlist `list` of named vectors, whose names represent
#'    set items, and whose values represent direction using 
#'    values `c(-1, 0, 1)`.
#' @param overlap_type `character` value indicating the type of
#'    overlap logic:
#'    * `"each"` records each combination of signs;
#'    * `"overlap"` disregards the sign and returns any match
#'    item overlap;
#'    * `"concordance"` represents counts for full
#'    agreement, or `"mixed"` for any inconsistent overlapping
#'    direction;
#'    * `"agreement"` represents full agreement in direction
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
#' @param keep_item_order `logical` default FALSE, to determine whether
#'    items will be stored and displayed in the order they are provided.
#'    Note: `keep_item_order=TRUE` enables the following behaviors:
#'    * Any `character` vector input will retain the order they appear.
#'    * Any `factor` vector input will sort items using factor `levels`,
#'    which maintains the factor level order.
#'    * Any named vector will use the `character` vector of names, keeping
#'    the order they appear in the vector.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param warn `logical` default FALSE, whether to print warnings during
#'    import in the event that input data is coerced to another type.
#' @param ... additional arguments are passed to `list2imsigned()`.
#' 
#' @returns `data.frame` with columns intended to support `venndir()`,
#' but which may be more widely useful:
#' * `"sets"` - character vector with sets and overlap names.
#' * one column indicating the `overlap_type` and corresponding values:
#'
#'    * `"overlap"` - This column is always included.
#'    * `"concordance"` - includes `1` (concordant) and `-1` (discordant)
#'    * `"agreement"` - includes `"agreement"` and `"disgreement"`
#'    * `"each"` - includes sign values `-1` and `1`.
#' 
#' * `"overlap"` - integer vector with overlap values, where `0` and `1`
#' indicate which sets contained these items. This column is always included,
#' even when `overlap_type` is something else.
#' * `"num_sets"` - integer number of sets represented in the overlap.
#' * `"count"` - integer number of items in the overlap.
#' * one colname for each set name represented in the `"sets"` column,
#' intended to help filter by each set. Values will be `0` or `1`.
#' * `overlap_label` - will represent only the non-0 elements from
#' `"overlap"` for convenience.
#' * `"items"` - when `return_items=TRUE` this column will contain
#' a `list` (in `AsIs` format) of `character` vectors, with the items.
#' 
#' 
#' @examples
#' setlist <- make_venn_test(100, 2, do_signed=FALSE);
#' setlist <- make_venn_test(1e6, 3, do_signed=FALSE);
#' 
#' # so is a data.frame
#' so <- signed_overlaps(setlist, verbose=TRUE);
#' so
#' 
#' # detect overlap_type
#' attr(signed_overlaps(setlist, "detect"), "overlap_type")
#' 
#' setlist <- make_venn_test(100, 2, do_signed=TRUE);
#' 
#' # detect overlap_type
#' attr(signed_overlaps(setlist, "detect"), "overlap_type")
#' 
#' # straight overlap counts
#' signed_overlaps(setlist, "overlap");
#' 
#' # each directional overlap count
#' signed_overlaps(setlist, "each");
#' 
#' # concordance overlap counts
#' signed_overlaps(setlist, "concordance");
#' 
#' # agreement overlap counts
#' signed_overlaps(setlist, "agreement");
#' 
#' # test to ensure factor input is handled properly
#' inputlist <- list(setA=factor(c("A", "B", "D")),
#'    setB=factor(c("A", "C", "E", "F")))
#' signed_overlaps(inputlist, return_items=TRUE)
#' 
#' # check to verify
#' signed_overlaps(inputlist, return_items=TRUE)$items
#' 
#' # test specific factor level order
#' inputlist <- list(
#'    setA=factor(c("A", "B", "D"), levels=c("D", "B", "A")),
#'    setB=factor(c("A", "C", "E", "F")))
#' signed_overlaps(inputlist, return_items=TRUE)
#' 
#' @export
signed_overlaps <- function
(setlist,
 overlap_type=c("detect",
    "each",
    "overlap",
    "concordance",
    "agreement"),
 return_items=FALSE,
 return_item_labels=return_items,
 sep="&",
 trim_label=TRUE,
 include_blanks=TRUE,
 keep_item_order=FALSE,
 verbose=FALSE,
 warn=FALSE,
 ...)
{
   ##
   overlap_type <- match.arg(overlap_type,
      several.ok=TRUE);
   
   ## 1sec
   # convert setlist to signed incidence matrix
   #svims <- list2im_value(setlist, ...);
   
   #
   if (verbose > 0) {
      jamba::printDebug("signed_overlaps(): ",
         "Processing input setlist.");
   }
   if (inherits(setlist, "Matrix") || inherits(setlist, "matrix")) {
      svims <- setlist;
   } else {
      # handle list input
      setlist <- lapply(setlist, function(i){
         if (length(names(i)) == 0) {
            ## No names, assume items are the elements
            if (TRUE %in% warn && inherits(i, c("numeric", "integer"))) {
               warning(paste("signed_overlaps():",
                  "un-named numeric values coerced to character."));
            }
            if (TRUE %in% warn && inherits(i, "factor")) {
               warning(paste("signed_overlaps():",
                  "factor values coerced to character."));
               if (TRUE %in% keep_item_order) {
                  i <- sort(i);
               }
            }
            i <- jamba::nameVector(rep(1, length(i)),
               as.character(i),
               makeNamesFunc=c);
         } else {
            ## Names exist, assume signed data
            if ("detect" %in% overlap_type) {
               # for "detect", convert numeric or integer input to sign(i)
               if (inherits(i, c("numeric", "integer")) &&
                     !all(i %in% c(-1, 0, 1, NA))) {
                  # convert numeric values to the sign
                  i[] <- sign(i);
               } else if (inherits(i, c("character", "factor"))) {
               # } else if (is.character(i) || is.factor(i)) {
                  if (!any(duplicated(i)) && length(i) > 3) {
                     if (TRUE %in% warn) {
                        warning(paste("signed_overlaps():",
                           "named character vector, non-duplicate items,",
                           "length > 3.",
                           "Values are used as items."));
                     }
                     i <- jamba::nameVector(rep(1, length(i)),
                        as.character(i),
                        makeNamesFunc=c);
                  }
               }
            }
         }
         i
      });
      svims <- list2im_value(setlist);
      # optionally maintain item order
      if (TRUE %in% keep_item_order) {
         item_order_list <- lapply(setlist, names);
         item_order <- unique(unlist(item_order_list));
         matchims <- match(item_order, rownames(svims));
         svims <- svims[matchims, , drop=FALSE];
      }
   }
   
   if (FALSE && !TRUE %in% keep_item_order) {
      if (verbose > 0) {
         jamba::printDebug("signed_overlaps(): ",
            "Processing keep_item_order=", keep_item_order);
      }
      item_order <- jamba::mixedSort(rownames(svims));
      matchims <- match(item_order, rownames(svims));
      svims <- svims[matchims, , drop=FALSE];
   }
   
   # optional verbose output
   if (verbose > 1) {
      jamba::printDebug("signed_overlaps(): ",
         "head(svims):");
      print(head(svims));# debug
   }
   
   # handle overlap_type="detect"
   if ("detect" %in% overlap_type) {
      if (verbose > 0) {
         jamba::printDebug("signed_overlaps(): ",
            "Processing overlap_type='", "detect", "'");
      }
      if (all(unique(as.vector(svims)) %in% c(0, 1, NA))) {
         overlap_type <- "overlap";
      } else {
         overlap_type <- head(setdiff(overlap_type, "detect"), 1);
         if (length(overlap_type) == 0) {
            overlap_type <- "concordance";
         }
      }
   }
   
   ## 0.02sec
   # convert to overlap vector (signed)
   # 0 0 1, 1 1 0, 1 1 1, etc.
   if (verbose > 0) {
      jamba::printDebug("signed_overlaps(): ",
         "Creating other data types.");
   }
   if (inherits(svims[1, 1], c("character", "factor"))) {
      # if data is stored as character, recognize c("",NA) as empty
      svimsl <- (svims != "") * 1
      # svimsl <- (!svims %in% c("", NA)) * 1; # new?
      svimsl[is.na(svims)] <- 0;
      svimss <- do.call(paste, lapply(seq_len(ncol(svims)), function(i){
         jamba::rmNA(naValue="", svims[, i]);
      }))
   } else {
      # if data is stored as numeric, recognize c(0,NA) as empty
      svimsl <- (svims != 0) * 1
      # svimsl <- (!svims %in% c(0, NA)) * 1; # new?
      svimsl[is.na(svims)] <- 0;
      svimss <- do.call(paste, lapply(seq_len(ncol(svims)), function(i){
         jamba::rmNA(naValue="0", svims[, i]);
      }))
   }
   
   ## 1.2sec
   # convert to overlap vector (un-signed)
   # 0 0 1, 1 1 0, 1 1 1, etc.
   if (verbose > 0) {
      jamba::printDebug("signed_overlaps(): ",
         "Creating overlap vector.");
   }
   svimsv <- do.call(paste,
      lapply(seq_len(ncol(svimsl)), function(i){
         svimsl[,i]
      }))
   
   ## concordance
   ## 0.02sec
   # for observed overlap vectors, determine which are concordant
   # (should be concordant* since up-up and down-down both get assigned TRUE)
   svimssu <- unique(svimss);
   if (verbose > 0) {
      jamba::printDebug("signed_overlaps(): ",
         "Creating concordance vector.");
   }
   svimssu_concordance <- jamba::nameVector(sapply(strsplit(svimssu, " "), function(i){
      j <- setdiff(i, c("","0"))
      length(unique(j)) == 1;
   }), svimssu);
   
   # optional verbose output
   if (verbose > 1) {
      jamba::printDebug("signed_overlaps(): ",
         "head(svimsl):");
      print(head(svimsl));# debug
      jamba::printDebug("signed_overlaps(): ",
         "head(svimss):");
      print(head(svimss));# debug
      jamba::printDebug("signed_overlaps(): ",
         "head(svimsv):");
      print(head(svimsv));# debug
      jamba::printDebug("signed_overlaps(): ",
         "svimssu:");
      print(svimssu);# debug
   }
   
   # alternate approach, split using overlap_type upfront, avoid data.table
   if (verbose > 0) {
      jamba::printDebug("signed_overlaps(): ",
         "Creating split names.");
   }
   svims_split_names <- sapply(jamba::nameVector(unique(svimsv)), function(i){
      paste(collapse=sep,
         colnames(svims)[strsplit(i, " ")[[1]] %in% "1"])
   });
   svims_split_name_ct <- sapply(strsplit(jamba::nameVectorN(svims_split_names), " "), function(i){
      sum(as.numeric(i))
   })
   if (verbose > 0) {
      jamba::printDebug("signed_overlaps(): ",
         "Creating final vector.");
   }
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
   if (verbose > 0) {
      jamba::printDebug("signed_overlaps(): ",
         "Splitting by observed directions per overlap.");
   }
   svims_split <- split(svimss, svimsv_olt);

   # Create labels for each split
   if (verbose > 0) {
      jamba::printDebug("signed_overlaps(): ",
         "Creating labels for each split.");
   }
   svims_df <- data.frame(
      sets=gsub("^(.+)[|]([^|]+)$",
         "\\1",
         names(svims_split)),
      overlap_type=gsub("^(.+)[|]([^|]+)$",
         "\\2",
         names(svims_split)),
      stringsAsFactors=FALSE);
   rownames(svims_df) <- names(svims_split);
   colnames(svims_df) <- c("sets", overlap_type);
   svims_df$overlap <- names(svims_split_names)[match(svims_df$sets, svims_split_names)];
   svims_df$num_sets <- svims_split_name_ct[svims_df$overlap];
   svims_df$count <- lengths(svims_split[names(svims_split)]);

   # add one column per setlist name
   sldf <- data.frame(jamba::rbindList(strsplit(svims_df$overlap, " ")),
      stringsAsFactors=FALSE);
   colnames(sldf) <- colnames(svims);
   svims_df[,colnames(svims)] <- sldf;
   
   # optionally include blank entries where no overlaps are present
   if (TRUE %in% include_blanks) {
      if (verbose > 0) {
         jamba::printDebug("signed_overlaps(): ",
            "Processing include_blanks=", "TRUE");
      }
      blank_df <- make_venn_combn_df(colnames(svims));
      blank_df_num <- rowSums(blank_df);
      blank_df$sets <- rownames(blank_df);
      blank_df$count <- 0;
      blank_df$overlap <- jamba::pasteByRow(blank_df[,colnames(svims),drop=FALSE], sep=" ");
      blank_df$num_sets <- blank_df_num;
      rownames(blank_df) <- paste0(blank_df$sets, "|", blank_df$overlap);
      if ("agreement" %in% overlap_type) {
         blank_df[[overlap_type]] <- "agreement";
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
   if (verbose > 0) {
      jamba::printDebug("signed_overlaps(): ",
         "Sorting rows by overlap count then set.");
   }
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
   if (TRUE %in% return_items) {
      if (verbose > 0) {
         jamba::printDebug("signed_overlaps(): ",
            "Processing return_items=", "TRUE");
      }
      if (TRUE %in% keep_item_order) {
         svitems_split <- split(
            factor(rownames(svims), levels=rownames(svims)),
            svimsv_olt);
      } else {
         svitems_split <- split(rownames(svims), svimsv_olt);
      }
      imatch <- match(rownames(svims_df2), names(svitems_split));
      svims_df2$items <- I(svitems_split[imatch]);
   }
   attr(svims_df2, "overlap_type") <- overlap_type;
   return(svims_df2);
}

#' Make full Venn combination data.frame
#' 
#' Make full Venn combination data.frame
#' 
#' This function returns a `data.frame` with all possible
#' combinations of set overlaps for the number of sets
#' provided.
#' 
#' @return `data.frame` where rownames indicate each possible
#'    Venn overlap, colnames indicate each set name, and values
#'    are `0` or `1` indicating whether each set should have
#'    a value in each row.
#' 
#' @family venndir utility
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
   setdf <- data.frame(
      check.names=FALSE,
      stringsAsFactors=FALSE,
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
