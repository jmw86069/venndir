
#' Shrink data.frame by group
#' 
#' Shrink data.frame by group
#' 
#' This function condenses a `data.frame` by groups of rows,
#' applying an appropriate function to `character` columns,
#' and `numeric` columns. It is intended to be a simple but
#' configurable tool for the majority of scenarios.
#' 
#' This function uses `data.table` for overall speed.
#' 
#' @family venndir internal
#' 
#' @import data.table
#' 
#' @examples
#' testdf <- data.frame(check.names=FALSE,
#'    SYMBOL=rep(c("ACTB", "GAPDH", "PPIA"), c(2, 3, 1)),
#'    `logFC B-A`=c(1.4, 1.4, 2.3, NA, 2.3, 5.1),
#'    probe=paste0("probe", 1:6))
#' shrink_df(testdf, by="SYMBOL", num_func=function(x){mean(x, na.rm=TRUE)})
#' 
#' # 60,000 row simulation
#' testdftall <- do.call(rbind, lapply(1:10000, function(i){
#'    idf <- testdf;
#'    idf$SYMBOL <- paste0(idf$SYMBOL, "_", i);
#'    idf;
#' }))
#' head(testdftall, 12)
#' shrunk_tall <- shrink_df(testdftall,
#'    by="SYMBOL",
#'    num_func=function(x){mean(x, na.rm=TRUE)})
#' 
#' if (requireNamespace("jamses", quietly=TRUE)) {
#'    shrunk_tall2 <- jamses::shrinkDataFrame(testdftall,
#'       groupBy="SYMBOL")
#'    print(head(shrunk_tall2, 12))
#' }
#' 
#' @export
shrink_df <- function
(df,
 by,
 string_func=jamba::cPasteU,
 num_func=mean,
 extra_funcs=NULL,
 do_test=FALSE,
 verbose=FALSE,
 ...)
{
   if (!suppressPackageStartupMessages(require(data.table))) {
      stop("The data.table package is required.");
   }
   if (do_test) {
      df <- data.frame(A=rep(LETTERS[1:3], c(1,2,3)),
         B=1:6,
         C=rep(LETTERS[4:6], c(3,2,1)),
         stringsAsFactors=FALSE);
      by <- "C";
   }
   by <- intersect(by, colnames(df));
   if (length(by) == 0) {
      stop("'by' not found colnames(df).");
   }
   
   # identify colnames with data to be grouped
   use_names <- jamba::nameVector(setdiff(colnames(df), by));
   
   # define the appropriate function for each column
   func_set <- lapply(use_names, function(i){
      if (i %in% names(extra_funcs)) {
         extra_funcs[[i]];
      } else if (is.numeric(df[[i]])) {
         num_func;
      } else {
         string_func;
      }
   });

   # this function finds groups of identical functions
   # which can be combined in the same operation to
   # save some processing time
   compare_func_list <- function
   (l)
   {
      func_i <- rep(NA, length(func_set));
      names(func_i) <- names(func_set);
      i_seq <- seq_along(func_i);
      for (i in i_seq) {
         if (is.na(func_i[i])) {
            func_i[i] <- i;
            j_seq <- tail(i_seq, -i);
            for (j in j_seq) {
               k <- identical(func_set[[i]], func_set[[j]]);
               if (k) {
                  func_i[j] <- i;
               }
            }
         }
      }
      func_name_l <- split(names(func_set), func_i);
      func_l <- func_set[match(unique(func_i), func_i)];
      names(func_l) <- unique(func_i);
      return(list(names=func_name_l, fn=func_l));
   }
   
   func_sets <- compare_func_list(func_set);
   
   # create data.table using a key
   dt <- tryCatch({
      data.table::data.table(df, key=by);
   }, error=function(e){
      data.table::data.table(
         data.frame(df,
            stringsAsFactors=FALSE,
            check.names=FALSE),
         key=by);
   });
   
   # run each set of identical functions
   if (verbose) {
      jamba::printDebug("shrink_df(): ",
         "Running each data.table function set.");
   }
   dts <- lapply(seq_along(func_sets[[1]]), function(i){
      i_names <- func_sets[[1]][[i]];
      i_func <- func_sets[[2]][[i]];
      id1 <- which(names(dt) %in% i_names);
      dt1 <- dt[, lapply(.SD, i_func), by=by, .SDcols=id1];
   });
   
   # combine each data.table result
   if (verbose) {
      jamba::printDebug("shrink_df(): ",
         "Merging data.table function sets.");
   }
   dt2 <- do.call(`[`, dts);
   dt3 <- dt2[,colnames(dt), with=FALSE];
   if (verbose) {
      jamba::printDebug("shrink_df(): ",
         "Applying original object class.");
   }
   df3 <- as(dt3, head(class(df), 1));
   return(df3);
}
