
#' Nudge venndir label
#' 
#' Nudge venndir label, work in progress currently
#' 
#' @family venndir utility
#' 
#' @param venndir_output output from `venndir()` as `Venndir` object.
#' @param set `character` name of the set or overlap to adjust
#' 
#' @returns `Venndir` object as output from `venndir()`
#' 
#' @examples
#' setlist1 <- make_venn_test(100, 3, do_signed=TRUE)
#' 
#' vo1 <- venndir(setlist1,
#'    overlap_type="each",
#'    return_items=TRUE,
#'    label_style="lite_box",
#'    main="Default venndir")
#' render_venndir(vo1)
#' 
#' vo2 <- nudge_venndir_label(vo1,
#'    set=c("set_A&set_B&set_C"),
#'    x_offset=0.05,
#'    y_offset=-0.05)
#' render_venndir(vo2)
#' 
#' @export
nudge_venndir_label <- function
(venndir_output,
 set=NULL,
 x_offset=0,
 y_offset=0,
 unit_type=c("relative",
    "absolute"),
 label_types=c("main",
    "signed"),
 label_location=c("all",
    "inside",
    "outside"),
 show_label=TRUE,
 verbose=FALSE,
 ...)
{
   # validate input
   label_location <- match.arg(label_location);
   unit_type <- match.arg(unit_type);
   if (!any(set %in% venndir_output@label_df$overlap_set)) {
      warning(paste0("No values found in label_df$overlap_set using set:",
         jamba::cPaste(paste0("'", set, "'"))));
      return(venndir_output);
   }
   x_offset <- rep(x_offset, length.out=length(set));
   y_offset <- rep(y_offset, length.out=length(set));
   show_label <- rep(show_label, length.out=length(set));
   
   # adjust by unit_type "relative" by using the bounding box dimensions
   if ("Venndir" %in% class(venndir_output)) {
      vo <- venndir_output;
   } else {
      stop("Input must be a Venndir object.");
   }
   if ("relative" %in% unit_type) {
      unit_scalars <- apply(bbox_JamPolygon(vo@jps), 1, diff)
      x_offset <- x_offset * unit_scalars[1];
      y_offset <- y_offset * unit_scalars[2];
   }
   use_set <- which(set %in% vo@label_df$overlap_set);
   if (verbose) {
      jamba::printDebug("nudge_venndir_label(): ",
         "x_offset: ", x_offset);
      jamba::printDebug("nudge_venndir_label(): ",
         "y_offset: ", y_offset);
      jamba::printDebug("nudge_venndir_label(): ",
         "unit_scalars: ", unit_scalars);
      jamba::printDebug("nudge_venndir_label(): ",
         "use_set: ", use_set);
   }
   # create new column if needed
   if (!"x_offset" %in% colnames(vo@label_df)) {
      vo@label_df$x_offset <- 0;
   }
   if (!"y_offset" %in% colnames(vo@label_df)) {
      vo@label_df$y_offset <- 0;
   }
   
   # adjust each set x_offset, y_offset
   for (i in use_set) {
      iset <- set[i];
      if (verbose) {
         jamba::printDebug("nudge_venndir_label(): ",
            "iset: ", iset);
      }
      voi <- which(vo@label_df$overlap_set %in% iset &
            vo@label_df$type %in% label_types);
      if ("all" %in% label_location) {
         # adjust the inside which also adjusts the outside
         vo@label_df$x[voi] <- vo@label_df$x[voi] + x_offset[i];
         vo@label_df$y[voi] <- vo@label_df$y[voi] + y_offset[i];
         vo@label_df$show_label[voi] <- show_label[i];
      } else if ("inside" %in% label_location) {
         # adjust main label location
         vo@label_df$x[voi] <- vo@label_df$x[voi] + x_offset[i];
         vo@label_df$y[voi] <- vo@label_df$y[voi] + y_offset[i];
         vo@label_df$show_label[voi] <- show_label[i];
         # adjust outside location exactly opposite to offset
         vo@label_df$x_offset[voi] <- vo@label_df$x_offset[voi] - x_offset[i];
         vo@label_df$y_offset[voi] <- vo@label_df$y_offset[voi] - y_offset[i];
         vo@label_df$show_label[voi] <- show_label[i];
      } else if ("outside" %in% label_location) {
         # adjust the outside offset only
         vo@label_df$x_offset[voi] <- vo@label_df$x_offset[voi] + x_offset[i];
         vo@label_df$y_offset[voi] <- vo@label_df$y_offset[voi] + y_offset[i];
         vo@label_df$show_label[voi] <- show_label[i];
      }
      if (verbose) {
         jamba::printDebug("nudge_venndir_label(): ",
            indent=6,
            "voi: ", voi);
         jamba::printDebug("nudge_venndir_label(): ",
            indent=6,
            "applied x_offset: ", x_offset[i],
            ", applied y_offset: ", y_offset[i],
            ", applied show_label: ", show_label[i])
         print(vo@label_df[voi, , drop=FALSE])
      }
   }
   return(vo);
}
