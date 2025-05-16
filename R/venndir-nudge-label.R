
#' Nudge venndir labels
#' 
#' Nudge venndir labels
#' 
#' Venndir labels are defined for each overlap polygon, with "inner"
#' and "outer" label coordinates for each polygon.
#' The `show_labels` argument to `venndir()` defines which labels
#' are placed inside and outside the Venn diagram polygons.
#' This function is useful to adjust the position of one or more
#' labels.
#' 
#' This function does not determine whether labels are displayed inside
#' or outside the Venn polygons.
#' 
#' @family venndir advanced
#' 
#' @param venndir_output output from `venndir()` as `Venndir` object.
#' @param set `character` name of the sets or overlaps to adjust.
#' @param x_offset,y_offset `numeric` coordinates to adjust, recycled
#'    to the number of entries provided in `set`.
#' @param offset_list `list` (default NULL) used as a shorthand alternative
#'    to `set`,`x_offset`,`y_offset`. The format is a `list` with x,y
#'    offset values, with list elements named by set. For example:
#'    `offset_list = list(setA=c(0, 0.1), set_B=c(-0.1, 0))`
#' @param align_y,align_x `character` string for optional alignment of
#'    labels, where all labels in `set` are aligned.
#'    * This option is recommended only with `label_location="outside"`,
#'    in order to align labels relative to each other.
#'    * It is recommeded for example, to make sure two top labels
#'    are placed at the same height relative to each other.
#'    * All labels in `set` are adjusted as a group, after applying
#'    `x_offset`,`y_offset`.
#'    * The coordinates of all labels in `set` are used, then the
#'    target coordinate is defined by the logic:
#'    * `"top","middle","bottom"` - uses the highest, middle, or lowest
#'    coordinate among the labels in `set`.
#'    * `"left","center","right"` - uses the left-most, center, or
#'    right-most  coordinate among the labels in `set`.
#' @param unit_type `character` string (default "relative") defining how
#'    to interpret the `x_offset`,`y_offset` values.
#'    * `"relative"` - interprets the adjustment relative to the plot
#'    bounding box, specifically the largest axis span.
#'    This option is useful when the plot span is not known.
#'    * `"absolute"` - interprets the adjustment with absolute units,
#'    which is useful when the plot span is known.
#' @param label_location `character` string (default "outside") indicating
#'    which label coordinate to adjust:
#'    * `"outside"` - will only adjust the outer label, leaving the inner
#'    label position unaffected.
#'    * `"inside"` - will only adjust the inner label, leaving the outer
#'    label position unaffected.
#'    * `"all"` - will adjust both the inner and outer label positions
#'    together.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#' 
#' @returns `Venndir` object after adjusting label coordinates
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
#' head(vo1@label_df[, c("x", "x_offset", "y", "y_offset")], 3)
#' subset(vo1@label_df, overlap_set %in% "set_A")[, c("x", "x_offset", "y", "y_offset")]
#' 
#' vo2 <- nudge_venndir_label(vo1,
#'    set=c("set_A", "set_B"),
#'    x_offset=c(-0.1, 0.1),
#'    y_offset=c(0))
#' render_venndir(vo2)
#' 
#' # alternative with offset_list
#' vo2b <- nudge_venndir_label(vo1,
#'    offset_list=list(
#'       set_A=c(0.1, 0),
#'       set_B=c(0.1, 0),
#'       set_C=c(0.4, 0.4)))
#' render_venndir(vo2b)
#' 
#' # now align two labels at the top
#' vo2c <- nudge_venndir_label(vo2b,
#'    set=c("set_A", "set_B"),
#'    align_y="top")
#' render_venndir(vo2c)
#' 
#' @export
nudge_venndir_label <- function
(venndir_output,
 set=NULL,
 x_offset=0,
 y_offset=0,
 offset_list=NULL,
 align_y=c("none",
    "top",
    "middle",
    "bottom"),
 align_x=c("none",
    "left",
    "center",
    "right"),
 unit_type=c("relative",
    "absolute"),
 label_location="outside",
 verbose=FALSE,
 ...)
{
   # validate input
   if (!inherits(venndir_output, "Venndir")) {
      stop("Input must inherit class Venndir.")
   }
   label_location <- match.arg(label_location,
      several.ok=TRUE,
      choices=c("outside", "inside", "all"));
   unit_type <- match.arg(unit_type);

   ## offset_list takes priority
   if (length(offset_list) > 0) {
      if (verbose) {
         jamba::printDebug("nudge_venndir_label(): ",
            "Using offset_list to define: ",
            c("set", "x_offset", "y_offset."));
      }
      set <- names(offset_list);
      x_offset <- sapply(offset_list, function(i){
         rep(i, length.out=2)[1]
      })
      y_offset <- sapply(offset_list, function(i){
         rep(i, length.out=2)[2]
      })
   }
   
   # confirm set is present in the data
   # todo: consider also recognizing setlist_names, if needed
   if (!any(set %in% venndir_output@label_df$overlap_set)) {
      warning(paste0(
         "No values found in venndir_output@label_df$overlap_set using set ",
         jamba::cPaste(paste0("'", set, "'"), sep=", ")));
      return(venndir_output);
   }
   x_offset <- rep(x_offset, length.out=length(set));
   y_offset <- rep(y_offset, length.out=length(set));
   label_location <- rep(label_location, length.out=length(set));
   names(x_offset) <- set;
   names(y_offset) <- set;
   names(label_location) <- set;
   
   # define units by max range on either axis
   if ("relative" %in% unit_type) {
      unit_scalars <- apply(bbox_JamPolygon(venndir_output@jps), 1, diff)
      unit_scalars <- rep(max(unit_scalars), length.out=2)
      x_offset <- x_offset * unit_scalars[1];
      y_offset <- y_offset * unit_scalars[2];
   }
   
   use_set <- which(set %in% venndir_output@label_df$overlap_set);
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
   if (!"x_offset" %in% colnames(venndir_output@label_df)) {
      venndir_output@label_df$x_offset <- 0;
   }
   if (!"y_offset" %in% colnames(venndir_output@label_df)) {
      venndir_output@label_df$y_offset <- 0;
   }
   
   # adjust each set x_offset, y_offset
   for (i in use_set) {
      iset <- set[i];
      if (verbose) {
         jamba::printDebug("nudge_venndir_label(): ",
            "iset: ", iset);
      }
      # define rows matching iset
      # 0.0.41.900 - do not subset by label type
      voi <- which(venndir_output@label_df$overlap_set %in% iset);
      # voi <- which(venndir_output@label_df$overlap_set %in% iset &
      #       venndir_output@label_df$type %in% label_types);
      
      if (any(c("inside", "all") %in% label_location)) {
         # adjust the inside which also adjusts the outside
         venndir_output@label_df$x[voi] <- venndir_output@label_df$x[voi] + x_offset[iset];
         venndir_output@label_df$y[voi] <- venndir_output@label_df$y[voi] + y_offset[iset];
         # venndir_output@label_df$show_label[voi] <- show_label[iset];
      }
      if ("inside" %in% label_location) {
         # if only the inside should be adjusted, counteradjust the outside
         venndir_output@label_df$x_offset[voi] <- venndir_output@label_df$x_offset[voi] - x_offset[iset];
         venndir_output@label_df$y_offset[voi] <- venndir_output@label_df$y_offset[voi] - y_offset[iset];
         # venndir_output@label_df$show_label[voi] <- show_label[iset];
      }
      if ("outside" %in% label_location) {
         # adjust the outside offset only
         venndir_output@label_df$x_offset[voi] <- venndir_output@label_df$x_offset[voi] + x_offset[iset];
         venndir_output@label_df$y_offset[voi] <- venndir_output@label_df$y_offset[voi] + y_offset[iset];
         # venndir_output@label_df$show_label[voi] <- show_label[i];
      }
      if (verbose) {
         jamba::printDebug("nudge_venndir_label(): ",
            indent=6,
            "adjusted set '", iset,
            "', rows: ", voi,
            ", x:", x_offset[iset],
            ", y:", y_offset[iset]);
         if (verbose > 1) {
            print(venndir_output@label_df[voi, , drop=FALSE]);# debug
         }
      }
   }
   
   # optional align_y
   if (!"none" %in% align_y) {
      isets <- set[use_set];
      voi <- which(venndir_output@label_df$overlap_set %in% isets);
      if (any(c("all", "inside") %in% label_location)) {
         if (verbose) {
            jamba::printDebug("nudge_venndir_label(): ",
               "Aligning inside label y values for ",
               jamba::cPaste(isets, sep=", "),
               ": ", align_y);
         }
         y_values <- venndir_output@label_df$y[voi];
         use_y <- ifelse("top" %in% align_y,
            max(y_values, na.rm=TRUE),
            ifelse("bottom" %in% align_y,
               min(y_values, na.rm=TRUE),
               mean(y_values, na.rm=TRUE)))
         y_diff <- use_y - y_values;
         venndir_output <- nudge_venndir_label(
            venndir_output=venndir_output,
            set=isets,
            x_offset=0,
            y_offset=y_diff,
            unit_type="absolute",
            label_location="inside",
            align_x="none",
            align_y="none")
      }
      if (any(c("all", "outside") %in% label_location)) {
         if (verbose) {
            jamba::printDebug("nudge_venndir_label(): ",
               "Aligning outside label y values for ",
               jamba::cPaste(isets, sep=", "),
               ": ", align_y);
         }
         y_values <- venndir_output@label_df$y[voi] +
            venndir_output@label_df$y_offset[voi]
         use_y <- ifelse("top" %in% align_y,
            max(y_values, na.rm=TRUE),
            ifelse("bottom" %in% align_y,
               min(y_values, na.rm=TRUE),
               mean(y_values, na.rm=TRUE)))
         y_diff <- use_y - y_values;
         venndir_output <- nudge_venndir_label(
            venndir_output=venndir_output,
            set=isets,
            x_offset=0,
            y_offset=y_diff,
            unit_type="absolute",
            label_location="outside",
            align_x="none",
            align_y="none")
      }
   }
   # optional align_x
   if (!"none" %in% align_x) {
      isets <- set[use_set];
      voi <- which(venndir_output@label_df$overlap_set %in% isets);
      if (any(c("all", "inside") %in% label_location)) {
         if (verbose) {
            jamba::printDebug("nudge_venndir_label(): ",
               "Aligning inside label x values for ",
               jamba::cPaste(isets, sep=", "),
               ": ", align_x);
         }
         x_values <- venndir_output@label_df$x[voi];
         use_x <- ifelse("right" %in% align_x,
            max(x_values, na.rm=TRUE),
            ifelse("left" %in% align_x,
               min(x_values, na.rm=TRUE),
               mean(x_values, na.rm=TRUE)))
         x_diff <- use_x - x_values;
         venndir_output <- nudge_venndir_label(
            venndir_output=venndir_output,
            set=isets,
            x_offset=x_diff,
            y_offset=0,
            unit_type="absolute",
            label_location="inside",
            align_x="none",
            align_y="none")
      }
      if (any(c("all", "outside") %in% label_location)) {
         if (verbose) {
            jamba::printDebug("nudge_venndir_label(): ",
               "Aligning outside label x values for ",
               jamba::cPaste(isets, sep=", "),
               ": ", align_x);
         }
         x_values <- venndir_output@label_df$x[voi] +
            venndir_output@label_df$x_offset[voi]
         use_x <- ifelse("right" %in% align_x,
            max(x_values, na.rm=TRUE),
            ifelse("left" %in% align_x,
               min(x_values, na.rm=TRUE),
               mean(x_values, na.rm=TRUE)))
         x_diff <- use_x - x_values;
         venndir_output <- nudge_venndir_label(
            venndir_output=venndir_output,
            set=isets,
            x_offset=x_diff,
            y_offset=0,
            unit_type="absolute",
            label_location="outside",
            align_x="none",
            align_y="none")
      }
   }
   
   return(venndir_output);
}
