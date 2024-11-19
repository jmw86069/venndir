
# customize Venndir
#
# prototype workflow
# - select a Venn overlap by name: "set_A&set_B"
# - modify the visual attributes
#   fontsize
#   fill,alpha,innerborder,outerborder,border
#   

#' Modify Venndir by overlap
#' 
#' Modify Venndir visual components by overlap name
#' 
#' This function provides a convenient method to modify visual features
#' of `Venndir` objects defined by overlap set. By "overlap set" it refers
#' to definitive sets, for example `"set_A"` refers to elements in `"set_A"`
#' which are not also present in any other set in the `Vennlist`.
#' Similarly `"set_A&set_B"` refers to elements only present in both
#' `"set_A"` and `"set_B"` and no other set in the `Vennlist`.
#' 
#' 
#' @returns `Venndir` object after making modifications to the `overlap_set`.
#' 
#' @param venndir_output `Venndir` object
#' @param overlap_set `character` string with an overlap set present in
#'    the `venndir_output` object.
#' @param params `list` of parameters to modify for the chosen `overlap_set`.
#'    Note that params with prefix `"label."` will be applied only to
#'    labels after removing the `"label."` prefix.
#'    For example: `"label.fill"` will apply the value to the `"fill"` color
#'    for the associated count label.
#'    Recognized parameter names:
#'    * `alpha` - alpha transparency to apply to the Venn fill color
#'    * `fill` - fill color for the Venn overlap polygon. Note that
#'    `alpha` is applied to this color, so to control both, the `alpha`
#'    must also be supplied with custom value.
#'    * `innerborder`,`innerborder.lwd` - innerborder color and line width.
#'    * `outerborder`,`outerborder.lwd` - outerborder color and line width.
#'    The default is NA for overlap polygons, since the outerborder is
#'    defined only for the main set.
#'    * `border`,`border.lwd` - border, displayed "on the line" between inner
#'    and outer border. The default is NA, since these lines are over-plotted
#'    when two Venn polygons are adjacent.
#'    * `fontsize` - `numeric` value applied to `jps` and `label_df`. When
#'    there are multiple labels, values are assigned in order. Usually:
#'    main count, then signed count(s).
#'    * `label.border`,`label.fill`,`label.lwd`,`label.lty` -
#'    applied to slot `"label_df"` columns `"border"`, `"fill"`, `"lwd"`, and
#'    `"lty"` respectively. They control the optional label border,fill,lwd,lty
#'    to allow custom styling for a specific overlap set.
#'    * `label.padding`,`label.padding_unit`,`label.r`,`label.r_unit`
#'    applied to slot `"label_df"` columns `"padding"`, `"padding_unit"`,
#'    `"r"`, and `"r_unit"` respectively. They control the optional label
#'    padding and r (corner radius) and are passed to
#'    `gridtext:richtext_grob()`. They may be useful to adjust the label border
#'    relative to the fontsize.
#' 
#' ## Other tips for customization
#' 
#' * The label placement can be customized using `nudge_venndir_label()`,
#' which allows moving the inside or outside labels for each overlap set.
#' When customizing the overlap set label fontsize, and border, it may
#' be useful to move the labels to minimize overlaps.
#' 
#' @family jam JamPolygon
#' 
#' @examples
#' testlist <- make_venn_test(n_sets=3, do_signed=TRUE);
#' vo2 <- venndir(testlist,
#'    show_labels="Ncsp",
#'    show_segments=FALSE,
#'    main="Default Venndir");
#' vo2m <- modify_venndir_overlap(vo2,
#'    overlap_set="set_A&set_B",
#'    params=list(fill="darkorange", alpha=0.8,
#'       innerborder="red", innerborder.lwd=3,
#'       outerborder="red", outerborder.lwd=3,
#'       fontsize=c(18, 13, 13, 13),
#'       label.border="black",
#'       label.padding=c(5, 1, 1, 1), label.r=c(5, 1, 1, 1),
#'       label.color=c("darkorange4", "firebrick4", "dodgerblue4", "grey44"),
#'       label.fill=jamba::alpha2col("gold", alpha=0.8)
#'    ));
#' render_venndir(vo2m, main="Highlighted overlap");
#' 
#' # nudge the label slightly higher
#' vo2mb <- nudge_venndir_label(vo2m,set="set_A&set_B",
#'    x_offset=-0.01, y_offset=0.08,
#'    label_location="inside");
#' render_venndir(vo2mb, main="Highlighted overlap");
#' 
#' # nudge a few labels using offset_list
#' vo2mc <- nudge_venndir_label(vo2m,
#'    offset_list=list(`set_A&set_B`=c(-0.01, 0.08),
#'       `set_A&set_C`=c(0, -0.03),
#'       `set_B&set_C`=c(0.04, -0.03),
#'       `set_A`=c(-0.02, 0),
#'       `set_B`=c(0.02, 0)),
#'    label_location="inside");
#' vo2mc <- nudge_venndir_label(vo2mc,
#'    set=c("set_A", "set_B"), align_y="top")
#' render_venndir(vo2mc, main="Highlighted overlap,\nnudged labels");
#' 
#' # to control drawing order, place one polygon at the end of the line
#' vo2md <- vo2mc;
#' vo2md@jps <- vo2mc@jps[c(1:6, 8:10, 7),]
#' render_venndir(vo2md, main="Altered drawing order");
#' 
#' # modify multiple overlap_sets at once
#' vo2multi2 <- modify_venndir_overlap(vo2,
#'    overlap_set=c("set_A", "set_B", "set_C"),
#'    params=list(fill=c("gold", "gold", "gold"),
#'       alpha=0.8,
#'       innerborder="red",
#'       innerborder.lwd=3,
#'       fontsize=list(c(18, 13, 13, 13)),
#'       label.border="black",
#'       label.fill=jamba::alpha2col("palegoldenrod", alpha=0.8)
#'    ), reorder=TRUE);
#' render_venndir(vo2multi2, main="Multiple effects at once");
#' 
#' # modify multiple overlap_sets at once
#' vo2multi3 <- modify_venndir_overlap(vo2,
#'    overlap_set=c("set_A&set_B", "set_B&set_C", "set_A&set_C",
#'       "set_A&set_B&set_C"),
#'    params=list(fill=c("gold"),
#'       alpha=0.8,
#'       border="red4",
#'       innerborder="red",
#'       innerborder.lwd=3,
#'       label.border="black",
#'       label.fill=jamba::alpha2col("palegoldenrod", alpha=0.8)
#'    ), reorder=TRUE);
#' render_venndir(vo2multi3, main="Multi-overlaps highlighed");
#' 
#' @export
modify_venndir_overlap <- function
(venndir_output,
 overlap_set,
 params=NULL,
 reorder=FALSE,
 debug=FALSE,
 ...)
{
   #
   if (!inherits(venndir_output, "Venndir")) {
      stop("Input must be Venndir.")
   }
   if (!all(overlap_set %in% venndir_output@jps@polygons$venn_name)) {
      stop(paste("overlap_set must be defined in",
         "venndir_output@jps@polygons$venn_name"))
   }
   
   # optionally recognize multiple overlap_set
   if (length(overlap_set) > 1) {
      # expand elements in params to length(overlap_set)
      params <- params[lengths(params) > 0];
      for (iparam in names(params)) {
         ivals <- params[[iparam]];
         if (inherits(ivals, "list")) {
            # expand a list by rep() in place
            params[[iparam]] <- rep(ivals, length.out=length(overlap_set))
         } else {
            # expand vector 
            params[[iparam]] <- rep(ivals, length.out=length(overlap_set))
         }
      }
      # iterate each overlap_set
      for (inum in seq_along(overlap_set)) {
         use_params <- lapply(params, function(i){
            unlist(i[[inum]]);
         })
         venndir_output <- modify_venndir_overlap(
            venndir_output=venndir_output,
            overlap_set=overlap_set[[inum]],
            params=use_params,
            reorder=reorder,
            debug=debug,
            ...);
      }
      return(invisible(venndir_output));
   }
   
   # it should always only define one row here
   rows_jps <- which(venndir_output@jps@polygons$venn_name %in% overlap_set &
         venndir_output@jps@polygons$type %in% "overlap");
   # optional debug output
   if (TRUE %in% debug) {
      jamba::printDebug("modify_venndir_overlap(): ",
         "jps@polygons:");
      print(venndir_output@jps@polygons[rows_jps, , drop=FALSE]);
   }

   ## Apply to the jps slot
   # recognized jps params
   params_jps <- c(
      alpha=0.6,
      fill=NA,
      innerborder=NA, innerborder.lwd=1,
      outerborder=NA, outerborder.lwd=1,
      border=NA, border.lwd=1)
   # populate params values
   for (iparam in names(params_jps)) {
      if (!iparam %in% names(params)) {
         # if not user-defined, use what exists
         if (iparam %in% colnames(venndir_output@jps@polygons)) {
            params[[iparam]] <- venndir_output@jps@polygons[rows_jps, iparam]
         } else {
            # apply the default if missing
            params[[iparam]] <- params_jps[[iparam]];
         }
      }
      # special handling for fill, to apply alpha as well
      if ("fill" %in% iparam) {
         params[["fill"]] <- jamba::alpha2col(
            unlist(params[["fill"]]),
            alpha=unlist(params[["alpha"]]))
      }
      # consider updating innerborder consistent with fill
      
      # ensure colname exists in jps@polygons
      if (!iparam %in% colnames(venndir_output@jps@polygons)) {
         # fill with NA of same type as column default value
         venndir_output@jps@polygons[[iparam]] <- head(
            c(NA, params_jps[[iparam]]), 1);
      }
      # apply the value to jps@polygons
      venndir_output@jps@polygons[rows_jps, iparam] <- params[[iparam]];
   }

   ## optionally grab some defaults from jps
   default_fontsize <- venndir_output@jps@polygons[rows_jps, "fontsize"]
   
   ## Apply to the label_df slot
   rows_label <- which(venndir_output@label_df$overlap_set %in% overlap_set);
   # optional debug output
   if (TRUE %in% debug) {
      jamba::printDebug("modify_venndir_overlap(): ",
         "label_df:");
      print(venndir_output@label_df[rows_label, , drop=FALSE]);
   }
   # recognized label params
   params_label <- c(
      fontsize=default_fontsize,
      label.color="black",
      label.border=NA,
      label.fill=NA,
      label.lwd=1,
      label.lty=1,
      label.padding=3,
      label.padding_unit="pt",
      label.r=3,
      label.r_unit="pt")
   # consider: border,fill,lty,lwd - for the label box
   # populate params values
   for (iparam in names(params_label)) {
      iparam2 <- gsub("^label[.]", "", iparam);
      if (!iparam %in% names(params)) {
         # if not user-defined, use what exists
         if (iparam2 %in% colnames(venndir_output@label_df)) {
            params[[iparam]] <- venndir_output@label_df[rows_label, iparam2]
         } else {
            # apply the default if missing
            params[[iparam]] <- params_jps[[iparam]];
         }
      }
      # apply the value to label_df
      venndir_output@label_df[rows_label, iparam2] <- rep(params[[iparam]],
         length.out=length(rows_label));
   }
   
   # optionally reorder polygons for rendering
   if (TRUE %in% reorder) {
      jpnrow <- nrow(venndir_output@jps@polygons);
      if (jpnrow > 1) {
         new_row_order <- c(setdiff(seq_len(jpnrow), rows_jps),
            rows_jps);
         venndir_output@jps <- venndir_output@jps[new_row_order, ];
      }
   }
   
   return(invisible(venndir_output));
}

#' Highlight Venndir by overlap
#' 
#' Highlight Venndir visual components by overlap name by calling
#' `modify_venndir_overlap()`
#' 
#' @rdname modify_venndir_overlap
#' @param bordercolor `character` color, provided as a convenient
#'    way to specify both `innerborder` and `outerborder` in
#'    `params`.
#' @param fill,innerborder,innerborder.lwd,border,border,lwd,outerborder,outerborder.lwd
#'    passed to `params` as convenient way to specify those elements.
#'    The default scenario uses innerborder with slightly darker border
#'    to be visible between adjacent border lines.
#'    To show the outerborder, use `outerborder.lwd=2` or higher.
#' 
#' @family venndir utility
#' 
#' @examples
#' testlist4 <- make_venn_test(n_sets=4, do_signed=FALSE)
#' vo4 <- venndir(testlist4, main="Default test case")
#' 
#' # show all sections with set_B
#' vo4h <- highlight_venndir_overlap(vo4,
#'    overlap_set=unique(grep("set_B", vo4@jps@polygons$venn_name, value=TRUE)))
#' render_venndir(vo4h, main="All overlaps with **set_B**")
#' 
#' # show set_C or set_D not set_B
#' vo4h2 <- highlight_venndir_overlap(vo4, outerborder.lwd=0,
#'    fill="gold",
#'    overlap_set=unique(grep("set_B", value=TRUE, invert=TRUE,
#'       grep("set_C|set_D", vo4@jps@polygons$venn_name, value=TRUE)))
#'       )
#' render_venndir(vo4h2, main="C or D, without B")
#' 
#' # show set_C not set_B or set_D
#' vo4h3 <- highlight_venndir_overlap(vo4,
#'    overlap_set=unique(grep("set_B|set_D", value=TRUE, invert=TRUE,
#'       grep("set_C", vo4@jps@polygons$venn_name, value=TRUE)))
#'       )
#' render_venndir(vo4h3, main="C without B or D")
#' 
#' # show set_A and set_C, not set_D
#' vo4h4 <- highlight_venndir_overlap(vo4,
#'    overlap_set=unique(grep("set_D", value=TRUE, invert=TRUE,
#'       grep("set_A.+set_C", vo4@jps@polygons$venn_name, value=TRUE)))
#'       )
#' render_venndir(vo4h4, main="A and C, without D")
#' 
#' # show set_A or set_B, each distinct
#' vo4h5 <- highlight_venndir_overlap(vo4,
#'    outerborder.lwd=0,
#'    overlap_set=unique(grep("&", value=TRUE, invert=TRUE,
#'       grep("set_A|set_B", vo4@jps@polygons$venn_name, value=TRUE)))
#'       )
#' render_venndir(vo4h5, main="A or B alone")
#' 
#' vo4h5 <- highlight_venndir_overlap(vo4,
#'    main="testing", bordercolor="blue", outerborder.lwd=0,
#'    overlap_set=unique(grep("&", value=TRUE, invert=TRUE,
#'       grep("set_C|set_D", vo4@jps@polygons$venn_name, value=TRUE)))
#'       )
#' vo4h5 <- highlight_venndir_overlap(vo4h5,
#'    main="testing", outerborder.lwd=0,
#'    overlap_set=unique(grep("&", value=TRUE, invert=TRUE,
#'       grep("set_A|set_B", vo4@jps@polygons$venn_name, value=TRUE)))
#'       )
#' render_venndir(vo4h5, main="A or B alone (red), or C or D alone (blue)")
#' 
#' # for complicated sections, use fill color as well
#' vo4h3way <- highlight_venndir_overlap(vo4,
#'    main="testing",
#'    params=list(innerborder="gold", innerborder.lwd=3,
#'       fill="yellow",
#'       outerborder.lwd=0),
#'    overlap_set=unique(grep("&.+&.+&", value=TRUE, invert=TRUE,
#'       grep("&.+&", vo4@jps@polygons$venn_name, value=TRUE)))
#'       )
#' render_venndir(vo4h3way, main="Three-way overlaps only")
#' 
#' # for complicated sections, use fill color as well
#' vo4h2way <- highlight_venndir_overlap(vo4,
#'    main="testing",
#'    params=list(innerborder="gold", innerborder.lwd=3,
#'       fill="yellow",
#'       outerborder.lwd=0),
#'    overlap_set=unique(grep("&.+&", value=TRUE, invert=TRUE,
#'       grep("&", vo4@jps@polygons$venn_name, value=TRUE)))
#'       )
#' render_venndir(vo4h2way, main="Two-way overlaps only")
#' 
#' @export
highlight_venndir_overlap <- function
(venndir_output,
 overlap_set,
 innerborder="red",
 innerborder.lwd=2,
 border="red3",
 border.lwd=1,
 outerborder=innerborder,
 outerborder.lwd=0,
 fill=NULL,
 params=list(
    fill=fill,
    innerborder=innerborder,
    innerborder.lwd=innerborder.lwd,
    border=border,
    outerborder=outerborder,
    outerborder.lwd=outerborder.lwd),
 reorder=TRUE,
 ...)
{
   #
   venndir_output <- modify_venndir_overlap(
      venndir_output=venndir_output,
      overlap_set=overlap_set,
      params=params,
      reorder=reorder,
      ...)
   return(invisible(venndir_output));
}

