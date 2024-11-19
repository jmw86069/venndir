
#' Check Venndir object
#' 
#' Check Venndir object integrity
#' 
#' ## Todo:
#' 
#' * Write additional validation checks.
#' 
#' @export
#' 
#' @family JamPolygon
#'
#' @param object `Venndir` object
#' 
#' @export
check_Venndir <- function
(object)
{
   # basic check to confirm all slot names exist
   # all(c("jp", "jps", "label_df", "setlist") %in% slotNames(object))
   is_valid <- (c("jps", "label_df", "setlist") %in% slotNames(object))
   # jamba::printDebug("is_valid:", is_valid);
   
   # new practical validation
   vnames <- object@jps@polygons$venn_name;
   vsets <- unique(unlist(strsplit(vnames, "&")))
   onames <- object@label_df$overlap_set;
   osets <- unique(unlist(strsplit(onames, "&")))
   # validate venn contents
   is_valid_venn <- (
      all(vsets %in% names(object@setlist)) &&
      all(osets %in% names(object@setlist)) &&
      all(length(vnames) == 0 ||
         all(object@jps@polygons$venn_name %in% object@label_df$overlap_set))
   )
   all(is_valid) && all(is_valid_venn)
}

#' Venndir class
#' 
#' Venndir class
#' 
#' This object is intended to contain all the required data to produce
#' a venndir figure. The components in this object can be edited.
#' 
#' Slots:
#' 
#' * **jps**: `JamPolygon` containing the set and overlap polygons.
#' 
#'    * Columns `"x"`,`"y"` contain polygon coordinates, see
#'    `JamPolygon-class` for details on multi-part polygons, and polygons
#'    with holes.
#'    * The polygon type is defined with column `type` and values
#'    `"set"` or `"overlap"`.
#'    * The "set" polygons are intended to be the full circle for each
#'    set, but may be `NA` or `NULL` to indicate the set is empty.
#'    * The "overlap" polygons are expected to be contained inside
#'    "set" polygons, and each overlap polygon may be multi-polygons
#'    with or without holes.
#'    For example, the "set" polygon for `set_A` which is a circle fully
#'    contained inside another set `set_B`.
#'    The "overlap" polygon for `set_A` would be empty, since there is no
#'    region unique to `set_A`.
#'    However the "overlap" polygon `set_A&set_B`
#'    would contain the full circle defined by "set" polygon `set_A`.
#'    Lastly the "overlap" polygon for `set_B` would be a circle with a hole
#'    located at "overlap" `set_A&set_B`.
#'    or ellipse. The overlap polygons are polygons, possibly multi-part
#'    with or without holes, or may be `NA` or `NULL` to indicate that
#'    this overlap does not have a corresponding polygon.
#'    * The `name` column contains set or overlap
#'    names, where multiple sets are concatenated with `"&"` by default.
#'    or ellipse corresponding to each set in `setlist`), and the overlap
#'    polygons. Where an overlap does not exist, the polygon coordinates
#'    will be empty, or will have entirely `NA` values for "x" and "y".
#' * **label_df**: `data.frame` which contains information about
#'    Venn labels placement and content.
#'    It should contain one row for each Venn overlap.
#'    When data contains signed overlaps, each overlap is sub-divided
#'    by the combination of overlap signs, with one row per overlap and
#'    overlap signs.
#'    Overlap signs are summarized based upon argument `overlap_type`,
#'    see `signed_overlaps()`.
#'    * `"overlap_sign"` represents the combination of overlap name,
#'    and the overlap sign across all sets.
#'    * `"text"` indicates the count label to be displayed.
#'    * `"venn_counts"` indicates the numeric number of Venn overlaps
#'    in each row.
#'    * `"item"` should be a `list` that contains `character` vectors
#'    of items in each overlap set. Column `"item"` is optional.
#'    * `"nsets"` indicates the number of sets involved in the overlap.
#'    * `"x"`, `"y"` represent the label coordinate to use inside
#'    each polygon overlap. When the label is displayed outside the polygon,
#'    the values `"x_offset"` and `"y_offset"` are added to `"x"` and `"y"`,
#'    respectively. In this way, a line segment can be drawn from the outer
#'    label back to the corresponding polygon.
#'    * `"color"` indicates the color for each label. Note the
#'    actual color may be modified based upon the background fill color
#'    to maximize the contrast and visibility of the color. For example
#'    blue on a dark background is displayed as light blue.
#'    * `"fontsize"` indicates the base font size, prior to any
#'    optional adjustment with `font_cex`.
#'    * `"fill"` indicates an optional color fill behind the count
#'    label. When `NULL` (default) there is no color fill. This value
#'    is usually populated by argument `label_style` which is passed to
#'    `venndir_label_preset()`.
#'    * `"border"` indicates an optional border around count labels,
#'    and is handled similar to `"fill"`.
#'    * `"padding"`, `"padding_unit"`, `"r"`, and `"r_unit"` are
#'    used with `gridtext::richtext_grob()` to define padding around count
#'    labels, and optional rounded corners when border is displayed.
#'    * Other columns are intended for internal use by `render_venndir()`
#'    and are not yet fully editable.
#' * **setlist**: `list` with the `setlist` used to create the Venn overlaps.
#'    Previously this data could be inferred from `label_df` which was
#'    tedious, and required column `"item"` which is optional.
#'    That said, `setlist` can be an empty `list()`.
#' * **metadata**: `list` with optional metadata, intended for future
#'    expansion, such as plot title.
#' 
#' @family JamPolygon
#' 
setClass("Venndir",
   slots=c(
      # jp="JamPolygon",
      jps="JamPolygon",
      label_df="data.frame",
      setlist="list",
      metadata="list"
   ),
   prototype=prototype(
      jps=NULL,
      label_df=data.frame(),
      setlist=list(),
      metadata=list()
   ),
   validity=check_Venndir
);


#' Plot Venndir object
#' 
#' Plot Venndir object
#' 
#' @returns `Venndir` object, invisibly.
#' 
#' @docType methods
#' @rdname Venndir-methods
#' @family venndir utility
#' 
#' @export
setMethod("plot",
   signature=c(x="Venndir", y="ANY"),
   definition=function(x, y, ...) {
      if (missing(y)) {
         y <- 0;
      }
      render_venndir(x, ...)
   })

# Todo:
# * print(), summary() functions

#' Return setlist length for Venndir object
#' 
#' @docType methods
#' @rdname Venndir-methods
#' 
#' @export
setMethod("length",
   signature=c(x="Venndir"),
   definition=function(x) {
      length(x@setlist);
   }
)

# if (!isGeneric("setlist")) {
setGeneric("setlist", function(x) standardGeneric("setlist"))
# }

#' Extract setlist from a Venndir object
#' 
#' @docType methods
#' @rdname Venndir-methods
#' 
#' @export
setMethod("setlist",
   signature(x="Venndir"),
   function(x) {
      x@setlist
   })

#' Extract setlist names from a Venndir object
#' 
#' @docType methods
#' @rdname Venndir-methods
#' 
#' @export
setMethod("names",
   signature=c(x="Venndir"),
   definition=function(x) {
      names(x@setlist);
   }
)

## show a summary of a Venndir object
#' Show summary of a Venndir object
#' 
#' @docType methods
#' @rdname Venndir-methods
#' 
#' @export
setMethod("show", "Venndir",
   # signature=c(x="Venndir"),
   function(object) {
      # number of sets, polygons (set, overlap)
      sets_num <- length(object@setlist);
      sets_names <- names(object@setlist);
      
      # number of polygons
      ct_polys <- table(object@jps@polygons$type)
      ct_polys_set <- as.integer(ct_polys["set"])
      ct_polys_ol <- as.integer(ct_polys["overlap"])
      
      # labels
      ldf <- subset(object@jps@polygons, type %in% "set");
      setdf <- ldf[, c("venn_name", "venn_label",
         "legend_label", "venn_color"), drop=FALSE]
      colnames(setdf)[1:4] <- c("set_name",
         "setlist_labels", "legend_labels", "set_colors")
      setdf$size <- lengths(object@setlist[setdf$set_name])
      rownames(setdf) <- match(setdf$set_name, names(object@setlist))

      # overlap_type
      # overlap_type needs to be inferred?? Probably a mistake not to include.
      ol_type <- NULL;
      # ol_type <- intersect(c("each", "overlap", "concordance", "agreement"),
      #    colnames(object@label_df))
      
      ## metadata
      main <- NULL;
      if ("metadata" %in% slotNames(object) && length(object@metadata) > 0) {
         main <- object@metadata$main;
         if ("overlap_type" %in% names(object@metadata)) {
            ol_type <- object@metadata$overlap_type
         }
      }
      
      summary_v <- c(
         paste0("class: Venndir"),
         paste0("slots: ", paste(slotNames(object), collapse=", ")),
         paste0("number of sets: ", sets_num),
         paste0("number of polygons: ", #sum(ct_polys),
            "", ct_polys_set, " sets, ", ct_polys_ol, " overlaps")
         );
      if (length(ol_type) > 0) {
         summary_v <- c(summary_v,
            paste0("overlap_type: '", ol_type, "'"))
      }
      if (length(main) > 0) {
         summary_v <- c(summary_v,
            paste0("Main title: '", main, "'"))
      }
      summary_v <- c(summary_v, "");
      summary_text <- jamba::cPaste(summary_v, sep="\n")
      cat(summary_text, sep="");
      print(setdf);
      # jamba::sdim(object)[, c("enrichIM", "geneIM", "memIM"), drop=FALSE]
   }
)

if (!isGeneric("metadata")) {
   setGeneric("metadata", function(x, ...) standardGeneric("metadata"))
}
#' Return metadata for a Venndir object
#' 
#' @docType methods
#' @rdname Venndir-methods
#' 
#' @export
setMethod("metadata",
   signature=c(x="Venndir"),
   function(x) {
      if (is.null(x@metadata) || is.character(x@metadata)) {
         list(metadata = x@metadata)
      } else {
         x@metadata
      }
   })

if (!isGeneric("metadata<-")) {
   setGeneric("metadata<-",
      function(x, ..., value) standardGeneric("metadata<-"))
}
#' Replace metadata for a Venndir object
#' 
#' @docType methods
#' @rdname Venndir-methods
#' 
#' @export
setReplaceMethod("metadata", "Venndir",
   function(x, value) {
      if (!is.list(value))
         stop("replacement 'metadata' value must be a list")
      if (!length(value))
         names(value) <- NULL # instead of character()
      x@metadata <- value
      x
   })


setGeneric("overlaplist", function(x) standardGeneric("overlaplist"))

#' Extract overlap list from a Venndir object
#' 
#' @docType methods
#' @rdname Venndir-methods
#' 
#' @export
setMethod("overlaplist",
   signature(x="Venndir"),
   function(x) {
      if (!"items" %in% colnames(x@label_df)) {
         stop("'items' are not present in this Venndir object.")
      }
      overlaplist <- lapply(x@label_df$items, c);
      names(overlaplist) <- x@label_df$overlap_sign;
      keep_ol <- (lengths(overlaplist) > 0)
      overlaplist <- overlaplist[keep_ol];
      ol_split <- factor(x@label_df$overlap_set[keep_ol],
         levels=unique(x@label_df$overlap_set[keep_ol]));
      if (any("factor" %in% jamba::sclass(overlaplist))) {
         overlaplist <- lapply(overlaplist, function(i){
            if (inherits(i, "factor")) {
               if (length(i) == 1) {
                  as.character(i)
               } else {
                  factor(i)
               }
            } else {
               i
            }
         })
      }
      overlaplist_list <- split(overlaplist, ol_split)
      if (any(lengths(overlaplist_list) > 1)) {
         overlaplist_list <- lapply(overlaplist_list, function(i){
            names(i) <- sub("^.+[|]", "", names(i));
            i
         })
         return(overlaplist_list)
      }
      names(overlaplist) <- gsub("[|].+", "", names(overlaplist));
      overlaplist
   })

setGeneric("overlapdf", function(x) standardGeneric("overlapdf"))

#' Extract overlaps as a data.frame from a Venndir object
#' 
#' @docType methods
#' @rdname Venndir-methods
#' 
#' @export
setMethod("overlapdf",
   signature(x="Venndir"),
   function(x) {
      if (!"items" %in% colnames(x@label_df)) {
         stop("'items' are not present in this Venndir object.")
      }
      ollist <- overlaplist(x)
      if (inherits(ollist[[1]], "list")) {
         odf <- data.frame(check.names=FALSE,
            overlap=rep(names(ollist), lengths(ollist)),
            sign=unlist(lapply(unname(ollist), names)),
            items=I(unlist(ollist, recursive=FALSE)))
         odf$sign <- jamba::cPaste(sep=" ",
            lapply(strsplit(odf$sign, " "), function(i){
               i[!i %in% "0"]
            }))
      } else {
         odf <- data.frame(check.names=FALSE,
            overlap=names(ollist),
            items=I(ollist))
      }
      odf$overlap <- factor(odf$overlap,
         levels=unique(odf$overlap));
      odf$count <- lengths(odf$items);
      odf
   })

# signed_counts
setGeneric("signed_counts", function(x) standardGeneric("signed_counts"))

#' Extract signed count list from a Venndir object
#' 
#' @docType methods
#' @rdname Venndir-methods
#' 
#' @export
setMethod("signed_counts",
   signature(x="Venndir"),
   function(x) {
      scdf <- x@label_df[, c("overlap_sign", "venn_counts",
         "text", "type", "overlap_set")];
      scdf$sign <- sub("^.+[|]", "", scdf$overlap_sign);
      scdf$sign <- jamba::cPaste(sep="_",
         lapply(strsplit(scdf$sign, " "), function(i){
            i[!i %in% "0"]
         }))
      scdf <- subset(scdf, venn_counts > 0);
      sclist <- split(scdf,
         scdf$overlap_set)
      lapply(sclist, function(idf){
         if ("signed" %in% idf$type) {
            idf <- subset(idf, type %in% "signed")
         }
         setNames(idf$venn_counts, idf$sign)
      })
   })


# im
setGeneric("im", function(x) standardGeneric("im"))

#' Extract incidence matrix (im) from a Venndir object
#' 
#' @docType methods
#' @rdname Venndir-methods
#' 
#' @export
setMethod("im",
   signature(x="Venndir"),
   function(x) {
      sl <- setlist(x);
      slim <- list2im_value(sl);
      slim
   })
