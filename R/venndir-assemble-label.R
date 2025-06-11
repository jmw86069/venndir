

#' Assemble grid grobs into venndir labels
#' 
#' Assemble grid grobs into venndir labels
#' 
#' Grobs are defined by `signed_labels`, `count_labels`, and `overlap_labels`,
#' or when any argument is `NULL` the label is skipped.
#' The labels are assembled with the following rules:
#' 
#' * For each of the labels, when multiple values are provided, they are
#' stacked on top of each other.
#' 
#'    * For `signed_labels`, they are left-justified when `template="wide"`,
#'    otherwise they and all other labels are center-justified.
#' 
#' * When both `count_labels` and `signed_labels` are defined, they
#' are combined according to `template`:
#' 
#'    * `template="wide"`: combines `count_labels` on the left, and
#'    `signed_labels` on the right. They are center/middle-justified
#'    relative to each other, in terms of height.
#'    * `template="tall"`: combines `count_labels` on the top, and
#'    `signed_labels` on the bottom.
#' 
#' * The combined labels of `count_labels` and/or `signed_labels`
#' are arranged  center-justified underneath `overlap_labels`,
#' when `overlap_labels` is defined.
#' * For each label, the font settings are applied in order, or recycled
#' to the vector length. For example if `signed_labels` contains three
#' values, then `fontsizes$signed` is recycled to length 3, and applied
#' to each label in order.
#' 
#'    * `fontsizes`: applies to each label in order
#'    * `fontfamilies`: by default uses `fontfamily` for all labels, but
#'    permits any label to use a custom font
#'    * `fontfaces`: applies fontface to each label in order, with recognized
#'    values: "plain", "bold", "italic", "bold.italic"
#'    * `fontcolors`: applies font color to each label in order
#' 
#' @family venndir internal
#' 
#' @examples
#' vl <- assemble_venndir_label(
#'    signed_labels=c("\u2191\u2191 15",
#'       "\u2193\u2193 23"),
#'    debug="overlap")
#' 
#' vl <- assemble_venndir_label(
#'    overlap_labels=c("\u2191\u2191 15",
#'       "\u2193\u2193 23")[1],
#'    debug="overlap")
#'    
#' vl2 <- assemble_venndir_label(
#'    signed_labels=c("\u2191\u2191 15",
#'       "\u2193\u2193 23"),
#'    count_labels=c("38"),
#'    debug="overlap")
#' 
#' vl3 <- assemble_venndir_label(
#'    signed_labels=c("\u2191\u2191 15",
#'       "\u2193\u2193 23"),
#'    count_labels=c("38"),
#'    overlap_labels="Set Name Goes Here",
#'    debug="overlap")
#' 
#' vl4 <- assemble_venndir_label(template="wide",
#'    signed_labels=c("\u2191\u2191 15",
#'       "\u2193\u2193 23"),
#'    count_labels=c("38", "12%"),
#'    overlap_labels="Set Name Goes Here",
#'    debug="overlap")
#' 
#' vl5 <- assemble_venndir_label(
#'    signed_labels=c("\u2191\u2191 15",
#'       "\u2191\u2193 8",
#'       "\u2193\u2193 23"),
#'    fontcolors=list(count="black", signed=c("red3", "darkorchid", "dodgerblue3")),
#'    count_labels=c("46", "12%"),
#'    overlap_labels="Set Name Goes Here",
#'    debug="overlap")
#' 
#' vl6 <- assemble_venndir_label(
#'    signed_labels=c("\u2191\u2191 15",
#'       "\u2193\u2193 23"),
#'    #count_labels=c("38", "12%"),
#'    overlap_labels="Set Name Goes Here",
#'    debug="overlap")
#' 
#' # example preparing multiple labels, one at a time
#' vlxy1 <- assemble_venndir_label(fontfamily="Times",
#'    just="right",
#'    x=grid::unit(0.25, "snpc"), grid::unit(0.5, "snpc"),
#'    count_labels=c("138"),
#'    signed_labels=c("\u2191\u2191 15",
#'       "\u2193\u2193 23"))
#' vlxy2 <- assemble_venndir_label(template="wide",
#'    just=c("right", "top"),
#'    x=grid::unit(0.25, "snpc"), grid::unit(0.25, "snpc"),
#'    count_labels=c("101", "24%"),
#'    signed_labels=c("\u2191\u2191 27",
#'       "\u2193\u2193 74"))
#' 
#' # each label can be drawn individually, or combined into gList (below)
#' new_gtree <- grid::gTree(children=grid::gList(
#'    vlxy1, vlxy2))
#' grid::grid.newpage();
#' grid::grid.draw(new_gtree);
#' grid::grid.points(x=grid::unit(c(0.25, 0.25), "snpc"),
#'    y=grid::unit(c(0.50, 0.25), "snpc"),
#'    pch=3,
#'    gp=grid::gpar(cex=0.8, lwd=2, col="darkorange"));
#' 
#' @param x,y `grid::unit`, default NULL, indicating optional placement
#'    of labels. When defined, a viewport is defined for the label so
#'    it is rendered at this position.
#' @param just `character` used only when `x`,`y` are defined, default
#'    "center" places the label at the center position; "right" will align
#'    the label so the right edge touches the x,y coordinate;
#'    `c("bottom", "left")` will align the label so the bottom-left corner
#'    touches the x,y coordinate.
#' @param signed_labels,count_labels,overlap_labels `character` vector,
#'    default NULL, with one or more entries indicating the label should
#'    be included.
#' @param template `character` default "wide" indicating the placement
#'    of counts and signed counts when both are defined:
#'    * `"wide"` places signed counts to the right of counts.
#'    * `"tall"` places signed counts below counts.
#' @param fontfamily `character` default "Helvetica" used as convenient
#'    default for `fontfamilies`.
#' @param fontfamilies,fontsizes,fontfaces,fontcolors,label_spacing,label_padding
#'    `list` with three named elements: "overlap", "count", "signed", providing
#'    one or more values for each type of label. When multiple values
#'    are provided for a label type, these values are recycled to the
#'    number of values.
#'    * For example `signed_labels=c("^ 21", "v 24")`
#'    and `fontcolors=list(signed=c("red", "blue", "grey"))` would use
#'    `c("red", "blue")` for these two labels.
#'    * `label_spacing` defines the line-to-line spacing for each type of
#'    label, used when there are multiple lines of text.
#'    * `label_padding` defines the buffer "padding" around the final label,
#'    whether the label is single-line or multiple-line.
#'    Concise labels can be created with small label_spacing, but slightly
#'    larger label_padding.
#' @param do_frame `logical` default TRUE, indicating whether to define
#'    a frame around the grouped labels, returning `grid::gTree` with the
#'    frame and labels together as one "grob".
#' @param frame_r `grid::unit` default 0.1snpc, indicating the corner
#'    radius when `do_frame=TRUE`, causing it to use `grid::roundrectGrob()`
#'    instead of `grid::rectGrob()`. Note the default is proportional to
#'    the plot coordinates, not the font size.
#' @param frame_border,frame_fill `character` R colors used when
#'    `do_frame=TRUE` for the frame border, and color fill, respectively.
#'    Default is black border, cream/beige fill.
#' @param text_grob_type `character` default `"marquee"` indicating the
#'    type of text grob to use for labels. In future, this choice should
#'    be substantially improved, but for now it is user choice.
#'    * `"marquee"` uses `marquee::marquee_grob()` - best overall:
#'    Supports markdown, and fallback glyph use so up/down arrows are
#'    displayed even for fonts that do not include them.
#'    Not compatible with MacOS and R-4.4.1 or older, so it will revert
#'    to `"textGrob"` in that specific scenario.
#'    * `"textGrob"` uses `grid::textGrob()` - solid all-around, however
#'    it does not support markdown.
#'    * `"gridtext"` (deprecated) uses `gridtext::richtext_grob()` -
#'    alternative for markdown support, however some graphics devices show
#'    inconsistent spacing between words.
#'    * `"richtext_grob"` (deprecated) uses `gridtext::richtext_grob()` -
#'    alternative for markdown support, however some graphics devices show
#'    inconsistent spacing between words.
#' @param use_devoid `logical` whether to open temporary `devoid::void_dev()`
#'    device to prevent opening a new device or advancing the page of
#'    an existing open device. Default `getOption("use_devoid", TRUE)`.
#'    This option is experimental in 0.0.55.900 under testing.
#' @param debug `character` default FALSE, indicating whether to run
#'    one of the debug modes used for testing:
#'    * `"overlap"` will display the label at the center of the plot.
#'    * `"groblist"` will return a `list` of grobs: signed_grobs, count_grobs,
#'    overlap_grobs.
#'    * `"list"` will return a `list` of grobs as "groblist", then the
#'    `overlap_frame` which contains the `gTree` or `gtable` final grob.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... additional arguments are ignored.
#'    
#' @export
assemble_venndir_label <- function
(x=NULL,
 y=NULL,
 just=c("center",
    "center"),
 signed_labels=NULL,
 count_labels=NULL,
 overlap_labels=NULL,
 template=c(
    "wide",
    "tall"),
 fontfamily="Helvetica",
 fontfamilies=list(signed=fontfamily,
    count=fontfamily,
    overlap=fontfamily),
 fontsizes=list(signed=12,
    count=c(16, 12),
    overlap=16),
 fontfaces=list(signed="plain",
    count=c("plain", "bold.italic"),
    overlap="bold"),
 fontcolors=list(signed=c("red3", "dodgerblue3", "grey55"),
    count="black",
    overlap="black"),
 label_spacing=list(signed=grid::unit(1, "mm"),
    count=grid::unit(1, "mm"),
    overlap=grid::unit(1, "mm")),
 label_padding=list(signed=grid::unit(2, "mm"),
    count=grid::unit(1, "mm"),
    overlap=grid::unit(1, "mm")),
 do_frame=TRUE,
 frame_r=grid::unit(0.1, "snpc"),
 frame_border="#44444477",
 frame_fill="#FDDD6644",
 text_grob_type=c(
    "marquee",
    "textGrob",
    "gridtext",
    "richtext_grob"),
 use_devoid=getOption("use_devoid", TRUE),
 debug=FALSE,
 verbose=FALSE,
 ...)
{
   template <- match.arg(template);

   text_grob_type <- match.arg(text_grob_type);
   # In theory marquee will always be installed, but just in case
   if ("marquee" %in% text_grob_type &&
         !requireNamespace("marquee", quietly=TRUE)) {
      warning(paste0("Changed text_grob_type 'marquee' to ",
         "'textGrob' because 'marquee' package is not installed."));
      text_grob_type <- "textGrob";
   }
   
   # fill in partial custom options
   avdf <- formals(assemble_venndir_label)
   fill_opts <- function(x, name, avdf) {
      #
      xdefault <- eval(avdf[[name]]);
      if (length(x) == 0) {
         x <- xdefault
      } else if (!all(names(eval(avdf[[name]])) %in% names(x))) {
         if (!is.list(x)) {
            x <- as.list(x);
            if (length(names(x)) == 0 && length(x) <= length(xdefault)) {
               names(x) <- head(names(xdefault), length(x))
            }
         }
         addn <- setdiff(names(xdefault), names(x));
         x[addn] <- xdefault[addn];
      }
      x
   }
   fontfamilies <- fill_opts(fontfamilies, "fontfamilies", avdf);
   fontsizes <- fill_opts(fontsizes, "fontsizes", avdf);
   fontfaces <- fill_opts(fontfaces, "fontfaces", avdf);
   fontcolors <- fill_opts(fontcolors, "fontcolors", avdf);
   label_spacing <- fill_opts(label_spacing, "label_spacing", avdf);
   label_padding <- fill_opts(label_padding, "label_padding", avdf);

   if ("gridtext" %in% text_grob_type) {
      text_grob_type <- "richtext_grob";
   }
   if ("richtext_grob" %in% text_grob_type &&
         !requireNamespace("gridtext", quietly=TRUE)) {
      warning(paste0("Changed text_grob_type 'richtext_grob' to ",
         "'textGrob' because 'gridtext' package is not installed."));
      text_grob_type <- "textGrob";
   }

   # sign1 <- "\u2191\u2191 1,234"
   # sign2 <- "\u2193\u2193 512"
   # count1 <- "11,246\n74%"
   # label1 <- "Set Label Goes Here\n(set A)"
   
   # Todo: validate arguments: fontsizes,fontfaces,fontfamilies,label_spacing
   # - confirm list, or expand into list
   # - confirm names: signed, count, overlap
   # - fill missing values as needed
   # - fontfamilies can be NULL, which should use the parent fontfamily
   req_names <- c("signed", "count", "overlap")

   # buffers around each grouped label type
   use_buffers_mm <- c(
      signed_grobs=2,
      count_grobs=2,
      overlap_grobs=2)
   
   #################################################################
   # Temporary devoid device so width=NA works with marquee_grob()
   if (TRUE %in% use_devoid) {
      if (requireNamespace("devoid", quietly=TRUE)) {
         dev1 <- dev.list();
         devoid::void_dev();
         dev2 <- dev.list();
         devVOID <- setdiff(dev2, dev1)
         on.exit(expr={
            tryCatch({
               dev.off(which=devVOID)
            }, error=function(e){
               # bleh
            })
         },
            add=TRUE,
            after=FALSE)
      }
   }
   
   ####################################
   # Assemble signed count labels
   signed_frame <- NULL;
   signed_grobs <- list();
   if (length(signed_labels) > 0) {
      sn <- length(signed_labels);
      # signed_frame <- grid::frameGrob()
      for (i in seq_along(signed_labels)) {
         fs <- rep(fontsizes$signed, length.out=sn)[[i]];
         ff <- rep(fontfaces$signed, length.out=sn)[[i]];
         fc <- rep(fontcolors$signed, length.out=sn)[[i]];
         fm <- rep(fontfamilies$signed, length.out=sn)[[i]];
         if (length(fs) == 0) {
            fs <- 10;
         }
         if (length(ff) == 0) {
            ff <- "plain";
         }
         if (length(fc) == 0) {
            fc <- "grey55";
         }
         # "tall" is center-justified, "wide" is left-justified
         # signed_hjust <- "left";
         signed_hjust <- 0; # 0 left
         signed_vjust <- 0.5; # 0.5 center
         if ("tall" %in% template & length(count_labels) > 0 && i == 1) {
            signed_vjust <- 0.5; # 1 top
         }
         signed_x <- grid::unit(0, "npc");
         
         ## optionally center labels for "tall" template
         # but it looks better left-justified (imo)
         # if ("tall" %in% template) {
         #    signed_hjust <- "center";
         #    signed_x <- grid::unit(0.5, "npc");
         # }
         
         if ("textGrob" %in% text_grob_type) {
            grob_sign1 <- grid::textGrob(
               label=signed_labels[[i]],
               hjust=signed_hjust,
               vjust=signed_vjust,
               x=signed_x,
               gp=grid::gpar(col=fc,
                  fontface=ff,
                  fontfamily=fm,
                  fontsize=fs))
         } else if ("marquee" %in% text_grob_type) {
            if (verbose) {
               jamba::printDebug("assemble_venndir_label(): ",
                  "marquee");
            }
            # bottom buffer corrects small whitespace diff compared to textGrob
            fi <- grepl("italic", ff);
            fweight <- gsub("italic|[.]", "", gsub("plain", "normal", ff));
            if (any(signed_hjust %in% c(0, 0.5, 1))) {
               signed_hjust <- c(`0`="left", `0.5`="center", `1`="right")[
                  as.character(signed_hjust)];
               signed_hjust <- paste0(signed_hjust, "-ink");
            }
            if (any(signed_vjust %in% c(0, 0.5, 1))) {
               signed_vjust <- c(`0`="bottom", `0.5`="center", `1`="top")[
                  as.character(signed_vjust)];
               signed_vjust <- paste0(signed_vjust, "-ink");
            }
            kk <- 2 * fs / 12;
            grob_sign1 <- marquee::marquee_grob(
               text=signed_labels[[i]],
               ignore_html=TRUE,
               width=NA,
               y=grid::unit(0.5, "npc"),
               x=grid::unit(signed_x, "npc"),
               default.units="snpc",
               force_body_margin=TRUE,
               style=marquee::classic_style(
                  base_size=fs,
                  body_font=fm,
                  italic=fi,
                  weight=fweight,
                  color=fc,
                  margin=marquee::trbl(grid::unit(-1*kk, "pt"),
                     grid::unit(0, "pt"),
                     grid::unit(-1*kk, "pt"),
                     grid::unit(0, "pt")),
                  padding=marquee::trbl(grid::unit(-1*kk, "pt"),
                     grid::unit(0, "pt"),
                     grid::unit(-1*kk, "pt"),
                     grid::unit(0, "pt")),
                  align="left"),
               hjust=signed_hjust,
               vjust=signed_vjust);
            # grid::grid.draw(grob_sign1);# debug
         } else if ("richtext_grob" %in% text_grob_type) {
            if (verbose) {
               jamba::printDebug("assemble_venndir_label(): ",
                  "richtext_grob");
            }
            kk <- 0.25 * fs / 12;
            grob_sign1 <- gridtext::richtext_grob(
               x=signed_x,
               y=grid::unit(0.5, "npc"),
               hjust=signed_hjust,
               vjust=signed_vjust,
               text=signed_labels[[i]],
               margin=grid::unit(c(-1*kk, 0, -1*kk, 0), "pt"),
               padding=grid::unit(c(-1*kk, 0, -1*kk, 0), "pt"),
               # box_gp=grid::gpar(col="red", fill=NA), # optional debug border
               gp=grid::gpar(col=fc,
                  fontface=ff,
                  fontfamily=fm,
                  fontsize=fs))
         }
         signed_grobs[[i]] <- grob_sign1;
      }
   }
   
   ####################################
   # Add main count label
   count_frame <- NULL;
   count_grobs <- list();
   if (length(count_labels) > 0) {
      # assemble count labels
      # count_frame <- grid::frameGrob()
      sn <- length(count_labels);
      for (i in seq_along(count_labels)) {
         fs <- rep(fontsizes$count, length.out=sn)[[i]];
         # jamba::printDebug("fontfaces:");print(fontfaces);# debug
         ff <- rep(fontfaces$count, length.out=sn)[[i]];
         # jamba::printDebug("ff:");print(ff);# debug
         fc <- rep(fontcolors$count, length.out=sn)[[i]];
         fm <- rep(fontfamilies$count, length.out=sn)[[i]];
         if (length(fs) == 0) {
            fs <- 12;
         }
         if (length(ff) == 0) {
            ff <- "plain";
         }
         if (length(fc) == 0) {
            fc <- "black";
         }
         if ("textGrob" %in% text_grob_type) {
            grob_count1 <- grid::textGrob(
               label=count_labels[[i]],
               just="centre", x=0.5,
               gp=grid::gpar(col=fc,
                  fontface=ff,
                  fontfamily=fm,
                  fontsize=fs))
         } else if ("marquee" %in% text_grob_type) {
            fi <- grepl("italic", ff);
            fweight <- gsub("italic|[.]", "", gsub("plain", "normal", ff));
            kk <- 1 * fs / 12;
            grob_count1 <- marquee::marquee_grob(
               text=count_labels[[i]],
               ignore_html=TRUE,
               width=NA,
               y=grid::unit(0.5, "npc"),
               x=grid::unit(0.5, "npc"),
               default.units="snpc",
               force_body_margin=TRUE,
               style=marquee::classic_style(
                  base_size=fs,
                  body_font=fm,
                  italic=fi,
                  weight=fweight,
                  color=fc,
                  # background="gold", # for debug
                  margin=marquee::trbl(grid::unit(-1*kk, "pt"),
                     grid::unit(0, "pt"),
                     grid::unit(-1*kk, "pt"),
                     grid::unit(0, "pt")),
                  padding=marquee::trbl(grid::unit(-1*kk, "pt"),
                     grid::unit(0, "pt"),
                     grid::unit(-1*kk, "pt"),
                     grid::unit(0, "pt")),
                  align="center"
               ),
               hjust="center-ink",
               vjust="center-ink");
         } else if ("richtext_grob" %in% text_grob_type) {
            if (verbose) {
               jamba::printDebug("assemble_venndir_label(): ",
                  "richtext_grob");
            }
            # bottom buffer corrects small whitespace diff compared to textGrob
            kk <- 0.5 * fs / 12;
            grob_count1 <- gridtext::richtext_grob(
               x=grid::unit(0.5, "npc"),
               y=grid::unit(0.5, "npc"),
               hjust=0.5,
               # text=gsub("([^\n])\n([^\n])", "\\1<br>\n\\2", count_labels[[i]]),
               # text=gsub("\n", "!<br>!", count_labels[[i]]),
               text=count_labels[[i]],
               margin=grid::unit(c(-1*kk, 0, -1*kk, 0), "pt"),
               padding=grid::unit(c(-1*kk, 0, -1*kk, 0), "pt"),
               # box_gp=grid::gpar(col="red", fill=NA), # optional debug border
               gp=grid::gpar(col=fc,
                  fontface=ff,
                  fontfamily=fm,
                  fontsize=fs))
         }
         count_grobs[[i]] <- grob_count1;
      }
   }
   
   ####################################
   # add overlap label
   overlap_grobs <- list();
   overlap_frame <- NULL;
   if (length(overlap_labels) > 0) {
      # overlap_frame <- grid::frameGrob()
      sn <- length(overlap_labels);
      for (i in seq_along(overlap_labels)) {
         fs <- rep(fontsizes$overlap, length.out=sn)[[i]];
         ff <- rep(fontfaces$overlap, length.out=sn)[[i]];
         fc <- rep(fontcolors$overlap, length.out=sn)[[i]];
         fm <- rep(fontfamilies$overlap, length.out=sn)[[i]];
         if (length(fs) == 0) {
            fs <- 14;
         }
         if (length(ff) == 0) {
            ff <- "bold";
         }
         if (length(fc) == 0) {
            fc <- "black";
         }
         # textGrob()
         if ("textGrob" %in% text_grob_type) {
            if (verbose) {
               jamba::printDebug("assemble_venndir_label(): ",
                  "grid.text");
            }
            grob_overlap1 <- grid::textGrob(
               label=overlap_labels[[i]],
               just="center",
               x=grid::unit(0.5, "npc"),
               gp=grid::gpar(col=fc,
                  fontface=ff,
                  fontfamily=fm,
                  fontsize=fs))
         } else if ("marquee" %in% text_grob_type) {
            fi <- grepl("italic", ff);
            fweight <- gsub("italic|[.]", "", gsub("plain", "normal", ff));
            kk <- fs / 12;
            grob_overlap1 <- marquee::marquee_grob(
               text=gsub("\n", "\n\n", overlap_labels[[i]]),
               ignore_html=TRUE,
               width=NA,
               # y=grid::unit(1, "snpc") - grid::unit(0.25, "char"),
               y=grid::unit(0.5, "npc"),
               x=grid::unit(0.5, "npc"),
               default.units="snpc",
               force_body_margin=TRUE,
               style=marquee::classic_style(
                  base_size=fs,
                  body_font=fm,
                  italic=fi,
                  weight=fweight,
                  # weight="normal",
                  color=fc,
                  # background="gold", # for debug
                  margin=marquee::trbl(grid::unit(-1*kk, "pt"),
                     grid::unit(0, "pt"),
                     grid::unit(-1*kk, "pt"),
                     grid::unit(0, "pt")),
                  padding=marquee::trbl(grid::unit(-1*kk, "pt"),
                     grid::unit(0, "pt"),
                     grid::unit(-1*kk, "pt"),
                     grid::unit(0, "pt")),
                  align="center"
               ),
               hjust="center-ink",
               vjust="center-ink");
               # hjust="center",
               # vjust="center")
         } else if ("richtext_grob" %in% text_grob_type) {
            if (verbose) {
               jamba::printDebug("assemble_venndir_label(): ",
                  "richtext_grob");
            }
            # bottom buffer corrects small whitespace diff compared to textGrob
            kk <- 0.25 * fs / 12;
            grob_overlap1 <- gridtext::richtext_grob(
               x=grid::unit(0.5, "npc"),
               y=grid::unit(0.5, "npc"),
               hjust=0.5,
               vjust=0.5,
               text=gsub("([^\n])\n([^\n])", "\\1<br>\n\\2", overlap_labels[[i]]),
               # text=overlap_labels[[i]],
               # margin=grid::unit(c(0, 0, -2, 0), "pt"),
               # padding=grid::unit(c(0, 0, -2, 0), "pt"),
               margin=grid::unit(c(1*kk, 0, 1*kk, 0), "pt"),
               padding=grid::unit(c(1*kk, 0, 1*kk, 0), "pt"),
               # box_gp=grid::gpar(col="red", fill=NA), # optional debug border
               gp=grid::gpar(col=fc,
                  fontface=ff,
                  fontfamily=fm,
                  fontsize=fs))
         }
         overlap_grobs[[i]] <- grob_overlap1;
      }
   }
   # if empty, there were no labels, return NULL
   if (length(signed_grobs) == 0 &&
         length(count_grobs) == 0 &&
         length(overlap_grobs) == 0) {
      return(invisible(NULL))
   }
   
   ################################################
   # gtable grob_strat assembly of label components
   {
      grob_list <- jamba::rmNULL(list(
         signed_grobs=signed_grobs,
         count_grobs=count_grobs,
         overlap_grobs=overlap_grobs))
      # iterate each type, convert to gtable
      gt_list <- lapply(jamba::nameVectorN(grob_list), function(iname){
         igrobs <- grob_list[[iname]]
         # print("iname:");print(iname);# debug
         ihtlist <- lapply(seq_along(igrobs), function(igrob_num){
            igrob <- igrobs[[igrob_num]];
            ## new without padding
            grid::grobHeight(igrob)
         })
         if (length(ihtlist) > 1) {
            ihts <- do.call(grid::unit.c, ihtlist)
         } else {
            ihts <- ihtlist[[1]];
         }
         iwdlist <- lapply(igrobs, function(igrob){
            grid::grobWidth(igrob)
         })
         if (length(iwdlist) > 1) {
            iwd <- do.call(max, iwdlist)
         } else {
            iwd <- iwdlist[[1]];
         }
         gt1 <- gtable::gtable_col(iname,
            grobs=igrobs,
            width=iwd,
            heights=ihts)
         # add buffer between rows when buffer is non-zero
         if (length(ihtlist) > 1 &&
               as.numeric(label_spacing[[gsub("_grobs", "", iname)]]) > 0) {
            gt1 <- gtable::gtable_add_row_space(x=gt1,
               height=label_spacing[[gsub("_grobs", "", iname)]])
         }
         gt1
      })

      # combine gtable objects
      gt1 <- NULL;
      gt23 <- NULL;
      gtfinal <- NULL;
      roundradius <- label_padding[["overlap"]];
      # top row uses overlap_grobs
      if (any(grepl("overlap", names(gt_list)))) {
         gt1 <- gt_list$overlap_grobs
      }
      # bottom row uses count_grobs, signed_grobs in two columns (as needed)
      if (any(grepl("sign", names(gt_list)))) {
         if (any(grepl("count", names(gt_list)))) {
            use_ws <- grid::unit.c(
               gtable::gtable_width(gt_list$count_grobs),
               gtable::gtable_width(gt_list$signed_grobs)
            )
            use_hs <- grid::unit.c(
               gtable::gtable_height(gt_list$count_grobs),
               gtable::gtable_height(gt_list$signed_grobs)
            )
            if ("wide" %in% template) {
               gt23 <- gtable::gtable_row("count_signed",
                  list(gt_list$count_grobs,
                     gt_list$signed_grobs),
                  widths=use_ws,
                  height=max(use_hs))
               # add spacing between columns
               gt23 <- gtable::gtable_add_col_space(x=gt23,
                  width=label_spacing[["count"]])
            } else if ("tall" %in% template) {
               gt23 <- gtable::gtable_col("count_signed",
                  list(gt_list$count_grobs,
                     gt_list$signed_grobs),
                  width=max(use_ws),
                  heights=use_hs)
               # add spacing between rows
               gt23 <- gtable::gtable_add_row_space(x=gt23,
                  height=label_spacing[["count"]])
            }
         } else {
            gt23 <- gt_list$signed_grobs
         }
      } else {
         if (any(grepl("count", names(gt_list)))) {
            gt23 <- gt_list$count_grobs
         }
      }
      ## Assemble overlap with count/signed gtables
      if (length(gt1) > 0) {
         # overlap label exists
         if (length(gt23) > 0) {
            use_ws <- grid::unit.c(
               gtable::gtable_width(gt1),
               gtable::gtable_width(gt23))
            use_hs <- grid::unit.c(
               gtable::gtable_height(gt1),
               gtable::gtable_height(gt23))
            gtfinal <- gtable::gtable_col("overlap_count_signed",
               list(gt1, gt23),
               width=max(use_ws),
               heights=use_hs)
            ## add space between elements
            gtfinal <- gtable::gtable_add_row_space(x=gtfinal,
               height=label_spacing[["overlap"]])
            ## add padding around label
            gtfinal <- gtable::gtable_add_padding(x=gtfinal,
               padding=rep(label_padding[["overlap"]], length.out=4))
         } else {
            # overlap label is used by itself
            ## add padding around label
            gtfinal <- gtable::gtable_add_padding(x=gt1,
               padding=rep(label_padding[["overlap"]], length.out=4))
         }
      } else if (length(gt23) > 0) {
         roundradius <- label_padding[["count"]];
         ## previous
         gtfinal <- gt23
         ## add padding around label
         gtfinal <- gtable::gtable_add_padding(x=gt23,
            padding=rep(label_padding[["count"]], length.out=4))
      }
      overlap_frame <- gtfinal;
   }
   
   # adjust roundradius by fontsizes
   roundradius <- min(unlist(fontsizes)) / 12 * roundradius;
   
   # if ("overlap" %in% debug) {
   if (any(c("signed", "count", "overlap", "list", "groblist") %in% debug)) {
      if (TRUE %in% verbose) {
         jamba::printDebug("Doing debug='", debug, "'");# debug
      }
      grid::grid.newpage()
      # add roundrect around grob
      if ("list" %in% debug) {
         retlist <- list(
            signed_grobs=signed_grobs,
            count_grobs=count_grobs,
            overlap_grobs=overlap_grobs,
            overlap_frame=overlap_frame);
         return(invisible(retlist));
      } else if ("groblist" %in% debug) {
         retlist <- list(
            signed_grobs=signed_grobs,
            count_grobs=count_grobs,
            overlap_grobs=overlap_grobs);
         return(invisible(retlist));
      } else if ("signed" %in% debug) {
         use_frame <- overlap_frame
         # use_frame <- signed_frame
      } else if ("count" %in% debug) {
         use_frame <- overlap_frame
         # use_frame <- count_frame
      } else if ("overlap" %in% debug) {
         use_frame <- overlap_frame
      }
      use_name <- head(use_frame$name, 1)
      if (verbose) {
         jamba::printDebug(
            "grid.ls(use_frame):");print(grid::grid.ls(use_frame));# debug
         jamba::printDebug(
            "use_name: ", use_name);# debug
      }
      if (length(frame_r) == 0) {
         frame_r <- 0;
      }
      frame_r <- head(frame_r, 1);
      frame_r <- roundradius;
      
      if (!grid::is.unit(frame_r)) {
         frame_r <- grid::unit(frame_r, "snpc");
      }

      if (inherits(use_frame, "gtable")) {
         use_h <- gtable::gtable_height(use_frame);
         use_w <- gtable::gtable_width(use_frame);
      } else {
         use_h <- grid::convertUnit(grid::unit(1, "grobwidth", use_frame), "pt");
         use_w <- grid::convertUnit(grid::unit(1, "grobheight", use_frame), "pt");
      }
      new_roundrect_grob <- grid::roundrectGrob(
         r=frame_r,
         width=use_w,
         height=use_h,
         gp=grid::gpar(fill=frame_fill,
            col=frame_border))
      use_frame2 <- grid::gTree(children=grid::gList(
         new_roundrect_grob,
         use_frame))
      grid::grid.draw(use_frame2)
   }
   
   # optional frame and color fill
   if (TRUE %in% do_frame) {
      if (verbose) {
         jamba::printDebug("assemble_venndir_label(): ",
            "adding frame");
      }
      if (length(frame_r) == 0) {
         frame_r <- 0;
      }
      frame_r <- head(frame_r, 1);
      # frame_r <- roundradius * 1.5;
      if (inherits(overlap_frame, "gtable")) {
         gw <- gtable::gtable_width(overlap_frame);
         gh <- gtable::gtable_height(overlap_frame);
      } else {
         gw <- grid::convertUnit(
            grid::unit(1, "grobwidth", overlap_frame), "pt");
         gh <- grid::convertUnit(
            grid::unit(1, "grobheight", overlap_frame), "pt");
      }
      # without radius, use rectGrob
      if (as.numeric(frame_r) == 0) {
         new_frame_grob <- grid::rectGrob(
            width=gw,
            height=gh,
            gp=grid::gpar(fill=frame_fill,
               col=frame_border))
      } else {
         if (!grid::is.unit(frame_r)) {
            frame_r <- grid::unit(frame_r, "snpc");
         }
         new_frame_grob <- grid::roundrectGrob(
            r=frame_r,
            width=gw,
            height=gh,
            gp=grid::gpar(fill=frame_fill,
               col=frame_border))
      }
      # assemble frame behind the label
      overlap_frame <- grid::gTree(
         children=grid::gList(
            new_frame_grob,
            overlap_frame))
   }
   
   # optional placement
   if (length(x) == 1 && length(y) == 1) {
      if (verbose) {
         jamba::printDebug("assemble_venndir_label(): ",
            "Placing object.");
      }
      if (TRUE %in% do_frame) {
         ## if there is a frame, get grobwidth from the child frame
         use_frame_grob <- grid::getGrob(overlap_frame,
            grid::childNames(overlap_frame)[2]);
         if (inherits(use_frame_grob, "gtable")) {
            gw <- gtable::gtable_width(use_frame_grob);
            gh <- gtable::gtable_height(use_frame_grob);
         } else {
            gw <- grid::convertUnit(
               grid::unit(1, "grobwidth", use_frame_grob), "pt")
            gh <- grid::convertUnit(
               grid::unit(1, "grobheight", use_frame_grob), "pt")
         }
      } else {
         if (inherits(overlap_frame, "gtable")) {
            gw <- gtable::gtable_width(overlap_frame);
            gh <- gtable::gtable_height(overlap_frame);
         } else {
            gw <- grid::convertUnit(
               grid::unit(1, "grobwidth", overlap_frame), "pt")
            gh <- grid::convertUnit(
               grid::unit(1, "grobheight", overlap_frame), "pt")
         }
      }
      # jamba::printDebug("gw (pt):");print(grid::convertUnit(gw, "pt"));# debug
      x1 <- x;
      y1 <- y;
      # 0.0.54.900 - handle numeric just
      if (inherits(just, "list")) {
         just <- unlist(unname(just));
      }
      if (is.numeric(unlist(just))) {
         x <- x - gw * (just[1] - 0.5);
         y <- y - gh * (just[2] - 0.5);
      } else {
         if ("left" %in% just) {
            x <- x + gw * 0.5;
         } else if ("right" %in% just) {
            x <- x - gw * 0.5;
         }
         if ("top" %in% just) {
            y <- y - gh * 0.5;
         } else if ("bottom" %in% just) {
            y <- y + gh * 0.5;
         }
      }
      xy_vp <- grid::viewport(x=x, y=y)
      
      # hard-set the viewport into the object
      overlap_frame$vp <- xy_vp;
   }
   
   return(invisible(overlap_frame))
}

