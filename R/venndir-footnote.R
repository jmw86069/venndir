
#' Add Venndir footnotes to a Venndir figure
#' 
#' Add Venndir footnotes to a Venndir figure
#' 
#' This function adds a subtle footnote symbol, or symbols, if
#' they are defined in the 'metadata' slot of the input `Venndir`
#' object, entry 'footnotes'. Footnote symbols are comma-delimited
#' and displayed together.
#' 
#' The intention is to have a visibly distinct indication when some
#' footnote exists and should be reviewed.
#' 
#' @family venndir internal
#' 
#' @param venndir_output `Venndir` object
#' @param x `character` position for the footnote
#' @param footnote_fontfamily,footnote_color,footnote_fontsize arguments
#'    to customize the font and color used to render the footnote.
#' @param footnote_outline `logical` whether to draw an outline around the
#'    footnote for visible clarity, default TRUE.
#'    It calls `jamba::setTextContrastColor()` using `footnote_color`.
#' @param draw_footnote `logical`, default TRUE, whether to draw the resulting
#'    footnote when one or more footnotes are defined.
#'    When `FALSE` a grid `grob` is returned if footnotes exist, otherwise
#'    it returns NULL.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' setlist3 <- make_venn_test(500, n_sets=4)
#' v3 <- venndir(setlist3,
#'    shape="ellipse",
#'    vector_method="label",
#'    proportional=TRUE)
#' footnotes(v3)
#' render_venndir_footnotes(v3)
#'
#' v3 <- venndir(setlist3,
#'    shape="ellipse",
#'    footnote_symbols="\u203B",
#'    proportional=TRUE)
#' footnotes(v3)
#' 
#' @export
render_venndir_footnotes <- function
(venndir_output,
 x="bottomright",
 footnote_x_inset=grid::unit(0.0, "lines"),
 footnote_y_inset=grid::unit(0.5, "lines"),
 footnote_fontfamily=NULL,
 footnote_color="black",
 footnote_fontsize=14,
 footnote_outline=TRUE,
 footnote_style=c("symbols",
    "header",
    "footnote"),
 draw_footnote=TRUE,
 use_devoid=getOption("use_devoid", TRUE),
 verbose=FALSE,
 ...)
{
   #
   if (!"footnotes" %in% names(venndir_output@metadata)) {
      if (verbose) {
         jamba::printDebug("render_venndir_footnotes(): ",
            "No 'footnotes'.");
      }
      return(NULL)
   }
   footnotes <- venndir_output@metadata[["footnotes"]];
   if (nrow(footnotes) == 0) {
      if (verbose) {
         jamba::printDebug("render_venndir_footnotes(): ",
            "Empty 'footnotes'.");
      }
      return(NULL)
   }
   
   footnote_style <- match.arg(footnote_style);
   
   if (length(footnote_fontfamily) == 0) {
      footnote_fontfamily <- metadata(venndir_output)$fontfamily
   }
   if (length(footnote_fontfamily) == 0) {
      footnote_fontfamily <- "sans";
   }
   
   if ("symbols" %in% footnote_style) {
      footnote_text <- jamba::cPaste(footnotes$symbol, sep=",");
   } else if ("header" %in% footnote_style) {
      footnote_text <- paste0("Footnote",
         ifelse(nrow(footnotes) > 1, "s", ""),
         ": ",
         jamba::cPaste(footnotes$symbol, sep=","));
   } else if ("footnote" %in% footnote_style) {
      footnote_text <- paste0(collapse="  \n",
         paste0(footnotes$symbol, ": ", footnotes$note));
   }
   
   outline <- NULL;
   outline_width <- 0;
   if (TRUE %in% footnote_outline) {
      outline <- jamba::setTextContrastColor(footnote_color)
      outline_width <- (footnote_fontsize / 7);
   }

   use_align <- "center";
   footnote_x <- grid::unit(0.5, "npc");
   use_hjust <- "center-ink";
   if (grepl("left", x)) {
      footnote_x <- grid::unit(0.0, "npc") + footnote_x_inset;
      use_hjust <- "left-ink";
      use_align <- "left";
   } else if (grepl("right", x)) {
      footnote_x <- grid::unit(1, "npc") - footnote_x_inset;
      use_hjust <- "right-ink";
      use_align <- "right";
   }
   footnote_y <- grid::unit(0.5, "npc");
   use_vjust <- "center-ink";
   if (grepl("top", x)) {
      # footnote_y <- grid::unit(1, "npc") - 0.5 * legend_height - y_inset
      footnote_y <- grid::unit(1, "npc") - footnote_y_inset;
      use_vjust <- "top-ink";
   } else if (grepl("bottom", x)) {
      footnote_y <- grid::unit(0, "npc") + footnote_y_inset;
      use_vjust <- "bottom-ink";
   }

   footnote_style <- marquee::classic_style(
      base_size=footnote_fontsize,
      align=use_align,
      lineheight=1,
      color=footnote_color,
      body_font=footnote_fontfamily)
      # outline=outline, outline_width=outline_width,
      # outline_join="round", outline_mitre=1.2,
      # border="black", border_size=marquee::trbl(1, 1, 1, 1), border_radius=3,
   # background="palegoldenrod",
   # padding=marquee::trbl(1, 3, 1, 3),
   
   #################################################################
   # Temporary devoid device so width=NA works with marquee_grob()
   if (!TRUE %in% draw_footnote &&
         TRUE %in% use_devoid) {
      devVOID <- NULL;
      if (requireNamespace("devoid", quietly=TRUE)) {
         dev1 <- dev.list();
         # ragg::agg_record(); # mimics the effect of devoid without pkg dep
         devoid::void_dev(); # might be more stable than pdf(NULL) in RStudio
         # pdf(NULL); # attempt pdf(NULL) as drop-in replacement
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
   
   footnote_grob <- marquee::marquee_grob(
      x=footnote_x,
      y=footnote_y,
      width=NA,
      text=footnote_text,
      force_body_margin=TRUE,
      ignore_html=TRUE,
      name="venndirfootnote",
      style=footnote_style,
      hjust=use_hjust,
      vjust=use_vjust);
   
      # width=NA,
      # x=footnote_x,
      # y=footnote_y,
      # x=adjx(itemlabels_df$x),
      # y=adjy(itemlabels_df$y),
      # default.units="snpc",
      # angle=jamba::rmNULL(nullValue=0, itemlabels_df$rot),
      # vp=jp_viewport,

   vp <- NULL;
   if ("viewport" %in% names(attributes(venndir_output))) {
      # footnote_grob$vp <- grid::viewport(
      #    x=footnote_x,
      #    y=footnote_y)
      vp <- attr(venndir_output, "viewport");
      footnote_grob$vp <- vp
   }
   # draw the object
   if (TRUE %in% draw_footnote) {
      grid::grid.draw(footnote_grob)
      # grid::pushViewport(vp);
      # grid::grid.points(x=footnote_x, y=footnote_y, pch=20,
      #    gp=grid::gpar(fill=NA, col="blue", cex=0.5),
      #    vp=vp);
      # grid::grid.rect(gp=grid::gpar(fill=NA, col="red"), vp=vp);
      # grid::popViewport();
   }
   return(invisible(footnote_grob))
}
