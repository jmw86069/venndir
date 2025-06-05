
#' Make venndir hexSticker
#' 
#' Make venndir hexSticker with simple labeling
#' 
#' This function is an internal convenience function to create an
#' updated or customized venndir hex sticker, using the optional
#' `hexSticker` and `ggplot2` packages to add some visual layers.
#' The text is drawn using marquee instead of `ggplot2::geom_text()`
#' which allows any 'systemfonts' compatible fonts to be used.
#' 
#' @family venndir internal
#' 
#' @returns `sticker` object defined in 'hexSticker'. Use `plot()` to
#'    employ the appropriate plot command, which defines the device
#'    size and aspect ratios accordingly.
#' 
#' @param venn_labels `character` labels passed to `venn_meme()`
#' @param set_colors `character` colors used as set fill colors, default
#'    uses gold and medium blue.
#' @param xyratio `numeric` default 2.5, the x/y ratio used for label placement
#' @param fontfamily `character` font used for item labels, default "Arial"
#' @param item_cex `numeric` item font size adjustments, default is 2.1 for
#'    each label.
#' @param fill `character` fill for the hex sticker background, default is
#'    light sky blue.
#' @param pkg_family,pkg_weight passed to `marquee::modify_style()` to
#'    provide custom package font family, and font weight.
#' @param ... additional arguments are passed to `venn_meme()` and
#'    to `render_venndir()`.
#' 
#' @examples
#' h2 <- hexsticker_venndir()
#' 
#' h2b <- hexsticker_venndir("blank 2")
#' h2c <- hexsticker_venndir("counts 2")
#' h3a <- hexsticker_venndir("arrows 3")
#' h3b <- hexsticker_venndir("blank 3")
#' 
#' # example with custom labels
#' h2w <- hexsticker_venndir("custom",
#'    venn_labels=c("venn",
#'       "{.firebrick \u2191}{.royalblue3 \u2193}\n{.em ectionality}",
#'       "dir"),
#'    item_cex=c(1.5, 0.9, 1.6))
#' 
#' # example with PNG image files
#' vlogo <- path.expand("~/blip.png")
#' mlogo <- system.file(package="marquee", "help", "figures", "logo.png")
#' h2w <- hexsticker_venndir("custom",
#'    venn_labels=c(
#'       "venn",
#'       paste0("![](", vlogo, ")"),
#'       paste0("![](", mlogo, ")")),
#'    item_cex=c(1.5, 0.9, 1.6))
#' 
#' # example using png loaded into a grid grob
#' if (requireNamespace("png", quietly=TRUE)) {
#'    logo <- system.file(package="marquee", "help", "figures", "logo.png")
#'    logo_grob <- grid::rasterGrob(png::readPNG(logo), interpolate=TRUE)
#'    h2w <- hexsticker_venndir("custom",
#'       venn_labels=c("venn",
#'          "![](logo_grob)",
#'          "dir"),
#'       item_cex=c(1.5, 2.5, 1.6))
#' }
#' 
#' @export
hexsticker_venndir <- function
(inspiration=c("arrows 2",
   "blank 2",
   "counts 2",
   "arrows 3",
   "plain 3",
   "blank 3",
   "custom"),
 venn_labels=NULL,
 set_colors=c("#FFC700", "#637EFE", "#B22222"),
 xyratio=2.5,
 fontfamily="Arial",
 item_cex=c(2.1, 2.1, 2.1),
 # fill="#DEEEFF",
 # border="#9ABBEF",
 fill="#F2E4C8",
 border=NULL,
 pkg_color="#214388",
 pkg_family="Avenir",
 pkg_weight="medium",
 plot_x=1,
 plot_y=1.23,
 outerborder.lwd=0.2,
 innerborder.lwd=0.2,
 expand_fraction=c(-0.5, -0.5, -0.5, -0.45)*9.2/9,
 use_plot=NULL,
 do_plot=TRUE,
 bg="white",
 ...)
{
   #
   inspiration <- match.arg(inspiration)
   if (!requireNamespace("hexSticker", quietly=TRUE)) {
      stop("hexsticker_venndir() requires the hexSticker package.");
   }
   if (length(border) == 0) {
      border <- jamba::makeColorDarker(fill, darkFactor=1.1);
   }
      
   use_meme <- TRUE;
   if (length(venn_labels) == 0) {
      if ("arrows 2" %in% inspiration) {
         venn_labels <- c("{.firebrick \u2191}",
            "{.royalblue4 \u2193}",
            "{.firebrick \u2191}{.royalblue4 \u2193}");
         set_colors <- head(set_colors, 2);
         expand_fraction <- c(-0.5, -0.48, -0.5, -0.42)
      } else if ("blank 2" %in% inspiration) {
         venn_labels <- c(" ",
            "  ",
            "   ");
         set_colors <- head(set_colors, 2);
         expand_fraction <- c(-0.5, -0.48, -0.5, -0.42)
      } else if ("arrows 3" %in% inspiration) {
         venn_labels <- c("{.royalblue3 \u2191}",
            "{.firebrick \u2193}",
            "{.royalblue3 \u2191} ",
            "{.royalblue3 \u2191}{.firebrick \u2193}",
            "{.royalblue3 \u2191}{.firebrick \u2193} ",
            "{.royalblue3 \u2191}{.royalblue3 \u2191}",
            "{.royalblue3 \u2191}{.royalblue3 \u2191}{.firebrick \u2193}")
         expand_fraction <- c(1, 1, 1, 1) * (-0.4);
         item_cex <- rep(item_cex, length.out=7) * rep(c(0.8, 0.4), c(3, 4));
      } else if ("plain 3" %in% inspiration) {
         venn_labels <- c("{.royalblue3 \u2191}",
            "{.firebrick \u2193}",
            "{.royalblue3 \u2191} ",
            " ",
            "  ",
            "   ",
            "    ")
         # "{.royalblue4 \u2191}{.royalblue4 \u2191}{#FF9D8D \u2193}")
         # set_colors <- c("#FFAA99", "darkorchid4", "#5599DD");
         xyratio <- 5;
         expand_fraction <- c(1, 1, 1, 1) * (-0.4);
         item_cex <- item_cex * 0.8;
      } else if ("blank 3" %in% inspiration) {
         venn_labels <- c("      ",
            # "{.firebrick \u2193}",
            # "{.royalblue3 \u2191} ",
            " ",
            "  ",
            "   ",
            "    ")
         # "{.royalblue4 \u2191}{.royalblue4 \u2191}{#FF9D8D \u2193}")
         # set_colors <- c("#FFAA99", "darkorchid4", "#5599DD");
         xyratio <- 5;
         expand_fraction <- c(1, 1, 1, 1) * (-0.4);
         # item_cex <- item_cex * 0.8;
      } else if ("counts 2" %in% inspiration) {
         use_meme <- FALSE;
         setlist <- make_venn_test(500, sizes=c(200, 300), 2,
            concordance=0.85, do_signed=TRUE);
         names(setlist) <- gsub("set_", "set ", names(setlist))
         set_colors <- head(set_colors, 2);
      }
   }
   use_env <- new.env();
   
   use_xyratio <- xyratio + 0;
   
   v4tempobject <- NULL;
   if (use_meme) {
      v4tempobject <- venn_meme(venn_labels,
         set_colors=set_colors,
         xyratio=xyratio,
         fontfamily=fontfamily,
         # circle_nudge=list(A=c(-0.25, 0), B=c(0.25, 0)),
         # proportional=TRUE,
         # font_cex=0.5,
         item_cex=item_cex,
         innerborder.lwd=innerborder.lwd,
         outerborder.lwd=outerborder.lwd,
         draw_legend=FALSE,
         do_plot=FALSE,
         ...)
   } else {
      v4tempobject <- venndir(setlist,
         show_labels="ncs",
         set_colors=set_colors,
         # proportional=TRUE,
         font_cex=c(1.2, 1, 0.7) * 0.5,
         innerborder.lwd=0.1, outerborder.lwd=0.1,
         draw_legend=FALSE,
         do_plot=FALSE)
   }
   # add white background fill
   v4tempobject@jps@polygons[seq_along(setlist(v4tempobject)), "fill"] <- bg
   
   assign("v4tempobject", value=v4tempobject, envir=use_env);
   assign("expand_fraction", value=expand_fraction, envir=use_env);
   assign("innerborder.lwd", value=innerborder.lwd, envir=use_env);
   assign("outerborder.lwd", value=outerborder.lwd, envir=use_env);
   
   attach(use_env);
   on.exit(
      expr={
         detach(use_env);
         rm(use_env)
      },
      add=TRUE)
   
   # make the sticker
   # hex fill
   venndir_sticker <- ggplot2::ggplot() +
      hexSticker::geom_hexagon(size=1.2,
         fill=fill,
         color=NA)
   
   if (length(use_plot) > 0) {
      v4_plot <- use_plot;
   } else {
      v4_plot <- ~render_venndir(v4tempobject,
         xyratio=2.5,
         innerborder.lwd=innerborder.lwd,
         outerborder.lwd=outerborder.lwd,
         expand_fraction=expand_fraction)
   }

   # venndir plot
   # warning occurs when using images inside the sticker
   withr::with_options(list(warn=-1), {
      venndir_sticker <- venndir_sticker +
         hexSticker::geom_subview(
            subview=v4_plot,
            x=plot_x, y=plot_y,
            width=0.4, height=0.5)
   })
   # hex border
   venndir_sticker <- venndir_sticker +
      hexSticker::geom_hexagon(size=1, fill=NA,
         color=border)
   
   # package label using marquee
   m_style <- marquee::modify_style(
      marquee::classic_style(base_size=1),
      "body",
      family=pkg_family,
      weight=pkg_weight,
      padding=marquee::skip_inherit(marquee::trbl(10)),
      border_radius=3)
   venndir_sticker2 <- venndir_sticker +
      ggplot2::annotate(marquee::GeomMarquee,
         x=1, y=0.5, size=9,
         label="venndir",
         style=m_style,
         vjust="center-ink",
         color=pkg_color)
   venndir_sticker2 <- venndir_sticker2 +
      hexSticker::theme_sticker(size=1.2)
   class(venndir_sticker2) <- unique(c("sticker", class(venndir_sticker2)))
   if (TRUE %in% do_plot) {
      plot(venndir_sticker2)
   }
   invisible(venndir_sticker2)
}
