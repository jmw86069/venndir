
#' Internal replacement for gridExtra text_grob using marquee
#' 
#' Internal replacement for gridExtra text_grob using marquee
#' 
#' Drop-in replacement for internal function `gridExtra::text_grob()`,
#' suitable to use for argument `fg_fun` in `gridExtra::tableGrob()`
#' to pass `...` to its internal functions.
#'
#' Note that this function intentionally sets `vjust=0` which forces
#' text to be bottom-justified. This approach is the only reliable
#' method to guarantee the baseline will be properly aligned when
#' systemfonts determines that it must use multiple different
#' fonts to render all the required glyphs in a given string.
#' 
#' This change also requires that the `y` value must also be changed
#' for each table cell, so the text is not `y=0.5` by default, but
#' instead `y=0.16` which positions the baseline near the bottom
#' each cell, the adjusts proportionally for the approximate
#' margin width.
#'
#' @noRd
#' @keywords internal
#' 
marquee_text_grob <- function
(label,
 parse=FALSE,
 col="black",
 fontsize=12,
 cex=1,
 fontfamily="",
 fontface=1L,
 lineheight=1.2,
 alpha=1,
 rot=0,
 check.overlap=FALSE,
 name=NULL,
 vp=NULL,
 just="centre",
 hjust=0.5,
 vjust=0.5,
 x=0.5,
 y=0.5,
 marquee_styles=NULL,
 default.units="npc",
 use_devoid=getOption("use_devoid", TRUE),
 ...)
{
   #
   if (parse) {
      label <- tryCatch(parse(text=label),
         error=function(e)label)
   }
   # define style
   use_weight <- ifelse(fontface %in% c(1, 3), "normal", "bold");
   use_italic <- (fontface %in% c(3, 4));
   use_lineheight <- marquee::relative(lineheight);
   use_align <- ifelse(hjust %in% c(0, "left"), "left",
      ifelse(hjust %in% c(0.5, "center"), "center",
         ifelse(hjust %in% c(1, "right"), "right", "left")))
   #
   kk <- fontsize * cex / 12;
   use_style <- marquee::classic_style(
      base_size=fontsize * cex,
      body_font=fontfamily,
      header_font=fontfamily,
      ltr=TRUE,
      italic=use_italic,
      weight=use_weight,
      color=col,
      lineheight=lineheight/5,
      margin=marquee::trbl(grid::unit(-2*kk, "pt"),
         grid::unit(2*kk, "pt"),
         grid::unit(-2*kk, "pt"),
         grid::unit(2*kk, "pt")),
      padding=marquee::trbl(grid::unit(-2*kk, "pt"),
         grid::unit(2*kk, "pt"),
         grid::unit(-2*kk, "pt"),
         grid::unit(2*kk, "pt")),
      align=use_align)
   # optional user-defined inline styles
   if (length(marquee_styles) > 0) {
      use_style <- combine_marquee_styles(
         mss=use_style,
         msl=marquee_styles,
         ...)
   }
   
   #################################################################
   # Temporary devoid device so width=NA works with marquee_grob()
   if (TRUE %in% use_devoid) {
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
   
   # create grob
   mg <- marquee::marquee_grob(
      force_body_margin=TRUE,
      text=label,
      ignore_html=TRUE,
      style=use_style,
      x=x,
      y=y,
      width=NA,
      hjust=hjust,
      # note: y value must be adjusted later to match custom vjust here
      vjust=marquee::ink(0, use_ink=FALSE),
      angle=rot,
      default.units=default.units,
      vp=vp,
      name=name,
      ...)
   mg;
}
