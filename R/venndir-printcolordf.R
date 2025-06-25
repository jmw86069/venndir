
#' Print colorized text table
#' 
#' Print colorized text table
#' 
#' This utility function takes `df` as a `data.frame` input,
#' and prints colorized text output.
#' 
#' Colors are defined using `dfcolor` provided as a `data.frame`.
#' 
#' Colors can be inverted using `dfinvert` that contains logical
#' `TRUE` or `FALSE` values, provided as a `data.frame`.
#' 
#' @family venndir internal
#' 
#' @param df `data.frame` with content to be printed to the R console.
#' @param dfcolor `data.frame` with R compatible colors in each cell.
#' @param dfinvert `data.frame` with `logical` values indicating whether
#'    each color should be inverted `TRUE` so the cell color is applied
#'    as a background color. In this case the foreground text color is
#'    determined using `jamba::setTextContrastColor()`.
#' @param dfjustify `character` or `data.frame` passed to `format()`
#'    to determine the text alignment in each cell. When supplied as
#'    a vector, it describes each column of output; when supplied as
#'    a `data.frame` it describes each cell.
#' @param header `logical` indicating whether to print column names
#'    at the top of the output.
#' @param padding `integer` value that defines the number of characters
#'    added before and after each text term to add visual space between
#'    adjacent cells.
#' @param comment,timeStamp `logical` arguments passed to
#'    `jamba::printDebug()`, which indicate whether to print output
#'    as a comment (with `"#"` at the start of each line), and whether
#'    to include a time stamp. The default `FALSE` turn these options
#'    off.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' set.seed(2);
#' df <- matrix(sample(head(colors(), 151), 12), ncol=3);
#' dfinvert <- matrix(sample(c(FALSE, FALSE, TRUE), 12, replace=TRUE), ncol=3);
#' print_color_df(df, df, dfinvert);
#' 
#' print_color_df(df, df, dfinvert, padding=1);
#' 
#' colnames(df) <- c("left", "centre", "right");
#' print_color_df(df, df, dfinvert,
#'    header=TRUE,
#'    padding=4,
#'    dfjustify=c("left", "centre", "right"));
#' 
#' @export
print_color_df <- function
(df,
 dfcolor=NULL,
 dfinvert=NULL,
 dfjustify="centre",
 header=FALSE,
 padding=2,
 pad_type=1,
 comment=FALSE,
 timeStamp=FALSE,
 htmlOut=getOption("jam.htmlOut", FALSE),
 debug=FALSE,
 ...)
{
   if (length(dfjustify) == 0) {
      dfjustify <- "centre";
   }
   if (is.atomic(dfjustify)) {
      dfjustify <- rep(dfjustify,
         length.out=ncol(df));
      dfjustify <- matrix(dfjustify,
         byrow=TRUE,
         ncol=ncol(df),
         nrow=nrow(df));
   }
   
   ## Optionally include column headers in the output
   if (any(header) && length(colnames(df)) > 0) {
      # extend df
      dfc <- colnames(df);
      dfci <- dfc;
      df <- do.call(cbind, lapply(seq_len(ncol(df)), function(i){
         c(dfc[i], df[,i]);
      }));
      # extend dfcolor
      if (length(colnames(dfcolor)) > 0) {
         dfc <- colnames(dfcolor);
      }
      dfcolor <- do.call(cbind, lapply(seq_len(ncol(dfinvert)), function(i){
         c(ifelse(jamba::isColor(dfc[i]),
            dfc[i],
            NA),
            dfcolor[,i]);
      }));
      # extend dfinvert
      if (length(colnames(dfinvert)) > 0) {
         dfci <- colnames(dfinvert);
      }
      dfinvert <- do.call(cbind, lapply(seq_len(ncol(dfinvert)), function(i){
         c(jamba::rmNA(
            as.logical(dfci[i]),
            naValue=FALSE),
            dfinvert[,i]);
      }));
   }
   
   for (i in seq_len(ncol(df))) {
      ilen <- max(jamba::rmNA(naValue=0, nchar(df[,i])))
      if (length(unique(dfjustify[,i])) == 1) {
         df[,i] <- format(df[,i],
            justify=dfjustify[1,i],
            width=ilen);
      } else {
         df[,i] <- unname(sapply(seq_along(df[,i]), function(j){
            format(df[j,i],
               justify=dfjustify[j,i],
               width=ilen)
         }));
      }
   }
   padchars <- paste(collapse="",
      rep(" ", length.out=padding));
   if (any(c("html", "data.frames") %in% debug)) {
      return(list(
         df=df,
         dfcolor=dfcolor));
   }
   for (iline in seq_len(nrow(df))) {
      line_invert <- dfinvert[iline,];
      line_fg <- ifelse(line_invert,
         jamba::setTextContrastColor(dfcolor[iline,]),
         dfcolor[iline,])
      line_bg <- ifelse(line_invert,
         dfcolor[iline,],
         NA);
      if (TRUE %in% htmlOut) {
         catlines <- capture.output(jamba::printDebug(
            timeStamp=FALSE,
            comment=FALSE,
            gsub(" ", "&nbsp;", paste0(padchars, df[iline,], padchars)),
            fgText=as.list(line_fg),
            bgText=as.list(line_bg),
            sep="",
            splitComments=TRUE,
            htmlOut=TRUE,
            ...))
         catlines <- paste0(
            "<span style=\"font-family: monospace\">",
            gsub("<br/>$", "", catlines),
            "</span><br/>\n");
         cat(catlines, sep="");
      } else {
         catlines <- jamba::printDebug(
            timeStamp=FALSE,
            comment=FALSE,
            paste0(padchars, df[iline,], padchars),
            fgText=as.list(line_fg),
            bgText=as.list(line_bg),
            sep="",
            splitComments=TRUE,
            htmlOut=FALSE,
            ...)
      }
   }
}

#' Make color contrast with background colors
#' 
#' Make color contrast with background colors
#' 
#' This function provides a simple method to ensure a color
#' has adequate visual contrast with a background color,
#' while retaining some color saturation. For example,
#' red on red background will return something close to
#' pink, so the pink retains the red color saturation
#' but is visually distinct from the background color red.
#' 
#' Similarly, pink on pink will return something close to
#' red.
#' 
#' @family venndir advanced
#' 
#' @param x `character` vector of text colors
#' @param y `character` vector of fill colors for which the colors
#'    in `x` should be adjusted for visual contrast.
#' @param bg `character` background color, default 'white', used
#'    when colors in `y` contain transparency.
#' @param L_threshold `numeric` threshold Luminance (L) value which
#'    defines the point where the background color should have
#'    light or dark text for optimum visual contrast. The default `65`
#'    is well-discussed in data visualization forums, however
#'    may not be ideal for all viewing conditions.
#' @param C_floor `numeric` to define the minimum Chroma (C) when
#'    the adjusted color Chroma is below an internal threshold 20.
#' @param L_hi,L_lo `numeric` value to define the Luminance (L) used
#'    for 'hi' (bright) output colors, or 'lo' (dark) output
#'    colors. The default 'L_hi=95' is fairly high, and 'L_lo=40'
#'    is moderately dark. In future the recommended default may
#'    become 'L_hi=85' to improve the resulting color hue.
#' @param blend_preset `character` string passed as `preset` to
#'    `colorjam::blend_colors()` to define the color wheel used
#'    during color blending operations. Default 'ryb' uses the
#'    red-yellow-blue color wheel. This adjustment only affects
#'    layering 'y' together with 'bg'.
#' @param do_plot `logical` whether to create a visual plot illustrating
#'    the input and output colors, on top of the background.
#' @param cex `numeric` text size adjustment used when `do_plot=TRUE`.
#' @param ... additional arguments are passed to internal functions,
#'    including `colorjam::blend_colors()`.
#' 
#' @examples
#' x <- c("firebrick", "dodgerblue", "gold", "pink", "white");
#' y <- rep("red3", 4);
#' make_color_contrast(x, y, do_plot=TRUE);
#' 
#' y <- rep("pink1", 4);
#' make_color_contrast(x, y, do_plot=TRUE);
#' 
#' y <- rep("gold", 4);
#' make_color_contrast(x, y, do_plot=TRUE);
#' 
#' y <- c("red4", "aquamarine4", "blue3", "yellow", "pink2")
#' make_color_contrast(x, y, do_plot=TRUE, C_floor=140);
#' 
#' @export
make_color_contrast <- function
(x,
 y=NULL,
 bg="white",
 L_threshold=65,
 C_floor=90,
 L_hi=95,
 L_lo=40,
 blend_preset="ryb",
 do_plot=FALSE,
 cex=2,
 ...)
{
   # determine whether x versus y is light or dark
   #y_ld <- jamba::setTextContrastColor(color=y,
   #   bg="white",
   #   useGrey=0);
   if (length(x) < length(y)) {
      x <- rep(x, length.out=length(y));
   }
   if (length(y) == 0) {
      y <- bg;
   }
   if (length(y) < length(x)) {
      y <- rep(y, length.out=length(x));
   }
   #y <- c("#FF0000FF", "#FF0000CC", "#FF0000AA", "#FF000099", "#FF000066", "#FF000033");
   bg <- rep(bg, length.out=length(y));
   y_alpha <- jamba::col2alpha(y);
   # adjust alpha
   y_alpha_adj <- y_alpha ^ (0.95)
   y1 <- jamba::alpha2col(y, alpha=y_alpha_adj);
   new_bg <- jamba::alpha2col(bg, alpha=1 - y_alpha_adj);
   ybg_list <- as.list(as.data.frame(t(cbind(y1, new_bg))));
   new_y <- colorjam::blend_colors(ybg_list,
      preset=blend_preset,
      ...);
   x_hcl <- jamba::col2hcl(x);
   y_hcl <- jamba::col2hcl(new_y);
   
   # honestly not sure what I was thinking here
   if (1 == 2) {
      lite_bg <- grepl("white|#[FEfe]{6}",
         jamba::setTextContrastColor(new_y,
            hclCutoff=L_threshold,
            useGrey=0,
            bg=bg));
   } else {
      lite_bg <- ifelse(y_hcl["L",] < L_threshold,
         TRUE,
         FALSE);
   }
   
   x_hcl["L",] <- ifelse(lite_bg,
      jamba::noiseFloor(x_hcl["L",], minimum=L_hi),
      jamba::noiseFloor(x_hcl["L",], ceiling=L_lo))
   x_hcl["C",] <- ifelse(x_hcl["C",] < 20,
      x_hcl["C",],
      C_floor);
   x2 <- jamba::hcl2col(x_hcl);
   if (do_plot) {
      jamba::imageByColors(
         matrix(ncol=length(y),
            rep(y, each=2)));
      text(x=rep(seq_along(x), 2),
         y=rep(c(2, 1), each=length(x)),
         labels=c(x, rep(c("output"),
            length(x))),
         cex=cex,
         col=c(x, x2));
   }
   #return(invisible(x2));
   return(x2);
}

