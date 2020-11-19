
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
#' @family venndir utility
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
   for (iline in seq_len(nrow(df))) {
      line_invert <- dfinvert[iline,];
      line_fg <- ifelse(line_invert,
         jamba::setTextContrastColor(dfcolor[iline,]),
         dfcolor[iline,])
      line_bg <- ifelse(line_invert,
         dfcolor[iline,],
         NA);
      jamba::printDebug(
         timeStamp=FALSE,
         comment=FALSE,
         paste0(padchars, df[iline,], padchars),
         fgText=as.list(line_fg),
         bgText=as.list(line_bg),
         sep="",
         splitComments=TRUE);
   }
}

#' Make color contrast
#' 
#' Make color contrast
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
#' @family venndir utility
#'
#' @examples
#' x <- c("red", "blue", "gold", "ivory", "pink", "white");
#' y <- rep("red4", 4);
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
 y,
 bg="white",
 L_threshold=55,
 C_floor=130,
 L_hi=100,
 L_lo=40,
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
   if (length(y) < length(x)) {
      y <- rep(y, length.out=length(x));
   }
   x_hcl <- jamba::col2hcl(x);
   y_hcl <- jamba::col2hcl(y);
   lite_bg <- grepl("white|#[FEfe]{6}",
      jamba::setTextContrastColor(y,
         hclCutoff=L_threshold,
         bg=bg));
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
   return(invisible(x2));
}

