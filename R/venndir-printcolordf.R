
#' Print colorized text table
#' 
#' Print colorized text table
#' 
#' @export
print_color_df <- function
(df,
 dfcolor=NULL,
 dfinvert=NULL,
 colnames=FALSE,
 padding=2,
 ...)
{
   for (i in seq_len(ncol(df))) {
      ilen <- max(jamba::rmNA(naValue=0, nchar(df[,i])))
      df[,i] <- format(df[,i], justify="centre", width=ilen + padding*2)
   }
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
         df[iline,],
         fgText=as.list(line_fg),
         bgText=as.list(line_bg),
         sep="",
         splitComments=TRUE);
   }
}
