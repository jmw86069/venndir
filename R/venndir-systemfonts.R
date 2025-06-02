
#' Subset system fonts for review
#' 
#' Subset system fonts for review, to organize fonts available to systemfonts
#' 
#' This function provides a convenient method to subset fonts
#' recognized by `systemfonts::system_fonts()`, while also sorting
#' and filtering out "problem fonts". The argument `do_plot=TRUE`
#' will visualize the resulting fonts using `marquee::marquee_grob()`.
#' 
#' Note that there are some "problem fonts" which are not recognized
#' by the Freetype API used by systemfonts, which appear to arise
#' from newer font file formats released on MacOS which are not yet
#' recognized by Freetype. Using one of these fonts may cause R crash,
#' therefore some steps were taken to avoid this possibility:
#' 
#' * The output will include one new column `"check"` with integer number
#' of valid associated font files, usually `1` for valid, and `0` for no
#' valid files.
#' * The argument `remove_problem_fonts=TRUE` removes any fonts with `0`,
#' thereby helping to avoid crashing the R session.
#' * The function `check_systemfonts_family()` will check one or more
#' font family values, returning the integer number of valid font files.
#' 
#' @returns `data.frame` with font information derived from
#'    `systemfonts::system_fonts()` and subset according to '...'.
#' 
#' @family venndir internal
#'
#' @param ... additional arguments are passed to `subset()` to subset
#'    the resulting table accordingly. This argument is first to
#'    avoid matching other named arguments below.
#' @param fonts_df `data.frame` full set of fonts, default uses
#'    `systemfonts::system_fonts()` to include all fonts recognized
#'    by systemfonts.
#' @param do_plot `logical` whether to plot the search results using marquee,
#'    default FALSE.
#' @param return_type `character` string with return class, default is
#'    'data.frame' which converts the tibble output to data.frame
#'    for convenience.
#' @param trim_path `logical` whether to trim the font file path, default
#'    TRUE removes the folder by calling `basename()`.
#' @param do_sort `logical` whether to sort output, using 'byCols'.
#' @param byCols `character` columns used when `do_sort==TRUE`.
#' @param remove_problem_fonts `logical` default TRUE, whether to check for
#'    errors accessing each font family by calling `systemfonts::font_info()`.
#' 
#' @examples
#' subset_systemfonts(grepl("(Arial(.*Narrow|)|Helvetica)$", family))
#' subset_systemfonts(grepl("(Arial(.*Narrow|)|Helvetica.*)$", family), do_plot=TRUE)
#' subset_systemfonts(grepl("Helvetica Neue", family), do_plot=FALSE)
#' 
#' subset_systemfonts(grepl("serif", ignore.case=TRUE, name), do_plot=TRUE)
#' 
#' @export
subset_systemfonts <- function
(...,
 fonts_df=systemfonts::system_fonts(),
 do_plot=FALSE,
 return_type=c("data.frame"),
 trim_path=TRUE,
 do_sort=TRUE,
 byCols=c("family", "italic", "weight", "style", "width", "name"),
 remove_problem_fonts=TRUE)
{
   #
   return_type <- match.arg(return_type);
   
   if (TRUE %in% trim_path && "path" %in% colnames(fonts_df)) {
      fonts_df$path <- basename(fonts_df$path)
   }
   
   if (!"check" %in% colnames(fonts_df)) {
      f <- unique(fonts_df$family)
      fcheck <- check_systemfonts_family(f)
      fonts_df$check <- fcheck[fonts_df$family]
   }
   if (TRUE %in% remove_problem_fonts && "check" %in% colnames(fonts_df)) {
      if (any(fonts_df$check %in% c(0, NA))) {
         fonts_df <- subset(fonts_df, !check %in% c(0, NA));
      }
   }
   
   sub_df <- subset(fonts_df, ...)
   if ("data.frame" %in% return_type) {
      sub_df <- data.frame(check.names=FALSE, sub_df)
   }
   
   # manually remove PingFangUI, haha. What a hack.
   # sub_df <- subset(sub_df, !grepl("PingFangUI.ttc", ignore.case=TRUE, path));
   
   if (TRUE %in% do_sort) {
      # style
      style_levels <- jamba::provigrep(c("regular",
         "Roman",
         "Monospaced",
         "ultra.*light",
         "light",
         "medium",
         "demi.*bold",
         "^bold",
         "bold",
         "black",
         "heavy",
         "italic",
         "."),
         unique(sub_df$style))
      sub_df$style <- factor(sub_df$style, levels=style_levels)
      # weight
      weight_levels <- jamba::provigrep(c(
         "thin",
         "ultralight",
         "light",
         "regular", "normal",
         "book",
         "roman",
         "medium",
         "semibold",
         "^bold",
         "ultrabold",
         "heavy",
         "italic",
         "Oblique",
         "."),
         unique(sub_df$weight))
      sub_df$weight <- factor(sub_df$weight, levels=weight_levels)
      # width
      width_levels <- jamba::provigrep(c("normal",
         "compressed", "condensed", "narrow", "."),
         unique(sub_df$width))
      sub_df$width <- factor(sub_df$width, levels=width_levels)
      
      sub_df <- jamba::mixedSortDF(sub_df, byCols=byCols)
   }
   
   # optional plot to visualize fonts
   if (TRUE %in% do_plot && nrow(sub_df) > 0) {
      #
      fam_ct <- jamba::tcount(sub_df$family);
      if (max(fam_ct) >= 4) {
         sub_list <- split(sub_df, sub_df$family);
      } else {
         sub_list <- list(sub_df);
      }
      x <- head(
         seq(from=0, to=1, length.out=length(sub_list) + 1),
         -1);
      y_vals <- tail(
         seq(from=1, to=0,
            length.out=max(sapply(sub_list, nrow)) + 2),
         -1);
      y_gap <- diff(y_vals[1:2]);
      y_offset <- rev(head(seq(0, -y_gap, length.out=length(sub_list) + 1), -1));
      
      grid::grid.newpage();
      for (j in seq_along(x)) {
         isub_df <- sub_list[[j]];
         y <- head(y_vals, nrow(isub_df)) + y_offset[j];
         
         for (i in seq_along(isub_df$name)) {
            i_style <- marquee::modify_style(
               marquee::classic_style(base_size=14),
               "body",
               family=isub_df$name[i],
               padding=marquee::skip_inherit(marquee::trbl(10)),
               border_radius=3)
            
            mgrob <- marquee::marquee_grob(text=isub_df$name[i],
               x=x[j], y=y[i],
               style=i_style,
               vjust="center-ink")
            grid::grid.draw(mgrob);
         }
      }
   }
   
   sub_df
}


#' Check systemfonts family for freetype compatibility
#' 
#' Check systemfonts family for freetype compatibility
#' 
#' This function simply checks whether the systemfonts API for freetype
#' recognizes a font file being valid, given a font family value.
#' The main purpose is to avoid trying to use a font family which
#' would may produce an error, since it may terminate the R session.
#' 
#' It is thin a wrapper for `systemfonts::font_info()`.
#' 
#' When a font 'fails' it causes an error that can be reviewed with argument
#' `debug=TRUE`. The error is usually this format:
#' 
#' `"Failed to open font file (/font/path/family.ttf) with freetype error 6"`
#' 
#' The error originates from the freetype API, and means the file is not
#' usable by that API. It sometimes happens when a font file has a newer
#' format than is recognized by the freetype API.
#' 
#' @family venndir internal
#'
#' @returns `integer` vector with the number of valid font info rows
#'    returned by `systemfonts::font_info()`, for each entry provided
#'    in `family`.
#' 
#' @param family `character` vector with font family names.
#' @param path `character`, default NULL, with optional file path to
#'    one or more specific font files. When provided, the 'family' argument
#'    is ignored.
#' @param debug `logical` whether to print any error message, default is FALSE.
#' @param ... additional arguments are passed to `systemfonts::font_info()`,
#'    which may be useful to specify arguments such as: `italic=TRUE`,
#'    `weight="bold"`, or `width="condensed"` for example.
#'    Alternatively, argument `path` can be used to point to a specific
#'    font file, thereby circumventing family and style.
#' 
#' @examples
#' check_systemfonts_family(c("Noto Sans Syriac", "Skia", "PingFang TC", "Arial"))
#' 
#' check_systemfonts_family(c("Arial", "Arial"))
#' 
#' xpaths <- subset_systemfonts(grepl("Helvetica", family), trim_path=FALSE)$path
#' check_systemfonts_family(xpaths[1])
#' 
#' @export
check_systemfonts_family <- function
(family="Arial",
 path=NULL,
 debug=FALSE,
 ...)
{
   #
   if (length(path) > 0) {
      family <- path
   }
   f <- unique(family);
   
   fcheck <- sapply(f, function(i){
      tryCatch({
         if (length(path) > 0) {
            xout <- systemfonts::font_info(path=i, ...)
         } else {
            xout <- systemfonts::font_info(i, ...)
         }
         nrow(xout)
      }, error=function(e){
         if (TRUE %in% debug) {
            print(e)
         }
         0
      })
   })
   fcheck[family]
}
