% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/venndir-nudge-label.R
\name{nudge_venndir_label}
\alias{nudge_venndir_label}
\title{Nudge venndir labels}
\usage{
nudge_venndir_label(
  venndir_output,
  set = NULL,
  x_offset = 0,
  y_offset = 0,
  offset_list = NULL,
  align_y = c("none", "top", "middle", "bottom"),
  align_x = c("none", "left", "center", "right"),
  unit_type = c("relative", "absolute"),
  label_location = "outside",
  verbose = FALSE,
  ...
)
}
\arguments{
\item{venndir_output}{output from \code{venndir()} as \code{Venndir} object.}

\item{set}{\code{character} name of the sets or overlaps to adjust.}

\item{x_offset, y_offset}{\code{numeric} coordinates to adjust, recycled
to the number of entries provided in \code{set}.}

\item{offset_list}{\code{list} (default NULL) used as a shorthand alternative
to \code{set},\code{x_offset},\code{y_offset}. The format is a \code{list} with x,y
offset values, with list elements named by set. For example:
\code{offset_list = list(setA=c(0, 0.1), set_B=c(-0.1, 0))}}

\item{align_y, align_x}{\code{character} string for optional alignment of
labels, where all labels in \code{set} are aligned.
\itemize{
\item This option is recommended only with \code{label_location="outside"},
in order to align labels relative to each other.
\item It is recommeded for example, to make sure two top labels
are placed at the same height relative to each other.
\item All labels in \code{set} are adjusted as a group, after applying
\code{x_offset},\code{y_offset}.
\item The coordinates of all labels in \code{set} are used, then the
target coordinate is defined by the logic:
\item \verb{"top","middle","bottom"} - uses the highest, middle, or lowest
coordinate among the labels in \code{set}.
\item \verb{"left","center","right"} - uses the left-most, center, or
right-most  coordinate among the labels in \code{set}.
}}

\item{unit_type}{\code{character} string (default "relative") defining how
to interpret the \code{x_offset},\code{y_offset} values.
\itemize{
\item \code{"relative"} - interprets the adjustment relative to the plot
bounding box, specifically the largest axis span.
This option is useful when the plot span is not known.
\item \code{"absolute"} - interprets the adjustment with absolute units,
which is useful when the plot span is known.
}}

\item{label_location}{\code{character} string (default "outside") indicating
which label coordinate to adjust:
\itemize{
\item \code{"outside"} - will only adjust the outer label, leaving the inner
label position unaffected.
\item \code{"inside"} - will only adjust the inner label, leaving the outer
label position unaffected.
\item \code{"all"} - will adjust both the inner and outer label positions
together.
}}

\item{verbose}{\code{logical} indicating whether to print verbose output.}

\item{...}{additional arguments are ignored.}
}
\value{
\code{Venndir} object after adjusting label coordinates
}
\description{
Nudge venndir labels
}
\details{
Venndir labels are defined for each overlap polygon, with "inner"
and "outer" label coordinates for each polygon.
The \code{show_labels} argument to \code{venndir()} defines which labels
are placed inside and outside the Venn diagram polygons.
This function is useful to adjust the position of one or more
labels.

This function does not determine whether labels are displayed inside
or outside the Venn polygons.
}
\examples{
setlist1 <- make_venn_test(100, 3, do_signed=TRUE)

vo1 <- venndir(setlist1,
   overlap_type="each",
   return_items=TRUE,
   label_style="lite_box",
   main="Default venndir")
render_venndir(vo1)

head(vo1@label_df[, c("x", "x_offset", "y", "y_offset")], 3)
subset(vo1@label_df, overlap_set \%in\% "set_A")[, c("x", "x_offset", "y", "y_offset")]

vo2 <- nudge_venndir_label(vo1,
   set=c("set_A", "set_B"),
   x_offset=c(-0.1, 0.1),
   y_offset=c(0))
render_venndir(vo2)

# alternative with offset_list
vo2b <- nudge_venndir_label(vo1,
   offset_list=list(
      set_A=c(0.1, 0),
      set_B=c(0.1, 0),
      set_C=c(0.4, 0.4)))
render_venndir(vo2b)

# now align two labels at the top
vo2c <- nudge_venndir_label(vo2b,
   set=c("set_A", "set_B"),
   align_y="top")
render_venndir(vo2c)

}
\seealso{
Other venndir advanced: 
\code{\link{collapse_im}()},
\code{\link{combine_marquee_styles}()},
\code{\link{make_color_contrast}()}
}
\concept{venndir advanced}
