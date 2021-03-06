% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/venndir-labels.R
\name{draw_gridtext_groups}
\alias{draw_gridtext_groups}
\title{Draw boundary around grouops of gridtext labels}
\usage{
draw_gridtext_groups(
  g_labels,
  gdf,
  segment_df = NULL,
  do_draw = TRUE,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{g_labels}{\code{richtext_grob} class as returned by
\code{gridtext::richtext_grob()}.}

\item{gdf}{\code{data.frame} with one row per \code{grob} in \code{g_labels},
with colnames \code{"x"}, \code{"y"}, \code{"overlap_set"}, \code{"r"},
\code{"r_unit"}, \code{"border_col"}, \code{"box_fill"}. The rows
are grouped using \code{c("x", "y", "overlap_set")}.
The \code{"r"} and \code{"r_unit"} values are used to define
the radius and radius unit for the rounded corner.
The row in \code{gdf} with the highest \code{"r"} value is the
reference row, for the \code{"r"} radius, and the \code{"border_col"}
and \code{"box_fill"} values.}
}
\description{
Draw boundary around grouops of gridtext labels
}
\details{
This function is a helper function used to take individual
labels defined by \code{gridtext::richtext_grob()}, and creating
\code{grid::roundrectGrob()} objects that cover each group of
labels with one rounded rectangular boundary.
}
\seealso{
Other venndir label: 
\code{\link{polygon_label_fill}()},
\code{\link{polygon_label_outside}()},
\code{\link{polygon_label_segment}()},
\code{\link{sp_polylabelr}()}
}
\concept{venndir label}
