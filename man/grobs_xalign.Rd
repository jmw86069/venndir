% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/venndir-grob-assembly.R
\name{grobs_xalign}
\alias{grobs_xalign}
\title{Left-align grobs}
\usage{
grobs_xalign(
  grobs,
  name = NULL,
  xalign = c("left", "center", "right", "tile", "none"),
  ref_grob = 1,
  use_x = NULL,
  verbose = FALSE,
  debug = FALSE,
  ...
)
}
\arguments{
\item{grobs}{\code{gList} collection of graphical objects}

\item{xalign}{\code{character} string indicating the type of alignment:
\itemize{
\item \code{"left"} - objects are aligned at the left edge
\item \code{"center"} - objects are aligned at the object center
\item \code{"right"} - objects are aligned at the right edge
\item \code{"tile"} - objects are aligned with the left edge
touching the right edge of the previous object (experimental)
\item \code{"none"}
}}

\item{ref_grob}{\code{integer} index of the grob in \code{grobs} to use as the
reference, around which other grobs will be oriented.}

\item{use_x}{\code{unit} used as a reference x coordinate, used in
favor of other options such as \code{ref_grob} or the first grob in \code{grobs}.}

\item{verbose}{\code{logical} indicating whether to print verbose output.}

\item{debug}{\code{logical} indicating whether to print debug output.}

\item{...}{additional arguments are ignored.}
}
\description{
Left-align grobs
}
\seealso{
Other grobs: 
\code{\link{grobs_exts}()},
\code{\link{grobs_stack}()},
\code{\link{grobs_tile}()},
\code{\link{grobs_yalign}()}
}
\concept{grobs}
\keyword{internal}
