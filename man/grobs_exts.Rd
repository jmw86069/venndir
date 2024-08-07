% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/venndir-grob-assembly.R
\name{grobs_exts}
\alias{grobs_exts}
\title{Get grid grobs x- and y-extents}
\usage{
grobs_exts(grobs, verbose = FALSE, ...)
}
\arguments{
\item{grobs}{one of:
\itemize{
\item \code{list} of grobs
\item \code{gList}
\item \code{gTree}
}}

\item{...}{additional arguments are ignored.}
}
\value{
\code{list} with elements \code{xext} and \code{yext}, each
of which are \code{list} with min and max values encoded as
\code{grid::unit} objects.
}
\description{
Get grid grobs x- and y-extents
}
\seealso{
Other grobs: 
\code{\link{grobs_stack}()},
\code{\link{grobs_tile}()},
\code{\link{grobs_xalign}()},
\code{\link{grobs_yalign}()}
}
\concept{grobs}
\keyword{internal}
