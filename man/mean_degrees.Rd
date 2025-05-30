% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/venndir-angles.R
\name{mean_degrees}
\alias{mean_degrees}
\title{Mean angle in degrees}
\usage{
mean_degrees(x, seed = 1, ...)
}
\arguments{
\item{x}{\code{numeric} vector of angles in degrees.}

\item{seed}{\code{numeric} value used as a random seed
for reproducibility. This seed affects the random
value added to \code{x} in the form \code{rnorm(length(x)) * 1e-6}.}

\item{...}{additional arguments are ignored.}
}
\value{
\code{numeric} angle in degrees representing the
mean angle of input angles in degrees from \code{x}.
An attribute \code{"radius"} included with the unit
radius from the mean unit vectors. This radius
may be useful as a sort of weight factor.
}
\description{
Mean angle in degrees
}
\details{
Simple utility function to calculate the mean angle
in degrees using vector directional logic. For
each angle, a unit vector is created with radius 1,
then the mean point is used to define the new angle.

A small random value is added to each input value to
reduce the effect of having identical opposite angles.
In that case, the output is a right angle to the
input angles.
}
\examples{
mean_degrees(c(355, 15))

mean_degrees(c(355, 15, 10))

mean_degrees(c(0, 180))

mean_degrees(c(0, 270))

}
\seealso{
Other venndir geometry: 
\code{\link{degrees_to_adj}()},
\code{\link{diff_degrees}()},
\code{\link{display_angles}()},
\code{\link{mean_degree_arc}()},
\code{\link{rescale_coordinates}()},
\code{\link{spread_degrees}()},
\code{\link{three_point_angle}()}
}
\concept{venndir geometry}
