% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/venndir-polyclip.R
\name{polygon_circles}
\alias{polygon_circles}
\title{Make polygon_list circles}
\usage{
polygon_circles(xcenter, ycenter, setnames = NULL, radius = 1, n = 60, ...)
}
\arguments{
\item{xcenter, ycenter}{\code{numeric} vector that defines the x and y
coordinate position of the center of each circle.}

\item{setnames}{\code{vector} that contains names for each circle, stored
as \code{names()} for each polygon.
When \code{setnames} is \code{NULL} then index numbers are used.}

\item{radius}{\code{numeric} vector that defines the radius of each circle.
This \code{vector} is recycled to \code{length(xcenter)}.}

\item{n}{\code{integer} value indicating the number of subdivisions to
use in the circle.}

\item{...}{additional arguments are ignored.}
}
\value{
object \code{list} with a number of circles encoded as polygons.
}
\description{
Make polygon_list circles
}
\details{
This function creates one or more circles as polygon_list \code{list} objects.
}
\examples{
polygon_list <- polygon_circles(c(3, 2), c(2, 3))
plot_polygon_list(polygon_list)
points(x=c(3, 2), y=c(2, 3), pch=c("1", "2"), add=TRUE);

}
\seealso{
Other venndir polygons: 
\code{\link{bbox_polygon_list}()},
\code{\link{eulerr_to_polygon_list}()},
\code{\link{get_largest_polygon_list}()},
\code{\link{get_venn_polygon_shapes}()},
\code{\link{intersect_polygon_list}()},
\code{\link{labelr_polygon_list}()},
\code{\link{minus_polygon_list}()},
\code{\link{nudge_polygon_coords}()},
\code{\link{nudge_polygon_list}()},
\code{\link{plot_polygon_list}()},
\code{\link{polygon_areas}()},
\code{\link{polygon_ellipses}()},
\code{\link{polygon_list_labelr}()},
\code{\link{polygon_list_to_xy_list}()},
\code{\link{rescale_polygon_list}()},
\code{\link{simple_ellipse}()},
\code{\link{union_polygon_list}()},
\code{\link{xy_list_to_polygon_list}()}
}
\concept{venndir polygons}
