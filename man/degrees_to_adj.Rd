% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/venndir-angles.R
\name{degrees_to_adj}
\alias{degrees_to_adj}
\title{Degrees to text adjustment}
\usage{
degrees_to_adj(
  degrees,
  top = 90,
  clockwise = FALSE,
  expand = 0,
  bias_side = 3,
  bias_height = 1,
  plot_ranges = FALSE,
  ...
)
}
\arguments{
\item{degrees}{\code{numeric} value for angles in degrees}

\item{top}{\code{numeric} value indicating the angle at the
top position}

\item{clockwise}{\code{logical} indicating whether the angle
increases in clockwise direction}

\item{expand}{\code{numeric} value intended to expand the adjust
value. For example \code{expand=0.5} will expand the adjust
value 50\%.}

\item{...}{additional arguments are ignored}
}
\description{
Degrees to text adjustment
}
\details{
Utility function to define \code{adj} values suitable
for text plotting, which arranges text relative
to the angle in degrees.
}
\examples{
degrees <- seq(from=1, to=360, by=15);
x <- cos(jamba::deg2rad(degrees));
y <- sin(jamba::deg2rad(degrees));
adjdf <- degrees_to_adj(degrees);
adjdf1 <- degrees_to_adj(degrees, bias_side=1, bias_height=1);
plot(x, y,
   pch=20, asp=1,
   xlim=c(-1.3, 1.3),
   ylim=c(-1.3, 1.3));
jamba::drawLabels(txt=seq_along(degrees),
   x=x, y=y, labelCex=0.8,
   adjX=adjdf1[,1], adjY=adjdf1[,2])
jamba::drawLabels(txt=seq_along(degrees),
   x=x, y=y, labelCex=0.8, boxColor="gold",
   adjX=adjdf[,1], adjY=adjdf[,2])
plot(x, y,
   pch=20, asp=1, cex=0.4, col="red",
   xlim=c(-1.3, 1.3),
   ylim=c(-1.3, 1.3));
for (i in seq_along(degrees)) {
   text(labels=i,
      x=x[i], y=y[i],
      adj=unlist(adjdf[i,]))
}

}
\seealso{
Other venndir geometry: 
\code{\link{diff_degrees}()},
\code{\link{display_angles}()},
\code{\link{mean_degree_arc}()},
\code{\link{mean_degrees}()},
\code{\link{rescale_coordinates}()},
\code{\link{spread_degrees}()},
\code{\link{three_point_angle}()}
}
\concept{venndir geometry}
