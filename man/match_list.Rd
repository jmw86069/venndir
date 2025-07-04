% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/venndir-utilities.R
\name{match_list}
\alias{match_list}
\title{Match list elements to another list}
\usage{
match_list(x, y, ...)
}
\arguments{
\item{x, y}{\code{list} objects to be compared}

\item{...}{additional arguments are ignored.}
}
\value{
\code{integer} \code{vector} with the same length as \code{x}. The
integer values give the position in \code{y} of the first match.
}
\description{
Match list elements to another list
}
\details{
This function takes two \code{list} objects, and matches
the first \code{list} elements to the second \code{list}.

Each list contains list elements, for example if \code{x}
is a \code{list}, then the element in position \code{i} is accessed
using \code{x[[i]]}.

A match between \code{x[[i]]} and \code{y[[j]]} is defined as follows:
\itemize{
\item all elements in \code{x[[i]]} are contained in \code{y[[j]]}
\item all elements in \code{y[[j]]} are contained in \code{x[[i]]}
}

For this function, item order and item duplication is
ignored.

This function uses logic in the form \code{all(x[[i]] \%in\% y[[j]])},
so it will operate properly with input objects compatible
with that format. The function is intended to be used with
\code{list} that contains \code{atomic} \code{vectors}.
}
\examples{
x <- list(a=LETTERS[1],
   b=LETTERS[1:2],
   c=LETTERS[2:4]);
x;
y <- list(
   d=LETTERS[1:2],
   e=LETTERS[2],
   f=LETTERS[2:4]);
y;
match_list(x, y)
match_list(y, x)

}
\seealso{
Other venndir internal: 
\code{\link{assemble_venndir_label}()},
\code{\link{assign_degree_groups}()},
\code{\link{check_systemfonts_family}()},
\code{\link{expand_range}()},
\code{\link{get_venn_polygon_shapes}()},
\code{\link{make_degrees_clockwise}()},
\code{\link{make_venn_combn_df}()},
\code{\link{print_color_df}()},
\code{\link{render_venndir_footnotes}()},
\code{\link{shrink_df}()},
\code{\link{simple_ellipse}()},
\code{\link{subset_systemfonts}()},
\code{\link{venndir_label_style}()},
\code{\link{venndir_to_df}()}
}
\concept{venndir internal}
