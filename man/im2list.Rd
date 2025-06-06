% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/venndir-im.R
\name{im2list}
\alias{im2list}
\title{Incidence matrix to list}
\usage{
im2list(x, empty = list(NA, "", 0, FALSE), ...)
}
\arguments{
\item{x}{\code{matrix} where non-empty values indicate presence of each
element (row) in each set (column).}

\item{empty}{\code{list} of values recognized as empty. Each item is
co-erced to the class in columns of \code{x}.}

\item{...}{additional arguments are ignored.}
}
\description{
Incidence matrix to list
}
\details{
This function is the reciprocal to \code{list2im()}. This function
will also convert a signed incidence matrix to a normal
list, removing the directional sign.
}
\examples{
setlist <- make_venn_test(100, 3, do_signed=TRUE)
ims <- list2im_value(setlist);
print(head(ims));

# convert back to list
im_value2list(ims);
im2list(ims);

}
\seealso{
Other venndir conversion: 
\code{\link{collapse_im}()},
\code{\link{counts2setlist}()},
\code{\link{im_value2list}()},
\code{\link{list2im_opt}()},
\code{\link{list2im_value}()},
\code{\link{overlaplist2setlist}()},
\code{\link{signed_counts2setlist}()}
}
\concept{venndir conversion}
