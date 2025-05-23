% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/venndir-conversion.R
\name{signed_counts2setlist}
\alias{signed_counts2setlist}
\title{venndir conversion from signed overlap counts to setlist}
\usage{
signed_counts2setlist(x, sep = "&", value_sep = "_", ...)
}
\value{
\code{list} where the list names are the names of each set,
and values of each list element is a vector of items.
The items are artificial labels used for convenience.
}
\description{
venndir conversion from signed overlap counts to setlist
}
\details{
This function takes input in the form of a \code{list}
whose names are set overlap labels, for example \code{"set_A"},
or \code{"set_A&set_B"}.

Each list element is an \code{integer} vector whose names
are value overlaps, for example \code{"1"}, or \code{"-1_1"},
and whose \code{integer} values contain the overlap counts.
}
\examples{
x <- list(
   "set_A"=c(
      "1"=80,
      "-1"=95
   ),
   "set_B"=c(
      "1"=15,
      "-1"=30
   ),
   "set_A&set_B"=c(
      "1_1"=100,
      "-1_-1"=125,
      "1_-1"=3,
      "-1_1"=4
   )
)
setlist <- signed_counts2setlist(x)
# default Venn diagram
vo <- venndir(setlist, overlap_type="each")
# show counts,percent outside, and sign for each item inside
venndir(setlist,
   overlap_type="each",
   show_labels="NCSPi",
   show_items="sign",
   item_degrees=10)

# show counts inside without set label (which is in the legend)
vo <- venndir(setlist,
   show_labels="cs",
   proportional=TRUE,
   label_style="fill_box")

# demonstrate interconversion
setlist2 <- make_venn_test(100, 3, do_signed=TRUE)
vo <- venndir(setlist2, do_plot=FALSE, overlap_type="each")
venndir(signed_counts2setlist(signed_counts(vo)), overlap_type="each")

}
\seealso{
Other venndir conversion: 
\code{\link{collapse_im}()},
\code{\link{counts2setlist}()},
\code{\link{im2list}()},
\code{\link{im_value2list}()},
\code{\link{list2im_opt}()},
\code{\link{list2im_value}()},
\code{\link{overlaplist2setlist}()}
}
\concept{venndir conversion}
