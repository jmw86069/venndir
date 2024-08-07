% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/venndir-to-df.R
\name{venndir_to_df}
\alias{venndir_to_df}
\title{Convert venndir output to data.frame}
\usage{
venndir_to_df(
  venndir_out,
  return_type = c("kable", "data.frame"),
  trim_blanks = TRUE,
  wrap_width = 80,
  colorize_headers = TRUE,
  set_colors = NULL,
  ...
)
}
\arguments{
\item{venndir_out}{\code{list} output from \code{venndir()}}

\item{return_type}{\code{character} string indicating how to format output:
\itemize{
\item \code{"kable"}: returns an object \code{"knitr_kable"} suitable for printing
as HTML or LaTeX. This output is recommended for RMarkdown documents.
\item \code{"data.frame"}: returns a \code{data.frame} object without colorization.
}}

\item{trim_blanks}{\code{logical} whether to remove rows with 0 items.}

\item{wrap_width}{\code{numeric} maximum width for item text before it
is word-wrapped. To disable word-wrapping use \code{wrap_width=Inf}.}

\item{colorize_headers}{\code{logical} indicating whether to color the
background of each header by the \code{set_colors}.}

\item{...}{additional arguments are ignored.}
}
\value{
object of class \code{"data.frame"} or \code{"knitr_kable"} based upon
argument \code{return_type}.
}
\description{
Convert venndir output to data.frame or kable format
for visual review.
}
\details{
\subsection{Todo:}{
\itemize{
\item Accept input from \code{signed_overlaps()} for purely table summary.
}
}
}
\examples{
setlist <- venndir::make_venn_test(100, 3);
venndir_out <- venndir::venndir(setlist, overlap_type="each")
kdf <- venndir_to_df(venndir_out)
kdf

kdf <- venndir_to_df(venndir_out, return_type="data.frame")
kdf

}
\seealso{
Other venndir utility: 
\code{\link{curate_venn_labels}()},
\code{\link{expand_range}()},
\code{\link{get_venn_shapes}()},
\code{\link{make_color_contrast}()},
\code{\link{make_venn_combn_df}()},
\code{\link{make_venn_test}()},
\code{\link{match_list}()},
\code{\link{nudge_venndir_label}()},
\code{\link{print_color_df}()},
\code{\link{shrink_df}()},
\code{\link{three_point_angle}()},
\code{\link{venndir_legender}()}
}
\concept{venndir utility}
