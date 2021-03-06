% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/venndir-curate.R
\name{curate_venn_labels}
\alias{curate_venn_labels}
\title{Curate Venn labels}
\usage{
curate_venn_labels(
  x,
  type = c("sign", "color"),
  curate_df = NULL,
  unicode = TRUE,
  blend_preset = "ryb",
  ...
)
}
\arguments{
\item{x}{\code{vector} of overlap labels.}

\item{type}{\code{character} string where \code{type="sign"} will curate
\code{x} into directional sign, and \code{type="color"} will curate
\code{x} into corresponding directional color.}

\item{curate_df}{\code{data.frame} or \code{NULL} with optional curation
rules. The colnames are expected to contain \code{c("from", "sign", "color")}.
In this case, each row of \code{curate_df} is applied in order,
and \code{gsub()} is called on each \code{"from"} and replaced using
the value in column given by \code{type}.}

\item{unicode}{\code{logical} indicating whether to use Unicode characters
when \code{type="sign"}. Note this argument only affects the default
values, it is not applied when using a custom \code{curate_df}.}

\item{blend_preset}{\code{character} string passed as \code{preset} to
\code{colorjam::blend_colors()} to define the color wheel used
during color blending operations.}

\item{...}{additional arguments are ignored.}
}
\value{
\code{vector} of labels or colors, based upon argument \code{type}.
}
\description{
Curate Venn labels
}
\details{
This venndir utility function is used to convert a basic
directional label such as \code{"0 1 0 -1"} suitable for display.
It can output either Unicode or non-Unicode text label,
or a corresponding color.

The input currently recognizes directional labels such as
\code{"0 -1 1 0"} or the character labels \code{"agreement"}, \code{"concordant"},
\code{"mixed"}. Note that zeros \code{"0"} are typically removed
before calling this function.
}
\examples{
options("warn"=-1); # make them stop

venn_labels <- c("0 1 0 -1", "1 -1", "1 1 1", "mixed", "agreement", "1 1 0 0");
(curate_venn_labels(venn_labels, "sign"))
(curate_venn_labels(venn_labels, "sign", unicode=FALSE))

(curate_venn_labels(venn_labels, "color"))

jamba::printDebug(as.list(curate_venn_labels(venn_labels, "sign")),
   collapse=", ",
   fgText=as.list(curate_venn_labels(venn_labels, "color")))

}
\seealso{
Other venndir utility: 
\code{\link{eulerr2polys}()},
\code{\link{expand_range}()},
\code{\link{find_vennpoly_overlaps}()},
\code{\link{get_venn_shapes}()},
\code{\link{make_color_contrast}()},
\code{\link{make_venn_combn_df}()},
\code{\link{make_venn_test}()},
\code{\link{match_list}()},
\code{\link{nudge_venndir_label}()},
\code{\link{print_color_df}()},
\code{\link{rescale_ps}()},
\code{\link{rescale_p}()},
\code{\link{shrink_df}()},
\code{\link{sp_percent_area}()},
\code{\link{venndir_label_style}()}
}
\concept{venndir utility}
