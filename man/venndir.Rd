% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/venndir.R
\name{venndir}
\alias{venndir}
\title{Directional Venn diagram}
\usage{
venndir(
  setlist,
  overlap_type = c("detect", "concordance", "each", "overlap", "agreement"),
  sets = NULL,
  set_colors = NULL,
  proportional = FALSE,
  return_items = FALSE,
  show_set = c("main", "all", "none"),
  show_label = NA,
  show_items = c(NA, "none", "sign item", "sign", "item"),
  max_items = 3000,
  display_counts = TRUE,
  show_zero = FALSE,
  font_cex = c(1, 1, 0.8),
  poly_alpha = 0.8,
  alpha_by_counts = FALSE,
  label_preset = c("none"),
  label_style = c("basic", "fill", "shaded", "shaded_box", "lite", "lite_box"),
  padding = 3,
  r = 2,
  circle_nudge = NULL,
  rotate_degrees = 0,
  unicode = TRUE,
  big.mark = ",",
  sep = "&",
  curate_df = NULL,
  venn_sp = NULL,
  inside_percent_threshold = NULL,
  plot_style = c("base", "gg"),
  item_cex = NULL,
  item_style = c("text", "gridtext"),
  item_buffer = -0.15,
  sign_count_delim = ": ",
  do_plot = TRUE,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{setlist}{\code{list} of named vectors, whose names represent
set items, and whose values represent direction using
values \code{c(-1, 0, 1)}.}

\item{overlap_type}{\code{character} value indicating the type of
overlap logic:
\itemize{
\item \code{"each"} records each combination of signs;
\item \code{"overlap"} disregards the sign and returns any match
item overlap;
\item \code{"concordance"} represents counts for full
agreement, or \code{"mixed"} for any inconsistent overlapping
direction;
\item \code{"agreement"} represents full agreement in direction
as \code{"agreement"}, and \code{"mixed"} for any inconsistent
direction.
}}

\item{sets}{\code{integer} index of sets in \code{setlist} to display in
the Venn diagram. This subset is useful when creating a Venn
diagram for a subset of a list, because it defines consistent
colors across all sets, and uses the appropriate subset of
colors in the Venn diagram.}

\item{set_colors}{\code{character} vector of R colors, or \code{NULL} to
use default colors defined by \code{colorjam::rainbowJam()}.}

\item{proportional}{\code{logical} indicating whether the Venn circles
are proportionally sized, also known as a Euler diagram. Note
that proportionally sized circles are not guaranteed to represent
every possible overlap.}

\item{return_items}{\code{logical} indicating whether to return
the items within each overlap set.}

\item{show_items}{\code{character} indicating the type of item
label: \code{"none"} does not display item labels; \code{"sign"}
displays the sign; \code{"item"} displays the item; \code{"sign item"}
will display the sign and item together; \code{"item sign"} will
reverse the order, with item followed by sign. In this
context \code{"sign"} refers to the incidence values concatenated
by space, sent to \code{curate_venn_labels(..., type="sign")}.}

\item{show_zero}{\code{logical} indicating whether empty overlaps
are labeled with zero \code{0} when \code{show_zero=TRUE}, or are
blank when \code{show_zero=FALSE}.}

\item{font_cex}{\code{numeric} \code{vector} with length up to 3,
to specify the relative font size for: (1) main set label,
(2) the main count label, (3) the directional count label.
The default \code{c(1, 1, 0.8)} defines the main set label
with 1x size, the main count label with 1x size,
and the directional labels with 80\% that size. It is usually
helpful to make the directional count labels slightly smaller
than the main count labels, but probably depends upon the
figure.}

\item{poly_alpha}{\code{numeric} value between 0 and 1, indicating the
alpha transparency of the polygon backgroun color, where
\code{poly_alpha=1} is completely 100\% opaque (no transparency), and
\code{poly_alpha=0.4} is 40\% opaque, therefore 60\% transparency.}

\item{alpha_by_counts}{\code{logical} indicating whether to define
alpha transparency to Venn polygon fill based upon the counts
contained in each polygon.}

\item{label_style}{\code{character} string indicating the style of label
to display:
\code{"basic"} displays text with no background shading or border,
\code{"fill"} displays text on opaque colored background,
\code{"shaded"} displays text on partially transparent colored background,
\code{"lite"} displays text on partially transparent lite background,
\code{"lite_box"} displays text on lite background with border.}

\item{unicode}{\code{logical} passed to \code{curate_venn_labels()}
indicating whether the directional label can include special
Unicode characters.}

\item{big.mark}{\code{character} passed to \code{format()} for numeric labels.}

\item{sep}{\code{character} used as a delimiter between set names,
the default is \code{"&"}.}

\item{curate_df}{\code{data.frame} or \code{NULL} passed to \code{curate_venn_labels()}.}

\item{venn_sp}{\code{NULL} or \code{sp::SpatialPolygons} that contains one
polygon per entry in \code{setlist}. This argument is intended to
allow custom Venn circles to be supplied. When \code{venn_sp} is
\code{NULL}, then \code{get_venn_shapes()} is called.}

\item{inside_percent_threshold}{\code{numeric} value indicating the percent
threshold, below which a polygon label is moved outside the polygon
by default. The threshold is calculated by area of the polygon
divided by total area of the enclosing polygon, multiplied by 100.
Therefore \code{inside_percent_threshold=5} will require a polygon to
represent at least 5 percent of the total area.}

\item{plot_style}{\code{character} indicating the style of graphics plot:
\code{"gg"} uses ggplot2; \code{"base"}
uses base R graphics. This argument is passed to \code{render_venndir()}.}

\item{item_cex}{\code{numeric} value used to resize item labels,
used when \code{show_items} is used, passed to \code{render_venndir()}.
\itemize{
\item When \code{item_cex=NULL} or is a single value, auto-scaling is
performed based upon the number of items in each overlap
polygon, and the relative polygon areas. Any \code{numeric}
value for \code{item_cex} is multiplied by the auto-scaled value.
\item When two or more values are supplied as a vector, the
values are recycled and applied to the number of Venn
overlap polygons, in the order of polygons with \code{type="overlap"}
represented in \code{venndir_output$venv_spdf}, which is also
the order returned by \code{signed_overlaps()}, for those overlaps
represented by a polygon.
}}

\item{item_style}{\code{character} string indicating the style used to display
item labels when they are enabled. The \code{"gridtext"} option is
substantially slower for a large number of labels, but enables
use of markdown. The \code{"text"} option is substantially faster, but
does not allow markdown. Therefore the default is \code{"text"}, and
\code{"gridtext"} is mostly useful for \code{venn_meme()} which usually
only has one or a small number of labels in each polygon.
\itemize{
\item \code{"text"} uses \code{text()} for base R, or \code{geom_text()} for ggplot2.
This option does not allow markdown, but is very fast.
\item \code{"gridtext"} uses \code{gridtext::richtext_grob()} for base R, or
\code{ggtext::geom_richtext()} for ggplot2. This option does allow
markdown, but for many item labels (more than 300) this option
is notably slower, on the order of several seconds to render.
}}

\item{item_buffer}{\code{numeric} value representing a fractional buffer
width inside each polygon applied before placing labels inside
each polygon. This argument is passed to \code{polygon_label_fill()}
as argument \code{scale_width}. The value should be negative, because
the value represents the size relative to the full polygon size,
and negative values make the polygon smaller.}

\item{sign_count_delim}{\code{character} string used as a delimiter between
the sign and counts, when \code{overlap_type} is not \code{"overlap"}.}

\item{...}{additional arguments are passed to \code{render_venndir()}.}
}
\description{
Directional Venn diagram
}
\details{
This function displays a Venn diagram, or when \code{proportional=TRUE}
it displays a Euler diagram, representing counts in each overlap
set.

Input data is supplied either as a \code{list} or \verb{incidence matrix},
or \verb{signed incidence matrix} (whose values indicate direction),
and is passed to \code{signed_overlaps()} to summarize counts by
Venn overlaps.

By default, when input data contains signed direction, the
counts include summary counts for the different forms of
agreement in direction. See \code{signed_overlaps()} for description
of each \code{overlap_type}, for different methods of summarizing
the overlapping directions.
\subsection{Detailed workflow}{

In more detail, input \code{setlist} is named by set names, and each
\code{vector} contains items.

When the \code{vector} elements in \code{setlist} are not named,
the values are considered items. In this case, the values are all
defined as \code{1} for the purpose of defining overlaps, and
\code{overlap_type} is automatically set to \code{overlap_type="overlap"}.
At this point the \code{"sign"} is no longer used.

When the \code{vector} elements in \code{setlist} are named, the
vector element names are considered items, and vector values
are considered the \code{"sign"}.
For common scenarios, the \code{"sign"} is usually one of the
values \code{c(-1, 1)} to indicate \code{"up"} and \code{"down"}. However,
the \code{"sign"} may contain any atomic value, including \code{numeric},
\code{integer}, or \code{character} values for example.

The \code{setlist} data is passed to \code{signed_overlaps()} which
in turn calls \code{list2im_signed()}. At this point the incidence
matrix values represent the values from each \code{vector} in \code{setlist}.

For each item, the \code{"sign"} is defined as the concatenated signs
from each \code{vector} in \code{setlist} for that item. For example
the \code{"sign"} may be \code{"1 1 -1"}, which indicates the item is
present in all three \code{vectors} of \code{setlist}, and is \code{"up"}, \code{"up"},
\code{"down"} in these vectors. The sign \code{"0 1 0"} indicates an
item is present only in the second \code{vector} of \code{setlist} and
is \code{"up"}.

Each item sign is curated by calling \code{curate_venn_labels()}.
This function is used to convert \code{"sign"} to visual symbols,
for example \code{"1"} may be converted to a Unicode up arrow
\code{"\\u2191"}. Unicode output can be disabled with \code{unicode=FALSE}.
The same function converts \code{"sign"} to color, which can be
a helpful visual cue. This step can be customized to use
any output valid in R and recognized by \code{gridtext::richtext_grob()}
or \code{ggtext::geom_richtext()}. Specifically, it can contain
Unicode characters, or limited markdown format recognized
by these functions.
}

\subsection{Display of Venn or Euler circles}{

The overlap counts are used to define suitable Venn circles
or ellipses when \code{proportional=FALSE}, or Euler proportional
circles when \code{proportional=TRUE}. This step is performed
by \code{get_venn_shapes()}.

For Venn circles, the method allows 1, 2, or 3 sets.

For Venn ellipses, the method allows 4 or 5 sets.

For Euler circles, the method allows as many sets as are
supported by \code{eulerr::euler()}.

In the event the circles or ellipses does not include
an overlap, a label is printed below the plot. See
\code{render_venndir()} and the argument \code{plot_warning=TRUE}.
For proportional Euler diagrams, even for 3-way diagrams
there are often missing overlaps, and this warning is
helpful to reinforce what is missing.
}

\subsection{Adjusting Venn or Euler circles}{

As indicated above, when \code{proportional=TRUE} sometimes
the Euler circles do not represent all set overlaps.
It may be helpful to nudge one or more circles to
represent a missing overlap, using the argument
\code{circle_nudge}. This argument takes a \code{list} named
by one or more \code{names(setlist)}, of vectors with \code{c(x, y)}
values to "nudge" that set circle.
}

\subsection{Display of counts}{

By default, total counts are displayed for each set overlap.
When \code{setlist} contains signed data, count signs are summarized
and displayed beside the total counts. The
summary options are defined by \code{overlap_type}.

Count labels can be styled using \code{label_style}, which
customizes the background color fill and optional border.
}

\subsection{Display of items}{

Displaying item labels inside the polygons can be a convenient
way to answer the question, "What are those shared items?"
This step can also include the \code{"sign"}, showing which shared items
also have the same or different \code{"sign"} values.

Note that when items are displayed, summary counts are currently
hidden. In future the counts may be positioned outside the
polygons.
}

\subsection{More customizations}{

This function actually calls \code{render_venndir()} to display the
diagram. The output from this function can be customized and
passed to \code{render_venndir()} or \code{ggrender_venndir()} to allow
much more customized options.
}
}
\examples{
setlist <- make_venn_test(100, 3);
print(setlist);
venndir(setlist)

setlist <- make_venn_test(100, 3, do_signed=TRUE);
venndir(setlist)
venndir(setlist)

venndir(setlist, label_style="basic")
venndir(setlist, label_style="fill")
venndir(setlist, label_style="shaded")
venndir(setlist, label_style="shaded_box")
venndir(setlist, label_style="lite")
venndir(setlist, label_style="lite_box")

# proportional Euler diagram
venndir(setlist,
   proportional=TRUE);

# nudge circles and hide the zero
venndir(setlist,
   proportional=TRUE,
   show_zero=FALSE,
   circle_nudge=list(set_C=c(-0.5, 0.1))
)

# nudge circles so one overlap is no longer shown
venndir(setlist,
   proportional=TRUE,
   show_zero=FALSE,
   circle_nudge=list(set_C=c(-1.4, 0.1))
)

setlist2k <- make_venn_test(2000, 3, 80, do_signed=TRUE);
venndir(setlist2k)
venndir(setlist2k, proportional=TRUE)

# example using character values
setlist <- make_venn_test(100, 3, do_signed=TRUE)
# make a simple character vector list
setlistv <- lapply(setlist, function(i){
   j <- letters[i+3];
   names(j) <- names(i);
   j;
});
# make custom curate_df
curate_df <- data.frame(
   from=c("b", "d"),
   sign=c("b", "d"),
   color=c("blue", "red"),
   stringsAsFactors=FALSE)
vo <- venndir(setlistv,
   overlap_type="each",
   font_cex=c(1.5, 1.5, 0.9), 
   curate_df=curate_df,
   show_zero=TRUE);


}
\seealso{
Other venndir core: 
\code{\link{render_venndir}()},
\code{\link{signed_overlaps}()},
\code{\link{textvenn}()},
\code{\link{venn_meme}()}
}
\concept{venndir core}
