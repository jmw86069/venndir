% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/venndir-render.R
\name{render_venndir}
\alias{render_venndir}
\title{Render Venn or Euler diagram}
\usage{
render_venndir(
  venndir_output = NULL,
  venn_spdf = NULL,
  label_df = NULL,
  asp = 1,
  xlim = NULL,
  ylim = NULL,
  expand_fraction = 0,
  xpd = NA,
  font_cex = 1,
  item_cex = NULL,
  item_cex_factor = 4,
  plot_warning = TRUE,
  show_label = NA,
  show_items = c(NA, "none", "sign item", "item", "sign"),
  item_degrees = 0,
  max_items = 100,
  display_counts = TRUE,
  show_zero = TRUE,
  show_segments = TRUE,
  segment_buffer = -0.05,
  label_preset = c("none"),
  label_style = c("custom", "basic", "fill", "shaded", "shaded_box", "lite", "lite_box"),
  fontfamily = "Arial",
  inside_percent_threshold = 5,
  plot_style = c("base", "gg"),
  item_style = c("text", "gridtext"),
  item_buffer = -0.15,
  group_labels = TRUE,
  ggtheme = ggplot2::theme_void,
  draw_buffer = FALSE,
  ...
)
}
\arguments{
\item{venndir_output}{\code{list} object produced by \code{venndir()} with
at least two elements: \code{"venn_spdf"} which is a
\code{sp::SpatialPolygonsDataFrame}; and \code{label_df} which is
a \code{data.frame}. See argument descriptions below for the
requirements of each object.}

\item{venn_spdf}{\code{sp::SpatialPolygonsDataFrame} that contains
one polygon for each displayed overlap set.
Note: In future this object will be \code{sf::sf} in form of an enhanced
\code{data.frame} with \code{"geometry"} column holding a series of polygon
coordinates. There will be backward compatibility for \code{sp}.
This object is expected to contain colnames:
\itemize{
\item \code{"color"} with fill color;
\item \code{"alpha"} with transparency optionally applied to \code{"color"};
\item \code{"border"} with border color, where \code{NA} draws no border;
\item \code{"lwd"} with line width; and \code{"lty"} with line type. Only
\code{"color"} is required, the others are filled with defaults
as needed.
}}

\item{label_df}{\code{data.frame} that contains these required
colnames:
\itemize{
\item \code{"x"} with x coordinate for each label,
\item \code{"y"} with y coordinate for each label,
\item \code{"text"} with the label to be displayed, in any
format compatible with \code{gridtext::richtext_grob()}.
}

Optional colnames are filled with defaults as needed:
\itemize{
\item \code{"show_label"} a \code{logical} vector for which labels to show \verb{[TRUE]};
\item \code{"show_items"} a \code{logical} indicating whether to show item labels \verb{[FALSE]};
\item \code{"vjust"}, \code{"hjust"} vertical/horizontal position \verb{[0.5, 0.5]};
\item \code{"halign"} with horizontal alignment \verb{[0.5]};
\item \code{"rot"} label rotation \verb{[0]};
\item \code{"color"} label color \verb{[black]};
\item \code{"fontsize"} fontsize used by \code{grid::par()} \verb{[14]};
\item \code{"border"} border color, where \code{NA} draws no border \verb{[NA]};
\item \code{"lty"}, \code{"lwd"} line type and line width \verb{[1, 1]};
\item \code{"fill"} label background fill, where \code{NA} draws no background fill \verb{[NA]};
\item \code{"padding"}, \code{"padding_unit"} passed to \code{gridtext::richtext_grob()}
to define padding around each label \verb{[2, "pt"]};
\item \code{"r"}, \code{"r_unit"} passed to \code{gridtext::richtext_grob()} to define
rounded corners, relevant only when \code{"border"} or \code{"fill"} are
not \code{NA} \verb{[2, "pt"]}.
\item \code{"item_cex"} cex adjustment for item labels
\item \code{"show_label"} \code{logical} indicating whether to show each label
\item \code{"show_items"} \code{logical} indicating whether to show items
}}

\item{asp}{\code{numeric} value indicating the aspect ratio, passed
to \code{plot()}. The default \code{asp=1} ensures that circles are
always drawn with correct aspect ratio so they are displayed
as proper circles.}

\item{xlim, ylim}{\code{numeric} range for x- and y-axis, respectively.
When \code{xlim} or \code{ylim} are \code{NULL}, values are derived from the
coordinates from \code{venn_spdf} and \code{label_df}.}

\item{expand_fraction}{\code{numeric} vector expanded to length 4,
used to expand the \code{xlim} and \code{ylim} ranges, where the vector
corresponds to: bottom, left, top, right.}

\item{xpd}{see \code{graphics::par()}, when \code{xpd=FALSE} it clips text
labels and polygons to the plot boundary; when \code{xpd=TRUE} it
clips to the figure region, and when \code{xpd=NA} it is clipped
to the full device region. This option is mainly helpful
as \code{xpd=NA} ensures labels are displayed even when they overlap
the plot boundary.}

\item{font_cex}{\code{numeric} value used to resize text font used in
Venn overlap labels overall. A value \code{font_cex=1.2} will make
text labels 20\% larger than normal.}

\item{item_cex}{\code{numeric} value used to resize item labels,
used when \code{show_items} is used.
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

\item{item_cex_factor}{\code{numeric} value that controls the extent
of font size auto-scaling based upon the number of items in
each overlap set. This value is multiplied by \code{1 / sqrt(count)},
where larger values for \code{item_cex_factor} will therefore result
in higher \code{item_cex} values with increasing number of labels.}

\item{plot_warning}{\code{logical} indicating whether to draw a text
label on the bottom of the plot whenever a non-zero overlap
count cannot be displayed given the \code{label_df} data. This
occurs when a proportional Venn (Euler) diagram does not
or cannot represent every possible overlap, causing some
overlaps to be hidden from the plot.}

\item{show_items}{\code{character} indicating how to display item labels,
relevant when items are contained in \code{label_df} and when a
relevant label preset is used that includes item display,
for example \code{label_preset="main items"} and \code{show_items="item"}.
The \code{show_items} character string can include \code{"item"} and/or
\code{"sign"} as substrings, for example \code{show_items="sign item"}
or \code{show_items="item sign"} or \code{show_items="sign'"}.}

\item{item_degrees}{\code{numeric} value used to adjust the angle of
item labels within each polygon. It can be useful to adjust
the angle of text as a method of reducing label overlaps.
Another alternative is to pass argument \code{layout_degrees}
through \code{...} to \code{polygon_label_fill()}, which adjusts the
angle of the label points inside each polygon.}

\item{max_items}{\code{numeric} value indicating the maximum number
of item labels to display in each polygon, after which the
only the count summary is displayed. (This argument is not
yet implemented.)}

\item{show_zero}{\code{logical} indicating whether to display \code{0} zero
in overlap regions that contain no items. For some diagrams
it can look cleaner not to display the zero, with
\code{show_zero=FALSE}.}

\item{show_segments}{\code{logical} indicating whether to draw a line
segment from labels outside their respective polygons into
the polygon.}

\item{segment_buffer}{\code{numeric} value indicating the buffer size
inside a polygon when drawing a line segment, when
\code{show_segments=TRUE}. A value \code{segment_buffer=-0.5} will create
an internal polygon buffer half the size it would take to remove
the polygon, and the line segment will be drawn to this border.
A value \code{segment_buffer=-1} will draw the line segment very near
the center of the polygon. A value \code{segment_buffer=0} will
draw the line segment to the border of the polygon, however
in some cases two polygon borders can be near each other, or
overlap, so this setting can be adjusted accordingly.}

\item{label_style}{\code{character} string indicating the style of label
to display, passed to \code{venndir_label_style()}.
The values \verb{"basic","none","shaded","lite","fill"}
style the label background fill, while presence of \code{"box"} in
the string will draw a border around the label:
\code{"basic"} or \code{"none"} uses no background fill,
\code{"lite"} uses lite background fill,
\code{"fill"} uses opaque fill with the overlap set color,
\code{"shaded"} uses slightly transparent fill with overlap set color,
\code{"box"} displays border around the label.}

\item{fontfamily}{\code{character} string indicating the font family,
which can be useful if Unicode characters are displayed as empty
boxes instead of proper Unicode characters. This value may be
device-dependent, for example fonts available for PDF output
may differ from those for PNG.}

\item{inside_percent_threshold}{\code{numeric} value indicating the percent
threshold, below which a polygon label is moved outside the polygon
by default. When a count label is automatically positioned outside
or inside a polygon, this argument defines the threshold for that
decision.
The threshold is calculated by area of the polygon
divided by total area of the enclosing polygon, multiplied by 100.
Therefore \code{inside_percent_threshold=5} will require a polygon to
represent at least 5 percent of the total area.}

\item{plot_style}{\code{character} string indicating the style
of plot: \code{"base"} uses base R graphics; \code{"gg"} uses
ggplot2 graphics (not yet implemented).}

\item{item_buffer}{\code{numeric} value representing a fractional buffer
width inside each polygon applied before placing labels inside
each polygon. This argument is passed to \code{polygon_label_fill()}
as argument \code{scale_width}. The value should be negative, because
the value represents the size relative to the full polygon size,
and negative values make the polygon smaller.}

\item{group_labels}{\code{logical} to enable a relatively new feature that
groups multiple \code{gridtext::richtext_grob()} elements together
by overlap set and position, so there is one border and fill
color for the set of labels. This feature is currently only
implemented for base R plots, and not yet available for
\code{plot_style="gg"} using ggplot2.}

\item{ggtheme}{\code{function} that outputs class \verb{"theme", "gg"},
compatible with output from \code{ggplot2::theme_void()}. This argument
is used to define the ggplot2 theme, when \code{plot_style="gg"}.}

\item{draw_buffer}{\code{logical} indicating whether to draw the \code{item_buffer}
used to determine item label positions inside each polygon,
only relevant when \code{label_preset} includes items, and
\code{show_items} is active.}

\item{...}{additional arguments are passed to \code{plot()}
when plotting \code{venn_spdf} which is expected to be an object
class \code{sf::sf} or \code{sp::SpatialPolygonsDataFrame}.}
}
\description{
Render Venn or Euler diagram
}
\details{
This function is intended to render a Venn or Euler diagram
directly from the output of \code{venndir()} to allow customization
of various aspects of the figure.
}
\examples{
setlist <- make_venn_test(100, 3, do_signed=TRUE);
vo <- venndir(setlist, 1:2, overlap_type="each", do_plot=FALSE);
render_venndir(vo);
render_venndir(vo, plot_style="gg");

vo <- venndir(setlist, 1:2, overlap_type="each", do_plot=FALSE,
   label_preset="main items",
   show_items="sign item");
render_venndir(vo,
   show_items="sign",
   draw_buffer=TRUE, buffer_w=0.2)
   
# turn off line segments, increase item inside buffer width
render_venndir(vo,
   show_segments=FALSE,
   label_preset="main items",
   label_style="lite_box",
   show_items="sign item",
   buffer_w=0.5)
render_venndir(vo, plot_style="gg", show_set="all",
   expand_fraction=c(0, 0.2, 0, 0.2),
   label_preset="main items",
   label_style="lite_box",
   show_items="sign item", buffer_w=0.5)

}
\seealso{
Other venndir core: 
\code{\link{signed_overlaps}()},
\code{\link{textvenn}()},
\code{\link{venn_meme}()},
\code{\link{venndir}()}
}
\concept{venndir core}
