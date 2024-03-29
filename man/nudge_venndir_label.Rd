% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/venndir-render.R
\name{nudge_venndir_label}
\alias{nudge_venndir_label}
\title{Nudge venndir label}
\usage{
nudge_venndir_label(
  venndir_output,
  set = NULL,
  x_offset = 0,
  y_offset = 0,
  unit_type = c("absolute", "relative"),
  label_types = c("main", "signed"),
  show_label = TRUE,
  ...
)
}
\description{
Nudge venndir label
}
\examples{
setlist <- make_venn_test(100, 3, do_signed=TRUE)

vo <- venndir(setlist,
   overlap_type="each",
   return_items=TRUE,
   label_style="lite_box",
   main="Default venndir")

vo <- nudge_venndir_label(vo,
   set=c("set_A&set_B&set_C"),
   x_offset=3,
   y_offset=-1)
render_venndir(vo,
   main="After nudging one count label")

}
\seealso{
Other venndir utility: 
\code{\link{curate_venn_labels}()},
\code{\link{eulerr2polys}()},
\code{\link{expand_range}()},
\code{\link{find_vennpoly_overlaps}()},
\code{\link{get_venn_shapes}()},
\code{\link{gridtext_make_outer_box}()},
\code{\link{gridtext_richtext_grob}()},
\code{\link{make_color_contrast}()},
\code{\link{make_venn_combn_df}()},
\code{\link{make_venn_test}()},
\code{\link{match_list}()},
\code{\link{print_color_df}()},
\code{\link{rescale_ps}()},
\code{\link{rescale_p}()},
\code{\link{shrink_df}()},
\code{\link{sp_percent_area}()},
\code{\link{venndir_label_style}()},
\code{\link{venndir_legender}()}
}
\concept{venndir utility}
