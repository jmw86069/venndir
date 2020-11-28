#
# venndir-plotly.R
#

#' Basic function for GeomRichText compatibility with ggplotly
#' 
#' Basic function for GeomRichText compatibility with ggplotly
#' 
#' Note that this function is still being tested, but is intended
#' to be used with ggplot2 output from `render_venndir()` so
#' it can be used with `plotly::ggplotly()`.
#' 
#' @family venndir plotly
#' 
#' @param data,prestats_data,layout,params,p arguments used
#'    by the ggplot2 infrastructure.
#' @param ... additional arguments are passed along
#' 
#' @export
to_basic.GeomRichText <- function
(data, prestats_data, layout, params, p, ...)
{
   plotly:::prefix_class(data, "GeomText");
}


#' Basic function for GeomTextBox compatibility with ggplotly
#' 
#' Basic function for GeomTextBox compatibility with ggplotly
#' 
#' Note that this function is still being tested, but is intended
#' to be used with ggplot2 output from `render_venndir()` so
#' it can be used with `plotly::ggplotly()`.
#' 
#' @family venndir plotly
#' 
#' @param data,prestats_data,layout,params,p arguments used
#'    by the ggplot2 infrastructure.
#' @param ... additional arguments are passed along
#' 
#' @export
to_basic.GeomTextBox <- function
(data, prestats_data, layout, params, p, ...)
{
   plotly:::prefix_class(data, "GeomText");
}

