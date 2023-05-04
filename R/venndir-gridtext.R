
#' Custom version of gridtext make_outer_box
#' 
#' Custom version of gridtext make_outer_box
#' 
#' This function is intended to provide support for vectorized `padding`,
#' so that each label can have its own distinct padding on all four sides.
#' 
#' @family venndir utility
#' 
#' @export
gridtext_make_outer_box <- function
(vbox_inner,
 width,
 height, 
 x, 
 y, 
 halign, 
 valign,
 hjust, 
 vjust, 
 rot,
 margin_pt, 
 padding_pt, 
 r_pt, 
 box_gp)
{
   if (is.null(width)) {
      width <- 0
      width_policy <- "native"
   } else {
      if (!length(padding_pt) == 4 &&
            length(padding_pt) == length(width)) {
         # expand by recycling sets of 4
         padding_pt <- rep(padding_pt,
            each=4)
      }
      # ensure finished result has 4 values per width
      padding_pt <- rep(padding_pt,
         length.out=length(width) * 4)
      pad_seq <- seq(from=2, to=length(padding_pt), by=4)
      
      # make space for margin and padding
      # width <- width + margin_pt[2] + margin_pt[4] + padding_pt[2] + padding_pt[4]
      width <- width + margin_pt[2] + margin_pt[4] + padding_pt[pad_seq] + padding_pt[pad_seq + 2]
      width_policy <- "fixed"
   }
   
   if (is.null(height)) {
      height <- 0
      height_policy <- "native"
   } else {
      if (!length(padding_pt) == 4 &&
            length(padding_pt) == length(height)) {
         # expand by recycling sets of 4
         padding_pt <- rep(padding_pt,
            each=4)
      }
      # ensure finished result has 4 values per width
      padding_pt <- rep(padding_pt,
         length.out=length(height) * 4)
      pad_seq <- seq(from=1, to=length(padding_pt), by=4)
      
      # make space for margin and padding
      # height <- height + margin_pt[1] + margin_pt[3] + padding_pt[1] + padding_pt[3]
      height <- height + margin_pt[1] + margin_pt[3] + padding_pt[pad_seq] + padding_pt[pad_seq + 2]
      height_policy <- "fixed"
   }
   
   rect_box <- gridtext:::bl_make_rect_box(
      vbox_inner,
      width,
      height,
      margin_pt,
      padding_pt,
      box_gp,
      content_hjust=halign,
      content_vjust=valign,
      width_policy=width_policy,
      height_policy=height_policy,
      r=r_pt
   )
   vbox_outer <- gridtext:::bl_make_vbox(
      list(rect_box),
      hjust=hjust,
      vjust=vjust,
      width_policy="native")
   
   gridtext:::bl_calc_layout(vbox_outer)
   grobs <- gridtext:::bl_render(vbox_outer)
   
   # calculate corner points
   # (We exclude x, y and keep everything in pt, to avoid unit calculations at this stage)
   # (lower left, lower right, upper left, upper right before rotation)
   theta <- rot*2*pi/360
   width <- gridtext:::bl_box_width(vbox_outer)
   height <- gridtext:::bl_box_height(vbox_outer)
   # lower left
   xll <- -hjust*cos(theta)*width + vjust*sin(theta)*height
   yll <- -hjust*sin(theta)*width - vjust*cos(theta)*height
   # lower right
   xlr <- xll + width*cos(theta)
   ylr <- yll + width*sin(theta)
   # upper left
   xul <- xll - height*sin(theta)
   yul <- yll + height*cos(theta)
   # upper right
   xur <- xul + width*cos(theta)
   yur <- yul + width*sin(theta)
   
   xext <- c(xll, xlr, xul, xur)
   yext <- c(yll, ylr, yul, yur)
   
   grid::gTree(
      x=x,
      y=y,
      xext=xext,
      yext=yext,
      children=grobs,
      vp=grid::viewport(
         x=x,
         y=y,
         just=c(0, 0),
         angle=rot)
   )
}



#' Custom gridtext richtext grob
#' 
#' @family venndir utility
#' 
#' @export
gridtext_richtext_grob <- function
(text,
 x = grid::unit(0.5, "npc"),
 y = grid::unit(0.5, "npc"),
 hjust = 0.5, 
 vjust = 0.5, 
 halign = hjust, 
 valign = vjust,
 rot = 0, 
 default.units = "npc",
 margin = grid::unit(c(0, 0, 0, 0), "pt"), 
 padding = grid::unit(c(0, 0, 0, 0), "pt"),
 r = grid::unit(0, "pt"), 
 align_widths = FALSE, 
 align_heights = FALSE,
 name = NULL, 
 gp = grid::gpar(), 
 box_gp = grid::gpar(col = NA), 
 vp = NULL,
 use_markdown = TRUE, 
 debug = FALSE) 
{
   # make sure x and y are units
   if (!grid::is.unit(x))
      x <- grid::unit(x, default.units)
   if (!grid::is.unit(y))
      y <- grid::unit(y, default.units)
   
   # make sure we can handle input text even if provided as factor
   text <- as.character(text)
   # convert NAs to empty strings
   text <- ifelse(is.na(text), "", text)
   
   # make sure margin and padding are of length 4
   margin <- rep(margin, length.out = 4)
   # padding <- rep(padding, length.out = 4)
   
   # margin, padding, and r need to be in points
   margin_pt <- rep(0, 4)
   margin_pt[c(1, 3)] <- grid::convertHeight(margin[c(1, 3)], "pt", valueOnly=TRUE)
   margin_pt[c(2, 4)] <- grid::convertWidth(margin[c(2, 4)], "pt", valueOnly=TRUE)
   
   # alternate handling of padding
   if (!length(padding) == 4 &&
         length(padding) == length(x)) {
      # expand by recycling sets of 4
      padding <- rep(padding, each=4)
   }
   # ensure finished result has 4 values per width
   padding <- rep(padding,
      length.out=length(x) * 4)
   pad_seq <- seq(from=1, to=length(padding), by=4)
   padding_pt <- rep(0, length(padding))
   padding_pt[pad_seq] <- grid::convertHeight(padding[pad_seq], "pt", valueOnly=TRUE)
   padding_pt[pad_seq + 1] <- grid::convertWidth(padding[pad_seq + 1], "pt", valueOnly=TRUE)
   padding_pt[pad_seq + 2] <- grid::convertHeight(padding[pad_seq + 2], "pt", valueOnly=TRUE)
   padding_pt[pad_seq + 3] <- grid::convertWidth(padding[pad_seq + 3], "pt", valueOnly=TRUE)
   
   padding_list <- split(padding,
      rep(seq_along(x), each=4));
   padding_pt_list <- split(padding_pt,
      rep(seq_along(x), each=4));
   
   # padding_pt <- rep(0, 4)
   # padding_pt[c(1, 3)] <- grid::convertHeight(padding[c(1, 3)], "pt", valueOnly = TRUE)
   # padding_pt[c(2, 4)] <- grid::convertWidth(padding[c(2, 4)], "pt", valueOnly = TRUE)
   
   r_pt <- grid::convertUnit(r, "pt", valueOnly=TRUE)
   
   # make sure text, x, and y have the same length
   n <- unique(length(text), length(x), length(y))
   if (length(n) > 1) {
      stop("Arguments `text`, `x`, and `y` must have the same length.", call. = FALSE)
   }
   gp_list <- gridtext:::recycle_gpar(gp, n)
   box_gp_list <- gridtext:::recycle_gpar(box_gp, n)
   # need to convert x and y to lists so mapply can handle them properly
   x_list <- gridtext:::unit_to_list(x)
   y_list <- gridtext:::unit_to_list(y)
   
   inner_boxes <- mapply(
      # gridtext_make_inner_box,
      gridtext:::make_inner_box,
      text,
      halign,
      valign,
      use_markdown,
      gp_list,
      SIMPLIFY=FALSE
   )
   
   # do we have to align the contents box sizes?
   if (isTRUE(align_widths) || isTRUE(align_heights)) {
      # yes, obtain max width and/or height as needed
      lapply(inner_boxes, gridtext:::bl_calc_layout)
      width <- vapply(inner_boxes, gridtext:::bl_box_width, numeric(1))
      height <-  vapply(inner_boxes, gridtext:::bl_box_height, numeric(1))
   }
   
   if (isTRUE(align_widths)) {
      width <- max(width)
   } else {
      width <- list(NULL)
   }
   if (isTRUE(align_heights)) {
      height <- max(height)
   } else {
      height <- list(NULL)
   }
   
   grobs <- mapply(
      gridtext_make_outer_box,
      inner_boxes,
      width,
      height,
      x_list,
      y_list,
      halign,
      valign,
      hjust,
      vjust,
      rot,
      list(margin_pt),
      # list(padding_pt),
      padding_pt_list,
      r_pt,
      box_gp_list,
      SIMPLIFY=FALSE
   )
   
   if (isTRUE(debug)) {
      ## calculate overall enclosing rectangle
      
      # first get xmax and xmin values for each child grob and overall
      xmax_pt <- vapply(grobs, function(x) {max(x$xext)}, numeric(1))
      xmin_pt <- vapply(grobs, function(x) {min(x$xext)}, numeric(1))
      xmax <- max(x + grid::unit(xmax_pt, "pt"))
      xmin <- min(x + grid::unit(xmin_pt, "pt"))
      
      # now similarly for ymax and ymin
      ymax_pt <- vapply(grobs, function(x) {max(x$yext)}, numeric(1))
      ymin_pt <- vapply(grobs, function(x) {min(x$yext)}, numeric(1))
      ymax <- max(y + grid::unit(ymax_pt, "pt"))
      ymin <- min(y + grid::unit(ymin_pt, "pt"))
      
      # now generate a polygon grob enclosing the entire area
      rect <- grid::polygonGrob(
         x = grid::unit.c(xmin, xmax, xmax, xmin),
         y = grid::unit.c(ymin, ymin, ymax, ymax),
         gp = grid::gpar(fill = "#E1F4FD", col = "#2523C1", lwd = 0.5)
      )
      
      grobs <- c(
         list(rect),
         grobs,
         list(
            grid::pointsGrob(
               x, y, 
               pch = 19, 
               size = grid::unit(5, "pt"),
               gp = grid::gpar(col = "#2523C1"), 
               default.units = default.units
            )
         )
      )
   }
   
   children <- do.call(grid::gList, grobs)
   
   grid::gTree(
      gp = gp,
      vp = vp,
      name = name,
      debug = debug,
      children = children,
      cl = "richtext_grob"
   )
}

