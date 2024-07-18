
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

gt_text_info_cache <- new.env(parent=emptyenv())
gt_font_info_cache <- new.env(parent=emptyenv())

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

   ## gridtext custom functions
   gt_recycle_gpar <- function(gp=NULL, n=1) {
      make_gpar <- function(n, ...) {
         structure(list(...), class="gpar")
      }
      args <- c(list(make_gpar, n=seq_len(n)), gp, list(SIMPLIFY=FALSE))
      do.call(mapply, args)
   }
   gt_unit_to_list <- function(u) {
      lapply(seq_along(u), function(i) u[i])
   }
   gt_update_gpar <- function(gp, gp_new) {
      names_new <- names(gp_new)
      names_old <- names(gp)
      gp[c(intersect(names_old, names_new), "fontface")] <- NULL
      gp_new["fontface"] <- NULL
      do.call(grid::gpar, c(gp, gp_new))
   }
   gt_text_info <- function (label, fontkey, fontfamily, font, fontsize, cache) 
   {
      key <- paste0(label, fontkey)
      info <- gt_text_info_cache[[key]]
      if (is.null(info)) {
         ascent_pt <- grid::convertHeight(
            grid::grobHeight(
               grid::textGrob(label=label,
                  gp=grid::gpar(fontsize=fontsize,
                     fontfamily=fontfamily,
                     font=font,
                     cex=1))), "pt", valueOnly=TRUE);
         width_pt <- grid::convertWidth(
            grid::grobWidth(
               grid::textGrob(label=label,
                  gp=grid::gpar(fontsize=fontsize,
                     fontfamily=fontfamily,
                     font=font,
                     cex=1))), "pt", valueOnly=TRUE);
         info <- list(width_pt=width_pt, ascent_pt=ascent_pt)
         if (cache) {
            gt_text_info_cache[[key]] <- info
         }
      }
      info
   }
   gt_font_info <- function (fontkey, fontfamily, font, fontsize, cache) {
      info <- gt_font_info_cache[[fontkey]]
      if (is.null(info)) {
         descent_pt <- grid::convertHeight(
            grid::grobDescent(
               grid::textGrob(label="gjpqyQ",
                  gp=grid::gpar(fontsize=fontsize,
                     fontfamily=fontfamily,
                     font=font,
                     cex=1))), "pt", valueOnly=TRUE);
         space_pt <- grid::convertWidth(
            grid::grobWidth(
               grid::textGrob(label=" ",
                  gp=grid::gpar(fontsize=fontsize,
                     fontfamily=fontfamily,
                     font=font,
                     cex=1))), "pt", valueOnly=TRUE);
         info <- list(descent_pt=descent_pt, space_pt=space_pt)
         if (cache) {
            gt_font_info_cache[[fontkey]] <- info
         }
      }
      info
   }
   gt_text_details <- function (label, gp=grid::gpar()) {
      fontfamily <- gp$fontfamily
      if (is.null(gp$fontfamily)) {
         fontfamily <- grid::get.gpar("fontfamily")$fontfamily
      }
      font <- gp$font
      if (is.null(gp$font)) {
         font <- grid::get.gpar("font")$font
      }
      fontsize <- gp$fontsize
      if (is.null(gp$fontsize)) {
         fontsize <- grid::get.gpar("fontsize")$fontsize
      }
      devname <- names(grDevices::dev.cur())
      fontkey <- paste0(devname, fontfamily, font, fontsize)
      if (devname == "null device") {
         cache <- FALSE
      } else {
         cache <- TRUE
      }
      if (length(fontkey) != 1 || length(label) != 1) {
         stop("Function `text_details()` is not vectorized.", call.=FALSE)
      }
      l1 <- gt_text_info(label, fontkey, fontfamily, font, fontsize, cache)
      l2 <- gt_font_info(fontkey, fontfamily, font, fontsize, cache)
      c(l1, l2)
   }
   gt_update_context <- function (drawing_context, ...) {
      dc_new <- list(...)
      names_new <- names(dc_new)
      names_old <- names(drawing_context)
      drawing_context[intersect(names_old, names_new)] <- NULL
      c(drawing_context, dc_new)
   }
   gt_set_context_gp <- function(drawing_context, gp=NULL) {
      gp <- gt_update_gpar(drawing_context$gp, gp)
      font_info <- gt_text_details("", gp)
      linespacing_pt <- gp$lineheight * gp$fontsize
      em_pt <- gp$fontsize
      gt_update_context(drawing_context,
         gp=gp,
         ascent_pt=font_info$ascent_pt,
         descent_pt=font_info$descent_pt,
         linespacing_pt=linespacing_pt,
         em_pt=em_pt)
   }
   gt_setup_context <- function(fontsize=12, fontfamily="", fontface="plain",
      color="black", lineheight=1.2, halign=0, word_wrap=TRUE, gp=NULL) {
      if (is.null(gp)) {
         gp <- grid::gpar(fontsize=fontsize,
            fontfamily=fontfamily, 
            fontface=fontface,
            col=color,
            cex=1,
            lineheight=lineheight)
      }
      gp <- gt_update_gpar(grid::get.gpar(), gp)
      gt_set_context_gp(
         list(yoff_pt=0,
            halign=halign,
            word_wrap=word_wrap),
         gp)
   }
   ## Note: bl_make_text_box cannot be reproduced because it uses .Call()
   gt_process_text <- function (node, drawing_context) {
      tokens <- stringr::str_split(stringr::str_squish(node),
         "[[:space:]]+")[[1]]
      boxes <- lapply(tokens, function(token) {
         list(
            gridtext:::bl_make_text_box(token,
               drawing_context$gp,
               drawing_context$yoff_pt), 
            gridtext:::bl_make_regular_space_glue(drawing_context$gp))
      })
      if (isTRUE(grepl("^[[:space:]]", node))) {
         boxes <- c(list(
            gridtext:::bl_make_regular_space_glue(drawing_context$gp)),
            boxes)
      }
      boxes <- unlist(boxes, recursive=FALSE)
      if (!isTRUE(grepl("[[:space:]]$", node))) {
         boxes[[length(boxes)]] <- NULL
      }
      boxes
   }
   gt_dispatch_tag <- function (node, tag, drawing_context) {
      if (is.null(tag) || tag == "") {
         gt_process_text(node, drawing_context)
      } else {
         switch(tag,
            b=gridtext:::process_tag_b(node, drawing_context),
            strong=gridtext:::process_tag_b(node, drawing_context),
            br=gridtext:::process_tag_br(node, drawing_context),
            i=gridtext:::process_tag_i(node, drawing_context),
            img=gridtext:::process_tag_img(node, drawing_context),
            em=gridtext:::process_tag_i(node, drawing_context),
            p=gridtext:::process_tag_p(node, drawing_context),
            span=gridtext:::process_tag_span(node, drawing_context),
            sup=gridtext:::process_tag_sup(node, drawing_context),
            sub=gridtext:::process_tag_sub(node, drawing_context),
            stop(paste0("gridtext has encountered a tag that isn't",
               " supported yet: <",
               tag, ">\n",
               "Only a very limited number of tags are currently supported."),
               call.=FALSE))
      }
   }
   gt_process_tags <- function (node, drawing_context) {
      tags <- names(node)
      boxes <- list()
      for (i in seq_along(node)) {
         boxes[[i]] <- gt_dispatch_tag(node[[i]], tags[i], drawing_context)
      }
      unlist(boxes, recursive=FALSE)
   }
   ## Note: bl_make_vbox cannot be reproduced because it uses .Call()
   gt_make_inner_box <- function(text, halign, valign, use_markdown, gp) {
      if (use_markdown) {
         text <- markdown::markdownToHTML(text=text,
            options=c("use_xhtml", "fragment_only"))
      }
      doctree <- xml2::read_html(paste0("<!DOCTYPE html>", text))
      drawing_context <- gt_setup_context(gp=gp,
         halign=halign,
         word_wrap=FALSE)
      boxlist <- gt_process_tags(xml2::as_list(doctree)$html$body, 
         drawing_context)
      vbox_inner <- gridtext:::bl_make_vbox(boxlist,
         vjust=0,
         width_policy="native")
      vbox_inner
   }
   
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
   padding_pt[pad_seq] <- grid::convertHeight(padding[pad_seq],
      "pt",
      valueOnly=TRUE)
   padding_pt[pad_seq + 1] <- grid::convertWidth(padding[pad_seq + 1],
      "pt",
      valueOnly=TRUE)
   padding_pt[pad_seq + 2] <- grid::convertHeight(padding[pad_seq + 2],
      "pt",
      valueOnly=TRUE)
   padding_pt[pad_seq + 3] <- grid::convertWidth(padding[pad_seq + 3],
      "pt",
      valueOnly=TRUE)
   
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
      stop("Arguments `text`, `x`, and `y` must have the same length.",
         call.=FALSE)
   }
   
   gp_list <- gt_recycle_gpar(gp, n)
   box_gp_list <- gt_recycle_gpar(box_gp, n)
   
   # need to convert x and y to lists so mapply can handle them properly
   x_list <- gt_unit_to_list(x)
   y_list <- gt_unit_to_list(y)
   
   inner_boxes <- mapply(
      gt_make_inner_box,
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

