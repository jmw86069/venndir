
# grid grob assembly
# - goal is to take two grobs, align them relative to each other
#
# - grobs_stack         - assemble top to bottom
# - grobs_xalign       - align left/center/right
# - grobs_yalign       - align top/center/bottom
#
# - consider defining a reference grob, all others move toward it
# - consider option to create a group to be aligned as a set
#
# - input grobs
#    - list of grobs
#    - list of grobs, gTrees

## Key lessons learned:
# - xext,yext are not consistently populated to all underlying gTree, grobs
#    - xext,yext seem to be specific to gridtext grobs
#      See `heightDetails.richtext_grob()` in gridtext `richtext-grob.R`
#      The yext is added to y to determine the actual coordinates
# - x,y are not consistently populated

#' Get grid grobs x- and y-extents
#' 
#' @family grobs
#' @keywords internal
#'
#' @param grobs one of:
#'    * `list` of grobs
#'    * `gList`
#'    * `gTree`
#' @param ... additional arguments are ignored.
#' 
#' @returns `list` with elements `xext` and `yext`, each
#'    of which are `list` with min and max values encoded as
#'    `grid::unit` objects.
#'
grobs_exts <- function
(grobs,
 verbose=FALSE,
 ...)
{
   #
   # potential wrapper function to help nested gTree
   get_grob_ext <- function
   (gcgrob,
      col="x",
      FUN=min,
      verbose=FALSE,
      indent=0,
      ...)
   {
      #
      extcol <- paste0(col, "ext");
      if (!all(c("x", "xext") %in% names(gcgrob)) &&
            inherits(gcgrob, "gTree")) {
         if (verbose) {
            jamba::printDebug("get_grob_ext(): ", "nested gTree query", indent=indent);
         }
         gcn <- grid::childNames(gcgrob);
         isums <- lapply(gcn, function(igcn){
            igcgrob <- grid::getGrob(gcgrob, igcn);
            isum <- get_grob_ext(igcgrob, col=col, FUN=FUN, verbose=verbose, indent=indent + 5)
         })
         isum <- do.call(FUN, isums);
         return(isum);
      }
      isum <- grid::unit(FUN(gcgrob[[extcol]]), "pt") + FUN(gcgrob[[col]])
      if (verbose) {
         jamba::printDebug(extcol, " isum:", indent=indent);print(isum);
      }
      isum
   }
   
   # logic follows the type of input
   if (inherits(grobs, "gTree")) {
      # grobs <- g_labels_gTree
      ## First check if the overall gTree has the xext,x data
      if (all(c("x", "xext") %in% names(grobs))) {
         use_exts <- grobs_exts(list(grobs), ...)
         return(use_exts);
      }
      
      ## Otherwise iterate each child grob or gTreee
      gcs <- grid::childNames(grobs);
      xmax <- do.call(max, lapply(gcs, function(gc){
         gcgrob <- grid::getGrob(grobs, gc)
         isum <- get_grob_ext(gcgrob=gcgrob, col="x", FUN=max, verbose=verbose);
         if (verbose) {
            jamba::printDebug("xmax isum (", gc, "):");print(isum);
            # jamba::printDebug("max(gcgrob$xext) (", gc, "):");print(max(gcgrob$xext));
            # jamba::printDebug("max(gcgrob$x) (", gc, "):");print(max(gcgrob$x));
            # jamba::printDebug("names(gcgrob) (", gc, "):");print(names(gcgrob));
            # jamba::printDebug("class(gcgrob) (", gc, "):");print(class(gcgrob));
         }
         # isum <- grid::unit(max(gcgrob$xext), "pt") + max(gcgrob$x)
         isum
      }))
      xmin <- do.call(min, lapply(gcs, function(gc){
         gcgrob <- grid::getGrob(grobs, gc)
         isum <- get_grob_ext(gcgrob=gcgrob, col="x", FUN=min, verbose=verbose);
         # grid::unit(min(gcgrob$xext), "pt") + min(gcgrob$x)
         isum
      }))
      xext <- list(xmin, xmax);
      ymax <- do.call(max, lapply(gcs, function(gc){
         gcgrob <- grid::getGrob(grobs, gc)
         isum <- get_grob_ext(gcgrob=gcgrob, col="y", FUN=max, verbose=verbose);
         # isum <- grid::unit(max(gcgrob$yext), "pt") + max(gcgrob$y)
         if (verbose) {
            jamba::printDebug("ymax isum (", gc, "):");print(isum);
         }
         isum
      }))
      ymin <- do.call(min, lapply(gcs, function(gc){
         gcgrob <- grid::getGrob(grobs, gc)
         isum <- get_grob_ext(gcgrob=gcgrob, col="y", FUN=min, verbose=verbose);
         # grid::unit(min(gcgrob$yext), "pt") + min(gcgrob$y)
         isum
      }))
      yext <- list(ymin, ymax);
   } else if (any(c("gList", "list") %in% class(grobs)) ||
         inherits(grobs, "gList")) {
      # grobs <- g_labels_gTree
      # grobs <- lapply(gcs[1:2], function(gc){grid::getGrob(grobs, gc)})
      gcs <- seq_along(grobs);
      xmax <- do.call(max, (lapply(jamba::nameVector(gcs), function(gc){
         # isum <- sum(grid::unit(max(grobs[[gc]]$xext), "pt"), max(grobs[[gc]]$x))
         gcgrob <- grobs[[gc]];
         isum <- get_grob_ext(gcgrob=gcgrob, col="x", FUN=max, verbose=verbose);
         if (verbose) {
            jamba::printDebug("xmax isum (", gc, "):");print(isum);
         }
         isum
      })))
      xmin <- do.call(min, (lapply(jamba::nameVector(gcs), function(gc){
         # sum(grid::unit(min(grobs[[gc]]$xext), "pt"), min(grobs[[gc]]$x))
         gcgrob <- grobs[[gc]];
         isum <- get_grob_ext(gcgrob=gcgrob, col="x", FUN=min, verbose=verbose);
         isum;
      })))
      xext <- list(xmin, xmax);
      ymax <- do.call(max, (lapply(jamba::nameVector(gcs), function(gc){
         # isum <- sum(grid::unit(max(grobs[[gc]]$yext), "pt"), max(grobs[[gc]]$y))
         gcgrob <- grobs[[gc]];
         isum <- get_grob_ext(gcgrob=gcgrob, col="y", FUN=max, verbose=verbose);
         if (verbose) {
            jamba::printDebug("ymax isum (", gc, "):");print(isum);
         }
         isum
      })))
      ymin <- do.call(min, (lapply(jamba::nameVector(gcs), function(gc){
         # sum(grid::unit(min(grobs[[gc]]$yext), "pt"), min(grobs[[gc]]$y))
         gcgrob <- grobs[[gc]];
         isum <- get_grob_ext(gcgrob=gcgrob, col="y", FUN=min, verbose=verbose);
         isum;
      })))
      yext <- list(ymin, ymax);
   } else if (inherits(grobs, "grob") && inherits(grobs, "points")) {
      ## pointsGrob - no xext,yext but should be possible to create valid data
      xmin <- grobs$x;
      xmax <- grobs$x;
      xext <- list(xmin, xmax);
      ymin <- grobs$y;
      ymax <- grobs$y;
      yext <- list(ymin, ymax);
   } else if (inherits(grobs, "grob") &&
         inherits(grobs, c("roundrect", "rect"))) {
      ## Note: grid.force() converts to polygonGrob with new viewport
      ## and the viewport makes things challenging. In theory it would
      ## need to convert inner viewport coordinates to enclosing viewport.
      ## Nah.
      # grid::grid.force(grobs);
      xmin <- grobs$x - grobs$width / 2;
      xmax <- grobs$x + grobs$width / 2;
      xext <- list(xmin, xmax);
      ymin <- grobs$y - grobs$height / 2;
      ymax <- grobs$y + grobs$height / 2;
      yext <- list(ymin, ymax);
   } else {
      stop(paste0("Input object class [",
         jamba::cPaste(class(grobs)),
         "] not recognized."));
   }
   return(list(xext=xext, yext=yext));
}


#' Stack grobs top to bottom
#'
#' Stack grobs top to bottom
#' 
#' This approach uses proper xext,yext min and max values from `grobs_exts()`
#' 
#' @family grobs
#' @keywords internal
#' 
#' @param grobs one of the following forms of `grid` graphical objects:
#'    * `list` containing grid grobs, or `gTree` objects.
#'    * single `gTree` objects, which is broken into a `list` of components
#'    * `gList` containing grid grobs or `gTree` objects.
#' @param ref_grob `integer` intended to define the reference object,
#'    and refers to the object in order they are defined in the `gTree`.
#' @param use_y `unit` object, experimental, used instead of `ref_grob`
#'    to define the starting position.
#' @param verbose `logical` indicating whether to print verbose output
#' @param debug `logical` with experimental debug output
#' @param ... additional arguments are ignored.
#'
grobs_stack <- function
(grobs,
 name=NULL,
 ref_grob=1,
 use_y=NULL,
 verbose=FALSE,
 debug=FALSE,
 ...)
{
   #
   ## Create gTree for the output
   if (inherits(grobs, "gList")) {
      new_gTree <- grid::grobTree(grobs,
         name=name)
   } else if (inherits(grobs, "gTree")) {
      new_gTree <- grobs;
   } else {
      new_gTree <- grid::grobTree(do.call(grid::gList, grobs),
         name=name)
   }
   
   ## childNames
   cns <- grid::childNames(new_gTree);
   
   ## Start with top y extent of the first object
   grob1 <- grid::getGrob(new_gTree, cns[[ref_grob]]);
   grob1_exts <- grobs_exts(grob1);
   if (length(use_y) == 0) {
      use_y <- grob1_exts$yext[[2]];
   }
   if (verbose) {
      jamba::printDebug("use_y:");
      print(use_y);
   }
   
   # iterate each childName
   for (i in cns) {
      if (verbose) {
         jamba::printDebug("iterating child: ", i);
         jamba::printDebug("use_y:");
         print(use_y);
      }
      igrob <- grid::getGrob(new_gTree, i);
      igrob_exts <- grobs_exts(igrob);
      # igrob_ymax <- grid::unit(max(igrob$yext), "pt")
      igrob_ymax <- igrob_exts$yext[[2]];
      if (verbose) {
         jamba::printDebug("igrob_ymax: ");
         print(igrob_ymax);
      }
      
      if (identical(use_y, igrob_ymax)) {
         y_diff <- 0;
         # if they are already the same, skip it
         igrobh <- igrob_exts$yext[[2]] - igrob_exts$yext[[1]];
         use_y <- use_y - igrobh;
         next;
      }
      y_diff <- (use_y - igrob_ymax);
      if (verbose) {
         jamba::printDebug("y_diff: ");
         print(y_diff);
      }
      # adjust y coordinate
      # igps <- setdiff(grid::grid.ls(igrob, print=FALSE)$name, i)
      igps <- grid::grid.ls(igrob, print=FALSE)$name
      # iterate each component of a gTree
      for (igp in igps) {
         jgrob <- grid::getGrob(igrob, igp)
         if (!"x" %in% names(jgrob)) {
            next
         }
         if (verbose) {
            jamba::printDebug("jgrob$y (before): ", indent=5);
            print(jgrob$y);
         }
         new_gTree <- grid::editGrob(new_gTree,
            gPath=igp,
            # global=TRUE,
            y=jgrob$y + y_diff)
         if (verbose) {
            jamba::printDebug("jgrob$y (after): ", indent=5);
            print(jgrob$y);
         }
         new_gTree
      }
      # igrobh <- grid::unit(diff(range(igrob$yext)), "pt")
      igrobh <- igrob_exts$yext[[2]] - igrob_exts$yext[[1]];
      use_y <- use_y - igrobh;
   }
   new_gTree
}


#' Tile grobs left to right
#' 
#' @family grobs
#' @keywords internal
#' 
#' @param grobs one of the following forms of `grid` graphical objects:
#'    * `list` containing grid grobs, or `gTree` objects.
#'    * single `gTree` objects, which is broken into a `list` of components
#'    * `gList` containing grid grobs or `gTree` objects.
#' @param ref_grob `integer` intended to define the reference object,
#'    and refers to the object in order they are defined in the `gTree`.
#' @param use_x `unit` object, experimental, used instead of `ref_grob`
#'    to define the starting position.
#' @param verbose `logical` indicating whether to print verbose output
#' @param debug `logical` with experimental debug output
#' @param ... additional arguments are ignored.
#' 
#' 
grobs_tile <- function
(grobs,
 name=NULL,
 ref_grob=1,
 use_x=NULL,
 verbose=FALSE,
 debug=FALSE,
 ...)
{
   #
   ## Create gTree for the output
   if (inherits(grobs, "gList")) {
      new_gTree <- grid::grobTree(grobs,
         name=name)
   } else if (inherits(grobs, "gTree")) {
      new_gTree <- grobs;
   } else {
      new_gTree <- grid::grobTree(do.call(grid::gList, grobs),
         name=name)
   }
   
   ## childNames
   cns <- grid::childNames(new_gTree);
   
   ## Start with left x extent of the first object
   grob1 <- grid::getGrob(new_gTree, cns[[ref_grob]]);
   grob1_exts <- grobs_exts(grob1);
   if (length(use_x) == 0) {
      use_x <- grob1_exts$xext[[1]];
   }
   ## starting y coordinate (OLD method)
   # y_init <- max(grid::getGrob(new_gTree, grid::childNames(new_gTree)[1])$yext)
   # use_y <- grid::unit(y_init, "pt")
   
   
   # iterate each childName
   cns <- grid::childNames(new_gTree);
   for (i in cns) {
      if (verbose) {
         jamba::printDebug("iterating child: ", i);
      }
      igrob <- grid::getGrob(new_gTree, i);
      igrob_exts <- grobs_exts(igrob);
      igrob_xmin <- igrob_exts$xext[[1]];
      if (verbose) {
         jamba::printDebug("igrob_xmin: ");
         print(igrob_xmin);
      }
      
      x_diff <- (use_x - igrob_xmin);
      if (identical(use_x, igrob_xmin)) {
         x_diff <- 0;
      }
      if (verbose) {
         jamba::printDebug("x_diff: ");
         print(x_diff);
      }
      if (identical(use_x, igrob_xmin)) {
         # if they are already the same, skip it
         # igrobh <- igrob_exts$yext[[2]] - igrob_exts$yext[[1]];
         igrobw <- igrob_exts$xext[[2]] - igrob_exts$xext[[1]];
         use_x <- use_x + igrobw;
         next;
      }
      
      # adjust y coordinate
      igps <- setdiff(grid::grid.ls(igrob, print=FALSE)$name, i)
      # iterate each component of a gTree
      for (igp in igps) {
         jgrob <- grid::getGrob(igrob, igp)
         if (!"x" %in% names(jgrob)) {
            next
         }
         if (verbose) {
            jamba::printDebug("jgrob$x (before): ", indent=5);
            print(jgrob$x);
         }
         new_gTree <- grid::editGrob(new_gTree,
            gPath=igp,
            # global=TRUE,
            x=jgrob$x + x_diff)
         if (verbose) {
            jamba::printDebug("jgrob$x (after): ", indent=5);
            print(jgrob$x);
         }
         new_gTree
      }
      igrobw <- igrob_exts$xext[[2]] - igrob_exts$xext[[1]];
      use_x <- use_x + igrobw;
   }
   new_gTree
}


#' Left-align grobs
#'
#' @family grobs
#' @keywords internal
#' 
#' @param grobs
#' @xalign `character` string indicating the type of alignment:
#'    * `"left"` - objects are aligned at the left edge
#'    * `"center"` - objects are aligned at the object center
#'    * `"right"` - objects are aligned at the right edge
#'    * `"tile"` - objects are aligned with the left edge
#'    touching the right edge of the previous object (experimental)
#'    * `"none"`
#' 
grobs_xalign <- function
(grobs,
 name=NULL,
 xalign=c("left",
    "center",
    "right",
    "tile",
    "none"),
 ref_grob=1,
 use_x=NULL,
 verbose=FALSE,
 debug=FALSE,
 ...)
{
   #
   xalign <- match.arg(xalign)
   
   ## Create gTree for the output
   if (inherits(grobs, "gList")) {
      new_gTree <- grid::grobTree(grobs,
         name=name)
   } else if (inherits(grobs, "gTree")) {
      new_gTree <- grobs;
   } else {
      new_gTree <- grid::grobTree(do.call(grid::gList, grobs),
         name=name)
   }
   
   # childNames
   cns <- grid::childNames(new_gTree);
   
   ## define x reference coordinate for alignment
   grob1 <- grid::getGrob(new_gTree, cns[[ref_grob]]);
   grob1_exts <- grobs_exts(grob1);
   xext_init <- grob1_exts$xext;
   # xext_init <- (grid::getGrob(new_gTree, grid::childNames(new_gTree)[1])$xext)
   if (length(use_x) > 0) {
      x_ref <- use_x;
      ref_grob <- 0;
   } else if ("center" %in% xalign) {
      x_ref <- sum(xext_init[[1]] + xext_init[[2]]) / 2;
   } else if ("left" %in% xalign) {
      x_ref <- xext_init[[1]];
   } else if ("right" %in% xalign) {
      x_ref <- xext_init[[2]];
   } else if ("tile" %in% xalign) {
      x_ref <- xext_init[[2]];
   } else {
      # no alignment requested
      return(new_gTree);
   }
   
   # iterate each childName
   for (i in cns) {
      if (verbose) {
         jamba::printDebug("childGrob i:", i);
      }
      igrob <- grid::getGrob(new_gTree, i);
      igrob_exts <- grobs_exts(igrob);
      igrob_xext <- igrob_exts$xext;
      # igrob_xext <- igrob$xext;
      if ("center" %in% xalign) {
         ix_ref <- sum(igrob_exts$xext[[1]] + igrob_exts$xext[[2]]) / 2;
      } else if ("left" %in% xalign) {
         ix_ref <- igrob_exts$xext[[1]];
      } else if ("right" %in% xalign) {
         ix_ref <- igrob_exts$xext[[2]];
      } else if ("tile" %in% xalign) {
         ix_ref <- igrob_exts$xext[[1]];
      }
      if (verbose) {
         jamba::printDebug("ix_ref:");
         print(ix_ref);
      }
      
      x_diff <- (x_ref - ix_ref);
      ## skip if no difference
      if (identical(x_ref, ix_ref)) {
         x_diff <- 0;
         if ("tile" %in% xalign) {
            x_ref <- ix_ref;
         }
         if (verbose) {
            jamba::printDebug("x_diff is zero, skipping.");
         }
         next;
      }
      if (verbose) {
         jamba::printDebug("x_diff:");
         print(x_diff);
      }
      
      # igrobw <- grid::unit(diff(range(igrob_xext)), "pt")
      if (ref_grob > 0 && i == cns[[ref_grob]]) {
         # skip the ref_grob
      } else {
         # adjust x coordinate
         # igps <- setdiff(grid::grid.ls(igrob, print=FALSE)$name, i)
         igps <- grid::grid.ls(igrob, print=FALSE)$name;
         for (igp in igps) {
            jgrob <- grid::getGrob(gTree=new_gTree,
               # strict=TRUE,
               gPath=igp)
            if (!"x" %in% names(jgrob)) {
               next
            }
            if ("x" %in% names(jgrob)) {
               ## option to modify then setGrob() instead of editGrob() below
               # jgrob$x <- jgrob$x + x_diff;
               # igrob <- grid::setGrob(gTree=igrob,
               #    newGrob=jgrob,
               #    gPath=igp)
               ## editGrob()
               new_gTree <- grid::editGrob(new_gTree,
                  gPath=igp,
                  x=jgrob$x + x_diff)
            }
         }
         ## update new_gTree - needed with getGrob(), setGrob()
         # jamba::printDebug("Updating i:", i);# debug
         # grid::grid.ls(new_gTree);# debug
         # new_gTree <- grid::setGrob(gTree=new_gTree,
         #    newGrob=igrob,
         #    strict=FALSE,
         #    gPath=i);
      }
      # for "tile" set the x_ref to the right edge of the previous object
      if ("tile" %in% xalign) {
         x_ref <- ix_ref;
      }
   }
   new_gTree
}

#' Top-bottom grobs
#'
#' @family grobs
#' @keywords internal
#'
#' 
grobs_yalign <- function
(grobs,
 name=NULL,
 yalign=c("top",
    "middle",
    "bottom"),
 ref_grob=1,
 use_y=NULL,
 verbose=FALSE,
 debug=FALSE,
 ...)
{
   #
   #
   yalign <- match.arg(yalign)
   
   ## Create gTree for the output
   if (inherits(grobs, "gList")) {
      new_gTree <- grid::grobTree(grobs,
         name=name)
   } else if (inherits(grobs, "gTree")) {
      new_gTree <- grobs;
   } else {
      new_gTree <- grid::grobTree(do.call(grid::gList, grobs),
         name=name)
   }
   
   # childNames
   cns <- grid::childNames(new_gTree);
   
   ## define y reference coordinate for alignment
   grob1 <- grid::getGrob(new_gTree, cns[[ref_grob]]);
   grob1_exts <- grobs_exts(grob1);
   yext_init <- grob1_exts$yext;
   
   ## define x reference coordinate for alignment
   if (verbose) {
      jamba::printDebug("yext_init:");print(yext_init);
   }
   if (length(use_y) > 0) {
      y_ref <- use_y;
      ref_grob <- 0;
   } else if ("middle" %in% yalign) {
      y_ref <- sum(yext_init[[1]] + yext_init[[2]]) / 2;
   } else if ("bottom" %in% yalign) {
      y_ref <- yext_init[[1]];
   } else if ("top" %in% yalign) {
      y_ref <- yext_init[[2]];
   } else {
      # no alignment requested
      return(new_gTree);
   }
   if (verbose) {
      jamba::printDebug("y_ref:");
      print(y_ref);
   }
   
   # iterate each childName
   for (i in cns) {
      if (verbose) {
         jamba::printDebug("childGrob i:", i);
      }
      igrob <- grid::getGrob(new_gTree, i);
      igrob_exts <- grobs_exts(igrob);
      igrob_yext <- igrob_exts$yext;
      if ("middle" %in% yalign) {
         iy_ref <- sum(igrob_exts$yext[[1]] + igrob_exts$yext[[2]]) / 2;
      } else if ("bottom" %in% yalign) {
         iy_ref <- igrob_exts$yext[[1]];
      } else if ("top" %in% yalign) {
         iy_ref <- igrob_exts$yext[[2]];
      }
      if (verbose) {
         jamba::printDebug("iy_ref:", indent=5);
         print(iy_ref);
      }
      
      y_diff <- (iy_ref - y_ref);
      ## skip if no difference
      if (identical(y_ref, iy_ref)) {
         y_diff <- 0;
         if (verbose) {
            jamba::printDebug("y_diff is zero, skipping.");
         }
         next;
      }
      if (verbose) {
         jamba::printDebug("y_diff:", indent=5);
         print(y_diff);
      }
      
      if (ref_grob > 0 && i == cns[[ref_grob]]) {
         # skip the ref_grob
      } else {
         # adjust y coordinate
         # igps <- setdiff(grid::grid.ls(igrob, print=FALSE)$name, i)
         igps <- grid::grid.ls(igrob, print=FALSE)$name;
         for (igp in igps) {
            # jgrob <- grid::getGrob(igrob, igp)
            jgrob <- grid::getGrob(gTree=new_gTree,
               # strict=TRUE,
               gPath=igp)
            if (!"x" %in% names(jgrob)) {
               next
            }
            if ("y" %in% names(jgrob)) {
               new_gTree <- grid::editGrob(new_gTree,
                  gPath=igp,
                  y=jgrob$y - y_diff)
            }
         }
      }
   }
   new_gTree
}
