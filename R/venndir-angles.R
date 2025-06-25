
#' Spread angles to minimum degrees difference
#' 
#' Spread angles to minimum degrees difference
#' 
#' The function takes a vector of angles in degrees, and
#' adjusts any angles with adjacent angle below a minimum 
#' angle `min_degrees` until the minimum angle is `min_degrees`.
#' If all input angles fit this criteria, it is returned
#' unchanged, otherwise it will adjust angles then
#' iteratively call itself until the condition is met.
#' 
#' If all angles are less the `min_degrees` degrees from
#' the nearest adjacent angle, then all angles are equally
#' spaced around 360 degrees.
#' 
#' @family venndir geometry
#' 
#' @param degrees `numeric` vector of angles in degrees, expected
#'    to range from `0` to `360`. Values are fit to the range `c(0, 360)`
#'    using `degrees %% 360`.
#' @param min_degrees `numeric` indicating the minimum angle in degrees
#'    to allow between adjacent angles.
#' @param iteration,max_iterations `numeric` used internally to iteratively
#'    confirm that angles are spread by `min_degrees`.
#' @param use_colors `character` optional colors to use when `do_plot=TRUE`,
#'    default NULL assigns color by groups of angles, the applies a
#'    light-to-dark color gradient.
#' @param do_plot `logical` whether to plot a visual with the output,
#'    default FALSE.
#' @param verbose `logical` indicating whether to print verbose output.
#'    Values 2 or 3 will print progressively more information.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' degrees <- c(5, 10, 15, 100, 105, 110, 200, 300, 358);
#' degrees
#' use_colors <- colorjam::rainbowJam(length(degrees))
#' withr::with_par(list(mfrow=c(2, 2)), {
#' spread_degrees(degrees, min_degrees=0,
#'    do_plot=TRUE, use_colors=use_colors)
#' spread_degrees(degrees,
#'    do_plot=TRUE, use_colors=use_colors)
#' spread_degrees(degrees, min_degrees=20,
#'    do_plot=TRUE, use_colors=use_colors)
#' spread_degrees(degrees, min_degrees=30,
#'    do_plot=TRUE, use_colors=use_colors)
#' })
#' 
#' degrees2 <- sample(degrees);
#' degrees2
#' spread_degrees(degrees2);
#' 
#' degrees3 <- c(0, 5, 6, 10, 15, 50)
#' names(degrees3) <- LETTERS[1:6];
#' colors3 <- colorjam::rainbowJam(6)
#' withr::with_par(list(mfrow=c(2, 2)), {
#' spread_degrees(degrees3, min_degrees=2,
#'    use_colors=colors3, do_plot=TRUE)
#' 
#' spread_degrees(degrees3, min_degrees=6,
#'    use_colors=colors3, do_plot=TRUE)
#' 
#' spread_degrees(degrees3, min_degrees=25,
#'    use_colors=colors3, do_plot=TRUE)
#' 
#' spread_degrees(degrees3, min_degrees=45,
#'    use_colors=colors3, do_plot=TRUE)
#' })
#' 
#' @export
spread_degrees <- function
(degrees,
 min_degrees=10,
 iteration=1,
 max_iterations=20,
 use_colors=NULL,
 do_plot=FALSE,
 verbose=FALSE,
 ...)
{
   if (verbose) {
      jamba::printDebug("spread_degrees(): ",
         "iteration:", iteration);
   }
   input_was_df <- FALSE;
   if (inherits(degrees, "data.frame")) {
      input_was_df <- TRUE;
      ddf <- degrees;
      degrees <- ddf$degrees;
   } else {
      # idx is the input order
      ddf <- data.frame(degrees,
         idx=seq_along(degrees))
         # order=seq_along(degrees))
      ddfg <- assign_degree_groups(ddf$degrees,
         min_degrees=min_degrees);
      ddf$group <- ddfg$group;
      ddf$order <- 0;
      for (igroup in unique(ddf$group)) {
         k <- which(ddf$group %in% igroup);
         if (length(k) == 1) {
            ddf$order[k] <- 1;
         } else {
            ddf$order[k] <- make_degrees_clockwise(ddf$degrees[k])$idx;
         }
      }
      ddf2 <- jamba::mixedSortDF(ddf, byCols=c("group", "order", "idx"));
      ddf2$order <- seq_len(nrow(ddf2));
      ddf <- jamba::mixedSortDF(ddf2, byCols=c("idx"));
   }
   
   # debug plot function
   spread_plot <- function() {
      if (length(use_colors) >= length(degrees)) {
         use_colors <- head(use_colors, length(degrees))
      } else {
         use_colors <- jamba::color2gradient(dex=2/3,
            colorjam::group2colors(ddf$group));
      }
      use_degrees <- jamba::nameVector(ddf$degrees,
         rownames(ddf));
      # jamba::printDebug("use_degrees:");print(use_degrees);# debug
      display_angles(use_degrees,
         # col=colorjam::rainbowJam(nrow(ddf)),
         col=use_colors,
         lwd=ifelse(ddf$degrees == degrees,
            2,
            4));
      title(main=paste0("min_degrees=", format(min_degrees, digits=1)),
         line=3);
   }
   
   if (length(degrees) == 1) {
      if (TRUE %in% do_plot && iteration == 1) {
         spread_plot()
      }
      if (input_was_df) {
         return(ddf)
      }
      return(degrees);
   }

   if (iteration > max_iterations) {
      if (verbose) {
         jamba::printDebug("spread_degrees(): ",
            "Hit max_iterations:", max_iterations);
      }
      if (TRUE %in% do_plot && iteration == 1) {
         spread_plot()
      }
      if (input_was_df) {
         return(ddf)
      }
      return(degrees);
   }
   # set min_degrees if too many items
   if (length(degrees) * min_degrees > 360) {
      min_degrees <- (360 / length(degrees)) / 1.35;
      if (verbose) {
         jamba::printDebug("spread_degrees(): ",
            "Defined min_degrees by n=", length(degrees));
      }
   }
   
   
   digits <- 4;
   # round min_degrees to 4 digits to prevent evil rounding errors?
   # min_degrees <- round(min_degrees, digits=digits);
   
   # 0.0.56.900 - repair large gaps
   new_ddf <- assign_degree_groups(ddf$degrees,
      min_degrees=min_degrees);
   new_cols <- c("degrees", "diff", "diff_pre", "diff_post", "group")
   ddf[, new_cols] <- new_ddf[, new_cols, drop=FALSE];
   if (verbose > 2) {
      jamba::printDebug("spread_degrees(): ",
         "ddf groups:");
      print(ddf);
   }

   # if no duplicate group then angles are already spread
   if (!any(duplicated(ddf$group))) {
      if (verbose) {
         jamba::printDebug("spread_degrees(): ",
            "all angles meet min_degrees=", min_degrees);
      }
      if (TRUE %in% do_plot && iteration == 1) {
         spread_plot()
      }
      if (input_was_df) {
         return(ddf)
      }
      return(degrees);
   }
   
   # if all angles are less than min_degrees, evenly space all angles
   if ((nrow(ddf) * min_degrees) > 360) {
      # more items than could fit into 360 degrees by min_degrees steps
      if (verbose) {
         jamba::printDebug("spread_degrees(): ",
            "spreading all angles equally");
      }
      seqdegree <- head(
         seq(from=ddf$degrees[1],
            to=ddf$degrees[1] + 360,
            length.out=length(ddf$degrees) + 1),
         length(ddf$degrees)) %% 360;
      # assign values with the same magnitude order
      ddf$degrees <- seqdegree[order(ddf$degrees)]
      # ddf <- jamba::mixedSortDF(ddf,
      #    byCols=c("degree_order"));
      # ddf$degrees <- seqdegree;
      ddf <- jamba::mixedSortDF(ddf,
         byCols=c("order"));
      if (TRUE %in% do_plot && iteration == 1) {
         spread_plot()
      }
      if (input_was_df) {
         return(ddf)
      }
      return(ddf$degrees);
   }

   group_use <- names(jamba::tcount(ddf$group, 2));
   if (verbose) {
      jamba::printDebug("spread_degrees(): ",
         "group_use:", group_use);
   }

   # spread each group of angles
   max_iterations <- 2;
   ddf$degrees1 <- ddf$degrees;
   for (igroup in group_use) {
      idf <- subset(ddf, group %in% igroup);
      if (verbose > 1) {
         jamba::printDebug("spread_degrees(): ",
            "igroup: ", igroup,
            ", idf:");
         print(idf);
      }
      idf$diff_degrees <- diff_degrees(c(
         tail(idf$degrees, 1),
         idf$degrees));
      idf$diff_sign <- sign(idf$diff_degrees);
      idf <- jamba::mixedSortDF(idf,
         byCols=c("diff_sign", "degrees"))
      # 0.0.56.900 - improve arc calculation
      clockwise_degrees <- make_degrees_clockwise(idf$degrees)
      meandegree <- mean_degree_arc(clockwise_degrees$x,
         use_median=FALSE,
         use_range=FALSE);
      idf <- jamba::mixedSortDF(idf,
         byCols=c("diff_sign", "degree_order", "degrees"))
      if (verbose > 2) {
         jamba::printDebug("spread_degrees(): ",
            "idf$degrees:", idf$degrees);
         jamba::printDebug("spread_degrees(): ",
            "meandegree:", meandegree);
      }
      startoffset <- meandegree - (nrow(idf) - 1) * min_degrees / 2;
      seqdegree <- seq(startoffset,
         by=min_degrees * 1.00,
         length=nrow(idf));
      #seqdegree <- sort(seqdegree %% 360);
      ddf[match(idf$order, ddf$order), "degrees"] <- seqdegree;
      if (verbose > 2) {
         jamba::printDebug("spread_degrees(): ",
            "new ddfsub:");
         print(ddf[match(idf$order, ddf$order),,drop=FALSE]);
      }
   }
   ddf <- jamba::mixedSortDF(ddf, byCols=c("degrees", "order"));
   ddf$new_diff <- diff_degrees(c(tail(ddf$degrees, 1), ddf$degrees));
   ddf <- jamba::mixedSortDF(ddf, byCols="order");
   
   # repeat as needed
   if (verbose > 2) {
      jamba::printDebug("spread_degrees(): ",
         "ddf end:");
      print(ddf);
   }

   # repair group order
   # - within group, sort by order (the arc order)
   # - then sort by idx which is the original input order
   new_ddf <- ddf;
   dupe_groups <- names(jamba::tcount(new_ddf$group,2));
   for (dupe_group in dupe_groups) {
      k <- which(new_ddf$group %in% dupe_group);
      new_ddf[k, ]
      new_ddf$degrees[k] <- make_degrees_clockwise(new_ddf$degrees[k])$x;
   }
   ddf <- jamba::mixedSortDF(new_ddf, byCols=c("idx"))

   if (iteration < (max_iterations + 1) &&
         any(round(ddf$new_diff, digits=1) < round(min_degrees / 1.02, digits=1))) {
      if (verbose > 2) {
         jamba::printDebug("spread_degrees(): ",
            "next iteration: ", iteration + 1);
      }
      # new_degrees <- spread_degrees(ddf$degrees,
      new_ddf <- spread_degrees(ddf,
         min_degrees=min_degrees,
         iteration=iteration + 1,
         max_iterations=max_iterations,
         verbose=verbose)
      
      # repair group order
      new_ddf <- jamba::mixedSortDF(new_ddf, byCols=c("order"))

      dupe_groups <- names(jamba::tcount(new_ddf$group,2));
      for (dupe_group in dupe_groups) {
         k <- which(new_ddf$group %in% dupe_group);
         new_ddf[k, ]
         new_ddf$degrees[k] <- make_degrees_clockwise(new_ddf$degrees[k])$x;
      }
      ddf <- jamba::mixedSortDF(new_ddf, byCols=c("idx"))
   } else {
      # new_degrees <- ddf$degrees %% 360;
      ddf$degrees <- ddf$degrees %% 360;
   }
   
   if (TRUE %in% do_plot && iteration == 1) {
      spread_plot()
   }
   if (input_was_df) {
      return(ddf)
   }
   ddf3 <- data.frame(input=degrees, jamba::mixedSortDF(ddf, byCols="idx"))
   # jamba::printDebug("ddf3:");print(ddf3);# debug
   return(ddf$degrees);
}

#' Mean angle in degrees
#' 
#' Mean angle in degrees
#' 
#' Simple utility function to calculate the mean angle
#' in degrees using vector directional logic. For
#' each angle, a unit vector is created with radius 1,
#' then the mean point is used to define the new angle.
#' 
#' A small random value is added to each input value to
#' reduce the effect of having identical opposite angles.
#' In that case, the output is a right angle to the
#' input angles.
#' 
#' @return `numeric` angle in degrees representing the
#'    mean angle of input angles in degrees from `x`.
#'    An attribute `"radius"` included with the unit
#'    radius from the mean unit vectors. This radius
#'    may be useful as a sort of weight factor.
#' 
#' @param x `numeric` vector of angles in degrees.
#' @param seed `numeric` value used as a random seed
#'    for reproducibility. This seed affects the random
#'    value added to `x` in the form `rnorm(length(x)) * 1e-6`.
#' @param ... additional arguments are ignored.
#' 
#' @family venndir geometry
#' 
#' @examples
#' mean_degrees(c(355, 15))
#' 
#' mean_degrees(c(355, 15, 10))
#' 
#' mean_degrees(c(0, 180))
#' 
#' mean_degrees(c(0, 270))
#' 
#' @export
mean_degrees <- function
(x,
 seed=1,
 ...)
{
   if (length(seed) == 1) {
      set.seed(seed);
   }
   x <- x + rnorm(length(x)) * 1e-6;
   xmean <- mean(cos(jamba::deg2rad(x)), na.rm=TRUE);
   ymean <- mean(sin(jamba::deg2rad(x)), na.rm=TRUE);
   degrees <- jamba::rad2deg(atan2(y=ymean, x=xmean)) %% 360;
   radius <- sqrt(xmean^2 + ymean^2);
   attr(degrees, "radius") <- radius;

   return(degrees)
}

#' Angular difference in degrees
#' 
#' Angular difference in degrees
#' 
#' This function simply returns the difference between two
#' angles in degrees. When `degree1` and `degree2` are vectors,
#' this function operates on the full set in one step.
#' 
#' When `degree1` is a vector, and `degree2` is `NULL`, this
#' function calculates the difference of each pair in `degree1`,
#' in the order supplied.
#' 
#' @family venndir geometry
#' 
#' @param degree1 `numeric` vector that contains angles in degrees.
#' @param degree2 `numeric` vector that contains angles in degrees,
#'    or `NULL` when the input should only consider `degree1`.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' diff_degrees(5, 355)
#' diff_degrees(355, 5)
#' diff_degrees(-10, 5)
#' diff_degrees(-10, 355)
#' diff_degrees(180, 361)
#' 
#' @export
diff_degrees <- function
(degree1,
 degree2=NULL,
 ...)
{
   if (length(degree1) == 0) {
      return(degree1);
   }
   if (length(degree2) == 0) {
      if (length(degree1) == 1) {
         return(0);
      }
      diff_degrees <- sapply(seq_len(length(degree1) - 1), function(i){
         diff_degrees(degree1[i],
            degree1[i+1])
      });
      return(diff_degrees);
   }
   degree1 <- degree1 %% 360;
   degree2 <- degree2 %% 360;
   while (degree2 < degree1) {
      degree2 <- degree2 + 360;
   }
   diff_degrees <- degree2 - degree1;
   if (diff_degrees > 180) {
      diff_degrees <- diff_degrees - 360;
   }
   diff_degrees;
}

#' Mean arc angle in degrees
#' 
#' Mean arc angle in degrees
#' 
#' This function differs from `mean_degrees()` in that it finds
#' the mean angle in degrees from angles along an arc, guaranteeing
#' that the mean angle is along that numeric arc. It is intended
#' that the arc does not cover more than 360 degrees, and for angles
#' whose numeric values are increasing.
#' 
#' The specific purpose is to enable supplying two angles like c(12, 45)
#' to imply the arc "from 12 to 45 degrees", for which the mean is 28.5.
#' Or one could supply c(45, 12) and imply "from 45, around 360,
#' back to 12 degrees" and the mean of this large arc would be 208.5.
#' 
#' @family venndir geometry
#' 
#' @param x `numeric` vector of angles in degrees.
#' @param use_range `logical` indicating whether to return the
#'    mean degree angle across the range, which therefore only
#'    uses the first and last angular values.
#' @param use_median `logical` indicating whether to use `median()`
#'    instead of `mean()`, included here for convenience.
#' @param do_plot `logical` whether to plot the result, default FALSE.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' set.seed(1);
#' steps <- sample((1:12)^1.5, size=14, replace=TRUE);
#' steps <- sort(steps);
#' x <- cumsum(steps);
#' x;
#' 
#' withr::with_par(list("mfrow"=c(2, 3)), {
#'    mean_degree_arc(x, do_plot=TRUE);
#'    mean_degree_arc(x, use_median=TRUE, do_plot=TRUE);
#'    mean_degree_arc(x, use_range=TRUE, do_plot=TRUE);
#' })
#' 
#' x <- x + 235;
#' mean_degree_arc(x, do_plot=TRUE);
#' mean_degree_arc(x, use_median=TRUE, do_plot=TRUE);
#' mean_degree_arc(x, use_range=TRUE, do_plot=TRUE);
#' 
#' @export
mean_degree_arc <- function
(x,
 use_range=FALSE,
 use_median=FALSE,
 do_plot=FALSE,
 ...)
{
   if (length(x) == 1) {
      degrees <- x;
      attr(degrees, "radius") <- 1;
   } else {
      x <- x %% 360;
      # 0.0.56.900 - considered, and rejected, correcting for >180 degree gap
      if (FALSE) {
         x <- sort(x);
         xdiff <- diff(c(tail(x, 1), x));
         if (any(xdiff > 180)) {
            xwhich <- which(xdiff > 180);
            xseq <- seq(from=xwhich, to=length(x));
            x[xseq] <- x[xseq] - 360;
            x <- sort(x);
         }
      }
      degree_diff <- diff_degrees(x);
      num_diff <- diff(x);
      if (any(num_diff < 0)) {
         which_rev <- which(num_diff < 0) + 1;
         for (i in which_rev) {
            j <- seq(from=i, to=length(x));
            x[j] <- x[j] + 360;
         }
      }
      if (use_range) {
         x_save <- x;
         x <- c(head(x, 1), tail(x, 1));
      }
      if (use_median) {
         degrees <- median(x) %% 360;
      } else {
         degrees <- mean(x) %% 360;
      }
      attr(degrees, "radius") <- 1;
      if (use_range) {
         x <- x_save;
      }
   }
   if (do_plot) {
      step_degrees <- seq(from=0, to=360, by=5);
      label_degrees <- which(
         (
            (seq_along(
               head(step_degrees, -1)
               ) - 1) %% 9) == 0);
      label_adj <- degrees_to_adj(step_degrees,
         expand=1);
      x1 <- cos(jamba::deg2rad(step_degrees));
      y1 <- sin(jamba::deg2rad(step_degrees));
      step_degrees2 <- c(seq(from=head(x, 1), to=tail(x, 1), by=5), tail(x, 1));
      x2 <- cos(jamba::deg2rad(step_degrees2));
      y2 <- sin(jamba::deg2rad(step_degrees2));
      withr::with_par(list(
         mar=c(1,1,1,1),
         lend="butt",
         ljoin="mitre"), {
         plot(x=x1,
            y=y1,
            bty="n",
            xaxt="n",
            yaxt="n",
            type="l",
            asp=1);
         lines(x=x2,
            y=y2,
            col="navy",
            lwd=4);
         arrows(x0=tail(x2, 2)[1],
            y0=tail(y2, 2)[1],
            x1=tail(x2, 2)[2],
            y1=tail(y2, 2)[2],
            col="navy",
            lwd=4)
         points(x=x1[label_degrees],
            y=y1[label_degrees],
            pch=20)
         for (i in label_degrees) {
            text(x=x1[i],
               y=y1[i],
               labels=step_degrees[i],
               adj=unlist(label_adj[i,c("adjx","adjy")]))
         }
         for (i in seq_along(x)) {
            x1 <- cos(jamba::deg2rad(x[i]));
            y1 <- sin(jamba::deg2rad(x[i]));
            arrows(x0=0,
               y0=0,
               x1=x1,
               y1=y1,
               col="darkorange",
               length=0.2,
               lwd=2)
         }
         x1 <- cos(jamba::deg2rad(degrees));
         y1 <- sin(jamba::deg2rad(degrees));
         arrows(x0=0,
            y0=0,
            x1=x1,
            y1=y1,
            col="red",
            lwd=6);
         jamba::drawLabels(x=0,
            y=-0.4,
            # valign=1,
            labelCex=1.5,
            boxColor="#DDAA77DD",
            txt=paste0("mean_degree_arc()",
               ifelse(use_range,
                  paste0("\nuse_range=", use_range),
                  ""),
               ifelse(use_median,
                  paste0("\nuse_median=", use_median),
                  "")
            )
         )
      })
   }
   return(degrees);
}

#' Display degree angles around a unit circle
#' 
#' Display degree angles around a unit circle
#' 
#' @family venndir geometry
#' 
#' @param x `numeric` angles in degrees
#' @param add `logical` whether to add to an existing plot, default FALSE.
#' @param col `character` R color to use for arrows, default 'darkorange'.
#' @param lwd `numeric` line width, default 2.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' display_angles(jamba::nameVector(c(10, 45, 90, 225)))
#' 
#' @export
display_angles <- function
(x,
 add=FALSE,
 col="darkorange",
 lwd=2,
 ...)
{
   if (!add) {
      step_degrees <- seq(from=0, to=360, by=5);
      label_degrees <- which(
         (
            (seq_along(
               head(step_degrees, -1)
            ) - 1) %% 9) == 0);
      label_adj <- degrees_to_adj(step_degrees,
         expand=1);
      x1 <- cos(jamba::deg2rad(step_degrees));
      y1 <- sin(jamba::deg2rad(step_degrees));
      withr::with_par(list(
         mar=c(1,1,1,1),
         lend="butt",
         ljoin="mitre"), {
         plot(x=x1,
            y=y1,
            bty="n",
            xaxt="n",
            yaxt="n",
            type="l",
            asp=1);
         points(x=x1[label_degrees],
            y=y1[label_degrees],
            pch=20)
         for (i in label_degrees) {
            text(x=x1[i],
               y=y1[i],
               labels=step_degrees[i],
               adj=unlist(label_adj[i,c("adjx","adjy")]))
         }
      })
   }
   # draw arrow for each angle in x
   col <- rep(col, length.out=length(x));
   lwd <- rep(lwd, length.out=length(x));
   for (i in seq_along(x)) {
      x1 <- cos(jamba::deg2rad(x[i]));
      y1 <- sin(jamba::deg2rad(x[i]));
      arrows(x0=0,
         y0=0,
         x1=x1,
         y1=y1,
         col=col[i],
         length=0.2,
         lwd=lwd[i]);
      if (1 == 2 && length(names(x)) > 0) {
         text(x=x1,
            y=y1,
            labels=names(x)[i],
            adj=unlist(degrees_to_adj(x[i], expand=c(1,1))[1,c("adjx","adjy")]))
      }
      
   }
}

#' Degrees to text adjustment
#' 
#' Degrees to text adjustment
#' 
#' Utility function to define `adj` values suitable
#' for text plotting, which arranges text relative
#' to the angle in degrees.
#' 
#' @family venndir geometry
#' 
#' @param degrees `numeric` value for angles in degrees
#' @param top `numeric` value indicating the angle at the
#'    top position
#' @param clockwise `logical` indicating whether the angle
#'    increases in clockwise direction
#' @param expand `numeric` value intended to expand the adjust
#'    value. For example `expand=0.5` will expand the adjust
#'    value 50%.
#' @param do_fractional `logical` whether to adjust fractional
#'    values along the outer edge, default TRUE.
#'    When FALSE, it "snaps" the label to either a right angle,
#'    or exact 45-degree angle relative to the incoming line segment.
#' @param bias_height,bias_side `numeric` values used only when
#'    `do_fractional=FALSE`, which expands the range of degree angles
#'    where a label is pushed to a 45-degree angle.
#'    For example, when `bias_side=2`
#'    the range of degrees on the left and right side are more
#'    likely to result in labels with 45-degree adjustment.
#' @param plot_ranges `logical` whether to plot a visual with the
#'    angular cutoffs which define the adjustment.
#' @param ... additional arguments are ignored
#' 
#' @examples
#' degrees <- seq(from=1, to=360, by=10);
#' x <- cos(jamba::deg2rad(degrees));
#' y <- sin(jamba::deg2rad(degrees));
#' 
#' adjdfF <- degrees_to_adj(degrees, do_fractional=TRUE);
#' plot(x, y,
#'    main="Sliding edge adjustment.",
#'    pch=20, asp=1,
#'    xlim=c(-1.3, 1.3),
#'    ylim=c(-1.3, 1.3));
#' jamba::drawLabels(txt=seq_along(degrees),
#'    x=x, y=y, labelCex=0.8, boxColor="gold",
#'    adjX=adjdfF[,1], adjY=adjdfF[,2])
#' 
#' adjdf1 <- degrees_to_adj(degrees, do_fractional=FALSE, bias_side=1);
#' plot(x, y,
#'    main="Snap to nearest 45-degree angle adjustment.",
#'    pch=20, asp=1,
#'    xlim=c(-1.3, 1.3),
#'    ylim=c(-1.3, 1.3));
#' jamba::drawLabels(txt=seq_along(degrees),
#'    x=x, y=y, labelCex=0.8,
#'    adjX=adjdf1[,1], adjY=adjdf1[,2])
#' 
#' adjdf <- degrees_to_adj(degrees, bias_side=3, do_fractional=FALSE);
#' plot(x, y,
#'    main="Snap to 45-degree with custom bias.",
#'    pch=20, asp=1,
#'    xlim=c(-1.3, 1.3),
#'    ylim=c(-1.3, 1.3));
#' jamba::drawLabels(txt=seq_along(degrees),
#'    x=x, y=y, labelCex=0.8, boxColor="gold",
#'    adjX=adjdf[,1], adjY=adjdf[,2])
#' 
#' adjdf <- degrees_to_adj(degrees, bias_side=3,
#'    expand=c(1, 1), do_fractional=FALSE);
#' plot(x, y,
#'    main="Example using base text()",
#'    pch=20, asp=1, cex=0.4, col="red",
#'    xlim=c(-1.3, 1.3),
#'    ylim=c(-1.3, 1.3));
#' for (i in seq_along(degrees)) {
#'    text(labels=i,
#'       x=x[i], y=y[i],
#'       adj=unlist(adjdf[i,]))
#' }
#' 
#' @export
degrees_to_adj <- function
(degrees,
 top=90,
 clockwise=FALSE,
 expand=0,
 do_fractional=TRUE,
 bias_side=1,
 bias_height=1,
 plot_ranges=FALSE,
 ...)
{
   if (length(top) != 1) {
      top <- 90
   }
   if (length(expand) == 0) {
      expand <- 0;
   }
   expand <- rep(expand, length.out=2);
   degrees <- degrees + (top - 90)
   if (length(clockwise) && any(clockwise)) {
      degrees <- -degrees1;
   }
   degrees <- degrees %% 360;
   if (do_fractional) {
      degreebreaks <- seq(from=0,
         to=360 + 45,
         by=45);
      bias_height <- 1;
      bias_side <- 1;
   } else {
      degreebreaks <- seq(from=-45/2,
         to=360 + 45/2,
         by=45);
   }
   # jamba::printDebug("degreebreaks:");print(degreebreaks);# debug
   if (bias_side != 1 || bias_height != 1) {
      lr <- c(3,4, 7,8);
      tb <- c(1,2, 5,6, 9,10);
      # bias_side <- 3;
      # bias_height <- 3;
      xbreaks <- cos(jamba::deg2rad(degreebreaks));
      ybreaks <- sin(jamba::deg2rad(degreebreaks));
      xbreaks[tb] <- cos(jamba::deg2rad(degreebreaks[tb])) * bias_height;
      ybreaks[tb] <- sin(jamba::deg2rad(degreebreaks[tb]));
      xbreaks[lr] <- cos(jamba::deg2rad(degreebreaks[lr]));
      ybreaks[lr] <- sin(jamba::deg2rad(degreebreaks[lr])) * bias_side;
      degreebreaks_out <- jamba::rad2deg(atan2(y=ybreaks, x=xbreaks)) %% 360;
      degreebreaks_out[1] <- degreebreaks_out[1] - 360;
      degreebreaks_out[10] <- degreebreaks_out[10] + 360;
      if (plot_ranges) {
         display_angles(unique(degreebreaks_out), col="darkorange", lwd=5, add=FALSE);
      }
      degreebreaks <- degreebreaks_out;
   } else {
      if (plot_ranges) {
         display_angles(unique(degreebreaks), col="darkorange", lwd=5, add=FALSE);
      }
   }
   if (1 == 2) {
      degreecuts <- jamba::nameVector(
         head(degreebreaks, -2) + 22.5,
         c("right", "topright", "top",
            "topleft", "left", "bottomleft",
            "bottom", "bottomright"));
      display_angles(degreebreaks, col="darkorange")
      display_angles(degreecuts, add=TRUE, col="transparent")
   }
   
   if (do_fractional) {
      degreecut <- cut(degrees,
         degreebreaks,
         labels=c("right", "top", "top",
            "left", "left", "bottom",
            "bottom", "right", "right"));
      degreeset <- as.character(degreecut);
      rads <- jamba::deg2rad(degrees);
      names(rads) <- round(degrees)
      adjx <- 1 - round(digits=2, jamba::noiseFloor(
         cos(rads) / sqrt(1/2),
         minimum=-1, ceiling=1) / 2 + 0.5);
      adjy <- 1 - round(digits=2, jamba::noiseFloor(
         sin(rads) / sqrt(1/2),
         minimum=-1, ceiling=1) / 2 + 0.5);
      adjx1 <- ifelse(grepl("right", degreeset),
         0,
         ifelse(grepl("left", degreeset),
            1,
            0.5));
      adjy1 <- ifelse(grepl("top", degreeset),
         0,
         ifelse(grepl("bottom", degreeset),
            1,
            0.5));
      adj_df <- data.frame(check.names=FALSE,
         row.names=as.character(seq_along(degrees)),
         degrees=degrees,
         degreeset=degreeset,
         adjx=adjx,
         adjy=adjy,
         adjx1=adjx1,
         adjy1=adjy1)
      # print(adj_df);# debug
      # expand <- c(0, 0);
      # adjx <- adjx1;
      # adjy <- adjy1;
   } else {
      degreecut <- cut(degrees,
         degreebreaks,
         labels=c("right", "topright", "top",
            "topleft", "left", "bottomleft",
            "bottom", "bottomright", "right"));
      degreeset <- as.character(degreecut);
      adjx <- ifelse(grepl("right", degreeset),
         0,
         ifelse(grepl("left", degreeset),
            1,
            0.5));
      adjy <- ifelse(grepl("top", degreeset),
         0,
         ifelse(grepl("bottom", degreeset),
            1,
            0.5));
   }
   # optionally expand beyond the point
   if (expand[1] != 0) {
      adjx <- (adjx - 0.5) * (1 + expand[1]) + 0.5;
   }
   if (expand[2] != 0) {
      adjy <- (adjy - 0.5) * (1 + expand[2]) + 0.5;
   }
   # data.frame(adjx=adjx, adjy=adjy, row.names="1");
   data.frame(adjx=adjx, adjy=adjy, row.names=seq_len(length(adjx)));
}
