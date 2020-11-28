
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
#' @family venndir spatial
#' 
#' @examples
#' degrees <- c(5, 10, 15, 100, 105, 110, 200, 300, 358);
#' degrees
#' spread_degrees(degrees);
#' 
#' degrees2 <- sample(degrees);
#' degrees2
#' spread_degrees(degrees2);
#' 
#' @param degrees `numeric` vector of angles in degrees, expected
#'    to range from `0` to `360`. Values are fit to the range `c(0, 360)`
#'    using `degrees %% 360`.
#' @param min_degrees `numeric` indicating the minimum angle in degrees
#'    to allow between adjacent angles.
#' @param ... additional arguments are ignored.
#' 
#' @export
spread_degrees <- function
(degrees,
 min_degrees=10,
 iteration=1,
 max_iterations=20,
 verbose=FALSE,
 ...)
{
   if (verbose) {
      jamba::printDebug("spread_degrees(): ",
         "iteration:", iteration);
   }
   if (length(degrees) == 1) {
      return(degrees);
   }

   if ("data.frame" %in% class(degrees)) {
      ddf <- degrees;
      degrees <- ddf$degrees;
   } else {
      ddf <- data.frame(degrees,
         order=seq_along(degrees))
   }
   if (iteration > max_iterations) {
      if (verbose) {
         jamba::printDebug("spread_degrees(): ",
            "Hit max_iterations:", max_iterations);
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
   
   if (verbose > 2) {
      jamba::printDebug("spread_degrees(): ",
         "ddf start:");
      print(ddf);
   }
   
   ddf$degrees <- ddf$degrees %% 360;
   ddf <- jamba::mixedSortDF(ddf, byCols="degrees");
   if (!"degree_order" %in% colnames(ddf)) {
      ddf$degree_order <- seq_len(nrow(ddf));
   }
   digits <- 4;
   ddf$diff <- round(
      diff_degrees(c(ddf$degrees,
         head(ddf$degrees, 1))),
      digits=digits);
   
   # round min_degrees to 3 digits to prevent evil rounding errors
   min_degrees <- round(min_degrees,
      digits=digits);
   
   # if no angles are less the min_degrees, return input unchanged
   if (!any(abs(ddf$diff) < min_degrees)) {
      if (verbose) {
         jamba::printDebug("spread_degrees(): ",
            "all angles meet min_degrees=", min_degrees);
      }
      return(degrees);
   }
   
   # if all angles are less than min_degrees, evenly space all angles
   #ddf$abs_diff <- ddf$diff %% 360;
   if (all(abs(ddf$diff) < (min_degrees / 1.01))) {
      #if (verbose) {
         jamba::printDebug("spread_degrees(): ",
            "spreading all angles equally");
      #}
      seqdegree <- head(
         seq(from=ddf$degrees[1],
            to=ddf$degrees[1] + 360,
            length.out=length(ddf$degrees) + 1),
         length(ddf$degrees)) %% 360;
      ddf <- jamba::mixedSortDF(ddf,
         byCols=c("degree_order"));
      ddf$degrees <- seqdegree;
      ddf <- jamba::mixedSortDF(ddf,
         byCols=c("order"));
      return(ddf$degrees);
   }

   find_contiguous_degrees <- function(xdiff, min_degrees) {
      xdf <- data.frame(diff=xdiff);
      xdf$fix <- abs(xdf$diff) < (min_degrees);
      fixrle <- rle(xdf$fix);
      xdf$set <- rep(seq_along(fixrle$values),
         fixrle$lengths)
      # if the last row is part of a set, join with the first row
      if (head(xdf$fix, 1) && tail(xdf$fix, 1)) {
         newset <- xdf[nrow(xdf),"set"];
         xdf[xdf$set %in% 1, "set"] <- newset;
      }
      tfrows <- which(
         c(tail(xdf$fix, 1), head(xdf$fix, -1)) & 
            xdf$fix %in% FALSE);
      xdf[tfrows,"set"] <- c(tail(xdf$set, 1), head(xdf$set, -1))[tfrows]
      #xdf$fix1 <- xdf$fix;
      xdf[tfrows,"fix"] <- TRUE;
      xdf;
   }
   # use slightly smaller degree angle threshold to flag rows to spread
   fcd <- find_contiguous_degrees(ddf$diff, min_degrees / 1.01);
   # use slightly larger degree angle threshold
   # to include neighboring rows in this process
   fcd2 <- find_contiguous_degrees(ddf$diff, min_degrees * 1.01);
   ddf$fix <- fcd$fix;
   ddf$set <- fcd$set;
   ddf$fix2 <- fcd2$fix;
   ddf$set2 <- fcd2$set;
   set2_use <- unique(subset(ddf, fix)$set2)
   
   # subset rows to be adjusted
   #ddfsub <- subset(ddf, fix);
   ddfsub <- subset(ddf, set2 %in% set2_use);
   
   if (verbose > 2) {
      jamba::printDebug("spread_degrees(): ",
         "ddf:");
      print(ddf);
      jamba::printDebug("spread_degrees(): ",
         "ddfsub:");
      print(ddfsub);
   }
   
   # spread each group of angles
   ddf$degrees1 <- ddf$degrees;
   for (idf in split(ddfsub, ddfsub$set2)) {
      if (verbose > 2) {
         jamba::printDebug("spread_degrees(): ",
            "idf:");
         print(idf);
      }
      idf$diff_degrees <- diff_degrees(c(
         tail(idf$degrees, 1),
         idf$degrees));
      idf$diff_sign <- sign(idf$diff_degrees);
      idf <- jamba::mixedSortDF(idf,
         byCols=c("diff_sign", "degrees"))
      meandegree <- mean_degree_arc(idf$degrees,
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
         by=min_degrees,
         length=nrow(idf));
      #seqdegree <- sort(seqdegree %% 360);
      ddf[match(idf$order, ddf$order),"degrees"] <- seqdegree;
      if (verbose > 2) {
         jamba::printDebug("spread_degrees(): ",
            "new ddfsub:");
         print(ddf[match(idf$order, ddf$order),,drop=FALSE]);
      }
   }
   ddf <- jamba::mixedSortDF(ddf, byCols="order");
   
   # repeat
   if (verbose > 2) {
      jamba::printDebug("spread_degrees(): ",
         "ddf end:");
      print(ddf);
   }
   if (verbose > 1) {
      if (iteration == 1) {
         display_angles(ddf$degrees1,
            col=colorjam::rainbowJam(nrow(ddf)),
            lwd=ifelse(ddf$degrees == ddf$degrees1,
               2,
               4));
         title(main=paste0("Iteration ", iteration - 1), line=2.5);
      }
      display_angles(ddf$degrees,
         col=colorjam::rainbowJam(nrow(ddf)),
         lwd=ifelse(ddf$degrees == ddf$degrees1,
            2,
            4));
      title(main=paste0("Iteration ", iteration), line=2.5);
   }
   if (!"original_order" %in% colnames(ddf)) {
      ddf$original_order <- ddf$order;
   }
   ddf_keep <- c("degrees",
      "order",
      "degree_order",
      "original_order");
   degrees <- spread_degrees(
      degrees=ddf[,ddf_keep,drop=FALSE],
      min_degrees=min_degrees,
      iteration=iteration + 1,
      max_iterations=max_iterations,
      verbose=verbose);
   return(degrees);
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
#' @family venndir spatial
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
#' @family venndir spatial
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
#' @family venndir spatial
#' 
#' @param x `numeric` vector of angles in degrees.
#' @param use_range `logical` indicating whether to return the
#'    mean degree angle across the range, which therefore only
#'    uses the first and last angular values.
#' @param use_median `logical` indicating whether to use `median()`
#'    instead of `mean()`, included here for convenience.
#' @param ... additional arguments are ignored.
#' 
#' @examples
#' set.seed(1);
#' steps <- sample((1:12)^1.5, size=14, replace=TRUE);
#' steps <- sort(steps);
#' x <- cumsum(steps);
#' x;
#' 
#' opar <- par("mfrow"=c(2, 3));
#' on.exit(par(opar));
#' mean_degree_arc(x, do_plot=TRUE);
#' mean_degree_arc(x, use_median=TRUE, do_plot=TRUE);
#' mean_degree_arc(x, use_range=TRUE, do_plot=TRUE);
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
      opar <- par(lend="square",
         ljoin="mitre",
         mar=c(1,1,1,1));
      on.exit(par(opar));
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
         valign=1,
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
   }
   return(degrees);
}

#' Display degree angles around a unit circle
#' 
#' Display degree angles around a unit circle
#' 
#' @family venndir spatial
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
      opar <- par(lend="square",
         ljoin="mitre",
         mar=c(1,1,1,1));
      on.exit(par(opar));
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
#' @family venndir spatial
#' 
#' @param degrees `numeric` value for angles in degrees
#' @param top `numeric` value indicating the angle at the
#'    top position
#' @param clockwise `logical` indicating whether the angle
#'    increases in clockwise direction
#' @param expand `numeric` value intended to expand the adjust
#'    value. For example `expand=0.5` will expand the adjust
#'    value 50%.
#' @param ... additional arguments are ignored
#' 
#' @examples
#' degrees <- seq(from=1, to=360, by=33);
#' adjdf <- degrees_to_adj(degrees);
#' x <- cos(jamba::deg2rad(degrees));
#' y <- sin(jamba::deg2rad(degrees));
#' plot(x, y,
#'    pch=20, asp=1,
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
 bias_side=3,
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
   degreebreaks <- seq(from=-45/2,
      to=360 + 45/2,
      by=45);
   if (bias_side != 1 || bias_height != 1) {
      lr <- c(3,4, 7, 8);
      tb <- c(1,2, 5,6, 9, 10);
      bias_side <- 3;
      bias_height <- 3;
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
   if (expand[1] != 0) {
      adjx <- (adjx - 0.5) * (1 + expand[1]) + 0.5;
   }
   adjy <- ifelse(grepl("top", degreeset),
      0,
      ifelse(grepl("bottom", degreeset),
         1,
         0.5));
   if (expand[2] != 0) {
      adjy <- (adjy - 0.5) * (1 + expand[2]) + 0.5;
   }
   data.frame(adjx=adjx, adjy=adjy);
}
