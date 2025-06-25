
context("venndir spatial")
# library(venndir)

test_that("degrees_to_adj", {
   df <- data.frame(adjx=c(0, 0.5, 1, 0.5, 0),
      adjy=c(0.5, 0, 0.5, 1, 0.5));
   expect_equal(
      degrees_to_adj(c(0, 90, 180, 270, 361), do_fractional=FALSE),
      df)
})

test_that("diff_degrees", {
   expect_equal(
      diff_degrees(45, 12),
      -33)
   expect_equal(
      diff_degrees(350, 12),
      22)
   expect_equal(
      diff_degrees(c(350, 12, 45, 230)),
      c(22, 33, -175))
})

test_that("mean_degrees", {
   expect_equivalent(
      mean_degrees(c(12, 45)),
      28.5)
   # reverse orientation makes no difference
   expect_equivalent(
      mean_degrees(c(45, 12)),
      28.5)
   expect_equivalent(
      round(digits=1, mean_degrees(c(350, 45, 12))),
      15.5)
   expect_equivalent(
      round(digits=1, mean_degrees(c(350, 45, 185))),
      32.7)
})

test_that("mean_degree_arc", {
   expect_equivalent(
      mean_degree_arc(c(12, 45)),
      28.5)
   # reverse orientation implies the larger arc
   expect_equivalent(
      round(digits=1, mean_degree_arc(c(45, 12))),
      208.5)
   expect_equivalent(
      round(digits=1, mean_degree_arc(c(350, 12, 45))),
      15.7)
   expect_equivalent(
      round(digits=1, mean_degree_arc(c(350, 45, 185))),
      73.3)
   expect_equivalent(
      round(digits=1, mean_degree_arc(c(350, 355, 5, 6))),
      359)
})

# To fix:
# spread_degrees(c(340,354, 355, 0.4, 0.5, 177, 180, 244), do_plot=TRUE)
test_that("spread_degrees", {
   expect_equal(
      round(spread_degrees(c(0.5, 0.4, 355, 180, 177, 338, 340, 354, 244))),
      c(-4, -14, -24, 184, 174, 6, 16, -34, 244))
})

test_that("label_outside_JamPolygon", {
   mammo_counts <- c(wob=870, wwbc=2,
      "wob&wwpm"=120, "wwbc&wwpm"=8)
   jp1 <- venndir::get_venn_polygon_shapes(mammo_counts, proportional=TRUE);
   jp2 <- find_venn_overlaps_JamPolygon(jp1, mammo_counts)
   jp2@polygons$fill <- jamba::alpha2col(jp2@polygons$fill, alpha=0.6)
   jp2@polygons$innerborder <- "white";
   jp2@polygons$outerborder <- "white";
   jp2@polygons$label <- ""
   test_outside1 <- function() {
      label_outside_JamPolygon(jp2, do_plot=TRUE, min_degrees=15, buffer=-0.4)
   }
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger(
         "EulerGlyphs labels outside",
         test_outside1)
   }
   
})
