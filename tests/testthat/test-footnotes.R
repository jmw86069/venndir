
context("venndir footnotes")

# Note: This test is dependent upon eulerr providing the same solution,
# which should be consistent with set.seed(123) except that
# eulerr could eventually update its algorithm.

test_that("has_footnote", {
   set.seed(123)
   setlist3 <- make_venn_test(500, n_sets=4)
   v3 <- venndir(setlist3,
      draw_legend=FALSE,
      do_plot=FALSE,
      proportional=TRUE)
   vf3 <- footnotes(v3)
   
   testthat::expect_equal(vf3$symbol,
      "\u2020")
   testthat::expect_equal(vf3$type,
      "hidden overlap")
   testthat::expect_equal(vf3$note,
      "4 overlaps cannot be displayed.")
})

test_that("plot_footnotes", {
   foot_fn <- function() {
      set.seed(123)
      setlist3 <- make_venn_test(500, n_sets=4)
      v3 <- venndir(setlist3,
         draw_legend=TRUE,
         footnote_style="symbol",
         footnote_fontsize=12,
         proportional=TRUE)
      render_venndir_footnotes(v3,
         x="bottomleft",
         footnote_fontfamily="serif",
         footnote_y_inset=grid::unit(0.5, "lines"),
         footnote_x_inset=grid::unit(0, "lines"),
         footnote_color="black")
      render_venndir_footnotes(v3,
         x="topleft",
         footnote_style="header")
      render_venndir_footnotes(v3,
         x="bottomleft",
         footnote_fontfamily="serif",
         footnote_y_inset=grid::unit(3.5, "lines"),
         footnote_style="header")
      render_venndir_footnotes(v3,
         x="bottomleft",
         footnote_y_inset=grid::unit(2.5, "lines"),
         footnote_style="header")
      render_venndir_footnotes(v3,
         x="bottomleft",
         footnote_fontsize=12,
         footnote_fontfamily="serif",
         footnote_y_inset=grid::unit(1.5, "lines"),
         footnote_style="footnote")
   }
   if (requireNamespace("vdiffr", quietly=TRUE)) {
      vdiffr::expect_doppelganger("render footnotes", foot_fn)
   }
})
