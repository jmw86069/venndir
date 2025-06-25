
test_that("venn_figure_3way_svglite", {
   set.seed(123)
   run_venndir3 <- function() {
      vo5p <- venndir(setlist=list(set_A=LETTERS, set_B=LETTERS[1:10], set_C=LETTERS[6:7]),
         proportional=FALSE, do_plot=TRUE, center_method="label")
   }
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Venn 3-way, venndir()",
         run_venndir3,
         writer=write_svg_with_svglite)
   }
})

test_that("venn_fonts_svglite", {
   set.seed(123)
   run_venndir3_font <- function() {
      vo5p <- venndir(
         setlist=list(set_A=LETTERS,
            set_B=LETTERS[1:10],
            set_C=LETTERS[6:7]),
         fontfamily="Optima", font_cex=2, legend_font_cex=1.5,
         proportional=FALSE, do_plot=TRUE, center_method="label")
   }
   if (jamba::check_pkg_installed("vdiffr") &&
         requireNamespace("svglite", quietly=TRUE)) {
      vdiffr::expect_doppelganger("Venn 3-way, Optima, venndir()",
         run_venndir3_font,
         writer=write_svg_with_svglite)
   }
})

test_that("venn_fontfamilies_svglite", {
   set.seed(123)
   run_venndir3_font <- function() {
      vo5p <- venndir(
         setlist=list(set_A=LETTERS,
            set_B=LETTERS[1:10],
            set_C=LETTERS[6:7]),
         fontfamilies=list(signed="ArialBlack",
            count="Times",
            overlap="Marker Felt"),
         fontfamily="Optima", font_cex=2, legend_font_cex=1.5,
         proportional=FALSE, do_plot=TRUE, center_method="label")
   }
   if (jamba::check_pkg_installed("vdiffr") &&
         requireNamespace("svglite", quietly=TRUE)) {
      vdiffr::expect_doppelganger("Venn 3-way, fontfamilies, venndir()",
         run_venndir3_font,
         writer=write_svg_with_svglite)
   }
})
