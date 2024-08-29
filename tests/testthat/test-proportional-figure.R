
test_that("venn_figure_3way", {
   # venndir()
   set.seed(123)
   run_venndir3 <- function() {
      vo5p <- venndir(setlist=list(set_A=LETTERS, set_B=LETTERS[1:10], set_C=LETTERS[6:7]),
         proportional=FALSE, do_plot=TRUE, center_method="label")
   }
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Venn 3-way, venndir()", run_venndir3)
   }
   
   # render_venndir()
   set.seed(123)
   vo5p <- venndir(setlist=list(set_A=LETTERS, set_B=LETTERS[1:10], set_C=LETTERS[6:7]),
      proportional=FALSE, do_plot=FALSE, center_method="label")
   run_render4 <- function() {
      render_venndir(vo5p)
   }
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Venn 3-way, render_venndir()", run_render4)
   }
})

test_that("venn_figure_4way_dir", {
   # proportional venndir()
   set.seed(123)
   run_venndir4 <- function() venndir(make_venn_test(100, 4, do_signed=TRUE))
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Venn 4-way, venndir()", run_venndir4)
   }
   
   # proportional render_venndir()
   set.seed(123)
   run_euler4 <- function() venndir(make_venn_test(100, 4, do_signed=TRUE), proportional=TRUE)
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Euler 4-way, venndir()", run_euler4)
   }
})

test_that("venn_figure_2way_dir", {
   # proportional venndir()
   set.seed(123)
   run_venndir2 <- function() venndir(make_venn_test(100, 2, do_signed=TRUE))
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Venn 2-way, venndir()", run_venndir2)
   }
   
   # proportional render_venndir()
   set.seed(123)
   run_euler2 <- function() venndir(make_venn_test(100, 2, do_signed=TRUE), proportional=TRUE)
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Euler 2-way, venndir()", run_euler2)
   }
})

test_that("proportional_nested_figure", {
   # proportional venndir()
   set.seed(123)
   run_venndir <- function() {
      vo5p <- venndir(setlist=list(set_A=LETTERS, set_B=LETTERS[1:10], set_C=LETTERS[6:7]),
         innerborder.lwd=0,
         proportional=TRUE, do_plot=TRUE, center_method="label")
   }
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Proportional, nested venndir()", run_venndir)
   }
   
   # proportional render_venndir()
   set.seed(123)
   vo5p <- venndir(setlist=list(set_A=LETTERS, set_B=LETTERS[1:10], set_C=LETTERS[6:7]),
      proportional=TRUE, do_plot=FALSE, center_method="label")
   run_vo5p <- function() {
      render_venndir(vo5p,
         innerborder.lwd=0)
   }
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Proportional, nested render_venndir()", run_vo5p)
   }
})
