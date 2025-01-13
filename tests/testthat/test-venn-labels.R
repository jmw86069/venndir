# test spacing between overlap/count/signed label components

test_that("venn_labels_3way_tall", {
   set.seed(123)
   run_venndir5 <- function() 
      venndir(make_venn_test(10000, 3, do_signed=TRUE), template="tall", label_style="lite box")
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Venn 3-way labels, template='tall'", run_venndir5)
   }
})

test_that("venn_labels_3way_wide", {
   set.seed(123)
   run_venndir5wide <- function() {
      venndir(make_venn_test(10000, 3, do_signed=TRUE), template="wide", label_style="lite box")
   }
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Venn 3-way labels, template='wide'", run_venndir5wide)
   }
})

test_that("venn_labels_3way_wide_0space", {
   set.seed(123)
   run_venndir5nospace <- function() {
      venndir(make_venn_test(10000, 3, do_signed=TRUE),
         label_borders=list(overlap=grid::unit(0, "mm"), count=grid::unit(0, "mm"), signed=grid::unit(0, "mm")),
         template="wide", label_style="lite box")
   }
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Venn 3-way labels, template='wide', no space", run_venndir5nospace)
   }
})

test_that("venn_labels_3way_wide_0space_pct", {
   set.seed(123)
   run_venndir5nospace <- function() {
      venndir(make_venn_test(10000, 3, do_signed=TRUE), show_labels="ncps",
         label_borders=list(overlap=grid::unit(0, "mm"), count=grid::unit(0, "mm"), signed=grid::unit(0, "mm")),
         template="wide", label_style="lite box")
   }
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Venn 3-way labels, template='wide', no space, percent", run_venndir5nospace)
   }
})
