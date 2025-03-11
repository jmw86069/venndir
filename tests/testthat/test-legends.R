
context("venndir_legender_tests")

test_that("venn_legender_3way", {
   # venndir()
   set.seed(123)
   run_legender1 <- function() {
      setlist <- make_venn_test(100, 3, do_signed=TRUE)
      vo5p <- venndir(setlist=setlist,
         expand_fraction=c(0.2, 0, 0, 0),
         draw_legend=FALSE)
      venndir_legender(vo5p, x="topleft", legend_signed=FALSE)
      venndir_legender(vo5p, x="topright",
         legend_color_style=c("nofill", "blackborder"))
      venndir_legender(vo5p, x="left",
         legend_signed=FALSE,
         legend_color_style=c("nofill", "greyborder"))
      venndir_legender(vo5p, x="bottomright",
         combine_size=FALSE,
         legend_headers=c(Sign="Direction"),
         legend_color_style=c("greyfill", "greyborder"))
      venndir_legender(vo5p, x="bottomleft",
         combine_size=FALSE, combine_sign=FALSE,
         alias=setNames(LETTERS[1:3], names(setlist)),
         labels=setNames(paste0("Items present in ", LETTERS[1:3]),
            names(setlist)),
         legend_headers=c(Set="Set Name",
            Size="Count",
            Label="Description"),
         legend_percentage=TRUE)
   }
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Venn 3-way, venndir_legender()",
         run_legender1)
   }
})
