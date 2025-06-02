
context("venndir warnings")

test_that("venndir warnings 3-way Euler", {
   # test first case, eulerr does not show one overlap
   setlist <- make_venn_test(n_sets=3, n_items=110, sizes=c(20, 30, 10))
   v <- venndir(setlist, proportional=TRUE, do_plot=FALSE)
   wv <- warnings(v)
   testthat::expect_equal(class(wv), "warnings")
   testthat::expect_equal(names(wv), "2")
   testthat::expect_equal(as.character(wv), "set_A&set_C")
   
   # test corrected figure
   # Note: This step depends upon consistent eulerr() output.
   # If this step fails check whether eulerr produced a new layout.
   v2 <- venndir(setlist, proportional=TRUE, do_plot=FALSE,
      circle_nudge=list(set_A=c(0.3, 0.3)))
   wv2 <- warnings(v2)
   testthat::expect_equal(class(wv2), "warnings")
   testthat::expect_equal(length(wv2), 0)
   
   # test another manually adjusted figure
   # to produce two hidden overlaps
   v3 <- venndir(setlist, proportional=TRUE, do_plot=FALSE,
      circle_nudge=list(set_A=c(-5, 0)))
   wv3 <- warnings(v3)
   testthat::expect_equal(class(wv3), "warnings")
   testthat::expect_equal(length(wv3), 2)
   testthat::expect_equal(names(wv3), c("7", "2"))
   testthat::expect_equal(as.character(wv3), c("set_A&set_B", "set_A&set_C"))
})
