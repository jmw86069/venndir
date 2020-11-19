
context("venndir utility")
library(venndir)

test_that("expand_range", {
   expect_equal(expand_range(x, 0.1), c(-0.5, 10.5))
   expect_equal(expand_range(x, c(0.1, 0)), c(-1, 10))
   expect_equal(expand_range(1, minimum_range=1), c(0.5, 1.5))
   expect_equal(expand_range(1, minimum_range=c(1, 0)), c(0, 1))
   expect_equal(
      expand_range(list(xlim=c(1, 10), ylim=c(1, 100))),
      list(xlim=c(0.55, 10.45), ylim=c(-3.95, 104.95)))
})

test_that("degrees_to_adj", {
   df <- data.frame(adjx=c(0, 0.5, 1, 0.5, 0),
      adjy=c(0.5, 0, 0.5, 1, 0.5));
   expect_equal(
      degrees_to_adj(c(0, 90, 180, 270, 361)),
      df)
})
