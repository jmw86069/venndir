
context("venndir spatial")
# library(venndir)

test_that("degrees_to_adj", {
   df <- data.frame(adjx=c(0, 0.5, 1, 0.5, 0),
      adjy=c(0.5, 0, 0.5, 1, 0.5));
   expect_equal(
      degrees_to_adj(c(0, 90, 180, 270, 361)),
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
   expect_equivalent(
      round(digits=1, mean_degree_arc(c(45, 12))),
      208.5)
   expect_equivalent(
      round(digits=1, mean_degree_arc(c(350, 12, 45))),
      15.7)
   expect_equivalent(
      round(digits=1, mean_degree_arc(c(350, 45, 185))),
      73.3)
})

