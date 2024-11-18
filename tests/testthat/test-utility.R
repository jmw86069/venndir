
# context("venndir utility")
# library(venndir)

test_that("expand_range", {
   x <- c(0, 10);
   expect_equal(expand_range(x, 0.1), c(-0.5, 10.5))
   expect_equal(expand_range(x, c(0.1, 0)), c(-1, 10))
   expect_equal(expand_range(1, minimum_range=1), c(0.5, 1.5))
   expect_equal(expand_range(1, minimum_range=c(1, 0)), c(0, 1))
   expect_equal(
      expand_range(list(xlim=c(1, 10), ylim=c(1, 100))),
      list(xlim=c(0.55, 10.45), ylim=c(-3.95, 104.95)))
})

test_that("make_venn_combn_df", {
   cdf3 <- make_venn_combn_df(letters[c(1,2,3)]);
   expect_equal(
      nrow(cdf3),
      7)
   expect_equal(
      ncol(cdf3),
      3)
   cdf5 <- make_venn_combn_df(letters[c(1,2,3,4,5)]);
   expect_equal(
      nrow(cdf5),
      31)
   expect_equal(
      ncol(cdf5),
      5)
   cdf6z <- make_venn_combn_df(letters[c(1,2,3,4,5,6)],
      include_zero=TRUE);
   expect_equal(
      nrow(cdf6z),
      64)
   expect_equal(
      ncol(cdf6z),
      6)
})

test_that("make_color_contrast", {
   mcc <- make_color_contrast(c("red", "blue"), c("red", "blue"),
      L_hi=85, L_lo=40, L_threshold=65, C_floor=80);
   testthat::expect_equal(
      mcc,
      c("#FFB5B5", "#CBCBFF"))
})

test_that("match_list", {
   l1 <- list(A=c(1,5,7,9),
      B=letters[c(3,5,7,8)],
      C=c(1,2,9));
   l2 <- list(
      E=letters[c(3,5,7,8)],
      F=c(1,9,2),
      F2=c(1,2,9,9),
      D=c(1,5,7,9),
      G=c(9))
   testthat::expect_equal(
      match_list(l2, l1),
      c(E=2, F=3, F2=NA, D=1, G=NA))
   testthat::expect_equal(
      match_list(l1, l2),
      c(A=4, B=1, C=2))
})

