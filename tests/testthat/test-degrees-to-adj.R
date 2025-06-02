
test_that("degrees_to_adj", {
   degrees <- seq(from=1, to=360, by=15);
   adjdf <- degrees_to_adj(degrees, bias_side=3, bias_height=3, do_fractional=FALSE);
   test_adjdf <- data.frame(adjx=rep(c(0.0, 0.5, 1, 0.5, 0.0),
      c(6, 1, 11, 1, 5)),
      adjy=rep(c(0.5, 0.0, 0.5, 1.0),
         c(1, 11, 1, 11)))
   expect_equal(
      adjdf,
      test_adjdf)

   adjdf1 <- degrees_to_adj(degrees, bias_side=1, bias_height=1, do_fractional=FALSE);
   test_adjdf1 <- data.frame(adjx=rep(c(0.0, 0.5, 1, 0.5, 0.0),
      c(5, 3, 9, 3, 4)),
      adjy=rep(c(0.5, 0.0, 0.5, 1.0, 0.5),
         c(2, 9, 3, 9, 1)))
   expect_equal(
      adjdf1,
      test_adjdf1)

   adjdfF <- degrees_to_adj(degrees, do_fractional=TRUE);
   test_adjdfF <- data.frame(
      adjx=rep(c(0.00, 0.01, 0.16, 0.33, 0.51, 0.69, 0.86, 1,
         0.99, 0.84, 0.67, 0.49, 0.31, 0.14, 0.00),
      c(3, 1, 1, 1, 1, 1, 1, 6,
         1, 1, 1, 1, 1, 1, 3)),
      adjy=rep(c(0.49, 0.31, 0.14, 0.00,
         0.01, 0.16, 0.33, 0.51, 0.69, 0.86,
         1.00, 0.99, 0.84, 0.67),
         c(1, 1, 1, 6, 1, 1, 1,
            1, 1, 1, 6, 1, 1, 1)))
   expect_equal(
      adjdfF,
      test_adjdfF)
   
})
