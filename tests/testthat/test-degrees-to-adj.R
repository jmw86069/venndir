
test_that("degrees_to_adj", {
   degrees <- seq(from=1, to=360, by=15);
   adjdf <- degrees_to_adj(degrees, bias_side=3);
   test_adjdf <- data.frame(adjx=rep(c(0.0, 0.5, 1, 0.5, 0.0),
      c(6, 1, 11, 1, 5)),
      adjy=rep(c(0.5, 0.0, 0.5, 1.0),
         c(1, 11, 1, 11)))
   expect_equal(
      adjdf,
      test_adjdf)

   adjdf1 <- degrees_to_adj(degrees, bias_side=1);
   test_adjdf1 <- data.frame(adjx=rep(c(0.0, 0.5, 1, 0.5, 0.0),
      c(5, 3, 9, 3, 4)),
      adjy=rep(c(0.5, 0.0, 0.5, 1.0, 0.5),
         c(2, 9, 3, 9, 1)))
   expect_equal(
      adjdf1,
      test_adjdf1)
})
