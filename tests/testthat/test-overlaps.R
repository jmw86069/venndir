
context("venndir overlaps")
library(venndir)

test_that("signed_overlaps", {
   inputlist <- list(setA=factor(c("A", "B", "D")),
      setB=factor(c("A", "C", "E", "F")));
   so <- signed_overlaps(inputlist, return_items=TRUE)
   
   expect_equal(so$overlap, c("1 0", "0 1", "1 1"))
   
   expect_equal(so$count, c(2, 3, 1))
   
   so_items_expected <- list(
      `setA|1 0`=c("B", "D"),
      `setB|0 1`=c("C", "E", "F"),
      `setA&setB|1 1`=c("A"))
   expect_equal(so$items, so_items_expected)
})

