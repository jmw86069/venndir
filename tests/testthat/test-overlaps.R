
context("venndir overlaps")
# library(venndir)

test_that("signed_overlaps", {
   inputlist <- list(setA=factor(c("A", "B", "D")),
      setB=factor(c("A", "C", "E", "F")));
   so <- signed_overlaps(inputlist, return_items=TRUE)
   
   testthat::expect_equal(
      so$overlap,
      c("1 0", "0 1", "1 1"))
   
   testthat::expect_equal(
      so$count,
      c(2, 3, 1))
   
   so_items_expected <- list(
      `setA|1 0`=c("B", "D"),
      `setB|0 1`=c("C", "E", "F"),
      `setA&setB|1 1`=c("A"))
   testthat::expect_equal(
      so$items,
      base::I(so_items_expected))
})

test_that("signed_overlaps item order", {
   testlist <- list(A=c("two", "one"),
      B=c("item B"),
      C=c("2 items", "1 item"))
   so <- signed_overlaps(testlist,
      return_items=TRUE,
      keep_item_order=TRUE)
   testthat::expect_equal(
      lapply(unname(so$items[1:3]), function(i)as.character(sort(i))),
      unname(testlist))

   sorted_testlist <- list(testlist[[1]][2:1],
      testlist[[2]], testlist[[3]][2:1])
   so2 <- signed_overlaps(testlist,
      return_items=TRUE,
      keep_item_order=FALSE)
   testthat::expect_equal(
      lapply(unname(so2$items[1:3]), function(i)as.character(sort(i))),
      sorted_testlist)
})
