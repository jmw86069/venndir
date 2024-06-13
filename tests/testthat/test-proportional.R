
context("venndir_proportional_tests")

test_that("proportional_nonoverlap", {
   setlist <- list(A=LETTERS[1:10], B=LETTERS[15:25])
   vo <- venndir(setlist, proportional=TRUE, do_plot=FALSE)
   # get label_df
   if (!"Venndir" %in% class(vo)) {
      vo <- vo$vo;
   }
   label_df <- vo@label_df;
   testthat::expect_true(
      is.numeric(label_df["A", "x"]) & !is.na(label_df["A", "x"]))
   testthat::expect_true(
      is.na(label_df["A&B", "x"]))

   testthat::expect_equal(
      label_df[c("A", "B", "A&B"), "venn_counts"],
      c(10, 11, 0))
})

test_that("proportional_withempty", {
   setlist <- list(A=LETTERS[1:10], B=LETTERS[15:25], C=character(0))
   vo1 <- venndir(setlist, proportional=FALSE, do_plot=FALSE)
   vo2 <- venndir(setlist, proportional=TRUE, do_plot=FALSE)
   if (!"Venndir" %in% class(vo2)) {
      vo2 <- vo2$vo;
   }
   label_df2 <- vo2@label_df;
   testthat::expect_equal(
      label_df2[c("A", "B", "A&B", "A&C", "B&C", "A&B&C"), "venn_counts"],
      c(10, 11, 0, 0, 0, 0))
})
