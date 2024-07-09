
test_that("venn_dupe_names", {
   setlist <- list(A=LETTERS[1:10], B=LETTERS[15:25], A=LETTERS[c(1,2,3,22,26)])
   vo <- venndir(setlist, do_plot=FALSE)
   # get label_df
   label_df <- vo@label_df;

   testthat::expect_equal(
      label_df[c("A_v1", "B", "A_v2"), "venn_counts"],
      c(7, 10, 1))
   testthat::expect_equal(
      label_df[c("A_v1&B", "A_v1&A_v2", "B&A_v2", "A_v1&B&A_v2"), "venn_counts"],
      c(0, 3, 1, 0))
})

test_that("venn_NA_names", {
   setlist <- list(A=LETTERS[1:10], B=LETTERS[15:25], A=LETTERS[c(1,2,3,22,26)])
   names(setlist)[3] <- NA;
   vo <- venndir(setlist, do_plot=FALSE)
   # get label_df
   label_df <- vo@label_df;

   testthat::expect_equal(
      label_df[c("A", "B", "set"), "venn_counts"],
      c(7, 10, 1))
   testthat::expect_equal(
      label_df[c("A&B", "A&set", "B&set", "A&B&set"), "venn_counts"],
      c(0, 3, 1, 0))
   
   setlist <- list(A=LETTERS[1:10], B=LETTERS[15:25], A=LETTERS[c(1,2,3,22,26)])
   names(setlist)[c(1, 3)] <- NA;
   vo <- venndir(setlist, do_plot=FALSE)
   label_df <- vo@label_df;

   testthat::expect_equal(
      label_df[c("set_v1", "B", "set_v2"), "venn_counts"],
      c(7, 10, 1))
   testthat::expect_equal(
      label_df[c("set_v1&B", "set_v1&set_v2", "B&set_v2", "set_v1&B&set_v2"), "venn_counts"],
      c(0, 3, 1, 0))
   
})
