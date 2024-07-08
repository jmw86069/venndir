
context("venndir_proportional_tests")

test_that("proportional_nonoverlap", {
   setlist <- list(A=LETTERS[1:10], B=LETTERS[15:25])
   vo <- venndir(setlist, proportional=TRUE, do_plot=FALSE)
   # get label_df
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
   # one empty set
   setlist <- list(A=LETTERS[1:10], B=LETTERS[15:25], C=character(0))
   vo1 <- venndir(setlist, proportional=FALSE, do_plot=FALSE)
   vo2 <- venndir(setlist, proportional=TRUE, do_plot=FALSE)
   label_df2 <- vo2@label_df;
   testthat::expect_equal(
      label_df2[c("A", "B", "A&B", "A&C", "B&C", "A&B&C"), "venn_counts"],
      c(10, 11, 0, 0, 0, 0))
   
   # nested with empty middle set
   setlist <- list(A=LETTERS[1:2], B=character(0), C=LETTERS[1:10])
   vo1 <- venndir(setlist, proportional=FALSE, do_plot=FALSE)
   label_df1 <- vo1@label_df;
   testthat::expect_equal(
      label_df1[c("A", "B", "C", "A&B", "A&C", "B&C", "A&B&C"), "venn_counts"],
      c(0, 0, 8, 0, 2, 0, 0))
   vo2 <- venndir(setlist[c(1, 2, 3)], proportional=TRUE, do_plot=FALSE)
   label_df2 <- vo2@label_df;
   testthat::expect_equal(
      label_df2[c("A", "B", "C", "A&B", "A&C", "B&C", "A&B&C"), "venn_counts"],
      c(0, 0, 8, 0, 2, 0, 0))
   
   # disjoint with empty middle set
   setlist <- list(A=LETTERS[1:10], B=character(0), C=LETTERS[11:21])
   vo1 <- venndir(setlist, proportional=FALSE, do_plot=FALSE)
   label_df1 <- vo1@label_df;
   testthat::expect_equal(
      label_df1[c("A", "B", "C", "A&B", "A&C", "B&C", "A&B&C"), "venn_counts"],
      c(10, 0, 11, 0, 0, 0, 0))
   vo2 <- venndir(setlist, proportional=TRUE, do_plot=FALSE)
   label_df2 <- vo2@label_df;
   testthat::expect_equal(
      label_df2[c("A", "B", "C", "A&B", "A&C", "B&C", "A&B&C"), "venn_counts"],
      c(10, 0, 11, 0, 0, 0, 0))
})

test_that("proportional_nested", {
   # proportional nested
   vo2p <- venndir(setlist=list(set_A=LETTERS, set_B=LETTERS[1:5]),
      proportional=TRUE, do_plot=FALSE)
   ko <- c("set_A|set", "set_B|set", "set_A", "set_B", "set_A&set_B")
   testthat::expect_equal(
      vo2p@jps@polygons[ko, "venn_counts"],
      c(NA, NA, 21, 0, 5))

   # reverse order of internal sets
   vo4p <- venndir(setlist=list(set_A=LETTERS[1:5], set_B=LETTERS),
      proportional=TRUE, do_plot=FALSE)
   ko <- c("set_A|set", "set_B|set", "set_A", "set_B", "set_A&set_B")
   testthat::expect_equal(
      vo4p@jps@polygons[ko, "venn_counts"],
      c(NA, NA, 0, 21, 5))
   
   # two internal nested sets
   set.seed(123)
   vo5p <- venndir(setlist=list(set_A=LETTERS, set_B=LETTERS[1:10], set_C=LETTERS[6:7]),
      proportional=TRUE, do_plot=FALSE)
   ko <- c("set_A|set", "set_B|set", "set_C|set", "set_A", "set_B", "set_C",
      "set_A&set_B", "set_A&set_C", "set_B&set_C", "set_A&set_B&set_C")
   testthat::expect_equal(
      vo5p@jps@polygons[ko, "venn_counts"],
      c(NA, NA, NA, 16, 0, 0, 8, 0, 0, 2))
   
   # two internal partial nested sets
   vo6p <- venndir(setlist=list(set_A=LETTERS, set_B=LETTERS[1:10], set_C=LETTERS[7:11]), proportional=TRUE, do_plot=FALSE)
   ko <- c("set_A|set", "set_B|set", "set_C|set", "set_A", "set_B", "set_C",
      "set_A&set_B", "set_A&set_C", "set_B&set_C", "set_A&set_B&set_C")
   testthat::expect_equal(
      vo6p@jps@polygons[ko, "venn_counts"],
      c(NA, NA, NA, 15, 0, 0, 6, 1, 0, 4))
   
   # weirdly nested sets
   vo7p <- venndir(setlist=list(set_A=LETTERS[1:15], set_B=LETTERS[1:10], set_C=LETTERS[10:16]), proportional=TRUE, do_plot=FALSE)
   ko <- c("set_A|set", "set_B|set", "set_C|set", "set_A", "set_B", "set_C",
      "set_A&set_B", "set_A&set_C", "set_B&set_C", "set_A&set_B&set_C")
   testthat::expect_equal(
      vo7p@jps@polygons[ko, "venn_counts"],
      c(NA, NA, NA, 0, 0, 1, 9, 5, 0, 1))
   
   # weirdly nested sets, shape="ellipse"
   vo7ps <- venndir(setlist=list(set_A=LETTERS[1:15], set_B=LETTERS[1:10], set_C=LETTERS[10:16]), shape="ellipse", proportional=TRUE, do_plot=FALSE)
   ko <- c("set_A|set", "set_B|set", "set_C|set", "set_A", "set_B", "set_C",
      "set_A&set_B", "set_A&set_C", "set_B&set_C", "set_A&set_B&set_C")
   testthat::expect_equal(
      vo7ps@jps@polygons[ko, "venn_counts"],
      c(NA, NA, NA, 0, 0, 1, 9, 5, 0, 1))
   
})
