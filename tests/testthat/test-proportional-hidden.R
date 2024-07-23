
context("venndir_proportional_hidden")

# Test proportional diagram for which there is one overlap without a polygon.
# Note: This test is dependent upon consistent output from eulerr::eulerr().
# In future I could supply a custom set of JamPolygons.

test_that("proportional_hidden", {
   ## Test proportional with hidden overlap counts
   overlaps <- c(A=187, B=146, C=499,
      `A&B`=1,
      `A&C`=181,
      `B&C`=219,
      `A&B&C`=20);
   # convert to setlist
   setlist_o <- counts2setlist(overlaps)

   set.seed(123)
   vn <- venndir(setlist_o,
      proportional=TRUE,
      do_plot=FALSE,
      show_labels="ncs",
      set_colors=c("firebrick2", "dodgerblue", "#9999AA"))
   testnames <- c("A", "B", "C", "A&B", "A&C", "B&C", "A&B&C")
   exp_count <- c("inside", "none")[c(1, 1, 1, 2, 1, 1, 1)]
   # expect "count" column to have "none" in the correct position
   testthat::expect_equal(
      vn@label_df[testnames, "count"],
      exp_count)

   testthat::expect_equal(
      vn@label_df[testnames, "venn_counts"],
      c(187, 146, 499, 1, 181, 219, 20))
   
   ## make signed setlist
   setlist_os <- lapply(setlist_o, function(i){
      setNames(rep(c(-1, 1), length.out=length(i)), i)
   })
   set.seed(123)
   vns <- venndir(setlist_os,
      proportional=TRUE,
      overlap_type="each",
      do_plot=FALSE,
      show_labels="ncs",
      set_colors=c("firebrick2", "dodgerblue", "#9999AA"))
   testnames_s <- c("A", "B", "C", "A&B", "A&C", "B&C", "A&B&C",
      "A.1", "A.-1", "B.1", "B.-1", "C.1", "C.-1",
      "A&B.1 -1", "A&C.1 -1", "A&C.-1 1", "B&C.1 -1", "B&C.-1 1",
      "A&B&C.1 -1 1", "A&B&C.-1 1 -1")
   exp_count_s <- c("inside", "none")[c(1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      2, 1, 1, 1, 1, 1, 1)]
   # expect "count" column to have "none" in the correct position
   testthat::expect_equal(
      vns@label_df[testnames_s, "count"],
      exp_count_s)
   exp_venn_counts_s <- c(187, 146, 499, 1, 181, 219, 20, 93, 94,
      73, 73, 249, 250, 1, 90, 91, 110, 109, 10, 10)
   # expect "venn_counts" column values
   testthat::expect_equal(
      vns@label_df[testnames_s, "venn_counts"],
      exp_venn_counts_s)
   
})

