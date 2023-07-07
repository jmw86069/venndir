
context("venndir curate_venn_labels")

test_that("curate_venn_labels non-unicode", {
   x <- c("1 1", "-1 1", "1 -1", "-1 -1");
   x_expected <- c("^^", "v^", "^v", "vv")
   testthat::expect_equal(
      curate_venn_labels(x, unicode=0),
      x_expected)
})

test_that("curate_venn_labels unicode", {
   x <- c("1 1", "-1 1", "1 -1", "-1 -1", "", "1");
   x_expected_unicode <- c("\u2191\u2191", "\u2193\u2191",
      "\u2191\u2193", "\u2193\u2193", "", "\u2191")
   testthat::expect_equal(
      curate_venn_labels(x, unicode=1),
      x_expected_unicode)
})

test_that("curate_venn_labels custom", {
   x <- c("1 1", "-1 1", "1 -1", "-1 -1");
   x_expected_custom <- c("UpUp", "DownUp",
      "UpDown", "DownDown")
   
   curate_df <- data.frame(from=c(-1, 1),
      sign=c("Down", "Up"),
      color=c("dodgerblue3", "firebrick"))
   testthat::expect_equal(
      curate_venn_labels(x, curate_df=curate_df),
      x_expected_custom)
})

test_that("curate_venn_labels reverse custom", {
   # this test uses "1" as the first pattern match,
   # to confirm that it will not match "-1" and result in "-Up"
   # it should only result in "Down"
   x <- c("1 1", "-1 1", "1 -1", "-1 -1");
   x_expected_custom <- c("UpUp", "DownUp",
      "UpDown", "DownDown")
   curate_df2 <- data.frame(from=c(1, -1),
      sign=c("Up", "Down"),
      color=c("firebrick", "dodgerblue3"))
   testthat::expect_equal(
      curate_venn_labels(x, curate_df=curate_df2),
      x_expected_custom)
})

# curate_list <- list(
#    c("-1", "Down", "dodgerblue3"),
#    c("1", "Up", "firebrick"));
# curate_df <- data.frame(do.call(rbind, curate_list))
# colnames(curate_df) <- c("from", "sign", "color")
# curate_df
