
context("venndir converters")
# library(venndir)

# has_Matrix <- suppressPackageStartupMessages(require(Matrix));

test_that("list2im_opt", {
   input_list <- list(A=letters[c(1,3,5,7,9)],
      B=letters[c(2,5,6,9,10)],
      C=letters[c(1,2,3,4,8)],
      D=character(0))
   ct <- c(a=2, c=2, e=2, g=1, i=2,
      b=2, f=1, j=1, d=1, h=1);
   im <- list2im_opt(input_list, do_sparse=FALSE)
   
   testthat::expect_mapequal(
      rowSums(im),
      ct)
   testthat::expect_mapequal(
      colSums(im),
      c(A=5, B=5, C=5, D=0))
   testthat::expect_setequal(
      rownames(im),
      letters[1:10])
})

test_that("list2im_opt_Matrix", {
   testthat::skip_if_not_installed("Matrix");
   {
      input_list <- list(A=letters[c(1,3,5,7,9)],
         B=letters[c(2,5,6,9,10)],
         C=letters[c(1,2,3,4,8)],
         D=character(0))
      ct <- c(a=2, c=2, e=2, g=1, i=2,
         b=2, f=1, j=1, d=1, h=1);
      im <- list2im_opt(input_list, do_sparse=TRUE)
      
      testthat::expect_equal(
         as.character(class(im)),
         "ngCMatrix")
      testthat::expect_mapequal(
         apply(im, 1, sum),
         ct)
      testthat::expect_mapequal(
         apply(im, 2, sum),
         c(A=5, B=5, C=5, D=0))
      testthat::expect_setequal(
         rownames(im),
         letters[1:10])
   }
})

test_that("im2list", {
   input_list <- list(A=letters[c(1,3,5,7,9)],
      B=letters[c(2,5,6,9,10)],
      C=letters[c(1,2,3,4,8)],
      D=character(0))
   ct <- c(a=2, c=2, e=2, g=1, i=2,
      b=2, f=1, j=1, d=1, h=1);
   im <- list2im_opt(input_list, do_sparse=FALSE)
   im <- im[sort(rownames(im)),,drop=FALSE]
   output_list <- im2list(im)
   
   testthat::expect_mapequal(
      input_list,
      output_list)
})

test_that("im2list_Matrix", {
   testthat::skip_if_not_installed("Matrix");
   {
      input_list <- list(A=letters[c(1,3,5,7,9)],
         B=letters[c(2,5,6,9,10)],
         C=letters[c(1,2,3,4,8)],
         D=character(0))
      ct <- c(a=2, c=2, e=2, g=1, i=2,
         b=2, f=1, j=1, d=1, h=1);
      im <- list2im_opt(input_list, do_sparse=TRUE)
      im <- im[sort(rownames(im)),,drop=FALSE]
      output_list <- im2list(im)
      
      testthat::expect_mapequal(
         input_list,
         output_list)
   }
})

test_that("list2im_value", {
   input_list <- list(A=letters[c(1,3,5,7,9)],
      B=letters[c(2,5,6,9,10)],
      C=letters[c(1,2,3,4,8)],
      D=character(0))
   input_valuelist <- lapply(input_list, function(i){
      j <- rep(c(1, -1), length.out=length(i));
      names(j) <- i;
      j;
   })
   im_value <- list2im_value(input_valuelist, do_sparse=FALSE)
   ct <- c(a=2, c=2, e=2, g=1, i=2,
      b=2, f=1, j=1, d=1, h=1);
   ct_value <- c(a=2, c=0, e=0, g=-1, i=0,
      b=0, f=1, j=1, d=-1, h=1);
   
   testthat::expect_mapequal(
      apply(abs(im_value), 1, sum),
      ct)
   testthat::expect_mapequal(
      apply(im_value, 1, sum),
      ct_value)
   testthat::expect_mapequal(
      apply(abs(im_value), 2, sum),
      c(A=5, B=5, C=5, D=0))
   testthat::expect_mapequal(
      apply(im_value, 2, sum),
      c(A=1, B=1, C=1, D=0))
   testthat::expect_setequal(
      rownames(im_value),
      letters[1:10])
})

test_that("list2im_value", {
   testthat::skip_if_not_installed("Matrix");
   {
      input_list <- list(A=letters[c(1,3,5,7,9)],
         B=letters[c(2,5,6,9,10)],
         C=letters[c(1,2,3,4,8)],
         D=character(0))
      input_valuelist <- lapply(input_list, function(i){
         j <- rep(c(1, -1), length.out=length(i));
         names(j) <- i;
         j;
      })
      im_value <- list2im_value(input_valuelist, do_sparse=TRUE)
      ct <- c(a=2, c=2, e=2, g=1, i=2,
         b=2, f=1, j=1, d=1, h=1);
      ct_value <- c(a=2, c=0, e=0, g=-1, i=0,
         b=0, f=1, j=1, d=-1, h=1);
      
      testthat::expect_mapequal(
         apply(abs(im_value), 1, sum),
         ct)
      testthat::expect_mapequal(
         apply(im_value, 1, sum),
         ct_value)
      testthat::expect_mapequal(
         apply(abs(im_value), 2, sum),
         c(A=5, B=5, C=5, D=0))
      testthat::expect_mapequal(
         apply(im_value, 2, sum),
         c(A=1, B=1, C=1, D=0))
      testthat::expect_setequal(
         rownames(im_value),
         letters[1:10])
   }
})

test_that("im_value2list", {
   input_list <- list(A=letters[c(1,3,5,7,9)],
      B=letters[c(2,5,6,9,10)],
      C=letters[c(1,2,3,4,8)],
      D=character(0))
   input_valuelist <- lapply(input_list, function(i){
      j <- rep(c(1, -1), length.out=length(i));
      names(j) <- i;
      j;
   })
   im_value <- list2im_value(input_valuelist, do_sparse=FALSE)
   im_value <- im_value[sort(rownames(im_value)),,drop=FALSE]
   output_valuelist <- im_value2list(im_value)
   
   testthat::expect_mapequal(
      input_valuelist,
      output_valuelist)
})

test_that("im_value2list_Matrix", {
   testthat::skip_if_not_installed("Matrix");
   {
      input_list <- list(A=letters[c(1,3,5,7,9)],
         B=letters[c(2,5,6,9,10)],
         C=letters[c(1,2,3,4,8)],
         D=character(0))
      input_valuelist <- lapply(input_list, function(i){
         j <- rep(c(1, -1), length.out=length(i));
         names(j) <- i;
         j;
      })
      im_value <- list2im_value(input_valuelist, do_sparse=TRUE)
      im_value <- im_value[sort(rownames(im_value)),,drop=FALSE]
      output_valuelist <- im_value2list(im_value)
      
      testthat::expect_mapequal(
         input_valuelist,
         output_valuelist)
   }
})

test_that("counts2setlist", {
   countlist_test <- list(Am=0,
      Bm=0,
      `Am&Bm`=16);
   peaklist_test <- counts2setlist(countlist_test)

   testthat::expect_mapequal(
      lengths(peaklist_test),
      c(Am=16, Bm=16))
   
   so_test <- signed_overlaps(peaklist_test)
   so_count <- so_test$count;
   names(so_count) <- so_test$sets;
   so_count;
   testthat::expect_mapequal(
      so_count,
      c(Am=0, Bm=0, `Am&Bm`=16))
})
