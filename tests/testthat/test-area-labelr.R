
test_that("area labelr JamPolygon", {
   # test data has two rectangles with holes
   df3 <- data.frame(name=c("polygon1", "polygon2"),
      label=c("polygon1", "polygon2"),
      x=I(list(
         list(c(1, 6, 6, 1),
            c(2, 5, 5, 2),
            c(3, 4, 4, 3)),
         list(
            c(9.6-1, 17.5, 17.5, 9.6-1),
            c(10, 17, 17, 10))
      )),
      y=I(list(
         list(c(1, 1, 6, 6),
            c(2, 2, 5, 5),
            c(3, 3, 4, 4)),
         list(
            c(2, 2, 5, 5),
            c(3, 3, 4, 4))
      )),
      fill=c("gold", "red4"))
   jp3 <- new("JamPolygon", polygons=df3);
   expect_equal(
      area_JamPolygon(jp3, return_list=TRUE),
      list(c(25, -9, 1), c(26.7, -7)))
   # confirm label coordinates
   expect_setequal(
      round(labelr_JamPolygon(jp3), digits=3),
      matrix(c(1.625, 9.35, 1.625, 2.75), ncol=2))
   
   # combine into one JamPolygon
   jp3u <- union_JamPolygon(jp3)
   expect_equal(
      area_JamPolygon(jp3u, return_list=TRUE),
      list(c(25, -9, 26.7, 1, -7)))
   
   # confirm label coordinates
   expect_setequal(
      round(labelr_JamPolygon(jp3u), digits=3),
      matrix(c(9.35, 2.75), ncol=2))
})
