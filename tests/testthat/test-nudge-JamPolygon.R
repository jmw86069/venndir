
test_that("nudge_JamPolygon", {
   DEdf <- data.frame(check.names=FALSE,
      name=c("D", "E"),
      x=I(list(
         c(-3, 3, 3, 0, -3),
         c(-4, 2, 2, -4))),
      y=I(list(
         c(-3, -3, 1.5, 4, 1.5),
         c(-2, -2, 4, 4))),
      fill=c("#FFD70055", "#B2222255"))
   DEjp <- new("JamPolygon", polygons=DEdf)

   nudge <- list(D=c(7, 1), E=c(-1, -1));
   DEjp_nudged <- nudge_JamPolygon(DEjp, nudge=nudge)
   expect_equal(
      DEjp_nudged@polygons[, "x"],
      I(list(c(4, 10, 10, 7, 4), c(-5, 1, 1, -5))))

   DEjp_scaled <- nudge_JamPolygon(DEjp, scale=c(D=2))
   expect_equal(
      DEjp_scaled@polygons[, "x"],
      I(list(list(c(-5.5, 6.5, 6.5, 0.5, -5.5)), c(-4, 2, 2, -4))))

   DEjp_rotated <- nudge_JamPolygon(DEjp, rotate_degrees=c(D=45), scale=c(E=0.8))
   expect_equal(
      rapply(DEjp_rotated@polygons[, "x"], how="list",
         f=function(i)round(i, digits=2)),
      list(list(c(-4.74, -0.50, 2.68, 2.33, -1.56)),
         list(c(-3.3, 1.5, 1.5, -3.3))))
   
})
