
test_that("venn_meme_bix", {
   bix <- list(
      s="Stats",
      cs="Computer<br>Science",
      b="Biology",
      `s&b`="Biostatistics",
      `s&cs`="Data<br>Science",
      `cs&b`="Computational<br>Biology",
      `s&cs&b`="Bioinformatics")
   run_venn_meme_bix <- function() venn_meme(bix, item_cex=2)
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Venn Meme, BIx", run_venn_meme_bix)
   }
   
})
