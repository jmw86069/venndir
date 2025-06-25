
test_that("venndir-marquee-styles", {
   run_venndir_styles <- function(){
      #
      ms <- list(
         chalk=marquee::style(
            family="Chalkduster",
            weight="normal", width="normal",
            size=marquee::relative(1.5)),
         cursive=marquee::style(
            family="Above The Sky",
            weight="normal", width="normal",
            size=marquee::relative(1.5)),
         gothic=marquee::style(
            family="AcademyEngravedLetPlain",
            weight="normal", width="normal",
            size=marquee::relative(1.5)))
      il <- split(LETTERS, rep(letters[1:3], c(10, 10, 6)))
      # apply inline markup around each label
      il[[1]] <- paste0("{.chalk ", il[[1]], "}")
      il[[2]] <- paste0("{.gothic ", il[[2]], "}")
      il[[3]] <- paste0("{.cursive ", il[[3]], "}")
      
      names(il) <- c("{.chalk A-J}", "{.gothic K-T}", "{.cursive U-Z}")
      vm <- venn_meme(il, item_buffer=-0.05, marquee_styles=ms,
         expand_fraction=0.02,
         fontfamily="Chalkduster",
         show_labels="Ni", draw_legend=TRUE,
         legend_headers=c(Set="{.cursive Sets}", Size="{.chalk Sizes}"),
         main="{.chalk Custom} {.gothic Text} {.cursive Styles}")
   }
   
   if (jamba::check_pkg_installed("vdiffr")) {
      vdiffr::expect_doppelganger("Venndir marquee styles",
         run_venndir_styles,
         writer=write_svg_with_svglite)
   }

   # note this test requires the svglite workaround to render images
   run_inline_image <- function(){
      img <- system.file(package="venndir", "images", "venndir-transparent.png")
      imgtag <- marquee::marquee_glue("![]({img})")
      venn_meme(c("{.gold3 Venn}", "{.#6799AC dir}", imgtag),
         item_cex_factor=c(1.5, 1.5, 7),
         innerborder.lwd=1, outerborder.lwd=1,
         poly_alpha=0.5,
         set_colors=c("#EEDD79", "#87B9CC"))
   }
   if (jamba::check_pkg_installed("vdiffr") &&
         requireNamespace("svglite", quietly=TRUE)) {
      vdiffr::expect_doppelganger("Venndir inline image",
         run_inline_image,
         writer=write_svg_with_svglite)
   }
   
})
