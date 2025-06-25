
# test to confirm marquee is functional without crashing R
# in rare edge condition

test_that("marquee_no_crashy", {
   test_fonts <- c("sans",
      "Arial",
      "Helvetica",
      "serif",
      "Times",
      "Consolas",
      "Menlo");
   test_y <- head(tail(
      seq(from=1, to=0, length.out=length(test_fonts) + 2),
      -1), -1);
   mglist <- lapply(seq_along(test_fonts), function(i){
      ifont <- test_fonts[i];
      use_style_set <- marquee::classic_style(body_font=ifont);
      mg <- marquee::marquee_grob(
         text=paste0(
            "\u2191\u2193 Set **\u2191\u2193Label** Here (", ifont, ") marquee_grob"),
         x=0.3,
         y=test_y[i],
         style=use_style_set);
      pg <- grid::pointsGrob(x=0.3,
         default.units="npc",
         y=test_y[i],
         pch=20,
         gp=grid::gpar(col="blue"))
      grid::gList(mg, pg)
   })
   gglist <- lapply(seq_along(test_fonts), function(i){
      ifont <- test_fonts[i];
      withr::with_options(list(warn=FALSE),{
         mg <- grid::textGrob(
            label=paste0(
               "\u2191\u2193 Set \u2191\u2193Label Here (", ifont, ") textGrob"),
            x=0.3,
            y=test_y[i],
            hjust=0, vjust=0,
            gp=grid::gpar(fontfamily=ifont))
      })
      pg <- grid::pointsGrob(x=0.3,
         default.units="npc",
         y=test_y[i],
         pch=20,
         size=grid::unit(0.5, "char"),
         gp=grid::gpar(col="red"))
      grid::gList(mg, pg)
   })
   test_grobs <- do.call(grid::gList, c(mglist, gglist));
   vdiffr::expect_doppelganger("marquee_grob() test fonts", {
      grid::grid.newpage();
      grid::grid.draw(test_grobs);
   })
   # writer=write_svg_with_svglite)
})
   

## test function
test_assembly <- function(type, overlap=FALSE, cex=1, ...) {
   overlap_labels <- NULL;
   if (overlap) {
      overlap_labels <- c("Set Name", "sub, caption")
   }
   signed_labels <- c("\u2191\u2191 15",
      "\u2193\u2193 2,234",
      paste0("**", type, "** P,q"));
   if ("textGrob" %in% type) {
      signed_labels <- gsub("[*]", "", signed_labels)
   }
   fontsizes <- list(signed=12 * cex,
      count=c(16, 12) * cex,
      overlap=16 * cex);
   vlm <- assemble_venndir_label(
      text_grob_type=type,
      count_labels=c("1,234", "12%"),
      signed_labels=signed_labels,
      overlap_labels=overlap_labels,
      fontsizes=list(signed=12 * cex,
         count=c(16, 12) * cex,
         overlap=16 * cex),
      debug="overlap")
}


test_that("assemble_textGrob", {
   vdiffr::expect_doppelganger(
      "assemble_venndir_label() using textGrob", {
         test_assembly("textGrob", FALSE, 1)
      })
   vdiffr::expect_doppelganger(
      "assemble_venndir_label() using textGrob larger", {
         test_assembly("textGrob", FALSE, 2)
      })
   vdiffr::expect_doppelganger(
      "assemble_venndir_label() using textGrob, title", {
         test_assembly("textGrob", TRUE, 1)
      })
   vdiffr::expect_doppelganger(
      "assemble_venndir_label() using textGrob, title larger", {
         test_assembly("textGrob", TRUE, 2)
      })
   vdiffr::expect_doppelganger(
      "assemble_venndir_label() using textGrob, title giant", {
         test_assembly("textGrob", TRUE, 4)
      })
})

test_that("assemble_marquee", {
   vdiffr::expect_doppelganger(
      "assemble_venndir_label() using marquee", {
         test_assembly("marque", FALSE, 1)
      })
   vdiffr::expect_doppelganger(
      "assemble_venndir_label() using marquee larger", {
         test_assembly("marque", FALSE, 2)
      })
   vdiffr::expect_doppelganger(
      "assemble_venndir_label() using marquee, title", {
         test_assembly("marque", TRUE, 1)
      })
   vdiffr::expect_doppelganger(
      "assemble_venndir_label() using marquee, title larger", {
         test_assembly("marque", TRUE, 2)
      })
   vdiffr::expect_doppelganger(
      "assemble_venndir_label() using marquee, title giant", {
         test_assembly("marque", TRUE, 4)
      })
})

