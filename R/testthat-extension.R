# testthat extension to support marquee and svglite::svglite() features

#' alternative SVG writer that supports gradients
#' @noRd
write_svg_with_svglite <- function
(plot,
 file,
 title="")
{
   ## use Liberation Sans and Symbola to avoid platform-specific font differences
   # liberation_sans <- fontquiver::font_styles("Liberation", "Sans")
   # symbola <- fontquiver::font("Symbola", "Symbols", "Regular")
   # sysfonts::font_add(
   #    "Liberation Sans",
   #    regular=liberation_sans$Regular$ttf,
   #    bold=liberation_sans$Bold$ttf,
   #    italic=liberation_sans$Italic$ttf,
   #    bolditalic=liberation_sans$`Bold Italic`$ttf,
   #    symbol=symbola$ttf)

   svglite::svglite(file,
      width=10,
      height=8,
      bg="white",
      pointsize=12,
      standalone=TRUE,
      always_valid=FALSE)
   
   on.exit(
      grDevices::dev.off(),
      add=TRUE,
      after=FALSE)
   
   if (inherits(plot, "function")) {
      plot()
   }
}
