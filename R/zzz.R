### Start up messages and actions to run for use of stockplotr
# Start up message

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Thanks for using stockplotr! Please visit the GitHub pages for more information on this package as well as latest developments. If you have any issues or inquires, please use the Issues page on GitHub (https://github.com/nmfs-ost/stockplotr/issues)")
}

# Things to load on start up
# .onLoad <- function(libname, pkgname){
#   # import NOAA approved fonts from package
#   # suppressPackageStartupMessages(
#   #   extrafont::loadfonts()
#   #   )
#   suppressWarnings(
#     suppressMessages(
#       extrafont::font_import(pattern = c("cambria", "arial"), prompt = FALSE)
#     )
#   )
#
# }
