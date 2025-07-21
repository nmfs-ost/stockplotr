#' Add NOAA theming to ggplot2 object
#'
#' @returns theme for NOAA Fisheries plots
#' @export
#'
#' @examples ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(x = 1:10, y = 1:10)) + theme_noaa()
theme_noaa <- function(...) {
  # might not need the ggplot2 dependencies if make this pkg an extension
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "transparent"),
    panel.background = ggplot2::element_rect(fill = "transparent"),
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.5),
    complete = TRUE
  ) +
    nmfspalette::scale_color_nmfs() +
    nmfspalette::scale_fill_nmfs()
}