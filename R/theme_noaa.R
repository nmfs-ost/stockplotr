#' Add NOAA theming to ggplot2 object
#'
#' @returns
#' @export
#'
#' @examples
theme_noaa <- function() {
  ggplot2::theme(
    plot.background = ggplot2::element_rect(fill = "transparent"),
    panel.background = ggplot2::element_rect(fill = "transparent"),
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.5)
  ) +
    nmfspalette::scale_color_nmfs() +
    nmfspalette::scale_fill_nmfs()
}