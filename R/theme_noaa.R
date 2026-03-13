#' Add NOAA theming to ggplot2 object. The palette is from the 'viridis' package, which contains palettes distinguishable by those with color vision deficiency.
#'
#' @param discrete Variable indicating whether the color or fill aesthetic is discrete. Default is TRUE.
#' @param ... Arguments passed to `ggplot2::theme()`.
#'
#' @returns A list applying a theme to a ggplot2 object.
#' @export
#'
#' @examples
#' ggplot2::ggplot(
#'   data = OrchardSprays,
#'   ggplot2::aes(
#'     x = rowpos,
#'     y = decrease,
#'     color = treatment
#'   )
#' ) +
#'   ggplot2::geom_point() +
#'   theme_noaa()
#'
#' ggplot2::ggplot(
#'   data = OrchardSprays,
#'   ggplot2::aes(
#'     x = rowpos,
#'     y = decrease,
#'     color = decrease
#'   )
#' ) +
#'   ggplot2::geom_point() +
#'   theme_noaa(discrete = FALSE)
#' \dontrun{
#' ggplot2::theme_set(stockplotr::theme_noaa())
#' }
theme_noaa <- function(discrete = TRUE,
                       ...) {
  if (utils::packageVersion("ggplot2") < "4.0.0") {
    cli::cli_warn(
      "Your {.pkg ggplot2} version is {.val {utils::packageVersion('ggplot2')}},
     which is older than the version required to use {.fn theme_noaa} (4.0.0).
     Please update your {.pkg ggplot2} package.",
      .frequency = "once",
      .frequency_id = "ggplot2_version_warning"
    )
  }

  mako_pal_discrete <- scales::pal_viridis(begin = 0.1, end = 0.85, option = "G")

  mako_pal_continuous <- scales::gradient_n_pal(
    scales::pal_viridis(begin = 0.1, end = 0.85, option = "G")(6)
  )

  thm <- ggplot2::theme_bw() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent"),
      panel.background = ggplot2::element_rect(fill = "transparent"),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.5),
      text = ggplot2::element_text(size = 14),
      plot.margin = ggplot2::margin(0.5, 1, 0.5, 0.5, "cm"),
      ...
    )

  if (discrete) {
    thm <- thm + ggplot2::theme(
      palette.colour.discrete = mako_pal_discrete,
      palette.fill.discrete   = mako_pal_discrete
    )
  } else {
    thm <- thm + ggplot2::theme(
      palette.colour.continuous = mako_pal_continuous,
      palette.fill.continuous   = mako_pal_continuous
    )
  }
  thm
}
