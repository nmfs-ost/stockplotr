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
#'\dontrun{
#'  ggplot2::theme_set(stockplotr::theme_noaa())
#'}
theme_noaa <- function(discrete = TRUE,
                       ...) {
  
  if (utils::packageVersion("ggplot2") < "4.0.0") {
    rlang::warn(
      message = paste0("Your `ggplot2` version is ", utils::packageVersion("ggplot2"), ", which is older than the version required to use `theme_noaa()` (4.0.0). Please update your `ggplot2` package."),
      .frequency = "once",
      .frequency_id = "ggplot2_version_warning"
    )
  }
  
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent"),
      panel.background = ggplot2::element_rect(fill = "transparent"),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.5),
      text = ggplot2::element_text(size = 14),
      plot.margin = ggplot2::margin(0.5, 1, 0.5, 0.5, "cm"),
      ...
  )

  # create main palette
  pal <- function(n) viridisLite::mako(n, begin = 0.1, end = 0.85)
  
  if (discrete) {
    theme <- theme +
      ggplot2::theme(
       palette.colour.discrete = pal,
       palette.fill.discrete = pal
       )
    } else {
      theme <- theme +
        ggplot2::theme(
          palette.colour.continuous = pal,
          palette.fill.continuous = pal
        )
    }
  
  theme
}
