#' Add NOAA theming to ggplot2 object. The palette is from the 'viridis' package, which contains palettes distinguishable by those with color vision deficiency.
#' 
#' @param discrete Variable indicating whether the color or fill aesthetic is discrete. Default is TRUE.
#' @param ... Arguments passed to `ggplot2::theme()`.
#'
#' @returns A list applying a theme to a ggplot2 object.
#' @export
#'
#' @examples
#' ggplot2::ggplot(data = OrchardSprays,
#'                 ggplot2::aes(x = rowpos, 
#'                              y = decrease,
#'                              color = treatment)) +
#'   ggplot2::geom_point() +
#'   theme_noaa()
theme_noaa <- function(discrete = TRUE,
                       ...) {
  theme <- list(
    # might not need the ggplot2 dependencies if make this pkg an extension
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent"),
      panel.background = ggplot2::element_rect(fill = "transparent"),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.5),
      complete = TRUE,
      text = ggplot2::element_text(size = 14),
      plot.margin = ggplot2::margin(0.5,1,0.5, 0.5, "cm")
    )
  ) 
    
    if (discrete){
      list(theme,
           ggplot2::scale_color_viridis_d(option = "mako",
                              begin = 0.1,
                              end = 0.85),
           ggplot2::scale_fill_viridis_d(option = "mako",
                             begin = 0.1,
                             end = 0.85)
           )
    } else {
      list(theme,
           ggplot2::scale_color_viridis_c(option = "mako",
                                          begin = 0.1,
                                          end = 0.85),
           ggplot2::scale_fill_viridis_c(option = "mako",
                                         begin = 0.1,
                                         end = 0.85)
           )
    }
}
