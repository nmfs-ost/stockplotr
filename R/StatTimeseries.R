StatTimeseries <- ggproto("StatTimeseries", Stat,
  required_aes = c("x", "y", "label", "era", "uncertainty_label", "uncertainty"),
  
  setup_params = function(data, params){
    params
  },
  
  compute_group = function(data,
                           scales,
                           label_name = "spawning_biomass"){
    data |>
      dplyr::filter(
        grepl(glue::glue("{label_name}$"), .data$label),
        .data$era == "time"
      ) |>
      dplyr::mutate(
        year = as.numeric(.data$x), # assume x is year
        estimate = as.numeric(.data$y), # assume y is estimate
        # calc uncertainty when se
        # TODO: calculate other sources of error to upper and lower (cv,)
        estimate_lower = dplyr::case_when(
          grepl("se", .data$uncertainty_label) ~ .data$estimate - 1.96 * .data$uncertainty,
          TRUE ~ NA_real_ # NA_real_ for numeric
        ),
        estimate_upper = dplyr::case_when(
          grepl("se", .data$uncertainty_label) ~ .data$estimate + 1.96 * .data$uncertainty,
          TRUE ~ NA_real_
        )
      )
    # Don't need a return bc its implicit
  }
)

stat_timeseries <- function(mapping = NULL, data = NULL, 
                            geom = "line", position = "identity", 
                            na.rm = FALSE, show.legend = NA, 
                            inherit.aes = TRUE, label_name = "spawning_biomass",
                            ...) {
  layer(
    stat = StatTimeseries, 
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

library(ggplot2)
ggplot(data = rs, aes(x = year, 
                      y = estimate, 
                      label = label, 
                      era = era, 
                      uncertainty_label = uncertainty_label, 
                      uncertainty = uncertainty)) +
  stat_timeseries(label_name = "spawning_biomass")
  # geom_ribbon(aes(ymin = estimate_lower, ymax = estimate_upper), alpha = 0.2) +
  # theme_minimal()
  
  
GeomTimeseries <- ggproto("GeomTimeseries", Geom,
                          required_aes = c("x", "y", "label"),
                          
                          setup_data = function(data, params, label) {
                            data
                          },
                          
                          draw_panel = function(data, panel_scales, coord) {
                            ggname("geom_timeseries", ggplot2::GeomPath$draw_panel(data, panel_scales, coord))
                          },
                          
                          default_aes = aes(colour = "black", size = 0.5, linetype = "solid", alpha = NA)
)

geom_timeseries <- function(mapping = NULL, data = NULL, stat = "identity",
                            position = "identity", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeseries,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    na.rm = na.rm,
    ...
  )
}


