#' Plot Spawn-Recruit Curve
#'
#' @inheritParams plot_recruitment
#' @param spawning_biomass_label Units for spawning biomass
#' @param recruitment_label units for recruitment
#'
#' @return Plot spawning recruitment relationship from a standardized output file originating from \link[asar]{convert_output}
#' @export
#'
#' @examples
#' \dontrun{
#' plot_spawn_recruitment(dat)
#'
#' plot_spawn_recruitment(
#'   dat,
#'   spawning_biomass_label = "sb label",
#'   recruitment_label = "rec label",
#'   end_year = 2024,
#'   make_rda = TRUE,
#'   figures_dir = getwd()
#' )
#' }
plot_spawn_recruitment <- function(
    dat,
    spawning_biomass_label = "mt",
    recruitment_label = "mt",
    make_rda = FALSE,
    figures_dir = getwd()) {
  # Extract recruitment
  recruitment <- prepare_data(
    dat = dat,
    label_name = "recruitment",
    geom = "point",
    interactive = FALSE
  ) 
  if (length(unique(recruitment$label)) > 1) {
    recruitment <- recruitment |>
      tidyr::pivot_wider(
      id_cols = c(year, model, group_var, estimate_lower, estimate_upper),
      names_from = label,
      values_from = estimate
    )
  } else {
    recruitment <- recruitment |>
    dplyr::rename(spawning_biomass = estimate) |>
    dplyr::select(-c(label))
  }
  
  # Extract spawning biomass
  sb <- prepare_data(
    dat = dat,
    label_name = "spawning biomass",
    geom = "point",
    interactive = FALSE
  ) |>
  dplyr::rename(spawning_biomass = estimate) |>
  dplyr::select(-c(label))

  # Merge recruitment and spawning biomass data
  sr <- dplyr::left_join(sb, recruitment)

  # Plot
  final <- plot_timeseries(
    dat = sr,
    x = "spawning_biomass",
    y = "predicted_recruitment",
    geom = "point",
    color = "black",
    facet = {
      if (length(unique(sr$model)) > 1) {
        "model"
      } else {
        NULL
      }
    }
  ) +
  ggplot2::geom_line(data = sr,
    ggplot2::aes(x = spawning_biomass, y = expected_recruitment),
    color = "red"
  ) +
  theme_noaa()
  
  # Make RDA
  if (make_rda) {
    create_rda(
      object = final,
      topic_label = ifelse(relative, "relative.spawning.biomass", "spawning.biomass"),
      fig_or_table = "figure",
      dat = rp_dat,
      dir = figures_dir# ,
      # unit_label = unit_label
    )
  }

  # Export final plot
  final
}
