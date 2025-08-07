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
    geom = "point"
  ) |>
  dplyr::rename(recruitment = estimate) |>
  dplyr::select(-c(label, estimate_lower, estimate_upper))


  # Extract spawning biomass
  sb <- prepare_data(
    dat = dat,
    label_name = "spawning biomass",
    geom = "point"
  ) |>
  dplyr::rename(spawning_biomass = estimate) |>
  dplyr::select(-c(label, estimate_lower, estimate_upper))

  # Merge recruitment and spawning biomass data
  sr <- dplyr::left_join(sb, recruitment)

  # Plot
  # final <- 
  plot_timeseries(
    dat = sr,
    x = "spawning_biomass",
    y = "recruitment",
    geom = "point",
    xlab = paste("Spawning Biomass (", spawning_biomass_label, ")", sep = ""),
    ylab = paste("Recruitment (", recruitment_label, ")", sep = "")
  ) +
  theme_noaa()
  
  # Make RDA
  if (make_rda) {
    create_rda(
      object = final,
      topic_label = ifelse(relative, "relative.spawning.biomass", "spawning.biomass"),
      fig_or_table = "figure",
      dat = rp_dat,
      dir = figures_dir,
      ref_line = ref_line,
      ref_point = ref_line, # need to remove this I think
      scale_amount = scale_amount,
      unit_label = unit_label
    )
  }

  # Export final plot
  final
}
