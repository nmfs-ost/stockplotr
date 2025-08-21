#' Plot Spawn-Recruit Relationship
#'
#' @inheritParams plot_spawning_biomass
#' @param interactive Indicate whether the environment the
#' plot is being made in is interactive. By default, this
#' is set to false. If true, dependent on your data, a
#' option menu will pop-up.
#' @param spawning_biomass_label Units for spawning biomass
#' @param recruitment_label units for recruitment
#'
#' @return Plot spawn recruitment relationship from a 
#' standardized output file originating from 
#' \link[asar]{convert_output}
#' @export
#'
#' @examples
#' \dontrun{
#' plot_spawn_recruitment(dat)
#'
#' plot_spawn_recruitment(
#'   dat,
#'   interactive = FALSE,
#'   spawning_biomass_label = "sb label",
#'   recruitment_label = "rec label",
#'   make_rda = TRUE,
#'   figures_dir = getwd()
#' )
#' }
plot_spawn_recruitment <- function(
    dat,
    spawning_biomass_label = "mt",
    recruitment_label = "mt",
    interactive = FALSE,
    make_rda = FALSE,
    figures_dir = getwd()) {
  # Extract recruitment
  recruitment <- prepare_data(
    dat = dat,
    label_name = "recruitment",
    geom = "point",
    interactive = interactive
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
    dplyr::rename(predicted_recruitment = estimate) |>
    dplyr::select(-c(label))
  }
  
  # Extract spawning biomass
  sb <- prepare_data(
    dat = dat,
    label_name = "spawning biomass",
    geom = "point",
    interactive = interactive
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
     xlab = paste0("Spawning Biomass (", spawning_biomass_label, ")"),
     ylab = paste0("Recruitment (", recruitment_label, ")"),
     facet = {
       if (length(unique(sr$model)) > 1) {
         "model"
       } else {
         NULL
       }
     }
   ) +
   theme_noaa()
 
   if ("expected_recruitment" %in% names(sr)) {
     final <- final +
      ggplot2::geom_line(
       data = sr,
       ggplot2::aes(x = spawning_biomass, y = expected_recruitment),
       color = "red"
     )
   }
   
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
  final
}
