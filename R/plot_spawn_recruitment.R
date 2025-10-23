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
#' @return Plot the spawn recruitment relationship  from an assessment model output file
#' translated to a standardized output (\link[asar]{convert_output}). There are options to return a
#' [ggplot2::ggplot()] object or export an rda object containing associated
#' caption and alternative text for the figure.
#' @export
#'
#' @examples
#' plot_spawn_recruitment(
#'   dat = stockplotr:::example_data,
#'   interactive = FALSE,
#'   spawning_biomass_label = "metric tons",
#'   recruitment_label = "metric tons",
#'   module = "SPAWN_RECRUIT"
#' )
plot_spawn_recruitment <- function(
    dat,
    spawning_biomass_label = "mt",
    recruitment_label = "mt",
    interactive = TRUE,
    # era = "time",
    module = NULL,
    scale_amount = 1,
    make_rda = FALSE,
    figures_dir = getwd()) {
  # Extract recruitment
  recruitment <- prepare_data(
    dat = dat,
    label_name = "recruitment",
    era = "time",
    geom = "point",
    scale_amount = scale_amount,
    interactive = interactive,
    module = module
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

  if (any(grepl("^recruitment$", colnames(recruitment)))) {
    recruitment <- dplyr::rename(recruitment, predicted_recruitment = recruitment)
  }
  
  # Extract spawning biomass
  sb <- prepare_data(
    dat = dat,
    label_name = "spawning biomass",
    geom = "point",
    era = "time",
    scale_amount = scale_amount,
    interactive = interactive,
    module = module
  ) |>
  dplyr::rename(spawning_biomass = estimate) |>
  dplyr::select(-c(label))

  # Merge recruitment and spawning biomass data
  sr <- dplyr::left_join(sb, recruitment)

  # Labs
  recruitment_lab <- label_magnitude(
    label = "Recruitment",
    unit_label = recruitment_label,
    scale_amount = scale_amount,
    legend = FALSE
  )
  sb_lab <- label_magnitude(
    label = "Spawning Biomass",
    unit_label = spawning_biomass_label,
    scale_amount = scale_amount,
    legend = FALSE
  )

   # Plot
   final <- plot_timeseries(
     dat = sr,
     x = "spawning_biomass",
     y = "predicted_recruitment",
     geom = "point",
     color = "black",
     xlab = sb_lab,
     ylab = recruitment_lab,
     facet = {
       if (length(unique(sr$model)) > 1) {
         "model"
       } else {
         NULL
       }
     }
   ) +
  #  ggplot2::scale_y_continuous(
  #      labels = scales::label_comma()
  #  ) +
  # want to overwrite the default for x-axis bc it's not year in this case
   ggplot2::scale_x_continuous(
     labels = scales::label_comma()
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
       topic_label = "sr",
       fig_or_table = "figure",
       dat = dat,
       dir = figures_dir# ,
       # unit_label = unit_label
     )
   }
  final  +
    ggplot2::scale_x_continuous(
      labels = scales::label_comma()
    )
}
