#' Plot discards
#'
#' Plot discards as a line graph.
#'
#' @inheritParams plot_spawning_biomass
#'
#' @returns A ggplot showing discards over time, usually by fleet.
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a `ggplot2` object or export an .rda object
#' containing associated caption and alternative text for the figure.
#'
#' @note
#' All plotting functions automatically recognize indexing variables and will
#' use them in groupings and/or facetting. @seealso [process_data()].
#'
#' @seealso [convert_output()], [plot_timeseries()], [calculate_reference_point()], [reference_line()], [filter_data()], [process_data()], [export_kqs()], [insert_kqs()], [create_rda()]
#'
#' @export
#'
#' @examples
#' plot_discard(
#'   dat = stockplotr:::example_data,
#'   module = "DISCARD_OUTPUT"
#' )
plot_discard <- function(
  dat,
  group = NULL,
  facet = NULL,
  unit_label = "mt",
  era = NULL,
  lbs = FALSE,
  scale_amount = 1,
  interactive = TRUE,
  module = NULL,
  make_rda = FALSE,
  figures_dir = getwd()
) {
  # this assumes that the previous units were metric tons
  if (lbs && unit_label %notin% c("lbs", "pounds", "lb")) {
    cli::cli_alert_info("Unit label was not changed. Setting unit_label to 'lbs'.")
    unit_label <- "lbs"
  }

  discard_label <- label_magnitude(
    label = "Discard",
    unit_label = unit_label,
    scale_amount = dplyr::if_else(
      lbs,
      ifelse(unit_label %in% c("mt", "mts", "metric tons", "metric ton"), 1000, 1) * scale_amount,
      scale_amount
    ),
    legend = TRUE
  )

  # Filter data for discards
  prepared_data <- filter_data(
    dat = dat,
    label_name = "^discard",
    geom = "line",
    era = era,
    group = group,
    facet = facet,
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  )

  if (any(grepl("observed|predicted", unique(prepared_data$label)))) {
    # select only the observed and predicted discards for plotting
    prepared_data <- prepared_data |>
      dplyr::filter(grepl("predicted|observed", label))
  } else if (length(unique(prepared_data$label)) > 1) {
    cli::cli_alert_info("Multiple discard labels found. Using the first label: {unique(prepared_data$label)[1]}")
    prepared_data <- prepared_data |>
      dplyr::filter(label == unique(prepared_data$label)[1])
  }

  # Process the data to remove unneccessary columns and information
  p_dat <- process_data(
    prepared_data,
    group = group,
    facet = facet,
    lbs = lbs
  )
  discards <- p_dat[[1]]
  group <- p_dat[[2]]
  facet <- p_dat[[3]]

  # make the plot
  if (any(grepl("observed|predicted", unique(prepared_data$label)))) {
    plt <- plot_obsvpred(
      dat = discards,
      observed_label = "discard_observed",
      predicted_label = "discard_predicted",
      ylab = discard_label,
      group = group,
      facet = facet
    )
  } else {
    plt <- plot_timeseries(
      discards,
      ylab = discard_label,
      facet = if (length(facet) > 0) facet else NULL,
      group = group
    )
  }

  final <- plot + theme_noaa()

  ### Make RDA ----
  if (make_rda) {
    # Obtain relevant key quantities for captions/alt text
    mod.fit.discards.min <- min(discards$estimate, na.rm = TRUE) |> round(digits = 3)
    mod.fit.discards.max <- max(discards$estimate, na.rm = TRUE) |> round(digits = 3)

    mod.fit.discards.start.year <- min(discards$year, na.rm = TRUE)
    mod.fit.discards.end.year <- max(discards$year, na.rm = TRUE)

    fleet.or.survey.name <- paste0(unique(discards$fleet), collapse = ", ")
    mod.fit.discards.units <- as.character(unit_label)

    # calculate & export key quantities
    export_kqs(
      mod.fit.discards.min,
      mod.fit.discards.max,
      mod.fit.discards.start.year,
      mod.fit.discards.end.year,
      fleet.or.survey.name,
      mod.fit.discards.units
    )

    # Add key quantities to captions/alt text
    insert_kqs(
      mod.fit.discards.min,
      mod.fit.discards.max,
      mod.fit.discards.start.year,
      mod.fit.discards.end.year,
      fleet.or.survey.name,
      mod.fit.discards.units
    )

    create_rda(
      object = final,
      topic_label = "mod.fit.discards",
      fig_or_table = "figure",
      dat = discards,
      dir = figures_dir,
      scale_amount = scale_amount,
      unit_label = unit_label
    )
  }
  # Output final plot
  final
}
