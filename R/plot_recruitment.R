#' Plot Recruitment
#'
#' @inheritParams plot_spawning_biomass
#' @param unit_label units for recruitment
#' 
#' @return Plot recruitment over time from an assessment model output file
#' translated to a standardized output (\link[asar]{convert_output}). There are options to return a
#' [ggplot2::ggplot()] object or export an rda object containing associated
#' caption and alternative text for the figure.
#' @export
#'
#' @examples
#' plot_recruitment(
#'   dat = stockplotr:::example_data,
#'   unit_label = "metric tons",
#'   scale_amount = 100,
#'   interactive = TRUE,
#'   module = "TIME_SERIES",
#'   make_rda = FALSE
#' )
plot_recruitment <- function(
    dat,
    unit_label = "mt",
    scale_amount = 1,
    era = "time",
    group = NULL,
    facet = NULL,
    # relative = FALSE,
    interactive = TRUE,
    module = NULL,
    make_rda = FALSE,
    figures_dir = getwd()
    ) {
  # TODO: Fix the unit label if scaling
  recruitment_label <- label_magnitude(
    label = "Recruitment",
    unit_label = unit_label,
    scale_amount = scale_amount,
    legend = FALSE
  )
  
  # Extract recruitment
  recruitment <- filter_data(
    dat = dat,
    label_name = "recruitment",
    geom = "line",
    era = era,
    group = group,
    facet = facet,
    interactive = interactive,
    module = module,
    scale_amount = scale_amount
  ) 
  # Check if contains 'rec devs' and filter out
  if (any(grepl("recruitment_deviations", unique(recruitment$label)))) {
    if (length(unique(recruitment$label)) > 1) {
      recruitment <- dplyr::filter(recruitment, !grepl("recruitment_deviations", label))
    } else {
      cli::cli_abort("Recruitment not present in selected module. Please re-run and select a new module.")
    }
  }
  if (length(unique(recruitment$label)) > 1) {
    recruitment <- recruitment |>
      tidyr::pivot_wider(
        id_cols = c(year, model, group_var, estimate_lower, estimate_upper),
        names_from = label,
        values_from = estimate
      )
    # change geom when error available
    geom <- "point"
  } else {
    recruitment <- recruitment |>
      dplyr::rename(predicted_recruitment = estimate) |>
      dplyr::select(-c(label))
    # change geom when pred. R available
    geom <- "line"
  }
  
  # Plot
  final <- plot_timeseries(
    dat = recruitment,
    x = "year",
    y = "predicted_recruitment",
    color = "black",
    geom = geom,
    xlab = "Year",
    ylab = recruitment_label,
    group = group,
    facet = facet
  ) +
    theme_noaa()
  
  if ("expected_recruitment" %in% names(recruitment)) {
    final <- final +
      ggplot2::geom_line(
        data = recruitment,
        ggplot2::aes(x = year, y = expected_recruitment),
        color = "red",
        size = 1
      )
    caption_label <- "recruitment.comp"
  } else {
    caption_label <- "recruitment"
  }

  # Plot vertical lines if era is not filtering
  if (is.null(era)) {
    # Find unique era
    eras <- unique(filter_data$era)
    if (length(eras) > 1) {
      year_vlines <- c()
      for (i in 2:length(eras)){
        erax <- filter_data |>
        dplyr::filter(era == eras[i]) |>
        dplyr::pull(year) |>
        min(na.rm = TRUE)
        year_vlines <- c(year_vlines, erax)
      }
    }
    final <- final +
      ggplot2::geom_vline(
        xintercept = year_vlines,
        color = "#999999"
      )
  }
  
  # Make RDA
  if (make_rda) {
    create_rda(
      object = final,
      topic_label = caption_label,
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir# ,
      # unit_label = unit_label
    )
  }
  final
}
