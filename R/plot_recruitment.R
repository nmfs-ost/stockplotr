#' Plot Recruitment
#'
#' @inheritParams plot_spawning_biomass
#' @param unit_label units for recruitment
#' 
#' @return Plot recruitment over time from an assessment model output file
#' translated to a standardized output. There are options to return a
#' [ggplot2::ggplot()] object or export an rda object containing associated
#' caption and alternative text for the figure.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_recruitment(dat)
#'
#' plot_recruitment(
#'   dat,
#'   unit_label = "my_unit_label",
#'   scale_amount = 100,
#'   relative = TRUE,
#'   interactive = TRUE,
#'   module = "TIME_SERIES",
#'   make_rda = TRUE,
#'   figures_dir = getwd()
#' )
#' }
plot_recruitment <- function(
    dat,
    unit_label = "mt",
    scale_amount = 1,
    era = "time",
    relative = FALSE,
    interactive = TRUE,
    module = NULL,
    make_rda = FALSE,
    figures_dir = getwd()
    ) {
  # TODO: Fix the unit label if scaling
  recruitment_label <- ifelse(
    relative,
    yes = "Relative recruitment",
    no = {
      label_magnitude(
        label = "Recruitment",
        unit_label = unit_label,
        scale_amount = scale_amount,
        legend = FALSE
      )
    }
  )
  
  # Extract recruitment
  recruitment <- prepare_data(
    dat = dat,
    label_name = "recruitment",
    geom = "line",
    era = era,
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
    hline = FALSE,
    facet = {
      if (length(unique(recruitment$model)) > 1) {
        "model"
      } else {
        NULL
      }
    }
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
      topic_label = "recruitment",
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir# ,
      # unit_label = unit_label
    )
  }
  final
}
