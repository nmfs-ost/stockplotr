#' Plot Recruitment Deviations
#'
#' @inheritParams plot_spawning_biomass
#'
#' @return Plot recruitment deviations relative to one over time from an
#' assessment model output file translated to a standardized output (\link[asar]{convert_output}). There are
#' options to return a `ggplot2` object or export an .rda object containing
#' associated caption and alternative text for the figure.
#'
#' @export
#'
#' @examples
#' plot_recruitment_deviations(
#'   dat = stockplotr:::example_data,
#'   interactive = FALSE,
#'   size = 1.5
#' )
#' plot_recruitment_deviations(
#'   dat = stockplotr:::example_data,
#'   era = "early",
#'   interactive = FALSE,
#'   shape = 2
#' )
plot_recruitment_deviations <- function(
  dat,
  module = NULL,
  era = "time",
  interactive = TRUE,
  make_rda = FALSE,
  figures_dir = getwd(),
  ...
) {
  # Filter data
  filter_data <- filter_data(
    dat = dat,
    label_name = "recruitment_deviations",
    module = module,
    era = era,
    interactive = interactive,
    geom = "point",
    group = NULL
  )
  # Process data
  processed_data <- process_data(
    dat = filter_data,
    group = "none",
    facet = NULL,
    method = "avg"
  )
  filter_data <- processed_data[[1]]
  group <- processed_data[[2]]
  facet <- processed_data[[3]]

  if (nrow(filter_data) == 0) {
    cli::cli_abort("No recruitment deviations found in data.")
  }

  # Create final plot
  final <- plot_error(
    filter_data,
    geom = "point",
    facet = if (length(unique(filter_data$model)) > 1) "model" else NULL,
    xlab = "Year",
    ylab = "Recruitment Deviations",
    # size = 2
    ...
  ) +
    # ggplot2::facet_wrap(~ model, scales = "free_y") +
    theme_noaa()

  # Plot vertical lines if era is not filtering
  if (is.null(era)) {
    # Find unique era
    eras <- unique(filter_data$era)
    if (length(eras) > 1) {
      # era1 <- filter_data |>
      #   dplyr::filter(era == eras[1]) |>
      #   dplyr::pull(year) |>
      #   max(na.rm = TRUE)
      year_vlines <- c()
      for (i in 2:length(eras)) {
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
    if (!is.data.frame(dat)) {
      selected_dat <- dat[[1]]
    } else {
      selected_dat <- dat
    }
    create_rda(
      object = final,
      topic_label = "recruitment.deviations",
      fig_or_table = "figure",
      dat = selected_dat,
      dir = figures_dir,
      unit_label = ""
    )
  }
  final
}
