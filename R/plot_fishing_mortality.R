#' Plot fishing mortality timeseries
#'
#' @inheritParams plot_spawning_biomass
#'
#' @returns A plot showing fishing mortality over time.
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a `ggplot2` object or export an .rda object
#' containing associated caption and alternative text for the figure.
#'
#' @seealso [convert_output()], [filter_data()], [process_data()], [plot_timeseries()], [reference_line()], [export_kqs()], [insert_kqs()], [create_rda()]
#'
#' @export
#'
#' @examples
#' plot_fishing_mortality(
#'   dat = stockplotr:::example_data,
#'   ref_line = c("target" = 0.2),
#'   group = "fleet",
#'   interactive = FALSE,
#'   module = "TIME_SERIES"
#' )
#' plot_fishing_mortality(
#'   dat = stockplotr:::example_data,
#'   ref_line = c("target" = 0.2),
#'   group = "none",
#'   interactive = FALSE,
#'   module = "TIME_SERIES"
#' )
plot_fishing_mortality <- function(
  dat,
  geom = "line",
  group = NULL,
  facet = NULL,
  ref_line = "msy",
  era = NULL,
  module = NULL,
  make_rda = FALSE,
  figures_dir = getwd(),
  interactive = TRUE,
  ...
) {
  orig_group <- group
  orig_facet <- facet
  # Filter out data for fishing mortality
  prepared_data <- filter_data(
    dat = dat,
    label_name = "^fishing_mortality$",
    geom = geom,
    era = era,
    group = group,
    facet = facet,
    module = module,
    scale_amount = 1,
    interactive = interactive
  )
  # Process data
  processed_data <- process_data(
    dat = prepared_data,
    group = group,
    facet = facet,
    method = "mean"
  )
  prepared_data <- processed_data[[1]]
  group <- processed_data[[2]]
  facet <- processed_data[[3]]

  # Create base plot
  plt <- plot_timeseries(
    dat = prepared_data,
    y = "estimate",
    geom = geom,
    ylab = "Fishing Mortality",
    group = group,
    facet = facet,
    ...
  )
  # Add reference line and theme
  final <- reference_line(
    plot = plt,
    dat = dat,
    label_name = "fishing_mortality",
    reference = ref_line,
    scale_amount = 1
  ) + theme_noaa()

  ### Make RDA ----
  if (make_rda) {
    F.min <- prepared_data$estimate |>
      na.omit() |>
      min() |>
      round(digits = 3)
    F.max <- prepared_data$estimate |>
      na.omit() |>
      max() |>
      round(digits = 3)

    export_kqs(F.min, F.max)
    insert_kqs(F.min, F.max)

    F.ref.pt <- as.character(ref_line)
    F.start.year <- min(prepared_data$year)
    F.end.year <- max(prepared_data$year)
    F.terminal.year <- filter_data(
      dat = dat,
      label_name = "^fishing_mortality$",
      geom = geom,
      era = "time",
      group = orig_group,
      facet = orig_facet,
      module = "TIME_SERIES",
      scale_amount = 1,
      interactive = interactive
    ) |>
      dplyr::filter(year == max(year)) |>
      dplyr::pull(year) |>
      unique()
    
    F.target <- dat |>
      dplyr::filter(grepl('f_target', label) | grepl('f_msy', label) | (grepl('fishing_mortality_msy', label) & is.na(year))) |>
      dplyr::pull(estimate) |>
      round(digits = 3)

    export_kqs(
      F.ref.pt,
      F.start.year,
      F.end.year,
      F.terminal.year,
      F.target
    )

    insert_kqs(
      F.ref.pt,
      F.start.year,
      F.end.year,
      F.terminal.year,
      F.target
    )

    create_rda(
      object = final,
      topic_label = "fishing_mortality", # ifelse(relative, "relative_fishing_mortality", "fishing_mortality"),
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir,
      ref_line = ifelse(!is.null(names(ref_line)), names(ref_line), ref_line),
      scale_amount = 1,
      unit_label = "" # no unit for F
    )
  }
  # Output final plot
  final
}
