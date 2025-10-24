#' Plot fishing mortality timeseries
#'
#' @inheritParams plot_spawning_biomass
#'
#' @return Create a plot ready for a stock assessment report of fishing mortality over time 
#' from the results of an assessment model translated to the a standardized output 
#' (\link[asar]{convert_output}). Includes options to group or facet data where available.
#' There are options to return a
#' [ggplot2::ggplot()] object or export an rda object containing associated
#' caption and alternative text for the figure.
#' @export
#'
#' @examples
#' plot_fishing_mortality(
#'   dat = stockplotr:::example_data,
#'   ref_line = c("target" = 0.2),
#'   group = "fleet",
#'   interactive = FALSE
#' )
plot_fishing_mortality <- function(
  dat,
  geom = "line",
  group = NULL,
  facet = NULL,
  ref_line = "msy",
  era = "time",
  module = NULL,
  relative = FALSE,
  make_rda = FALSE,
  figures_dir = getwd(),
  interactive = TRUE,
  ...
) {
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

  # Extract reference point unless explicit
  if (!is.null(names(ref_line))) {
    ref_line_val <- ref_line[[1]]
  } else {
    reference_point <- calculate_reference_point(
      dat = dat,
      reference_name = glue::glue("fishing_mortality_", ref_line)
    )
  }

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
    era = "time",
    label_name = "fishing_mortality",
    reference = ref_line,
    relative = relative,
    scale_amount = 1
  ) + theme_noaa()

  ### Make RDA ----
  if (make_rda) {
    create_rda(
      object = final,
      topic_label = ifelse(relative, "relative.fishing.mortality", "fishing.mortality"),
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