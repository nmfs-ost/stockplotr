#' Plot observed landings
#'
#' @inheritParams plot_spawning_biomass
#'
#' @return Create a plot ready for a stock assessment report of cumulative landings
#' over time (\link[asar]{convert_output}). There are options to return a
#' [ggplot2::ggplot()] object or export an rda object containing associated
#' caption and alternative text for the figure.
#' @export
#'
#' @examples
#' plot_landings(
#'   dat = stockplotr:::example_data,
#'   unit_label = "metric tons",
#'   group = "fleet",
#'   facet = "area",
#'   interactive = FALSE,
#'   make_rda = FALSE
#' )
plot_landings <- function(
  dat,
  unit_label = "metric tons",
  geom = "line",
  group = NULL,
  facet = NULL,
  era = NULL,
  scale_amount = 1,
  module = NULL,
  interactive = TRUE,
  make_rda = FALSE,
  figures_dir = getwd(),
  ...
) {
  # Units
  landings_label <- label_magnitude(
    label = "Landings",
    unit_label = unit_label,
    scale_amount = scale_amount,
    legend = FALSE
  )

  # Filter data for landings
  prepared_data <- filter_data(
    dat = dat,
    label_name = "landings",
    geom = geom,
    era = NULL,
    group = group,
    facet = facet,
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  ) # |> 
    # filter NA from year
    # dplyr::filter(!is.na(year))
  # Process data
  processed_data <- process_data(
    dat = prepared_data,
    group = group,
    facet = facet
  )
  prepared_data <- processed_data[[1]]
  group <- processed_data[2]
  facet <- processed_data[[3]]
  
  # Check if there is >1 label
  if (length(prepared_data$label) > 1) {
    prepared_data <- prepared_data |>
      # always select the first label if TRUE
      dplyr::filter(label == unique(label)[1])
  }
    
  # Override grouping variable when there is only NA's
  if (!is.null(group)) {
    if (group %notin% colnames(prepared_data)) group = NULL
  }
    
  # inital base plot
  plt <- plot_timeseries(
    dat = prepared_data,
    y = "estimate",
    geom = geom,
    ylab = landings_label,
    group = group,
    facet = facet,
    ...
  ) + theme_noaa()
    
  ### Make RDA ----
  if (make_rda) {
    create_rda(
      object = plt,
      topic_label = "landings",
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir,
      scale_amount = scale_amount,
      unit_label = unit_label
    )
  }
  # Output final plot
  plt
}
