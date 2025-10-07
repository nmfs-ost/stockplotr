#' Plot observed landings by fleet
#'
#' @inheritParams plot_spawning_biomass
#'
#' @return Create a plot ready for a stock assessment report of cumulative landings
#' over time by fleet.Includes options to plot by fleet, total observed landings
#' with and without predicted landings. Indicate if fleet should be faceted or on one plot (default). Warning: i
#' @export
#'
#' @examples
#' \dontrun{
#' plot_landings(dat)
#'
#' plot_landings(
#'   dat,
#'   unit_label = "my_unit",
#'   end_year = 2024,
#'   make_rda = TRUE,
#'   figures_dir = getwd()
#' )
#' }
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
  filter_data <- prepare_data(
    dat = dat,
    label_name = "landings",
    geom = geom,
    era = NULL,
    group = group,
    facet = facet,
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  ) |> 
    # filter NA from year
    dplyr::filter(!is.na(year))
    
  # Check if there is >1 label
  if (length(filter_data$label) > 1) {
    filter_data <- filter_data |>
      # always select the first label if TRUE
      dplyr::filter(label == unique(label)[1])
  }
    
  # Override grouping variable when there is only NA's
  if (!is.null(group)) {
    if (group %notin% colnames(filter_data)) group = NULL
  }
    
  # inital base plot
  plt <- plot_timeseries(
    dat = filter_data,
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
