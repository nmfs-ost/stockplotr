#' Plot Total Biomass at Age (BAA)
#'
#' @inheritParams plot_abundance_at_age
#' @param scale_amount A number describing how much to scale down the biomass at
#' age. For example, scale_amount = 100 would scale down a value
#' from 500,000 --> 5,000. This scale will be reflected in the legend label if
#' proportion is set to FALSE.
#' @param interactive TRUE/FALSE; indicate whether the environment in which the
#' function is operating  is interactive. This bypasses some options for
#' filtering when preparing data for the plot. Default is FALSE.
#' @return Plot total biomass at age from a stock assessment model as found in a NOAA
#' stock assessment report. Units of total biomass can either be manually added
#' or will be extracted from the provided file if possible.
#' @export
#'
#' @examples
#' plot_biomass_at_age(
#'   dat = stockplotr:::example_data,
#'   unit_label = "mt",
#'   scale_amount = 100,
#'   proportional = FALSE, # displays legend
#'   make_rda = FALSE
#' )
plot_biomass_at_age <- function(
    dat,
    facet = NULL,
    unit_label = "mt",
    scale_amount = 1000,
    proportional = TRUE,
    interactive = FALSE,
    make_rda = FALSE,
    figures_dir = getwd()) {
  # Create label for abundance units in legend
  biomass_label <- label_magnitude(
    label = "Biomass",
    unit_label = unit_label,
    scale_amount = scale_amount
  )
  # Filter data
  b <- filter_data(
    dat = dat,
    label_name = "^biomass",
    geom = "point",
    group = "age",
    scale_amount = scale_amount,
    interactive = interactive
  )
  # Process data to recognize grouping and faceting variables
  processed_data <- process_data(
    dat = b,
    group = "age",
    facet = facet
  )
  data <- processed_data[[1]]
  group <- processed_data[[2]]
  facet <- processed_data[[3]]
  # Check for extracted data, if not return warning and empty plot
  if (nrow(b) == 0) {
    cli::cli_alert_warning("No data found for biomass at age. Please check the input data.")
    return(
      ggplot2::ggplot()
    )
  }

  # Plot data
  plot <- plot_aa(
    dat = data,
    facet = facet,
    label = biomass_label,
    proportional = proportional
  ) +
  average_age_line(
    dat = data,
    facet = facet
  )
  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    create_rda(
      object = plot,
      topic_label = "pop.baa",
      fig_or_table = "figure",
      dat,
      unit_label = "mt"
    )
  }
  plot
}
