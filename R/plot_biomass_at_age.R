#' Plot Total Biomass at Age (BAA)
#'
#' @inheritParams plot_abundance_at_age
#' @param scale_amount A number describing how much to scale down the biomass at
#' age. For example, scale_amount = 100 would scale down a value
#' from 500,000 --> 5,000. This scale will be reflected in the legend label if
#' proportion is set to FALSE.
#' @return Plot total biomass at age from a stock assessment model as found in a NOAA
#' stock assessment report. Units of total biomass can either be manually added
#' or will be extracted from the provided file if possible.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_biomass_at_age(dat)
#'
#' plot_biomass_at_age(
#'   dat,
#'   unit_label = "mt",
#'   scale_amount = 100,
#'   proportional = FALSE, # displays legend
#'   make_rda = TRUE,
#'   figures_dir = getwd()
#' )
#' }
plot_biomass_at_age <- function(
  dat,
  facet = NULL,
  unit_label = "mt",
  scale_amount = 1000,
  proportional = TRUE,
  interactive = FALSE,
  make_rda = FALSE,
  figures_dir = getwd()
) {
  # Create label for abundance units in legend
  biomass_label <- label_magnitude(
    label = "Biomass",
    unit_label = unit_label,
    scale_amount = scale_amount
  )
  # Filter data
  b <- prepare_data(
    dat = dat,
    label_name = "^biomass",
    geom = "point",
    group = "age",
    scale_amount = scale_amount,
    interactive = interactive
  )
  # Check for extracted data, if not return warning and empty plot
  if (nrow(b) == 0) {
    cli::cli_alert_warning("No data found for biomass at age. Please check the input data.")
    return(
      ggplot2::ggplot()
    )
  }

  # Plot data
  plot <- plot_aa(
    dat = b,
    facet = facet,
    label = biomass_label,
    proportional = proportional
  ) +
    average_age_line(
      dat = b,
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
