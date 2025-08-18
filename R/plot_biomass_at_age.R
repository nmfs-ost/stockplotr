#' Plot Total Biomass at Age (BAA)
#'
#' @inheritParams plot_abundance_at_age
#' @param scale_amount A number describing how much to scale down the biomass at
#' age. For example, scale_amount = 100 would scale down a value
#' from 500,000 --> 5,000. This scale will be reflected in the legend label.
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
#'   unit_label = "my_unit",
#'   scale_amount = 100,
#'   end_year = 2024,
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
    make_rda = FALSE,
    figures_dir = getwd()) {
  magnitude <- floor(log10(scale_amount))
  if (magnitude == 0) {
    scale_unit <- ""
    unit_mag <- ""
  } else if (magnitude > 0 & magnitude < 10) {
    scale_unit <- c(
      "tens of ",
      "hundreds of ",
      "thousands of ",
      "tens of thousands of ",
      "hundreds of thousands of ",
      "millions of ",
      "tens of millions of ",
      "hundreds of millions of ",
      "billions of "
    )
    unit_mag <- paste(scale_unit[magnitude])
  } else {
    cli::cli_abort("Scale_amount is out of bounds. Please choose a value ranging from 1-1000000000 (one billion) in orders of magnitude (e.g., 1, 10, 100, 1000, etc.)", wrap = TRUE)
  }
  # Create label for abundance units in legend
  biomass_label <- glue::glue("Biomass \n({unit_mag}{unit_label})")
  # Filter data
  b <- prepare_data(
    dat = dat,
    label_name = "biomass",
    geom = "point",
    group = "age",
    scale_amount = scale_amount,
    interactive = FALSE
  )
  # Check for extracted data, if not return warning and empty plot
  if (nrow(b) == 0) {
    cli::cli_alert_warning("No data found for abundance at age. Please check the input data.")
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
