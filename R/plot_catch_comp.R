#' Plot catch composition
#'
#' @inheritParams plot_abundance_at_age
#' @param unit_label indicate the name of the units of catch as to label the axis
#' @param scale_amount A number describing how much to scale down the catch at
#' age. Please choose a value ranging from 1-1,000,000,000 (one billion) in orders
#' of magnitude (e.g., 1, 10, 100, 1000, etc.). For example, scale_amount = 100
#' would scale down a value from 500,000 --> 5,000 and would report catch in
#' hundreds of the 'unit_label'. This scale will be reflected
#' in the legend label if proportional is set to FALSE. The default is 1.
#' @param era a string naming the era of data such as historical ("early"), current ("time"), or 
#' projected ("fore") data if filtering should occur. Default is set to "time" which is 
#' the current time. To plot all data, set era to NULL.
#' @param interactive TRUE/FALSE; indicate whether the environment in which the
#' function is operating  is interactive. This bypasses some options for
#' filtering when preparing data for the plot. Default is FALSE.
#' @param module (Optional) A string indicating the linked module_name associated
#' with the label for the plot if known. Default is NULL. By default, the function
#' will select the most relevant module if more than 1 exists.
#'
#' @return A plot ready for a stock assessment report of catch or landings  composition.
#' This plot is made only when catch or landings are explicitly named in the output file.
#' The current plot function does not combine all sources of catch.
#'
#' @export
#' @examples
#' plot_catch_comp(
#'   dat = stockplotr:::example_data,
#'   facet = "area",
#'   unit_label = "mt",
#'   scale_amount = 100,
#'   interactive = FALSE,
#'   make_rda = FALSE,
#'   figures_dir = getwd()
#' )
plot_catch_comp <- function(
  dat,
  facet = NULL,
  era = "time",
  unit_label = "mt",
  scale_amount = 1,
  proportional = TRUE,
  interactive = FALSE,
  module = NULL,
  make_rda = FALSE,
  figures_dir = getwd()
) {
  # Create label for abundance units in legend
  catch_label <- label_magnitude(
    label = "catch",
    unit_label = unit_label,
    scale_amount = scale_amount,
    legend = TRUE
  )
  # Filter data
  catch <- filter_data(
    dat = dat,
    label_name = "catch|landings",
    era = era,
    geom = "point",
    group = "age",
    scale_amount = scale_amount,
    interactive = interactive,
    module = module
  )
  # Check for extracted data, if not return warning and empty plot
  if (nrow(catch) == 0) {
    cli::cli_alert_warning("No data found for catch at age. Please check the input data.")
    # Did you use a BAM model?
    cli::cli_abort("catch was not found in the data.")
  }

  # Plot data
  plot <- plot_aa(
    dat = catch,
    facet = facet,
    label = catch_label,
    proportional = proportional
  ) +
  cohort_line(catch)
  
  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    create_rda(
      object = plot,
      topic_label = "pop.caa",
      fig_or_table = "figure",
      dat,
      unit_label = "mt"
    )
  }
  plot
}
