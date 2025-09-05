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
#'
#' @return A plot ready for a stock assessment report of catch composition.
#' This plot is made only when catch is explicitly named in the output file.
#' The current plot function does not combine all sources of catch.
#'
#' @examples
#' \dontrun{
#' plot_catch_comp(
#'   dat,
#'   facet = "area",
#'   unit_label = "mt",
#'   scale_amount = 100,
#'   make_rda = TRUE,
#'   figures_dir = getwd())
#' }
plot_catch_comp <- function(
  dat,
  facet = NULL,
  era = "time",
  unit_label = "mt",
  scale_amount = 1,
  proportional = TRUE,
  make_rda = FALSE,
  figures_dir = getwd()
) {
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
  catch_label <- glue::glue("Catch \n({unit_mag}{unit_label})")
  # Filter data
  catch <- prepare_data(
    dat = dat,
    label_name = "catch",
    era = era,
    geom = "point",
    group = "age",
    scale_amount = scale_amount,
    interactive = TRUE
  )
  # Check for extracted data, if not return warning and empty plot
  if (nrow(catch) == 0) {
    cli::cli_alert_warning("No data found for catch at age. Please check the input data.")
    # Did you use a BAM model?
    cli::cli_abort()
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
