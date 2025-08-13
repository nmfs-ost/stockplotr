#' Plot catch composition by fleet
#'
#' @inheritParams plot_recruitment
#' @param unit_label indicate the name of the units of catch as to label the axis
#'
#' @return A plot ready for a stock assessment report of catch composition by fleet.
#' This plot is made only when landings are explicitly named in the output file.
#' The current plot function does not combine all sources of catch.
#'
plot_catch_comp <- function(
  dat,
  group = NULL,
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
    dat = catch,
    facet = group,
    label = catch_label,
    proportional = proportional
  ) +
  cohort_line(catch)
  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    create_rda(
      object = plot,
      topic_label = "pop.naa",
      fig_or_table = "figure",
      dat,
      unit_label = "mt"
    )
  }
  plot
}
