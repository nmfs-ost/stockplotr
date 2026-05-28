#' Plot catch composition
#'
#' @inheritParams plot_abundance_at_age
#' @param unit_label indicate the name of the units of catch as to label the axis
#'
#' Default: "mt"
#'
#' @param scale_amount A number describing how much to scale down the catch at
#' age. Please choose a value ranging from 1-1,000,000,000 (one billion) in orders
#' of magnitude (e.g., 1, 10, 100, 1000, etc.). For example, scale_amount = 100
#' would scale down a value from 500,000 --> 5,000 and would report catch in
#' hundreds of the 'unit_label'. This scale will be reflected
#' in the legend label if proportional is set to FALSE.
#'
#' Default: 1
#'
#' @param era A string naming the era of data.
#'
#' Default: "time"
#'
#' Options: "early", "time", "fore" (forecast), or NULL (all data)
#'
#' @param interactive A logical value indicating if the environment is interactive.
#'
#' Default: `FALSE`
#'
#' @param module (Optional) A string indicating the module_name found in `dat`.
#' If selecting >1 module, place them in a vector like c("module1", "module2").
#'
#' Default: NULL
#'
#' If the interactive and >1 module_name is found, user will select the
#' module_name in the console. @seealso [filter_data()]
#'
#' @returns A plot showing catch or landings composition.
#'
#' @details This plot is made only when catch or landings are explicitly named
#' in the output file. The current plot function does not combine all sources of
#' catch.
#' The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a `ggplot2` object or export an .rda object
#' containing associated caption and alternative text for the figure.
#'
#' @seealso [convert_output()], [filter_data()], [process_data()], [plot_aa()], [export_kqs()], [insert_kqs()], [create_rda()]
#'
#' @export
#' @examples
#' plot_catch_comp(
#'   dat = stockplotr:::example_data,
#'   facet = "fleet",
#'   unit_label = "mt",
#'   scale_amount = 100,
#'   interactive = FALSE,
#'   make_rda = FALSE,
#'   figures_dir = getwd()
#' )
#'
#' plot_catch_comp(
#'   dat = stockplotr:::example_data,
#'   facet = "none",
#'   unit_label = "mt",
#'   scale_amount = 100,
#'   interactive = FALSE,
#'   era = "fore",
#'   proportional = FALSE
#' )
plot_catch_comp <- function(
  dat,
  facet = NULL,
  era = "time",
  unit_label = "mt",
  scale_amount = 1,
  proportional = TRUE,
  interactive = TRUE,
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
  if (!is.null(facet) && facet == "none") {
    data <- catch |>
      dplyr::group_by(year, age) |>
      dplyr::summarise(
        estimate = sum(estimate),
        estimate_lower = sum(estimate_lower),
        estimate_upper = sum(estimate_upper)
      )
    group <- NULL
    facet <- NULL
  } else {
    # Process data to recognize grouping and faceting variables
    processed_data <- process_data(
      dat = catch,
      group = "age",
      facet = facet
    )
    data <- processed_data[[1]]
    group <- processed_data[[2]]
    facet <- processed_data[[3]]
  }

  data <- data |>
    dplyr::mutate(age = as.numeric(age))

  # Plot data
  plot <- plot_aa(
    dat = data,
    facet = facet,
    label = catch_label,
    proportional = proportional
  ) +
    cohort_line(data)

  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    # Obtain relevant key quantities for captions/alt text
    caa.start.year <- min(data$year)
    caa.end.year <- max(data$year)
    caa.age.min <- min(data$age)
    caa.age.max <- max(data$age)
    tot.catch.min <- min(data$estimate) |> round(digits = 3)
    tot.catch.max <- max(data$estimate) |> round(digits = 3)

    # calculate & export key quantities
    export_kqs(
      caa.start.year,
      caa.end.year,
      caa.age.min,
      caa.age.max,
      tot.catch.min,
      tot.catch.max
    )

    # Add key quantities to captions/alt text
    insert_kqs(
      caa.start.year,
      caa.end.year,
      caa.age.min,
      caa.age.max,
      tot.catch.min,
      tot.catch.max
    )

    create_rda(
      object = plot,
      # get name of function and remove "plot_" from it
      topic_label = gsub("plot_", "", as.character(sys.call()[[1]])),
      fig_or_table = "figure",
      dat,
      unit_label = "mt"
    )
  }
  plot
}
