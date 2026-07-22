#' Plot Total Biomass at Age (BAA)
#'
#' @inheritParams plot_abundance_at_age
#' @param scale_amount Number. A number describing how much to scale down the biomass at
#' age. For example, scale_amount = 100 would scale down a value
#' from 500,000 --> 5,000. This scale will be reflected in the legend label if
#' proportion is set to FALSE.
#'
#' Default: 1,000
#'
#' @param interactive Logical. TRUE/FALSE; indicate whether the environment in which the
#' function is operating  is interactive. This bypasses some options for
#' filtering when preparing data for the plot.
#'
#' Default: `TRUE`
#'
#' @returns A plot showing total biomass at age.
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a `ggplot2` object or export an .rda object
#' containing associated caption and alternative text for the figure.
#'
#' @seealso [convert_output()], [filter_data()], [process_data()], [plot_aa()], [export_kqs()], [insert_kqs()], [create_rda()]
#'
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
#' plot_biomass_at_age(
#'   dat = stockplotr:::example_data,
#'   unit_label = "mt",
#'   facet = "none",
#'   proportional = FALSE
#' )
plot_biomass_at_age <- function(
  dat,
  facet = NULL,
  unit_label = "mt",
  scale_amount = 1000,
  proportional = TRUE,
  module = NULL,
  interactive = TRUE,
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
  b <- filter_data(
    dat = dat,
    label_name = "^biomass",
    geom = "point",
    group = "age",
    era = "time",
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  )
  if (!is.null(facet) && facet == "none") {
    data <- b |>
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
      dat = b,
      group = "age",
      facet = facet
    )
    data <- processed_data[[1]]
    group <- processed_data[[2]]
    facet <- processed_data[[3]]
  }

  # Check for extracted data, if not return warning and empty plot
  if (nrow(b) == 0) {
    cli::cli_alert_warning("No data found for biomass at age. Please check the input data.")
    return(
      ggplot2::ggplot()
    )
  }

  data <- data |>
    dplyr::mutate(age = as.numeric(age))

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
    # Obtain relevant key quantities for captions/alt text
    pop.baa.start.year <- min(data$year)
    pop.baa.end.year <- max(data$year)
    pop.baa.age.min <- min(data$age)
    pop.baa.age.max <- max(data$age)
    pop.baa.fish.min <- min(data$estimate) |> round(digits = 3)
    pop.baa.fish.max <- max(data$estimate) |> round(digits = 3)

    # calculate & export key quantities
    export_kqs(
      pop.baa.start.year,
      pop.baa.end.year,
      pop.baa.age.min,
      pop.baa.age.max,
      pop.baa.fish.min,
      pop.baa.fish.max
    )

    # Add key quantities to captions/alt text
    insert_kqs(
      pop.baa.start.year,
      pop.baa.end.year,
      pop.baa.age.min,
      pop.baa.age.max,
      pop.baa.fish.min,
      pop.baa.fish.max
    )

    create_rda(
      object = plot,
      # get name of function and remove "plot_" from it
      topic_label = gsub("plot_", "", utils::tail(as.character(sys.call()[[1]]), n = 1)),
      fig_or_table = "figure",
      dat,
      unit_label = "mt"
    )
  }
  plot
}
