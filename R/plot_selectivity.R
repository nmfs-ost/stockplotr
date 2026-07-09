#' Plot selectivity
#'
#' @inheritParams plot_spawning_biomass
#' 
#' @returns A plot showing selectivity by age.
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a `ggplot2` object or export an .rda object
#' containing associated caption and alternative text for the figure.
#'
#' @seealso [convert_output()], [filter_data()], [process_data()], [plot_timeseries()], [export_kqs()], [insert_kqs()], [create_rda()]
#'
#' @export
#'
#' @examples
#' plot_selectivity(
#'   dat = stockplotr:::example_data,
#'   interactive = TRUE,
#'   module = "TIME_SERIES",
#'   make_rda = FALSE
#' )
#' plot_selectivity(
#'   dat = stockplotr:::example_data,
#'   era = "fore",
#'   module = "TIME_SERIES",
#'   make_rda = FALSE
#' )
plot_selectivity <- function(
  dat,
  era = NULL,
  group = NULL,
  facet = NULL,
  interactive = TRUE,
  module = NULL,
  make_rda = FALSE,
  figures_dir = getwd(),
  ...
) {
  
  #TODO: update alt text/caption
  label_name <- "selectivity"
  
  # Extract selectivity
  selectivity <- filter_data(
    dat = dat,
    label_name = label_name,
    geom = "line",
    era = era,
    group = group,
    facet = facet,
    interactive = interactive,
    module = module
  )
  
  # process data
  processed_data <- process_data(
    dat = selectivity,
    group = "year",
    facet = c(group, facet)
  )
  
  prepared_data <- processed_data[[1]]
  group <- processed_data[[2]]
  facet <- processed_data[[3]]
  
  # replace group with first element of facet if
  # group = age or length_bins
  if (any(grepl("age|length_bins", facet))){
    facet <- facet[!grepl("age|length_bins", facet)]
  }
  if (group == "age" | group == "length_bins") {
    group <- facet[1]
    facet <- facet[-1]
    prepared_data <- prepared_data |>
      dplyr::mutate(group_var = as.character(.data[[group]]))
  }
  
  if ("age" %in% facet | "length_bins" %in% facet) {
    facet <- facet[!facet %in% c("age", "length_bins")]
  }
  
  # Check if there is >1 label
  if (length(unique(prepared_data$label)) > 1) {
    prepared_data <- prepared_data |>
      # always select the first label if TRUE
      dplyr::filter(label == unique(label)[1])
  }
  
  # Plot
  # TODO: left off here. Need to show fleets, models, other groupings
  age_type <- grepl("age", unique(prepared_data$label))
  
  if (age_type){
    prepared_data <- prepared_data |>
      dplyr::mutate(age = as.numeric(age))
  } else {
    prepared_data <- prepared_data |>
      dplyr::mutate(length_bins = as.numeric(length_bins))
  }
  
  final <- plot_timeseries(
    dat = prepared_data |>
      dplyr::mutate(group_var = as.character(group_var)),
    x = ifelse(age_type,
               "age",
               "length_bins"),
    y = "estimate",
    # color = group,
    geom = "line",
    xlab = ifelse(age_type,
                  "Age",
                  "Length Bin"),
    ylab = ifelse(age_type,
                  "Selectivity at Age",
                  "Selectivity"),
    group = group,
    facet = facet#,
    #...
  ) +
    theme_noaa()
final


  # Make RDA
  if (make_rda) {
    # Obtain relevant key quantities for captions/alt text
    selectivity.units <- as.character(unit_label)
    selectivity.start.year <- min(selectivity$year)
    selectivity.end.year <- max(selectivity$year)
    selectivity.min <- min(selectivity$predicted_selectivity) |> round(digits = 3)
    selectivity.max <- max(selectivity$predicted_selectivity) |> round(digits = 3)

    # calculate & export key quantities
    export_kqs(
      selectivity.units,
      selectivity.start.year,
      selectivity.end.year,
      selectivity.min,
      selectivity.max
    )

    # Add key quantities to captions/alt text
    insert_kqs(
      selectivity.units,
      selectivity.start.year,
      selectivity.end.year,
      selectivity.min,
      selectivity.max
    )


    create_rda(
      object = final,
      # get name of function and remove "plot_" from it
      topic_label = gsub("plot_", "", tail(as.character(sys.call()[[1]]), n = 1)),
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir # ,
      # unit_label = unit_label
    )
  }
  final
}
