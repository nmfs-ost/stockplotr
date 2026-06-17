#' Plot selectivity
#'
#' @inheritParams plot_spawning_biomass
#' 
#' @param type Type of selectivity to plot
#' 
#' Default: "age"
#' 
#' Options: "age", "length"
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
  type = "age",
  era = NULL,
  group = NULL,
  facet = NULL,
  # relative = FALSE,
  interactive = TRUE,
  module = NULL,
  make_rda = FALSE,
  figures_dir = getwd(),
  ...
) {
  
  #TODO: update alt text/caption
  #TODO: revamp this to work for age as type, and for different blocks and
  #other complexities
  
  label_name <- ifelse(type == "length",
         "length_selectivity",
         "selectivity")
  
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
    group = group,
    facet = facet,
    method = "mean" # should this be sum?
  )
  
  # this extracts all possible groups and facets- disregards
  # user's specified group and facet in args (made "group" vs "groups",
  # "facet" vs "facets")
  prepared_data <- processed_data[[1]]
  # |> dplyr::mutate(group_var = NA)
  groups <- processed_data[[2]]
  facets <- processed_data[[3]]
  
  if ("age" %in% groups){
    group <- stringr::str_remove(group, "age")
    
    prepared_data <- prepared_data |>
      dplyr::mutate(age = as.numeric(age))
    if (groups == ""){
      groups <- NULL
    }
  }
  
  # Check if there is >1 label
  if (length(prepared_data$label) > 1) {
    prepared_data <- prepared_data |>
      # always select the first label if TRUE
      dplyr::filter(label == unique(label)[1])
  }

  # Plot
  # TODO: left off here. Need to show fleets, models, other groupings
  final <- plot_timeseries(
    dat = prepared_data,
    x = ifelse(type == "length",
               "year",
               "age"),
    y = "estimate",
   # color = "group_var",
    geom = "line",
    xlab = ifelse(type == "length",
                  "Year",
                  "Age"),
    ylab = ifelse(type == "length",
                  "Length",
                  "Selectivity at Age"),
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
