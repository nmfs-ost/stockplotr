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
    group = "year",
    facet = c(group, facet)
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
    selectivity.type.cap <- ifelse(age_type,
                              "Age",
                              "Length")
    
    selectivity.type.low <- tolower(selectivity.type.cap)
    
    selectivity.x <- ifelse(age_type,
                                "years",
                                "cm")
    
    selectivity.start.year <- min(prepared_data$year)
    
    selectivity.end.year <- max(prepared_data$year)
    
    selectivity.x.min <- ifelse(age_type,
                                min(prepared_data$age),
                                min(prepared_data$length_bins)) |>
      as.numeric() |>
      round(digits = 3)
    
    selectivity.x.max <- ifelse(age_type,
                                max(prepared_data$age),
                                max(prepared_data$length_bins)) |>
      as.numeric() |>
      round(digits = 3)

    # calculate & export key quantities
    export_kqs(
      selectivity.type.cap,
      selectivity.type.low,
      selectivity.x.min,
      selectivity.x.max,
      selectivity.x,
      selectivity.start.year,
      selectivity.end.year
    )

    # Add key quantities to captions/alt text
    insert_kqs(
      selectivity.type.cap,
      selectivity.type.low,
      selectivity.x.min,
      selectivity.x.max,
      selectivity.x,
      selectivity.start.year,
      selectivity.end.year
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
