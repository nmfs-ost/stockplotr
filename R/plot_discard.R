#' Plot discards
#' 
#' Plot discards as a line graph.
#'
#' @inheritParams plot_spawning_biomass
#'
#' @returns A ggplot showing discards over time, usually by fleet.
#' 
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a `ggplot2` object or export an .rda object
#' containing associated caption and alternative text for the figure.
#' 
#' @note
#' All plotting functions automatically recognize indexing variables and will
#' use them in groupings and/or facetting. @seealso [process_data()].
#'
#' @seealso [convert_output()], [plot_timeseries()], [calculate_reference_point()], [reference_line()], [filter_data()], [process_data()], [export_kqs()], [insert_kqs()], [create_rda()]
#' 
#' @export
#'
#' @examples
#' plot_discard(
#'   dat = example_data
#'   
#' )
plot_discard <- function(
    dat,
    group = NULL,
    facet = NULL,
    unit_label = "mt",
    era = NULL,
    lbs = FALSE,
    scale_amount = 1,
    interactive = TRUE,
    module = NULL,
    make_rda = FALSE,
    figures_dir = getwd()
) {
  # this assumes that the previous units were metric tons
  if (lbs && unit_label %notin% c("lbs", "pounds", "lb")) {
    cli::cli_alert_info("Unit label was not changed. Setting unit_label to 'lbs'.")
    unit_label <- "lbs"
  }
  
  discard_label <- label_magnitude(
        label = "Discard",
        unit_label = unit_label,
        scale_amount = dplyr::if_else(
          lbs,
          ifelse(unit_label %in% c("mt", "mts", "metric tons", "metric ton"), 1000, 1) * scale_amount,
          scale_amount
        ),
        legend = TRUE
      )
  
  # Filter data for discards
  prepared_data <- filter_data(
    dat = dat,
    label_name = "^discard$|discard_weight",
    geom = "line",
    era = era,
    group = group,
    facet = facet,
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  )
  
  # Process the data to remove unneccessary columns and information
  p_dat <- process_data(
    prepared_data,
    group = group,
    facet = facet,
    lbs = lbs
  )
  discards <- p_dat[[1]]
  group <- p_dat[[2]]
  facet <- p_dat[[3]]
  
  # make the plot
  plt <- plot_timeseries(
    discards,
    ylab = discard_label,
    group = group,
    facet = if (length(facet) > 0) facet else NULL#,
    # ...
  )
  
  ### Make RDA ----
  if (make_rda) {
    if (relative) {
      rel.ssb.min <- calc_kqs(returned_kq = "rel.ssb.min",
                              final = final)
      
      
      rel.ssb.max <- calc_kqs(returned_kq = "rel.ssb.max",
                              final = final)
      
      # calculate & export key quantities
      export_kqs(rel.ssb.min, rel.ssb.max)
      
      # Add key quantities to captions/alt text
      insert_kqs(rel.ssb.min, rel.ssb.max)
    } else {
      ssb.min <- min(plot_data$estimate) |> round(digits = 3)
      ssb.max <- max(plot_data$estimate) |> round(digits = 3)
      
      export_kqs(ssb.min, ssb.max)
      insert_kqs(ssb.min, ssb.max)
    }
    
    # Obtain relevant key quantities for captions/alt text
    ssb.ref.pt <- as.character(ref_line)
    ssb.units <- as.character(unit_label)
    ssb.start.year <- min(plot_data$year)
    ssb.end.year <- max(plot_data$year)
    
    # calculate & export key quantities
    export_kqs(
      ssb.ref.pt,
      ssb.units,
      ssb.start.year,
      ssb.end.year
    )
    
    # Add key quantities to captions/alt text
    insert_kqs(
      ssb.ref.pt,
      ssb.units,
      ssb.start.year,
      ssb.end.year
    )
    
    create_rda(
      object = final,
      topic_label = "discards",
      fig_or_table = "figure",
      dat = rp_dat,
      dir = figures_dir,
      scale_amount = scale_amount,
      unit_label = unit_label
    )
  }
  # Output final plot
  plt + theme_noaa()
}