#' Plot spawning potential ratio (SPR)
#'
#' @inheritParams plot_spawning_biomass
#' @param quantity String. SPR quantity to plot.
#' 
#' Default: "spr".
#'
#' Options: "spr", "fishing_intensity", or "spr_ratio".
#' 
#' "spr" plots spawning potential ratio. "fishing_intensity" plots 1-SPR.
#' "spr_ratio" plots quantity associated with SPRratio label (may be user
#' specified in some models; e.g., Stock Synthesis).
#' 
#' @param ylab String. Y-axis label to show.
#'
#' @returns A plot showing spawning potential ratio (SPR).
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
#' plot_spr(
#'   dat = stockplotr:::example_data,
#'   quantity = "spr",
#'   unit_label = "metric tons",
#'   group = "fleet",
#'   interactive = FALSE,
#'   make_rda = FALSE,
#'   ylab = "SPR"
#' )
#' plot_spr(
#'   dat = stockplotr:::example_data,
#'   quantity = "spr_ratio
#'   unit_label = "metric tons",
#'   facet = "fleet",
#'   interactive = FALSE,
#'   make_rda = FALSE,
#'   ylab = "(1-SPR)/(1-SPR_ref)"
#' )
plot_spr <- function(
  dat,
  quantity = c("spr", "fishing_intensity", "spr_ratio"),
  geom = "line",
  group = NULL,
  facet = NULL,
  ref_line = "target",
  era = "time",
  module = NULL,
  interactive = TRUE,
  make_rda = FALSE,
  figures_dir = getwd(),
  ylab = NULL,
  ...
) {
  
  quantity <- match.arg(quantity)
  
  label_filter <- switch(quantity,
                          "spr"="spr$", # only get SPR from SPR_SERIES
                          "fishing_intensity"="spr_report$", # spr_report is apparently 1-SPR
                          "spr_ratio"="^spawning_potential_ratio"
                         )
  
  # Filter data for spr
  prepared_data <- filter_data(
    dat = dat,
    label_name = label_filter,
    geom = geom,
    #TODO: change this to era once stockplotr::example_data updated
    era = NULL,
    group = group,
    facet = facet,
    module = module,
    scale_amount = 1,
    interactive = interactive
  )  |>
  # filter NA from year
   dplyr::filter(!is.na(year))
  
  # set y axis label
  # default values if ylab not provided
  if(is.null(ylab)){
    ylab <- switch(quantity, 
                   "spr"="SPR (Spawning Potential Ratio)",
                   "fishing_intensity"="Fishing Intensity (1-SPR)",
                   "spr_ratio"="SPR Ratio"
                   )
  }
  
  
  # Process data
  processed_data <- process_data(
    dat = prepared_data,
    group = group,
    facet = facet,
    method = "sum"
  )
  prepared_data <- processed_data[[1]]
  group <- processed_data[2]
  facet <- processed_data[[3]]

  # Check if there is >1 label
  if (length(prepared_data$label) > 1) {
    prepared_data <- prepared_data |>
      # always select the first label if TRUE
      dplyr::filter(label == unique(label)[1])
  }

  # Override grouping variable when there is only NA's
  if (!is.null(group)) {
    if (group %notin% colnames(prepared_data)) group <- NULL
  }
  
  # inital base plot
  plt <- plot_timeseries(
    dat = prepared_data,
    y = "estimate",
    geom = geom,
    ylab = ylab,
    group = group,
    facet = facet,
    # ...
  ) +
    ggplot2::geom_hline(yintercept = 1,
                        color = "grey") +
    ggplot2::geom_hline(yintercept = 0,
                        color = "grey") +
    # ggplot2::geom_hline(yintercept = ref_pt,
    #                     color = "red") +
    theme_noaa()
  
    plt <- reference_line(
      plot = plt,
      dat = dat,
      lbs = FALSE,
      label_name = "spawning_potential_ratio",
      reference = ref_line,
      scale_amount = 1
    ) + theme_noaa()
  

  if (length(unique(prepared_data$group_var)) == 1) {
    plt <- plt + ggplot2::theme(legend.position = "none")
  }

  ### Make RDA ----
  if (make_rda) {
    # TODO: Update caption, alt text, and quantities once plot is finalized
    # Obtain relevant key quantities for captions/alt text
    spr.quantity <- switch(quantity,
                           "spr"="spawning potential ratio (SPR) (SB~current~/SB~unfished~)",
                           "fishing_intensity"="fishing intensity (1-SPR) (1-SB~current~/SB~unfished~)",
                           "spr_ratio"="relative fishing intensity (1-SPR)/(1-SPR~target~)"
                          )
    
    spr.start.year <- min(prepared_data$year)
    spr.end.year <- max(prepared_data$year)
    spr.min <- min(prepared_data$estimate) |> round(digits = 3)
    spr.max <- max(prepared_data$estimate) |> round(digits = 3)
    spr.ref.pt <- as.character(ref_line)
    
    # calculate & export key quantities
    export_kqs(
      spr.quantity,
      spr.start.year,
      spr.end.year,
      spr.min,
      spr.ref.pt
    )

    # Add key quantities to captions/alt text
    insert_kqs(
      spr.quantity,
      spr.start.year,
      spr.end.year,
      spr.min,
      spr.max,
      spr.ref.pt
    )

    create_rda(
      object = plt,
      # get name of function and remove "plot_" from it
      topic_label = gsub("plot_", "", utils::tail(as.character(sys.call()[[1]]), n = 1)),
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir,
      scale_amount = scale_amount,
      unit_label = unit_label
    )
  }
  # Output final plot
  plt
}
