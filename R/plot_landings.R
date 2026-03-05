#' Plot observed landings
#'
#' @inheritParams plot_spawning_biomass
#'
#' @return Create a plot ready for a stock assessment report of cumulative landings
#' over time (\link[stockplotr]{convert_output}). There are options to return a
#' [ggplot2::ggplot()] object or export an rda object containing associated
#' caption and alternative text for the figure.
#' @export
#'
#' @examples
#' plot_landings(
#'   dat = stockplotr:::example_data,
#'   unit_label = "metric tons",
#'   group = "fleet",
#'   interactive = FALSE,
#'   make_rda = FALSE
#' )
#' plot_landings(
#'   dat = stockplotr:::example_data,
#'   unit_label = "metric tons",
#'   facet = "fleet",
#'   interactive = FALSE,
#'   make_rda = FALSE
#' )
plot_landings <- function(
  dat,
  unit_label = "metric tons",
  geom = "line",
  group = NULL,
  facet = NULL,
  era = NULL,
  scale_amount = 1,
  module = NULL,
  interactive = TRUE,
  make_rda = FALSE,
  figures_dir = getwd(),
  ...
) {
  # Units
  landings_label <- label_magnitude(
    label = "Landings",
    unit_label = unit_label,
    scale_amount = scale_amount,
    legend = FALSE
  )

  # Filter data for landings
  prepared_data <- filter_data(
    dat = dat,
    label_name = "landings",
    geom = geom,
    era = NULL,
    group = group,
    facet = facet,
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  ) # |>
  # filter NA from year
  # dplyr::filter(!is.na(year))
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
    ylab = landings_label,
    group = group,
    facet = facet,
    ...
  ) + theme_noaa()

  if (length(unique(prepared_data$group_var)) == 1) {
    plt <- plt + ggplot2::theme(legend.position = "none")
  }

  ### Make RDA ----
  if (make_rda) {
    
    if (file.exists(fs::path(dir, "key_quantities.csv"))) {
      cli::cli_alert_info("Key quantities text file (key_quantities.csv) exists. Newly calculated key quantities will be added to it.", wrap = TRUE)
      caps_alttext <- utils::read.csv(
        file.path(dir, "key_quantities.csv")
        )
    } else {
      caps_alttext <- utils::read.csv(
        system.file("resources", "key_quantity_template.csv", package = "stockplotr")
      )
    }
      
      # Obtain relevant key quantities for captions/alt text
      landings.end.year <- max(prepared_data$year)
      landings.max <- max(prepared_data$estimate)
      landings.min <- min(prepared_data$estimate)
      landings.start.year <- min(prepared_data$year)
      landings.units <- unit_label
      
      fill_in_kqs <- function(df, ...) {
 
          arg_names <- sapply(substitute(list(...))[-1], deparse)
          arg_values <- list(...)
          
          lookup_df <- tibble::tibble(
            key_quantity = arg_names,
            value_new = purrr::map_chr(arg_values, as.character)
          )
          
          # TODO: Add message when certain values aren't overwritten (already present)
          df <- df |>
            dplyr::mutate(across(everything(), as.character)) |>
            dplyr::left_join(lookup_df, by = "key_quantity") |>
            dplyr::mutate(value = dplyr::if_else(
              (is.na(value) | value == "" & !is.na(value_new)),
                          value_new,
                          value)) |>
            dplyr::select(-value_new)
        }
      }
      
      caps_alttext_filled <- fill_in_kqs(caps_alttext,
                          landings.max,
                          landings.min,
                          landings.start.year,
                          landings.end.year,
                          landings.units)
      
      # next step: export caps_alttext_filled
      
      # export df with updated captions and alt text to csv
      # utils::write.csv(
      #   x = caps_alttext,
      #   file = fs::path(
      #     dir,
      #     "key_quantities.csv"
      #   ),
      #   row.names = FALSE
      # )
    
    
    # create_rda(
    #   object = plt,
    #   # get name of function and remove "plot_" from it
    #   topic_label = gsub("plot_", "", as.character(sys.call()[[1]])), 
    #   fig_or_table = "figure",
    #   dat = dat,
    #   dir = figures_dir,
    #   scale_amount = scale_amount,
    #   unit_label = unit_label
    # )
  }
  # Output final plot
  plt
}
