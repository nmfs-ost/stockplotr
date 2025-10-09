#' Plot Index of Abundance
#'
#' @inheritParams plot_spawning_biomass
#' @param facet a string or vector of strings of a column that facets the data
#' (e.g. "year", "area", etc.) "fleet" is always added on to any faceting selections
#' @param unit_label units for index of abundance/CPUE
#' @param focus a string or vector of strings indicating how data should be 
#' filtered. (i.e. select names of fleets to zoom into the plot)
#'
#' @return Plot the expected and predicted indices as indicated from a standard 
#' assessment model output file.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_indices(
#'   petrale,
#'   unit_label = "fish/hr",
#'   interactive= TRUE,
#'   module = NULL,
#'   make_rda = FALSE,
#'   figures_dir = getwd()
#' )
#' }
plot_indices <- function(
    dat,
    unit_label = "",
    group = NULL,
    # facet always assigned to fleet since that is how indices are calc'd -- unless replaced with NULL
    facet = "fleet",
    interactive = TRUE,
    module = NULL,
    focus = NULL,
    make_rda = FALSE,
    figures_dir = NULL,
    ...
    ) {
  # Set cpue unit label for plot
  u_units <- ifelse(
    is.null(unit_label) | unit_label == "",
    glue::glue("Estimated Index"),
    glue::glue("Estimated Index ({unit_label})")
  )
  
  if (!is.null(facet)) {
    if (facet != "fleet") {
      facet <- c(facet, "fleet")
    }
  }
  facet <- "fleet"
  # Filter data
  filter_data <- prepare_data(
    dat,
    label_name = "indices",
    era = NULL,
    geom = "line",
    # ifelse guarantees the code doesn't miss grouping when label has > 1 value
    group = ifelse(length(unique(filter_data$label)) > 1, "label", group),
    facet = facet, 
    interactive = interactive,
    module = module
  )
  
  # Subset data if focus
  if (!is.null(focus)) {
    filter_data <- filter_data |> 
      dplyr::filter(fleet %in% focus)
  }

  # identify if there is >1 label and create plot
  if (length(unique(filter_data$label)) > 1) {
    plt <- plot_timeseries(
      dat = filter_data,
      ylab = u_units,
      group = "label",
      facet = facet,
      size = 1,
      ...
    ) +
      # commenting out but might need this later -- not sure if this will always be true
    # labs(
    #   linetype = "",
    #   fill = ""
    # ) +
    theme_noaa() +
      ggplot2::scale_x_continuous(
        breaks = ggplot2::waiver(),
        # labels = scales::label_number(accuracy = 1),
        guide = ggplot2::guide_axis(
          minor.ticks = TRUE
        )
      )
    # Overwrite facets from base plot_timeseries bc scales need to be free
    facet <- paste("~", paste(facet, collapse = " + "))
    facet_formula <- stats::reformulate(facet)
    plt <- plt + ggplot2::facet_wrap(facet_formula, scales = "free")
  } else {
    # plot time series
    plt <- plot_error(
      dat = filter_data,
      ylab = u_units,
      group = group,
      facet = facet,
      ...
    )
  }
  
  ### Make RDA ----
  if (make_rda) {
    create_rda(
      object = plt,
      topic_label = "indices",
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir,
      unit_label = unit_label
    )
  }
  # Output final plot
  plt
}
