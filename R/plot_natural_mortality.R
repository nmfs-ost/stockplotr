#' Plot natural mortality (M) at age
#'
#' @inheritParams plot_spawning_biomass
#'
#' @return Plot natural mortality at age from a stock assessment model as
#' found in a NOAA stock assessment report.
#' @export
#'
#' @examples
#' plot_natural_mortality(
#'   dat = stockplotr:::example_data,
#'   module = "Natural_Mortality",
#'   interactive = FALSE
#' )
plot_natural_mortality <- function(
  dat,
  group = NULL,
  facet = NULL,
  era = NULL,
  interactive = TRUE,
  module = NULL,
  make_rda = FALSE,
  figures_dir = getwd(),
  ...
) {
  # TODO:
  # -update M.rate.min, max in write_captions once prev point done
  # -Make test
  # -add to exp_all_figs_tables

  # Extract natural mortality
  # if (is.null(group)) group <- "age"

  prepared_data <- filter_data(
    dat,
    label_name = "natural_mortality",
    era = era,
    group = group,
    facet = facet,
    geom = "line",
    interactive = interactive,
    module = module
  )

  # Set group back to NULL if it is age since this is an at age plot
  # if (group == "age") {
  #   prepared_data <- prepared_data |>
  #     dplyr::mutate(group_var = "1")
  #   group <- NULL
  # }

  # STOP if there are no ages -- indicating this is a single M and would not be plotted
  if (all(is.na(prepared_data$age))) cli::cli_abort("Natural mortality by age not found.")

  # Alert if group = "none"
  if (!is.null(group) && group == "none") {
    cli::cli_alert_warning("Setting group to 'none' is not functional for this plot. Setting group to NULL.")
    group <- NULL
  }
  # This process overrides grouping if it is inaccurate
  processing <- process_data(
    dat = prepared_data,
    group = "age",
    facet = c(group, facet)
  )

  processed_data <- processing[[1]]
  if (!is.null(processing[[3]])) facet <- processing[[3]]
  # make sure group is the one input by user
  if (!is.null(group)) {
    group <- group
    processed_data <- processed_data |>
      dplyr::mutate(group_var = .data[[group]])
    # Remove grouping from facet bc included when run through process_data
    facet <- facet[grepl(group, facet)]
  }
  # Move first facet into group if group is NULL
  if (is.null(group) & length(facet) > 0) {
    group <- facet[1]
    facet <- facet[-1]
    processed_data <- processed_data |>
      dplyr::mutate(group_var = .data[[group]])
  }

  plt <- plot_timeseries(
    dat = processed_data |> dplyr::mutate(age = as.numeric(age)),
    x = "age",
    y = "estimate",
    geom = "line",
    xlab = "Age",
    ylab = "Natural Mortality",
    group = group,
    facet = facet
  )

  final <- plt + theme_noaa(discrete = TRUE)

  ### Make RDA ----
  if (make_rda) {
    create_rda(
      object = final,
      topic_label = "natural.mortality",
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir
    )
  }
  # Output final plot
  final
}
