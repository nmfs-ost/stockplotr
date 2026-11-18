#' Plot observed and predicted discards
#'
#' @inheritParams plot_spawning_biomass
#' @param geom String. How the discards are drawn. Default: "line".
#' Options: "line" draws the predicted discards as lines (with intervals as
#' ribbons) and the observed discards as points, or the observed discards as
#' lines when there are no predicted discards to show; "bar" draws the
#' observed discards of each fleet as side-by-side bars for each year (pass
#' `position = "stack"` to stack them); "point" draws the observed discards
#' as points.
#' @param predicted Logical. TRUE (Default) or FALSE; show the discards
#' predicted by the model as lines, with the observed discards as points, to
#' show the fit to the observed discards. The predicted discards are shown
#' only when they differ from the observed discards by more than `tolerance`.
#' @param interval Number. Coverage of the intervals drawn as ribbons around
#' the lines (e.g., 0.95), based on the observation error of the discards.
#' NULL draws no intervals. Default: 0.95
#' @param tolerance Number. The predicted discards are shown only when the
#' range of their differences from the observed discards is more than this
#' proportion of the largest observed discards, as in [plot_landings()].
#' Models that fit the discards almost exactly otherwise show two identical
#' series. Use 0 to always show the predicted discards. Default: 0.001 (0.1%)
#' @param ... Arguments passed to the geom that draws the main series
#' ([ggplot2::geom_line()] for the predicted discards, [ggplot2::geom_col()]
#' for bars, e.g., `position = "stack"`).
#'
#' @returns A ggplot showing discards over time, usually by fleet.
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a `ggplot2` object or export an .rda object
#' containing associated caption and alternative text for the figure.
#'
#' When `dat` has observed and predicted discards ("discard_observed" and
#' "discard_predicted") and `predicted = TRUE`, the predicted discards are
#' drawn as lines in the color of each fleet, with the observed discards as
#' points, for the fleets and years with observed discards, as in
#' [plot_landings()]. Use `facet = "fleet"` to plot each fleet in its own
#' panel. When the observed and predicted discards are the same (within
#' `tolerance`), only the observed discards are drawn, as lines, with a
#' message. Otherwise, the observed discards (or the first label starting with
#' "discard") are drawn as lines.
#'
#' The intervals are lognormal, from the log-scale standard error or the
#' coefficient of variation of the discards, as in [plot_index()]. Around
#' the predicted discards, they show the range in which observations are
#' expected given the fit and the observation error; observed discards
#' outside the ribbon are poorly fit. Discards with another type of
#' uncertainty are drawn without intervals.
#'
#' @note
#' All plotting functions automatically recognize indexing variables and will
#' use them in groupings and/or facetting. @seealso [process_data()].
#'
#' @seealso [convert_output()], [plot_landings()], [plot_timeseries()], [filter_data()], [process_data()], [export_kqs()], [insert_kqs()], [create_rda()]
#'
#' @export
#'
#' @examples
#' plot_discard(
#'   dat = stockplotr::example_data,
#'   unit_label = "fish",
#'   module = "DISCARD_OUTPUT"
#' )
#' # Fit to the discards: observed (points) and predicted (lines, with 95%
#' # intervals), one panel per fleet
#' plot_discard(
#'   dat = stockplotr::vermilion_snapper,
#'   facet = "fleet",
#'   interactive = FALSE
#' )
#' # Observed discards of each fleet as stacked bars
#' plot_discard(
#'   dat = stockplotr::vermilion_snapper,
#'   geom = "bar",
#'   position = "stack",
#'   interactive = FALSE
#' )
plot_discard <- function(
  dat,
  geom = "line",
  group = NULL,
  facet = NULL,
  unit_label = NULL,
  era = NULL,
  lbs = FALSE,
  scale_amount = 1,
  interactive = TRUE,
  module = NULL,
  model = NULL,
  predicted = TRUE,
  interval = 0.95,
  tolerance = 0.001,
  make_rda = FALSE,
  figures_dir = getwd(),
  ...
) {
  check_data(dat)
  geom <- rlang::arg_match0(geom, c("line", "point", "bar"))
  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance < 0) {
    cli::cli_abort("{.arg tolerance} must be a number of 0 or more (e.g., 0.001).")
  }

  # this assumes that the previous units were metric tons
  if (lbs && unit_label %notin% c("lbs", "pounds", "lb")) {
    cli::cli_alert_info("Unit label was not changed. Setting unit_label to 'lbs'.")
    unit_label <- "lbs"
  }

  # Filter data for discards
  prepared_data <- filter_data(
    dat = dat,
    label_name = "^discard",
    geom = "line",
    era = era,
    group = group,
    facet = facet,
    module = module,
    model = model,
    scale_amount = scale_amount,
    interactive = interactive,
    allow_multiple_models = TRUE
  )

  # extract unit if unit_label is null and set lbs to F by default
  lbs <- FALSE
  if (is.null(unit_label)) {
    unit_label <- unique(stats::na.omit(prepared_data$unit))[1]
    if (is.na(unit_label)) {
      cli::cli_alert_warning("No unit label found in the data. Please provide a unit label using the 'unit_label' argument.")
      unit_label <- ""
    }
  } else {
    if (grepl("lbs|pounds|lb", unit_label, ignore.case = TRUE)) lbs <- TRUE
  }
  discard_label <- label_magnitude(
    label = "Discard",
    unit_label = unit_label,
    scale_amount = scale_amount,
    legend = TRUE
  )

  # The observed (and predicted) discards, or else the first discard label
  fit_labels <- "^discard_(observed|predicted)$"
  has_fit <- "discard_observed" %in% prepared_data$label
  if (has_fit) {
    prepared_data <- prepared_data |>
      dplyr::filter(grepl(fit_labels, .data$label))
  } else if (length(unique(prepared_data$label)) > 1) {
    cli::cli_alert_info("Multiple discard labels found. Using the first label: {unique(prepared_data$label)[1]}")
    prepared_data <- prepared_data |>
      dplyr::filter(.data$label == unique(prepared_data$label)[1])
  }
  predicted <- predicted && geom == "line" && has_fit &&
    "discard_predicted" %in% prepared_data$label

  # Intervals from the observation error of the discards
  if (is.null(interval)) {
    prepared_data$estimate_lower <- NA_real_
    prepared_data$estimate_upper <- NA_real_
  } else {
    prepared_data <- index_observed_interval(
      prepared_data, interval,
      observed_label = if (has_fit) fit_labels else "^discard"
    )
  }

  # Process the data to remove unneccessary columns and information
  facet_asked <- facet
  p_dat <- process_data(
    prepared_data,
    group = group,
    facet = facet,
    lbs = lbs
  )
  discards <- p_dat[[1]] |>
    dplyr::filter(!is.na(.data$estimate))
  group <- p_dat[[2]]
  facet <- p_dat[[3]]
  # process_data() makes the first facet the group when no group is given;
  # keep it as a facet too, so `facet = "fleet"` plots a panel per fleet
  if (!is.null(facet_asked) && identical(group, facet_asked[1]) &&
    facet_asked[1] %notin% facet && facet_asked[1] %in% colnames(discards)) {
    facet <- c(facet_asked[1], facet)
  }
  if (!is.null(group) && group %notin% colnames(discards)) group <- NULL

  is_predicted <- discards$label == "discard_predicted"
  observed <- discards[!is_predicted, , drop = FALSE]
  if (predicted) {
    predicted_data <- fit_match_observed(discards[is_predicted, , drop = FALSE], observed)
    difference <- fit_difference(observed, predicted_data)
    if (is.na(difference)) {
      cli::cli_alert_info("No predicted discards found for the years with observed discards; plotting the observed discards only.")
      predicted <- FALSE
    } else if (difference <= tolerance) {
      cli::cli_alert_info(
        "The observed and predicted discards differ by less than {.arg tolerance} ({tolerance * 100}% of the largest observed discards); plotting the observed discards only."
      )
      predicted <- FALSE
    }
  }

  # make the plot
  plt <- if (geom == "bar") {
    fit_plot_bars(observed, group = group, facet = facet, ylab = discard_label, ...)
  } else if (predicted) {
    # The ribbons are drawn around the predicted discards
    observed$estimate_lower <- NA_real_
    observed$estimate_upper <- NA_real_
    fit_plot_lines(observed, predicted_data, group = group, facet = facet, ylab = discard_label, ...)
  } else {
    plot_timeseries(
      observed,
      geom = geom,
      ylab = discard_label,
      facet = if (length(facet) > 0) facet else NULL,
      group = group,
      ...
    )
  }

  final <- plt + theme_noaa()

  ### Make RDA ----
  if (make_rda) {
    # Obtain relevant key quantities for captions/alt text
    mod.fit.discards.min <- min(discards$estimate, na.rm = TRUE) |> round(digits = 3)
    mod.fit.discards.max <- max(discards$estimate, na.rm = TRUE) |> round(digits = 3)

    mod.fit.discards.start.year <- min(discards$year, na.rm = TRUE)
    mod.fit.discards.end.year <- max(discards$year, na.rm = TRUE)

    fleet.or.survey.name <- paste0(unique(discards$fleet), collapse = ", ")
    mod.fit.discards.units <- as.character(unit_label)

    # calculate & export key quantities
    export_kqs(
      mod.fit.discards.min,
      mod.fit.discards.max,
      mod.fit.discards.start.year,
      mod.fit.discards.end.year,
      fleet.or.survey.name,
      mod.fit.discards.units,
      dir = figures_dir
    )

    # Add key quantities to captions/alt text
    insert_kqs(
      mod.fit.discards.min,
      mod.fit.discards.max,
      mod.fit.discards.start.year,
      mod.fit.discards.end.year,
      fleet.or.survey.name,
      mod.fit.discards.units,
      dir = figures_dir
    )

    create_rda(
      object = final,
      topic_label = "mod.fit.discards",
      fig_or_table = "figure",
      dat = discards,
      dir = figures_dir,
      scale_amount = scale_amount
    )
  }

  if (exists("selected_module")) {
    rm(selected_module, envir = .GlobalEnv)
  }

  final
}
