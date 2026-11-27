#' Plot observed and predicted landings
#'
#' @inheritParams plot_spawning_biomass
#' @param geom String. How the landings are drawn. Default: "line".
#' Options: "line" draws the predicted landings as lines (with intervals as
#' ribbons) and the observed landings as points, or the observed landings as
#' lines when there are no predicted landings to show; "bar" draws the
#' observed landings of each fleet as side-by-side bars for each year
#' (pass `position = "stack"` to stack them, as in
#' `r4ss::SSplotCatch()`); "point" and "area" draw the observed landings only.
#' @param predicted Logical. TRUE (Default) or FALSE; show the landings
#' predicted by the model as lines, with the observed landings as points, to
#' show the fit to the observed landings. The predicted landings are shown
#' only when they differ from the observed landings by more than `tolerance`.
#' @param interval Number. Coverage of the intervals drawn as ribbons around
#' the lines (e.g., 0.95), based on the observation error of the landings.
#' NULL draws no intervals. Default: 0.95
#' @param tolerance Number. The predicted landings are shown only when the
#' range of their differences from the observed landings is more than this
#' proportion of the largest observed landings, as in `r4ss::SSplotCatch()`.
#' Models that fit the landings almost exactly (e.g., Stock Synthesis with a
#' small catch standard error) otherwise show two identical series. Use 0 to
#' always show the predicted landings. Default: 0.001 (0.1%)
#' @param ... Arguments passed to the geom that draws the main series
#' ([ggplot2::geom_line()] for the predicted landings, [ggplot2::geom_col()]
#' for bars, e.g., `position = "stack"`).
#'
#' @returns A plot showing the landings over time.
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a `ggplot2` object or export an .rda object
#' containing associated caption and alternative text for the figure.
#'
#' The landings plotted are the observed landings: "landings_observed",
#' "landings_observed_weight", "landings_observed_numbers", or "catch" when
#' the output also has "catch_predicted" (the first of these in `dat`). When
#' `dat` has none of these, the first label containing "landings" is
#' plotted, with a message.
#'
#' When `predicted = TRUE` and `geom = "line"`, the matching predicted
#' landings ("landings_predicted", "landings_predicted_weight",
#' "landings_predicted_numbers", or "catch_predicted") are drawn as lines in
#' the color of each fleet, with the observed landings as points, for the
#' fleets and years with observed landings. Use `facet = "fleet"` to plot
#' each fleet in its own panel. When the observed and predicted landings are
#' the same (within `tolerance`), only the observed landings are drawn, as
#' lines, with a message.
#'
#' The intervals are lognormal, from the log-scale standard error or the
#' coefficient of variation of the landings, as in [plot_index()]. Around
#' the predicted landings, they show the range in which observations are
#' expected given the fit and the observation error; observed landings
#' outside the ribbon are poorly fit. Landings are often given a small
#' observation error (e.g., a standard error of 0.01 in Stock Synthesis),
#' so their intervals can be narrow.
#'
#' @seealso [convert_output()], [filter_data()], [process_data()], [plot_timeseries()], [export_kqs()], [insert_kqs()], [create_rda()]
#'
#' @export
#'
#' @examples
#' plot_landings(
#'   dat = stockplotr::example_data,
#'   unit_label = "metric tons",
#'   group = "fleet",
#'   interactive = FALSE,
#'   make_rda = FALSE
#' )
#' # Fit to the landings: observed (points) and predicted (lines, with 95%
#' # intervals), one panel per fleet
#' plot_landings(
#'   dat = stockplotr::vermilion_snapper,
#'   facet = "fleet",
#'   interactive = FALSE
#' )
#' # Observed landings of each fleet as side-by-side bars
#' plot_landings(
#'   dat = stockplotr::vermilion_snapper,
#'   geom = "bar",
#'   interactive = FALSE
#' )
#' # ... or stacked, as in r4ss::SSplotCatch()
#' plot_landings(
#'   dat = stockplotr::vermilion_snapper,
#'   geom = "bar",
#'   position = "stack",
#'   interactive = FALSE
#' )
plot_landings <- function(
  dat,
  unit_label = NULL,
  geom = "line",
  group = NULL,
  facet = NULL,
  lbs = FALSE,
  era = NULL,
  scale_amount = 1,
  module = NULL,
  model = "All",
  interactive = TRUE,
  predicted = TRUE,
  interval = 0.95,
  tolerance = 0.001,
  make_rda = FALSE,
  figures_dir = getwd(),
  ...
) {
  check_data(dat)
  geom <- rlang::arg_match0(geom, c("line", "point", "area", "bar"))
  if (!is.numeric(tolerance) || length(tolerance) != 1 || tolerance < 0) {
    cli::cli_abort("{.arg tolerance} must be a number of 0 or more (e.g., 0.001).")
  }

  # The observed landings and, when `predicted`, the matching predicted
  # landings. Bars, points, and areas show the observed landings only.
  observed_label <- landings_label_name(dat, module = module)
  label_name <- observed_label
  predicted <- predicted && geom == "line"
  if (predicted) {
    predicted_label <- landings_predicted_label(dat, observed_label)
    if (is.null(predicted_label)) {
      predicted <- FALSE
    } else {
      label_name <- paste0(
        "^(", sub("^\\^(.*)\\$$", "\\1", observed_label), "|", predicted_label, ")$"
      )
    }
  }

  # Filter data for landings
  prepared_data <- filter_data(
    dat = dat,
    label_name = label_name,
    geom = if (geom == "area") "area" else "line",
    era = NULL,
    group = group,
    facet = facet,
    module = module,
    model = model,
    scale_amount = scale_amount,
    interactive = interactive,
    allow_multiple_models = TRUE
  )

  # Intervals from the observation error of the observed and predicted
  # landings; which are drawn is decided below
  if (is.null(interval)) {
    prepared_data$estimate_lower <- NA_real_
    prepared_data$estimate_upper <- NA_real_
  } else {
    prepared_data <- index_observed_interval(prepared_data, interval, observed_label = label_name)
  }

  # Process data
  facet_asked <- facet
  processed_data <- process_data(
    dat = prepared_data,
    group = group,
    facet = facet,
    lbs = lbs
  )
  # Years without a value (e.g., a fleet with no landings) are not plotted,
  # so the legends show the same fleets
  landings_data <- processed_data[[1]] |>
    dplyr::filter(!is.na(.data$estimate))
  group <- processed_data[[2]]
  facet <- processed_data[[3]]
  # process_data() makes the first facet the group when no group is given;
  # keep it as a facet too, so `facet = "fleet"` plots a panel per fleet
  if (!is.null(facet_asked) && identical(group, facet_asked[1]) &&
    facet_asked[1] %notin% facet && facet_asked[1] %in% colnames(landings_data)) {
    facet <- c(facet_asked[1], facet)
  }

  is_observed <- grepl(observed_label, landings_data$label)
  predicted_data <- landings_data[!is_observed, , drop = FALSE]
  landings_data <- landings_data[is_observed, , drop = FALSE]

  # Check if there is >1 label
  if (length(unique(landings_data$label)) > 1) {
    landings_data <- landings_data |>
      # always select the first label if TRUE
      dplyr::filter(label == unique(label)[1])
  }

  if (predicted) {
    predicted_data <- fit_match_observed(predicted_data, landings_data)
    difference <- fit_difference(landings_data, predicted_data)
    if (is.na(difference)) {
      cli::cli_alert_info("No predicted landings found for the years with observed landings; plotting the observed landings only.")
      predicted <- FALSE
    } else if (difference <= tolerance) {
      cli::cli_alert_info(
        "The observed and predicted landings differ by less than {.arg tolerance} ({tolerance * 100}% of the largest observed landings); plotting the observed landings only."
      )
      predicted <- FALSE
    }
  }
  # The ribbons are drawn around the lines: the predicted landings when they
  # are shown, otherwise the observed landings
  if (predicted) {
    landings_data$estimate_lower <- NA_real_
    landings_data$estimate_upper <- NA_real_
  }

  # extract unit if unit_label is null and set lbs to F by default
  lbs <- FALSE
  if (is.null(unit_label)) {
    labels <- unique(landings_data$label)
    unit_label <- prepared_data |>
      dplyr::filter(label %in% labels) |>
      dplyr::pull("unit") |>
      unique()
    unit_label <- unit_label[!is.na(unit_label)][1]
    if (is.na(unit_label)) {
      cli::cli_alert_warning("No unit label found in the data. Please provide a unit label using the 'unit_label' argument.")
      unit_label <- ""
    }
  } else {
    if (grepl("lbs|pounds|lb", unit_label, ignore.case = TRUE)) lbs <- TRUE
  }

  # Units
  landings_label <- if (unit_label == "" && scale_amount == 1) {
    "Landings"
  } else {
    label_magnitude(
      label = "Landings",
      unit_label = unit_label,
      scale_amount = dplyr::if_else(lbs, 1000 * scale_amount, scale_amount),
      legend = FALSE
    )
  }

  # Override grouping variable when there is only NA's
  if (!is.null(group)) {
    if (group %notin% colnames(landings_data)) group <- NULL
  }

  plt <- if (geom == "bar") {
    fit_plot_bars(landings_data, group = group, facet = facet, ylab = landings_label, ...)
  } else if (predicted) {
    fit_plot_lines(landings_data, predicted_data, group = group, facet = facet, ylab = landings_label, ...)
  } else {
    one_series <- length(unique(landings_data$group_var)) == 1 &&
      length(unique(landings_data$model)) == 1
    p <- plot_timeseries(
      dat = landings_data,
      y = "estimate",
      geom = geom,
      ylab = landings_label,
      group = group,
      facet = facet,
      ...
    )
    if (one_series) p <- p + ggplot2::theme(legend.position = "none")
    p
  }
  plt <- plt + theme_noaa()

  ### Make RDA ----
  if (make_rda) {
    # Obtain relevant key quantities for captions/alt text
    landings.start.year <- min(landings_data$year)
    landings.end.year <- max(landings_data$year)
    landings.min <- min(landings_data$estimate) |> round(digits = 3)
    landings.max <- max(landings_data$estimate) |> round(digits = 3)
    landings.units <- unit_label

    # calculate & export key quantities
    export_kqs(
      landings.start.year,
      landings.end.year,
      landings.min,
      landings.max,
      landings.units,
      dir = figures_dir
    )

    # Add key quantities to captions/alt text
    insert_kqs(
      landings.start.year,
      landings.end.year,
      landings.min,
      landings.max,
      landings.units,
      dir = figures_dir
    )

    create_rda(
      object = plt,
      # get name of function and remove "plot_" from it
      topic_label = gsub("plot_", "", utils::tail(as.character(sys.call()[[1]]), n = 1)),
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir,
      scale_amount = scale_amount
    )
  }

  if (exists("selected_module")) {
    rm(selected_module, envir = .GlobalEnv)
  }

  if (exists("selected_module")) {
    rm(selected_model, envir = .GlobalEnv)
  }

  plt
}

# The label pattern of the observed landings in `dat` (in `module`, when
# given): the first of the observed landings labels present, or any label
# containing "landings" (with a message) when there are none
landings_label_name <- function(dat, module = NULL) {
  if (!is.null(module)) {
    dat <- dat[dat$module_name %in% module, , drop = FALSE]
  }
  observed <- c("landings_observed", "landings_observed_weight", "landings_observed_numbers")
  if ("catch_predicted" %in% dat$label) {
    observed <- c(observed, "catch")
  }
  found <- observed[observed %in% dat$label][1]
  if (is.na(found)) {
    cli::cli_alert_info(
      "No observed landings ({.val {observed}}) found; using the first label containing {.val landings}."
    )
    return("landings")
  }
  paste0("^", found, "$")
}

# The predicted landings label matching the observed landings label pattern,
# or NULL (with a message) when `dat` has none
landings_predicted_label <- function(dat, observed_label) {
  observed <- sub("^\\^(.*)\\$$", "\\1", observed_label)
  predicted <- if (observed == "catch") "catch_predicted" else sub("observed", "predicted", observed)
  if (observed == "landings" || !predicted %in% dat$label) {
    cli::cli_alert_info("No predicted landings found to match the observed landings; plotting the observed landings only.")
    return(NULL)
  }
  predicted
}
