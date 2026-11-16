#' Plot observed landings
#'
#' @inheritParams plot_spawning_biomass
#' @param predicted Logical. TRUE/FALSE; plot the observed landings of each
#' fleet as points, with the landings predicted by the model as a line, to
#' show the fit to the landings. Default: FALSE
#' @param interval Number. Coverage of intervals around the observed landings
#' based on their observation error (e.g., 0.95), drawn as vertical lines.
#' Setting `interval` also plots the observed landings of each fleet as
#' points. NULL draws no intervals. Default: NULL
#'
#' @returns A plot showing cumulative landings over time.
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
#' When `predicted = TRUE` or `interval` is set, these observed landings are
#' plotted as points and the matching predicted landings ("landings_predicted",
#' "landings_predicted_weight", "landings_predicted_numbers", or
#' "catch_predicted") as a line, with one panel for each fleet, as in
#' [plot_index()]. Predicted landings are plotted for fleets with observed
#' landings, within the years of those observations. Intervals are lognormal,
#' from a log-scale standard error or a coefficient of variation, as in
#' [plot_index()]; landings are often given a small observation error (e.g.,
#' a standard error of 0.01 in Stock Synthesis), so their intervals can be
#' narrow.
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
#' plot_landings(
#'   dat = stockplotr::example_data,
#'   unit_label = "metric tons",
#'   facet = "fleet",
#'   interactive = FALSE,
#'   make_rda = FALSE
#' )
#' # Fit to the landings: observed (points, with 95% intervals) and predicted
#' plot_landings(
#'   dat = stockplotr::vermilion_snapper,
#'   predicted = TRUE,
#'   interval = 0.95,
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
  interactive = TRUE,
  predicted = FALSE,
  interval = NULL,
  make_rda = FALSE,
  figures_dir = getwd(),
  ...
) {
  check_data(dat)

  if (predicted || !is.null(interval)) {
    fit <- landings_fit_plot(
      dat = dat,
      predicted = predicted,
      interval = interval,
      unit_label = unit_label,
      group = group,
      facet = facet,
      module = module,
      scale_amount = scale_amount,
      interactive = interactive
    )
    plt <- fit$plot
    landings_data <- fit$observed
    unit_label <- fit$unit_label
  } else {
    # Filter data for landings
    prepared_data <- filter_data(
      dat = dat,
      label_name = landings_label_name(dat, module = module),
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
      lbs = lbs
    )
    # Years without an observed value (e.g., a fleet with no landings) are not
    # plotted, so the line and ribbon legends show the same fleets
    landings_data <- processed_data[[1]] |>
      dplyr::filter(!is.na(.data$estimate))
    group <- processed_data[[2]]
    facet <- processed_data[[3]]

    # Check if there is >1 label
    if (length(landings_data$label) > 1) {
      landings_data <- landings_data |>
        # always select the first label if TRUE
        dplyr::filter(label == unique(label)[1])
    }

    # extract unit if unit_label is null and set lbs to F by default
    lbs <- FALSE
    if (is.null(unit_label)) {
      labels <- unique(landings_data$label)
      unit_label <- prepared_data |>
        dplyr::filter(label %in% labels) |>
        dplyr::pull(unit) |>
        unique()
      if (is.na(unit_label)) {
        cli::cli_alert_warning("No unit label found in the data. Please provide a unit label using the 'unit_label' argument.")
        unit_label <- ""
      }
    } else {
      if (grepl("lbs|pounds|lb", unit_label, ignore.case = TRUE)) lbs <- TRUE
    }

    # Units
    landings_label <- label_magnitude(
      label = "Landings",
      unit_label = unit_label,
      scale_amount = dplyr::if_else(lbs, 1000 * scale_amount, scale_amount),
      legend = FALSE
    )

    # Override grouping variable when there is only NA's
    if (!is.null(group)) {
      if (group %notin% colnames(landings_data)) group <- NULL
    }

    # inital base plot
    plt <- plot_timeseries(
      dat = landings_data,
      y = "estimate",
      geom = geom,
      ylab = landings_label,
      group = group,
      facet = facet,
      ...
    ) + theme_noaa()

    if (length(unique(landings_data$group_var)) == 1) {
      plt <- plt + ggplot2::theme(legend.position = "none")
    }
  }

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
# The fit to the landings: observed landings of each fleet as points (with
# intervals) and, when `predicted`, predicted landings as a line, one panel
# per fleet. Returns the plot, the observed landings (for key quantities),
# and the unit label.
landings_fit_plot <- function(dat, predicted, interval, unit_label, group, facet,
                              module, scale_amount, interactive) {
  # The observed landings, as in the default plot, and the matching
  # predicted landings
  observed_label <- sub("^\\^(.*)\\$$", "\\1", landings_label_name(dat, module = module))
  if (observed_label == "landings") {
    cli::cli_abort("No observed landings found in {.arg dat} to compare with predicted landings.")
  }
  predicted_label <- if (observed_label == "catch") {
    "catch_predicted"
  } else {
    sub("observed", "predicted", observed_label)
  }
  if (predicted && !predicted_label %in% dat$label) {
    cli::cli_alert_info("No predicted landings ({.val {predicted_label}}) found; plotting the observed landings only.")
    predicted <- FALSE
  }
  labels <- c(observed_label, if (predicted) predicted_label)

  # One panel per fleet, as in plot_index()
  if (is.null(facet) || "fleet" %notin% facet) {
    facet <- c(facet, "fleet")
  }
  prepared_data <- filter_data(
    dat,
    label_name = paste0("^(", paste(labels, collapse = "|"), ")$"),
    era = NULL,
    geom = "line",
    group = group,
    facet = facet,
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  )
  # Years without an observation (e.g., forecast years) are not plotted, and
  # predicted landings are plotted only for fleets with observations, within
  # the years of those observations
  is_observed <- prepared_data$label == observed_label
  prepared_data <- prepared_data[!(is_observed & is.na(prepared_data$estimate)), , drop = FALSE]
  is_observed <- prepared_data$label == observed_label
  keys <- intersect(c("model", "fleet"), names(prepared_data))
  key <- do.call(paste, c(prepared_data[keys], sep = "\r"))
  first <- tapply(prepared_data$year[is_observed], key[is_observed], min, na.rm = TRUE)
  last <- tapply(prepared_data$year[is_observed], key[is_observed], max, na.rm = TRUE)
  in_range <- key %in% names(first) &
    prepared_data$year >= first[key] & prepared_data$year <= last[key]
  prepared_data <- prepared_data[is_observed | in_range %in% TRUE, , drop = FALSE]
  if (is.null(unit_label)) {
    data_units <- unique(stats::na.omit(prepared_data$unit))
    if (length(data_units) == 1 && nzchar(data_units)) {
      unit_label <- data_units
    } else {
      cli::cli_alert_warning(
        "No unit label found in the data. Please provide a unit label using the 'unit_label' argument."
      )
      unit_label <- ""
    }
  }
  lbs <- grepl("lbs|pounds|lb", unit_label, ignore.case = TRUE)
  ylab <- if (unit_label == "" && scale_amount == 1) {
    "Landings"
  } else {
    label_magnitude(
      label = "Landings",
      unit_label = unit_label,
      scale_amount = dplyr::if_else(lbs, 1000 * scale_amount, scale_amount),
      legend = FALSE
    )
  }
  if (!is.null(interval)) {
    prepared_data <- index_observed_interval(prepared_data, interval, observed_label = observed_label)
  }
  processed_data <- process_data(dat = prepared_data, group = group, facet = facet)
  prepared_data <- processed_data[[1]]
  group <- processed_data[[2]]
  facet <- processed_data[[3]]
  if (!is.null(group)) {
    facet <- c(facet, group)
  }
  facet <- intersect(facet, colnames(prepared_data))
  if (length(facet) == 0) facet <- NULL

  plt <- plot_obsvpred(
    dat = prepared_data,
    x = "year",
    y = "estimate",
    observed_label = paste0("^", observed_label, "$"),
    predicted_label = paste0("^", predicted_label, "$"),
    geom = "line",
    xlab = "Year",
    ylab = ylab,
    group = group,
    facet = facet
  ) +
    theme_noaa()
  if (!predicted) {
    # Only the observed landings were filtered, so the line layer is empty
    plt$layers <- Filter(function(l) !inherits(l$geom, "GeomLine"), plt$layers)
  }
  observed <- prepared_data[grepl(paste0("^", observed_label, "$"), prepared_data$label), , drop = FALSE]
  if (!is.null(interval)) {
    # Draw the intervals beneath the observed and predicted landings
    intervals <- ggplot2::geom_linerange(
      data = observed[!is.na(observed$estimate_lower), , drop = FALSE],
      ggplot2::aes(
        x = .data$year,
        ymin = .data$estimate_lower,
        ymax = .data$estimate_upper,
        color = .data$group_var
      ),
      alpha = 0.6,
      show.legend = FALSE
    )
    plt$layers <- c(list(intervals), plt$layers)
  }
  if (!is.null(facet)) {
    plt <- plt + ggplot2::facet_wrap(stats::reformulate(facet), scales = "free_y")
  }
  if (length(unique(prepared_data$model)) <= 1) {
    plt <- plt + ggplot2::guides(shape = "none")
  }
  if (length(unique(prepared_data$group_var)) <= 1) {
    plt <- plt + ggplot2::theme(legend.position = "none")
  }
  list(plot = plt, observed = observed, unit_label = unit_label)
}
