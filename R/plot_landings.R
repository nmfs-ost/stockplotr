#' Plot observed landings
#'
#' @inheritParams plot_spawning_biomass
#' @param predicted Logical. TRUE/FALSE; add the landings predicted by the
#' model as dashed lines, to show the fit to the observed landings.
#' Default: FALSE
#' @param interval Number. Coverage of intervals around the observed landings
#' based on their observation error (e.g., 0.95), drawn as a ribbon. NULL
#' draws no intervals. Default: NULL
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
#' When `predicted = TRUE`, the matching predicted landings
#' ("landings_predicted", "landings_predicted_weight",
#' "landings_predicted_numbers", or "catch_predicted") are added as dashed
#' lines in the color of each fleet, for the fleets and years with observed
#' landings. Use `facet = "fleet"` to plot each fleet in its own panel.
#'
#' When `interval` is set, the observed landings are drawn with a ribbon at
#' that coverage. Intervals are lognormal, from a log-scale standard error or
#' a coefficient of variation, as in [plot_index()]; landings are often given
#' a small observation error (e.g., a standard error of 0.01 in Stock
#' Synthesis), so their intervals can be narrow.
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
#' # Fit to the landings: observed (solid, with 95% intervals) and
#' # predicted (dashed), one panel per fleet
#' plot_landings(
#'   dat = stockplotr::vermilion_snapper,
#'   predicted = TRUE,
#'   interval = 0.95,
#'   facet = "fleet",
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
  model = NULL,
  interactive = TRUE,
  predicted = FALSE,
  interval = NULL,
  make_rda = FALSE,
  figures_dir = getwd(),
  ...
) {
  check_data(dat)

  # The observed landings and, when `predicted`, the matching predicted landings
  observed_label <- landings_label_name(dat, module = module)
  label_name <- observed_label
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
    label_name = landings_label_name(dat, module = module),
    geom = geom,
    era = NULL,
    group = group,
    facet = facet,
    module = module,
    model = model,
    scale_amount = scale_amount,
    interactive = interactive,
    allow_multiple_models = TRUE
  ) # |>
  # filter NA from year
  # dplyr::filter(!is.na(year))

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

  # extract unit if unit_label is null and set lbs to F by default
  lbs <- FALSE
  if (is.null(unit_label)) {
    labels <- unique(landings_data$label)
    unit_label <- prepared_data |>
      dplyr::filter(label %in% labels) |>
      dplyr::pull(unit) |>
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

  one_series <- length(unique(landings_data$group_var)) == 1 &&
    length(unique(landings_data$model)) == 1
  if (predicted) {
    plt <- landings_add_predicted(plt, landings_data, predicted_data, one_series)
  } else if (one_series) {
    plt <- plt + ggplot2::theme(legend.position = "none")
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

# Add the predicted landings to the plot of observed landings as dashed
# lines, in the color of each fleet, for the fleets and years with observed
# landings
landings_add_predicted <- function(plt, observed, predicted, one_series) {
  keys <- intersect(c("model", "group_var", "fleet"), names(observed))
  key <- function(d) do.call(paste, c(d[keys], sep = "\r"))
  first <- tapply(observed$year, key(observed), min)
  last <- tapply(observed$year, key(observed), max)
  predicted_key <- key(predicted)
  in_range <- predicted_key %in% names(first) &
    predicted$year >= first[predicted_key] & predicted$year <= last[predicted_key]
  predicted <- predicted[in_range %in% TRUE, , drop = FALSE]

  # The observed landings are the solid lines
  for (i in seq_along(plt$layers)) {
    if (inherits(plt$layers[[i]]$geom, "GeomLine")) {
      plt$layers[[i]]$mapping$linetype <- ggplot2::aes(linetype = "Observed")$linetype
    }
  }
  # Color the predicted landings as plot_timeseries() colors the observed ones
  series <- function(d) {
    if (length(unique(observed$model)) > 1) {
      as.character(interaction(d$model, d$group_var))
    } else {
      as.character(d$group_var)
    }
  }
  mapping <- ggplot2::aes(x = .data$year, y = .data$estimate, linetype = "Predicted")
  if (!one_series) {
    predicted$color_var <- factor(series(predicted), levels = levels(factor(series(observed))))
    mapping$colour <- ggplot2::aes(colour = .data$color_var)$colour
  }
  plt <- plt +
    ggplot2::geom_line(data = predicted, mapping = mapping) +
    ggplot2::scale_linetype_manual(values = c(Observed = "solid", Predicted = "dashed")) +
    ggplot2::guides(linetype = ggplot2::guide_legend(title = NULL))
  if (one_series) {
    plt <- plt + ggplot2::guides(colour = "none", fill = "none")
  }
  plt
}
