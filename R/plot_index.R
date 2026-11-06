#' Plot Index of Abundance
#'
#' @inheritParams plot_spawning_biomass
#' @param facet Character vector. Column name or names used for faceting. "fleet" is always added on to any faceting selections
#'
#' Default: NULL
#'
#' Options: May include, but is not limited to: "fleet", "year", "area", NULL
#' @param unit_label String. Index of abundance/CPUE units
#'
#' Default: ""
#' @param interval Number. Coverage of intervals around the observed index
#' based on its observation error (e.g., 0.95), drawn as vertical lines.
#' Defaults to 0.95; NULL draws no intervals.
#'
#' Default: NULL
#' @param log Logical. TRUE/FALSE; plot the index on a log scale. Indices
#' with lognormal errors are often easier to judge on a log scale, where the
#' intervals are symmetric.
#'
#' Default: FALSE
#' @param focus Character vector. Column name or names used to filter data.
#' (i.e. select names of fleets to zoom into the plot)
#'
#' Default: NULL
#' @returns A plot showing the expected and predicted index.
#'
#' @details Intervals assume lognormal observation errors, as in Stock
#' Synthesis, FIMS, BAM, and Rceattle: \eqn{o \exp(\pm z \cdot \sigma)},
#' where \eqn{o} is the observed index and \eqn{\sigma} is its log-scale
#' standard error. Uncertainty labeled "se", "sd", or "log_sd" is used as
#' \eqn{\sigma}, and a coefficient of variation ("cv") is converted to
#' \eqn{\sigma = \sqrt{\log(1 + CV^2)}}. Observations without an
#' uncertainty have no interval.
#'
#' The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a `ggplot2` object or export an .rda object
#' containing associated caption and alternative text for the figure.
#'
#' @seealso [convert_output()], [filter_data()], [process_data()], [plot_obsvpred()], [export_kqs()], [insert_kqs()], [create_rda()]
#'
#' @export
#'
#' @examples
#' plot_index(
#'   dat = stockplotr::example_data,
#'   unit_label = "fish/hr",
#'   interactive = FALSE
#' )
#'
#' # 95% intervals around the observed index, on a log scale
#' plot_index(
#'   dat = stockplotr::example_data,
#'   interval = 0.95,
#'   log = TRUE,
#'   interactive = FALSE
#' )
#'
plot_index <- function(
  dat,
  unit_label = NULL,
  group = NULL,
  # facet always assigned to fleet since that is how indices are calc'd -- unless replaced with NULL
  facet = NULL,
  interactive = TRUE,
  module = NULL,
  focus = NULL,
  interval = 0.95,
  log = FALSE,
  make_rda = FALSE,
  figures_dir = getwd(),
  ...
) {
  check_data(dat)

  if (is.null(facet) || "fleet" %notin% facet) {
    facet <- c(facet, "fleet")
  }
  # Filter data
  prepared_data <- filter_data(
    dat,
    # Only the observed and predicted index are plotted; other labels that
    # contain "index" (e.g., catchability or index numbers at age) are not
    label_name = "^index_(observed|predicted)$",
    era = NULL,
    geom = "line",
    # ifelse guarantees the code doesn't miss grouping when label has > 1 value
    # ifelse(length(unique(prepared_data$label)) > 1, "label", group)
    # above does not work if it happens during this step lol
    group = group,
    facet = facet,
    interactive = interactive,
    module = module
  )
  if (!is.null(unit_label) && !is.na(unique(prepared_data$unit))) unit_label <- unique(prepared_data$unit)
  if (is.na(unit_label) || is.null(unit_label)) {
    cli::cli_alert_warning("No unit label found in the data. Please provide a unit label using the 'unit_label' argument.")
    unit_label <- ""
  }
  # Set cpue unit label for plot
  u_units <- ifelse(
    is.null(unit_label) | unit_label == "",
    glue::glue("Estimated Index"),
    glue::glue("Estimated Index ({unit_label})")
  )

  # Observation-error intervals for the observed index
  if (!is.null(interval)) {
    prepared_data <- index_observed_interval(prepared_data, interval)
  }

  # Subset data if focus
  if (!is.null(focus)) {
    prepared_data <- prepared_data |>
      dplyr::filter(fleet %in% focus)
  }

  processed_data <- process_data(
    dat = prepared_data,
    group = group,
    facet = facet
  )
  prepared_data <- processed_data[[1]]
  group <- processed_data[[2]]
  facet <- processed_data[[3]]

  # move label to grouping and set grouping into facet
  if (!is.null(group)) {
    facet <- c(facet, group)
  }
  # process_data() drops columns with a single value (e.g., an index from one
  # fleet), so facet only by the columns that remain
  facet <- intersect(facet, colnames(prepared_data))
  if (length(facet) == 0) facet <- NULL
  # transform prep data so group_var = label
  # prepared_data <- prepared_data |>
  #   dplyr::mutate(
  #     group_var = label
  #   )
  # plot time series with multiple labels
  plt <- plot_obsvpred(
    dat = prepared_data,
    x = "year",
    y = "estimate",
    observed_label = "index_observed",
    predicted_label = "index_predicted",
    geom = "line",
    xlab = "Year",
    ylab = u_units,
    group = group,
    facet = facet
  ) +
    theme_noaa() +
    ggplot2::scale_x_continuous(
      breaks = ggplot2::waiver(),
      # labels = scales::label_number(accuracy = 1),
      guide = ggplot2::guide_axis(
        minor.ticks = TRUE
      )
    )
  if (!is.null(interval)) {
    obs <- prepared_data[
      grepl("index_observed", prepared_data$label) & !is.na(prepared_data$estimate_lower),
    ]
    # Draw the intervals beneath the observed and predicted index
    intervals <- ggplot2::geom_linerange(
      data = obs,
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
  if (log) {
    # plot_obsvpred() expands the y-axis to include zero, which has no log
    plt$layers <- Filter(function(l) !inherits(l$geom, "GeomBlank"), plt$layers)
    plt <- suppressMessages(
      plt + ggplot2::scale_y_log10(labels = scales::label_comma())
    ) + ggplot2::labs(y = "Estimated Index (log scale)")
  }
  # Overwrite facets from base plot_timeseries bc scales need to be free
  if (!is.null(facet) && "fleet" %in% colnames(prepared_data)) {
    facet <- paste("~", paste(facet, collapse = " + "))
    facet_formula <- stats::reformulate(facet)
    plt <- plt + ggplot2::facet_wrap(facet_formula, scales = "free", labeller = function(labels) {
      # Clean column names (e.g., "growth_pattern" -> "Growth Pattern")
      names(labels) <- tools::toTitleCase(gsub("_", " ", names(labels)))
      # Combine into "Variable: Value" format
      lapply(names(labels), function(var) paste0(var, ": ", labels[[var]]))
    })
  }

  ### Make RDA ----
  if (make_rda) {
    # Obtain relevant key quantities for captions/alt text
    cpue.start.year <- min(prepared_data$year)
    cpue.end.year <- max(prepared_data$year)
    cpue.min <- min(prepared_data$estimate) |> round(digits = 3)
    cpue.max <- max(prepared_data$estimate) |> round(digits = 3)
    cpue.units <- ifelse(unit_label == "",
      "an estimated index ratio", # default if empty
      unit_label
    )

    # calculate & export key quantities
    export_kqs(
      cpue.start.year,
      cpue.end.year,
      cpue.min,
      cpue.max,
      cpue.units,
      dir = figures_dir
    )

    # Add key quantities to captions/alt text
    insert_kqs(
      cpue.start.year,
      cpue.end.year,
      cpue.min,
      cpue.max,
      cpue.units,
      dir = figures_dir
    )

    create_rda(
      object = plt,
      # get name of function and remove "plot_" from it
      topic_label = gsub("plot_", "", utils::tail(as.character(sys.call()[[1]]), n = 1)),
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir
    )
  }
  # Output final plot
  plt
}

# Lognormal intervals around the observed index from its log-scale standard
# error (uncertainty labeled "se", "sd", or "log_sd") or its coefficient of
# variation ("cv"); other rows get no interval
index_observed_interval <- function(dat, interval) {
  if (!is.numeric(interval) || length(interval) != 1 || interval <= 0 || interval >= 1) {
    cli::cli_abort("{.arg interval} must be a number between 0 and 1 (e.g., 0.95).")
  }
  z <- stats::qnorm(1 - (1 - interval) / 2)
  type <- tolower(as.character(dat$uncertainty_label))
  sigma <- dplyr::case_when(
    type %in% c("se", "sd", "log_sd", "logsd", "stddev") ~ dat$uncertainty,
    type == "cv" ~ sqrt(log(1 + dat$uncertainty^2)),
    TRUE ~ NA_real_
  )
  observed <- grepl("index_observed", dat$label) & !is.na(sigma)
  dat$estimate_lower <- ifelse(observed, dat$estimate * exp(-z * sigma), NA_real_)
  dat$estimate_upper <- ifelse(observed, dat$estimate * exp(z * sigma), NA_real_)
  dat
}
