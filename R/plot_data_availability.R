#' Plot the data available to an assessment model
#'
#' Plot the years in which each fleet has each type of data (landings,
#' discards, indices of abundance, and age and length compositions), as in
#' `r4ss` (`SSplotData()`), `stockassessment` (`dataplot()`), and `spict`
#' (`plotspict.data()`). Each type of data is a panel, each fleet a row, and
#' each year with data a square.
#'
#' @inheritParams plot_spawning_biomass
#' @param fleet A string or vector of strings. Fleets to plot.
#'
#' Default: NULL (all fleets)
#'
#' @returns A `ggplot2` object.
#'
#' @details The data are the observations in output from [convert_output()],
#' found by their labels:
#'
#' * Landings: "landings_observed", "landings_observed_weight",
#'   "landings_observed_numbers", or "catch_observed", or "catch" when the
#'   output also has "catch_predicted". Only the first of these labels in
#'   the output is used, so that landings reported in more than one table
#'   are not shown twice.
#' * Discards: "discard_observed".
#' * Index: "index_observed".
#' * Age and length compositions: "age_composition_observed" and
#'   "length_composition_observed", or the composition fit tables of Stock
#'   Synthesis ("all_obs_mean" in the age and length composition modules).
#' * Compositions: "composition_observed", when the type of composition is
#'   not reported.
#'
#' A year has data when it has an observation greater than zero. All
#' modules are plotted, so `interactive` is not used; it is accepted for
#' consistency with the other plotting functions (e.g., in
#' [save_all_plots()]).
#'
#' @export
#'
#' @examples
#' plot_data_availability(stockplotr::example_data)
plot_data_availability <- function(
  dat,
  fleet = NULL,
  interactive = TRUE,
  make_rda = FALSE,
  figures_dir = getwd()
) {
  check_data(dat)
  avail <- data_availability(dat, fleet = fleet)

  plt <- ggplot2::ggplot(avail, ggplot2::aes(x = .data$year, y = .data$fleet)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$fleet), shape = 15, size = 2.5) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$type),
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::labs(x = "Year", y = NULL) +
    theme_noaa() +
    ggplot2::theme(
      legend.position = "none",
      strip.placement = "outside",
      strip.text.y.left = ggplot2::element_text(angle = 0, hjust = 1),
      panel.grid.major.y = ggplot2::element_line(color = "gray90")
    )

  if (make_rda) {
    # Obtain relevant key quantities for captions/alt text
    data.availability.start.year <- min(avail$year)
    data.availability.end.year <- max(avail$year)
    data.availability.n.fleets <- length(unique(avail$fleet))
    data.availability.types <- data_availability_list(levels(droplevels(avail$type)))

    # calculate & export key quantities
    export_kqs(
      data.availability.start.year,
      data.availability.end.year,
      data.availability.n.fleets,
      data.availability.types,
      dir = figures_dir
    )

    # Add key quantities to captions/alt text
    insert_kqs(
      data.availability.start.year,
      data.availability.end.year,
      data.availability.n.fleets,
      data.availability.types,
      dir = figures_dir
    )

    create_rda(
      object = plt,
      topic_label = "data_availability",
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir
    )
  }
  plt
}

# The years in which each fleet has each type of data: one row per type,
# fleet, and year, with `type` a factor in the order the types are plotted
data_availability <- function(dat, fleet = NULL) {
  if ("model" %in% names(dat) && length(unique(stats::na.omit(dat$model))) > 1) {
    cli::cli_abort("{.arg dat} has output from more than one model; plot one model at a time.")
  }
  label <- dat$label
  module <- tolower(dat$module_name)
  # Landings: the first label present, so landings in more than one table
  # are not shown twice
  landings_labels <- c(
    "landings_observed", "landings_observed_weight", "landings_observed_numbers",
    "catch_observed"
  )
  if ("catch_predicted" %in% label) {
    landings_labels <- c(landings_labels, "catch")
  }
  landings_label <- landings_labels[landings_labels %in% label][1]
  age_fit <- label %in% "all_obs_mean" & module %in% c("fit_age_comps", "age_comp_fit_table")
  length_fit <- label %in% "all_obs_mean" & module %in% c("fit_len_comps", "len_comp_fit_table")
  type <- dplyr::case_when(
    label %in% landings_label ~ "Landings",
    label %in% "discard_observed" ~ "Discards",
    label %in% "index_observed" ~ "Index",
    label %in% "age_composition_observed" | age_fit ~ "Age compositions",
    label %in% "length_composition_observed" | length_fit ~ "Length compositions",
    label %in% "composition_observed" ~ "Compositions",
    TRUE ~ NA_character_
  )
  estimate <- suppressWarnings(as.numeric(dat$estimate))
  keep <- !is.na(type) & !is.na(dat$year) & !is.na(estimate) & estimate > 0
  if (!any(keep)) {
    cli::cli_abort("No observed data found in {.arg dat}; see {.fn plot_data_availability} for the labels used.")
  }
  avail <- tibble::tibble(
    type = type[keep],
    fleet = as.character(dat$fleet[keep]),
    year = as.numeric(dat$year[keep])
  )
  if (any(is.na(avail$fleet))) {
    cli::cli_alert_info(
      "{sum(is.na(avail$fleet))} observation{?s} without a fleet {?is/are} shown as fleet {.val unknown}."
    )
    avail$fleet[is.na(avail$fleet)] <- "unknown"
  }
  if (!is.null(fleet)) {
    missing <- setdiff(fleet, avail$fleet)
    if (length(missing) > 0) {
      cli::cli_alert_info("No observed data for fleet{?s} {.val {missing}}.")
    }
    avail <- avail[avail$fleet %in% fleet, , drop = FALSE]
    if (nrow(avail) == 0) {
      cli::cli_abort("No observed data for the selected fleets.")
    }
  }
  types <- c("Landings", "Discards", "Index", "Age compositions", "Length compositions", "Compositions")
  avail <- dplyr::distinct(avail)
  avail$type <- factor(avail$type, levels = types[types %in% avail$type])
  dplyr::arrange(avail, .data$type, .data$fleet, .data$year)
}

# "a", "a and b", or "a, b, and c" (lower case), for captions
data_availability_list <- function(x) {
  x <- tolower(x)
  if (length(x) <= 2) {
    return(paste(x, collapse = " and "))
  }
  paste0(paste(x[-length(x)], collapse = ", "), ", and ", x[length(x)])
}
