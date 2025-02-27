#' Plot observed landings by fleet
#'
#' @inheritParams plot_recruitment
#' @param unit_label indicate the name of the units of landings as to label the axis
#'
#' @return Create a plot ready for a stock assessment report of cumulative landings
#' over time by fleet.Includes options to plot by fleet, total observed landings
#' with and without predicted landings. Indicate if fleet should be faceted or on one plot (default). Warning: i
#' @export
#'
plot_landings <- function(dat,
                          unit_label = "metric tons",
                          make_rda = FALSE,
                          rda_dir = getwd()) {
  # Units
  # TODO: fix unit label is scaling
  land_label <- glue::glue("Landings ({unit_label})")

  # read standard data file and extract target quantity
  land <- dat |>
    dplyr::filter(
      c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
      # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
      !is.na(fleet)
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year),
      fleet = as.character(fleet)
    ) |>
    suppressWarnings() |>
    dplyr::filter(
      !is.na(year)
    )

  # Check number of areas and season - if any are >1 then need to use alternative plot (or summarize)
  narea <- length(unique(land$area))
  nseas <- length(unique(land$season))

  if (narea > 1) {
    factors <- TRUE
    # will need facet if TRUE
  } else {
    factors <- FALSE
  }
  # Check for nseas > 1 - mean of landings through the year
  if (nseas > 1) {
    land <- land |>
      dplyr::group_by(year, fleet, sex, area, growth_pattern) |>
      dplyr::summarize(estimate = mean(estimate)) |>
      dplyr::mutate(fleet = as.character(fleet))
  }

  # Make generic plot
  plt <- ggplot2::ggplot(data = land) +
    ggplot2::geom_area(
      ggplot2::aes(
        x = year,
        y = estimate,
        fill = fleet
      )
    ) +
    # ggplot2::facet_wrap(~label)
    # Apply std NOAA theme
    # add_theme(plt)
    ggplot2::labs(
      x = "Year",
      y = land_label,
      fill = "Fleet"
    )

  final <- suppressWarnings(add_theme(plt))

  # create plot-specific variables to use throughout fxn for naming and IDing
  topic_label <- "landings"

  # identify output
  fig_or_table <- "figure"

  # run write_captions.R if its output doesn't exist
  if (!file.exists(
    fs::path(getwd(), "captions_alt_text.csv")
  )
  ) {
    stockplotr::write_captions(
      dat = dat,
      dir = rda_dir,
      year = NULL
    )
  }

  # add more key quantities included as arguments in this fxn
  add_more_key_quants(
    topic = topic_label,
    fig_or_table = fig_or_table,
    dir = rda_dir,
    units = unit_label
  )

  # extract this plot's caption and alt text
  caps_alttext <- extract_caps_alttext(
    topic_label = topic_label,
    fig_or_table = fig_or_table,
    dir = rda_dir
  )

  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    export_rda(
      final = final,
      caps_alttext = caps_alttext,
      rda_dir = rda_dir,
      topic_label = topic_label,
      fig_or_table = fig_or_table
    )
  }
  return(final)
}
