#' Plot observed landings by fleet
#'
#' @inheritParams plot_recruitment
#' @param unit_label indicate the name of the units of landings as to label the axis
#' @param fleet_names Indicate the full names of the fleet that will be used in
#' the plot legend. Format should follow
#' c("fleet_name_in_model" = "Desired Fleet Name")
#'
#' @return Create a plot ready for a stock assessment report of cumulative landings
#' over time by fleet.Includes options to plot by fleet, total observed landings
#' with and without predicted landings. Indicate if fleet should be faceted or on one plot (default). Warning: i
#' @export
#'
#' @examples
#' \dontrun{
#' plot_landings(dat)
#'
#' plot_landings(
#'   dat,
#'   unit_label = "my_unit",
#'   end_year = 2024,
#'   make_rda = TRUE,
#'   figures_dir = getwd()
#' )
#' }
plot_landings <- function(dat,
                          unit_label = "mt",
                          end_year = format(Sys.Date(), "%Y"),
                          make_rda = FALSE,
                          figures_dir = getwd(),
                          fleet_names = NULL) {
  # Units
  # TODO: fix unit label is scaling
  land_label <- glue::glue("Landings ({unit_label})")

  # restructure fleet_names
  if (!is.null(fleet_names)) {
    fleet_names <- data.frame(
      fleet = names(fleet_names),
      fleet_names = fleet_names
    )
  }

  # read standard data file and extract target quantity
  land <- dat |>
    dplyr::filter(
      c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
      # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
      !is.na(fleet),
      year <= end_year
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

  if (!is.null(fleet_names)) {
    land <- land |>
      dplyr::left_join(
        fleet_names,
        by = "fleet"
      ) |>
      dplyr::select(-fleet) |>
      dplyr::rename(fleet = fleet_names)
  }

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

  # create plot-specific variables to use throughout fxn for naming and IDing
  topic_label <- "landings"

  # identify output
  fig_or_table <- "figure"

  # check year isn't past end_year if not projections plot
  check_year(
    end_year = end_year,
    fig_or_table = fig_or_table,
    topic = topic_label
  )

  # Choose number of breaks for x-axis
  x_n_breaks <- round(length(unique(land[["year"]])) / 10)
  if (x_n_breaks <= 5) {
    x_n_breaks <- round(length(unique(land[["year"]])) / 5)
    if (length(unique(land[["year"]])) <= 10) {
      x_n_breaks <- NULL
    }
  } else if (x_n_breaks > 10) {
    x_n_breaks <- round(length(unique(land[["year"]])) / 15)
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
    ) +
    ggplot2::scale_x_continuous(
      n.breaks = x_n_breaks
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::label_comma()
    )

  final <- suppressWarnings(add_theme(plt))

  # run write_captions.R if its output doesn't exist
  if (!file.exists(
    fs::path(getwd(), "captions_alt_text.csv")
  )
  ) {
    stockplotr::write_captions(
      dat = dat,
      dir = figures_dir,
      year = end_year
    )
  }

  # add more key quantities included as arguments in this fxn
  add_more_key_quants(
    dat = dat,
    topic = topic_label,
    fig_or_table = fig_or_table,
    dir = figures_dir,
    end_year = end_year,
    units = unit_label
  ) |> suppressWarnings()

  # extract this plot's caption and alt text
  caps_alttext <- extract_caps_alttext(
    topic_label = topic_label,
    fig_or_table = fig_or_table,
    dir = figures_dir
  )

  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    export_rda(
      object = final,
      caps_alttext = caps_alttext,
      figures_tables_dir = figures_dir,
      topic_label = topic_label,
      fig_or_table = fig_or_table
    )
  }
  final
}
