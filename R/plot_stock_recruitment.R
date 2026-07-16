#' Plot Stock Recruit Relationship
#'
#' @inheritParams plot_spawning_biomass
#' @param interactive logical. Indicate whether the environment the
#' plot is being made in is interactive. By default, this
#' is set to false. If true, dependent on your data, a
#' option menu will pop-up.
#'
#' Default: TRUE
#'
#' @param spawning_biomass_label string. Units for spawning biomass
#'
#' Default: "mt"
#'
#' @param recruitment_label string. units for recruitment
#'
#' Default: "mt"
#'
#' @returns A plot showing the stock recruitment relationship.
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a `ggplot2` object or export an .rda object
#' containing associated caption and alternative text for the figure.
#'
#' @seealso [convert_output()], [filter_data()], [process_data()], [plot_timeseries()], [export_kqs()], [insert_kqs()], [create_rda()]
#'
#'
#' @export
#'
#' @examples
#' plot_stock_recruitment(
#'   dat = stockplotr:::example_data,
#'   interactive = FALSE,
#'   spawning_biomass_label = "metric tons",
#'   recruitment_label = "metric tons",
#'   module = "DERIVED_QUANTITIES"
#' )
plot_stock_recruitment <- function(
  dat,
  spawning_biomass_label = "mt",
  recruitment_label = "mt",
  interactive = TRUE,
  era = NULL,
  module = NULL,
  scale_amount = 1,
  make_rda = FALSE,
  figures_dir = getwd()
) {
  # Extract recruitment
  recruitment <- filter_data(
    dat = dat,
    # TODO: change string to ^recruitment in naming convention change PR
    label_name = "recruitment",
    era = era,
    geom = "point",
    scale_amount = scale_amount,
    interactive = interactive,
    module = module
  ) |>
    # filter for year !na
    dplyr::filter(
      !is.na(year),
      # filter out rec devs if in data
      !grepl("deviations", label)
    )

  process_rec <- process_data(
    recruitment
  )

  rec_proc <- process_rec[[1]]
  group <- process_rec[[2]]
  facet <- process_rec[[3]]

  if (length(unique(recruitment$label)) > 1) {
    rec_proc <- rec_proc |>
      tidyr::pivot_wider(
        id_cols = dplyr::any_of(c("year", "model", "group_var", facet)),
        names_from = label,
        values_from = c(estimate, estimate_lower, estimate_upper)
      )
    # rename columns to remove "estimate"
    colnames(rec_proc) <- gsub("estimate_", "", colnames(rec_proc))
  } else {
    rec_proc <- rec_proc |>
      dplyr::rename(
        predicted_recruitment = estimate,
        lower_predicted_recruitment = estimate_lower,
        upper_predicted_recruitment = estimate_upper
      ) |>
      dplyr::select(-label)
  }

  # if (any(grepl("^recruitment$", colnames(recruitment)))) {
  #   # TODO: adjust naming to recruitment_predicted in naming convention change PR
  #   recruitment <- dplyr::rename(recruitment, predicted_recruitment = recruitment)
  # }

  # Extract spawning biomass
  sb <- filter_data(
    dat = dat,
    label_name = "spawning biomass",
    geom = "point",
    era = era,
    scale_amount = scale_amount,
    interactive = interactive,
    module = module
  ) |>
    dplyr::filter(!is.na(year))

  process_sb <- process_data(
    sb
  )
  sb_proc <- process_sb[[1]] |>
    dplyr::rename(
      spawning_biomass = estimate,
      lower_spawning_biomass = estimate_lower,
      upper_spawning_biomass = estimate_upper
    ) |>
    dplyr::select(-label)
  # group <- process_sb[[2]]
  # facet <- process_sb[[3]]

  # Merge recruitment and spawning biomass data
  sr <- dplyr::left_join(sb_proc, rec_proc, by = c("year", "model", "group_var"))

  # Labs
  recruitment_lab <- label_magnitude(
    label = "Recruitment",
    unit_label = recruitment_label,
    scale_amount = scale_amount,
    legend = FALSE
  )
  sb_lab <- label_magnitude(
    label = "Spawning Biomass",
    unit_label = spawning_biomass_label,
    scale_amount = scale_amount,
    legend = FALSE
  )

  # Plot
  final <- plot_timeseries(
    dat = sr,
    x = "spawning_biomass",
    # TODO: change name to recruitment_predicted in naming convention change PR
    y = "predicted_recruitment",
    geom = "point",
    color = "black",
    xlab = sb_lab,
    ylab = recruitment_lab,
    facet = {
      if (length(unique(sr$model)) > 1) {
        "model"
      } else {
        NULL
      }
    }
  ) +
    #  ggplot2::scale_y_continuous(
    #      labels = scales::label_comma()
    #  ) +
    # want to overwrite the default for x-axis bc it's not year in this case
    ggplot2::scale_x_continuous(
      labels = scales::label_comma()
    ) +
    theme_noaa()

  if ("expected_recruitment" %in% names(sr)) {
    final <- final +
      ggplot2::geom_line(
        data = sr,
        ggplot2::aes(x = spawning_biomass, y = expected_recruitment),
        color = "red"
      )
  }

  # Make RDA
  if (make_rda) {
    # Obtain relevant key quantities for captions/alt text
    sr.age.min <- dat |>
      dplyr::filter(!is.na(year) & !is.na(age)) |>
      dplyr::slice(which.min(age)) |>
      dplyr::select(age) |>
      as.numeric()
    sr.ssb.units <- spawning_biomass_label
    sr.ssb.min <- min(sr$spawning_biomass, na.rm = TRUE) |> round(digits = 3)
    sr.ssb.max <- max(sr$spawning_biomass, na.rm = TRUE) |> round(digits = 3)
    recruitment.min <- min(sr$predicted_recruitment, na.rm = TRUE) |> round(digits = 3)
    recruitment.max <- max(sr$predicted_recruitment, na.rm = TRUE) |> round(digits = 3)
    recruitment.units <- recruitment_label

    # calculate & export key quantities
    export_kqs(
      sr.age.min,
      sr.ssb.units,
      sr.ssb.min,
      sr.ssb.max,
      recruitment.min,
      recruitment.max,
      recruitment.units
    )

    # Add key quantities to captions/alt text
    insert_kqs(
      sr.age.min,
      sr.ssb.units,
      sr.ssb.min,
      sr.ssb.max,
      recruitment.min,
      recruitment.max,
      recruitment.units
    )

    create_rda(
      object = final,
      # get name of function and remove "plot_" from it
      topic_label = gsub("plot_", "", utils::tail(as.character(sys.call()[[1]]), n = 1)),
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir # ,
      # unit_label = unit_label
    )
  }
  final +
    ggplot2::scale_x_continuous(
      labels = scales::label_comma()
    )
}
