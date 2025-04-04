#' Landed catch by fleet and year table
#'
#' @inheritParams plot_recruitment
#' @param unit_label Abbreviated units of landings
#'
#' @return Create a table ready for a stock assessment report of landed catch by
#' fleet and year.
#' @export
#'
table_landings <- function(dat,
                           unit_label = "mt",
                           make_rda = FALSE,
                           rda_dir = getwd()) {

  # Units
  land_label <- glue::glue("Landings ({unit_label})")

  # read standard data file and extract target quantity
  land_dat <- dat |>
    dplyr::filter(
      c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
      # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
      !is.na(fleet)
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year)
    ) |>
    suppressWarnings() |>
    dplyr::filter(
      !is.na(year)
    )

  # if (is.numeric(land_dat$fleet)){
  #   land_dat$fleet <- paste0("00", land_dat$fleet)
  # }

  if ("uncertainty" %in% names(land_dat)){
    if ("uncertainty_label" %in% names(land_dat)) {
      uncert_label <- land_dat |>
        dplyr::select(uncertainty_label) |>
        unique() |>
        as.character() |>
        toupper()

      if (uncert_label != "NA"){
        land_dat <- land_dat |>
          dplyr::rename(!!(uncert_label) := "uncertainty")

        piv_vals <- c("Landings",
                      uncert_label)
      } else {
        uncert_label <- NULL
        piv_vals <- "Landings"
      }
    }
  } else {
    uncert_label <- NULL
    piv_vals <- "Landings"
  }

  # TODO: Reorder column names so that numeric fleets show up in chronological
  # order (currently, lists 1, 10, 11, 12, etc.)

  # Check number of areas and season - if any are >1 then need to use alternative plot (or summarize)
  narea <- length(unique(land_dat$area))
  nseas <- length(unique(land_dat$season))

  if (narea > 1) {
   # factors <- TRUE
   idcols <- c("year", "Area")
    # will need facet if TRUE
  } else {
    idcols <- c("year")
    # factors <- FALSE
  }

  # Check for nseas > 1 - mean of landings through the year
  if (nseas > 1) {
    land_dat <- land_dat |>
      dplyr::group_by(year, fleet, sex, area, growth_pattern) |>
      dplyr::summarize(estimate = mean(estimate)) |>
      dplyr::mutate(fleet = as.factor(fleet)) |>
      dplyr::rename("Area" = area)
  }

  # Extract fleet names
  fleet_names <- unique(as.character(land_dat$fleet))

  land <- land_dat |>
    dplyr::mutate(
      fleet = as.factor(fleet),
    #  fleet = paste0("Fleet_", fleet),
                  year = as.factor(year),
                  estimate = round(estimate, digits = 0)) |>
    dplyr::rename("Landings" = estimate) |>
    dplyr::relocate(fleet, .after = season) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(idcols),
      names_from = fleet,
    #  names_prefix = "Fleet_",
      values_from = piv_vals,
    names_glue = "Fleet {fleet}_{.value}"
    ) |>
    dplyr::rename("Year" = year)

  land <- land |>
    dplyr::select(order(colnames(land),
                  method = "auto")) |>
    dplyr::relocate(Year, .before = 1) |>
    dplyr::rename_with(~stringr::str_replace(.,
                                               'Landings',
                                             land_label))

  # add theming to final table
  final <- land |>
    flextable::flextable() |>
    flextable::separate_header() |>
    flextable::merge_h(part = "header") |>
    flextable::align(part = "header") |>
    add_theme() |>
    suppressWarnings()
  final

  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    # create plot-specific variables to use throughout fxn for naming and IDing
    topic_label <- "landings"

    # identify output
    fig_or_table <- "table"

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
      dat,
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

    export_rda(
      final = final,
      caps_alttext = caps_alttext,
      rda_dir = rda_dir,
      topic_label = topic_label,
      fig_or_table = fig_or_table
    )
  }
  # Return finished table
  return(final)
}
