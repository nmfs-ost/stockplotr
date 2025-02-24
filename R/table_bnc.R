#' Biomass, abundance, and catch time series table
#'
#' @inheritParams plot_recruitment
#' @param biomass_unit_label abbreviated units for biomass
#' @param catch_unit_label abbreviated units for catch
#'
#' @return Create a table of biomass, abundance, and catch through all years of
#' the assessment model output translated to a standard structure.There are
#' options to return a {flextable} object or export an rda object containing
#' associated caption for the table.
#' @export
#'
table_bnc <- function(
    dat,
    end_year = NULL,
    biomass_unit_label = "mt",
    catch_unit_label = "mt",
    make_rda = FALSE,
    rda_dir = getwd()
    ) {

  biomass_label <- glue::glue("Biomass ({biomass_unit_label})")
  catch_label <- glue::glue("Catch ({catch_unit_label})")

  if (is.null(end_year)){
    end_year <- {max(as.numeric(dat$year), na.rm = TRUE) - 10} |> suppressWarnings()
  }

  dat <- dplyr::filter(dat, year<=end_year)

  biomass <- dat |>
    dplyr::filter(
      # SS3 params
      label == "biomass",
      !is.na(year),
      module_name %in% c("TIME_SERIES","t.series"),
      is.na(fleet), is.na(sex), is.na(area), is.na(growth_pattern),
      module_name != "DERIVED_QUANTITIES"
    ) |>
    dplyr::mutate(estimate = round(as.numeric(estimate), digits = 2)) |>
    dplyr::rename(biomass = estimate) |>
    dplyr::select(year, biomass)

  if (length(unique(biomass$year)) != nrow(biomass)){
    stop("Duplicate years found in biomass df.")
  }

  catch <- dat |>
    dplyr::filter(
      # SS3 params
      grepl("catch$", label) | grepl("landings_observed", label),
      !is.na(year), is.na(age),
      # module_name %in% c("TIME_SERIES","t.series"),
      # is.na(fleet), is.na(sex), is.na(area), is.na(growth_pattern),
      module_name != "DERIVED_QUANTITIES"
    ) |>
    dplyr::mutate(estimate = round(as.numeric(estimate), digits = 2))
  # Check if df is by age and summarize to time series
  if (length(unique(catch$year)) == nrow(catch)) {
    catch <- catch |>
      dplyr::rename(total_catch = estimate) |>
      dplyr::select(year, total_catch)
  } else {
    catch <- catch |>
      # dplyr::filter(!is.na(estimate)) |>
      dplyr::group_by(year) |> #, fleet
      dplyr::summarise(total_catch = sum(estimate, na.rm = TRUE))
    # if  ("fleet" %in% colnames(catch)) {
    #   catch <- catch |>
    #     tidyr::pivot_wider(
    #       id_cols = year,
    #       names_from = fleet,
    #       values_from = total_catch
    #     )
    # }
  }

  abundance <- dat |>
    dplyr::filter(
      # SS3 params
      grepl("mature_abundance", label)| grepl("^abundance", label),
      !is.na(year),
      module_name %in% c("TIME_SERIES","t.series"),
      # is.na(fleet), is.na(sex), is.na(area), is.na(growth_pattern),
      module_name != "DERIVED_QUANTITIES"
    )
  # Check if there is more than one year aka the values are factored
  # TODO: Review that sum is OK to use in these cases - otherwise what are
  #       the alternatives?
  if(length(unique(abundance$year) != nrow(abundance))) {
    abundance <- abundance |>
      dplyr::group_by(year) |>
      dplyr::summarise(estimate = sum(as.numeric(estimate)))
  }

  abundance <- abundance |>
    dplyr::mutate(estimate = round(as.numeric(estimate), digits = 0)) |>
    dplyr::rename(abundance = estimate) |>
    dplyr::select(year, abundance)

  # Checks
  if (length(unique(catch$year)) != nrow(catch)) warning("[catch] dataframe needs review.")
  if (length(unique(biomass$year)) != nrow(biomass)) warning("[biomass] dataframe needs review.")
  if (length(unique(abundance$year)) != nrow(abundance)) warning("[abundance] dataframe needs review.")

  # Bring together quantities for table
  bnc <- biomass |>
    dplyr::left_join(abundance, by = "year") |>
    dplyr::left_join(catch, by = "year") |>
    dplyr::mutate(year = as.factor(year)) |>
    # apply table
    flextable::flextable() |>
    flextable::set_header_labels(
      year = "Year",
      biomass = biomass_label,
      abundance = "Abundance",
      total_catch = catch_label
    )

  # add theming to final table
  final <- suppressWarnings(add_theme(bnc))

  # export figure to rda if argument = T
  if (make_rda == TRUE){
  # create plot-specific variables to use throughout fxn for naming and IDing
  topic_label <- "bnc"

  # identify output
  fig_or_table <- "table"

  # run write_captions.R if its output doesn't exist
  if (!file.exists(
    fs::path(getwd(), "captions_alt_text.csv"))
  ) {
    stockplotr::write_captions(dat = dat,
                         dir = rda_dir,
                         year = NULL)
  }

  # extract this plot's caption and alt text
  caps_alttext <- extract_caps_alttext(topic_label = topic_label,
                                       fig_or_table = fig_or_table,
                                       dir = rda_dir)

  export_rda(final = final,
               caps_alttext = caps_alttext,
               rda_dir = rda_dir,
               topic_label = topic_label,
               fig_or_table = fig_or_table)
  }
  # Return finished table
  return(final)
}
