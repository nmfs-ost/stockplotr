#' Biomass, abundance, and catch time series table
#'
#' @inheritParams plot_recruitment
#' @param biomass_unit_label Abbreviated units for biomass
#' Default: "mt"
#' 
#' @param catch_unit_label Abbreviated units for catch
#' Default: "mt"
#' 
#' @param sb_unit_label Abbreviated units for spawning biomass
#' Default: "mt"
#' 
#' @param group A string of a single column that groups the data.
#'
#' Set group = "none" to summarize data over all indexing values.
#'
#' Default: NULL
#' Options: Including, but not limited to: "year", "area", "fleet", "sex", "none", NULL
#' @param method A string describing the method of summarizing data when group
#' is set to "none".
#'
#' Default: "sum"
#'
#' Options: "sum" or "mean"
#' 
#' @param label The label that will be chosen from the input file. If unspecified,
#' the function will search the "label" column and use the first matching label
#' in this ordered list: "landings_weight",  "landings_numbers", "landings_expected",
#' "landings_predicted", "landings".
#' 
#' Default: NULL
#'
#' @param tables_dir The location of the folder containing the table
#' rda files ("tables") that will be created if the argument `make_rda` = TRUE.
#' Default: the working directory (`getwd()`)
#'
#' @returns A table of biomass, abundance, catch, and spawning biomass.
#' @details There are
#' options to return a [gt::gt()] object or export an rda object containing
#' a gt-based table, caption, and LaTeX-based table.
#' @seealso [convert_output()], [filter_data()], [process_table()], [export_kqs()], [insert_kqs()], [create_rda()]
#' @export
#'
#' @examples
#' table_bnc(stockplotr::example_data)
#'
#' table_bnc(stockplotr::example_data,
#'   biomass_unit_label = "b label",
#'   catch_unit_label = "catch label"
#' )
table_bnc <- function(
    dat,
    biomass_unit_label = "mt",
    catch_unit_label = "mt",
    sb_unit_label = "mt",
    era = NULL,
    interactive = TRUE,
    group = NULL,
    method = "sum",
    module = NULL,
    label = NULL,
    make_rda = FALSE,
    tables_dir = getwd()) {
  
  # TODO: do group and facet need to be uncommented and updated?
  # Filter data for landings
  prepared_data <- filter_data(
    dat = dat,
    label_name = "bnc",
    geom = "line",
    era = era,
    module = module,
    scale_amount = 1,
    interactive = interactive
  ) |>
    dplyr::mutate(estimate = round(as.numeric(estimate), digits = 0)) |>
    dplyr::mutate(uncertainty = round(as.numeric(uncertainty), digits = 2))
  
  # Add check if there is any data
  if (nrow(prepared_data) == 0) {
    cli::cli_abort("No bnc data found.")
  }
  
  # get uncertainty label by model
  uncert_lab <- prepared_data |>
    dplyr::filter(!is.na(uncertainty_label)) |>
    dplyr::group_by(model) |>
    dplyr::reframe(unique_uncert = unique(uncertainty_label)) # changed to reframe -- may cause errors
  uncert_lab <- stats::setNames(uncert_lab$unique_uncert, uncert_lab$model)
  # if (length(unique(uncert_lab)) == 1) uncert_lab <- unique(uncert_lab) # might need this line
  
  # This needs to be adjusted when comparing different models and diff error
  if (length(uncert_lab) > 1 & length(unique(uncert_lab)) == 1 | length(names(uncert_lab)) == 1) { # prepared_data$model
    # cli::cli_alert_warning("More than one value for uncertainty exists: {uncert_lab}")
    uncert_lab <- uncert_lab[[1]]
    # cli::cli_alert_warning("The first value ({uncert_lab}) will be chosen.")
  }
  
  if (is.na(uncert_lab)) uncert_lab <- "uncertainty"
  
  # get fleet names
  # TODO: change from fleets to id_group AFTER the process data step and adjust throughout the table based on indexing
  fleets <- unique(prepared_data$fleet) |>
    # sort numerically even if fleets are 100% characters
    stringr::str_sort(numeric = TRUE)
  
  # TODO: fix this so that fleet names aren't removed if, e.g., group = "fleet"
  table_data_info <- process_table(
    dat = prepared_data,
    # group = group,
    method = method,
    label = label
  )
  table_data <- table_data_info[[1]]
  indexed_vars <- table_data_info[[2]]
  id_col_vals <- table_data_info[[3]]
  
  # id_group_vals <- sapply(id_cols, function(x) unique(prepared_data[[x]]), simplify = FALSE)
  # TODO: add check if there is a landings column for every error column -- if not remove the error (can keep landings)
  
  # merge error and landings columns and rename
  df_list <- merge_error(
    table_data,
    uncert_lab,
    fleets,
    label = "landings",
    unit_label
  )
  
  # transform dfs into tables
  final <- lapply(df_list, function(df) {
    df |>
      gt::gt() |>
      add_theme()
  })
  
  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    if (length(df_list) == 1) {
      # Obtain relevant key quantities for captions/alt text
      bnc.b.units <- biomass_unit_label
      bnc.catch.units <- catch_unit_label
      bnc.sb.units <- sb_unit_label
      
      # calculate & export key quantities
      export_kqs(bnc.b.units,
                 bnc.catch.units,
                 bnc.sb.units)
      
      # Add key quantities to captions/alt text
      insert_kqs(bnc.b.units,
                 bnc.catch.units,
                 bnc.sb.units)
      
      create_rda(
        object = final$label,
        # get name of function and remove "table_" from it
        topic_label = gsub("table_", "", as.character(sys.call()[[1]])),
        fig_or_table = "table",
        dat = dat,
        dir = tables_dir,
        scale_amount = 1,
        unit_label = biomass_unit_label,
        table_df = final
      )
    }
  } else {
    cli::cli_alert_warning("Multiple tables cannot be exported at this time.")
    cli::cli_alert_info("We are currently developing this feature.")
  }
  
  # Send table(s) to viewer
  if (!is.data.frame(table_data)) {
    for (t in final) {
      print(t)
    }
    # Return table list invisibly
    return(invisible(final))
  } else {
    # Return finished table (when only one table)
    return(final)
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
#   biomass_label <- glue::glue("Biomass ({biomass_unit_label})")
#   catch_label <- glue::glue("Catch ({catch_unit_label})")
#   sb_label <- glue::glue("Spawning biomass ({sb_unit_label})")
# TODO: Update documentation to match formatting of other functions, with
# lines for Default and Options

#   biomass <- dat |>
#     dplyr::filter(
#       # SS3 params
#       label == "biomass",
#       !is.na(year),
#       module_name %in% c("TIME_SERIES", "t.series"),
#       is.na(fleet), is.na(sex), is.na(area), is.na(growth_pattern),
#       module_name != "DERIVED_QUANTITIES"
#     ) |>
#     dplyr::mutate(estimate = round(as.numeric(estimate), digits = 2)) |>
#     dplyr::rename(biomass = estimate) |>
#     dplyr::select(year, biomass)

#   if (length(unique(biomass$year)) != nrow(biomass)) {
#     cli::cli_abort("Duplicate years found in biomass df.")
#   }

#   catch <- dat |>
#     dplyr::filter(
#       # SS3 params
#       grepl("catch$", label) | grepl("landings_observed", label),
#       !is.na(year), is.na(age),
#       # module_name %in% c("TIME_SERIES","t.series"),
#       # is.na(fleet), is.na(sex), is.na(area), is.na(growth_pattern),
#       module_name != "DERIVED_QUANTITIES"
#     ) |>
#     dplyr::mutate(estimate = round(as.numeric(estimate), digits = 2))
#   # Check if df is by age and summarize to time series
#   if (length(unique(catch$year)) == nrow(catch)) {
#     catch <- catch |>
#       dplyr::rename(total_catch = estimate) |>
#       dplyr::select(year, total_catch)
#   } else {
#     catch <- catch |>
#       # dplyr::filter(!is.na(estimate)) |>
#       dplyr::group_by(year) |> # , fleet
#       dplyr::summarise(total_catch = sum(estimate, na.rm = TRUE))
#     # if  ("fleet" %in% colnames(catch)) {
#     #   catch <- catch |>
#     #     tidyr::pivot_wider(
#     #       id_cols = year,
#     #       names_from = fleet,
#     #       values_from = total_catch
#     #     )
#     # }
#   }

#   abundance <- dat |>
#     dplyr::filter(
#       # SS3 params
#       grepl("mature_abundance", label) | grepl("^abundance", label),
#       !is.na(year),
#       module_name %in% c("TIME_SERIES", "t.series"),
#       # is.na(fleet), is.na(sex), is.na(area), is.na(growth_pattern),
#       module_name != "DERIVED_QUANTITIES"
#     )
#   # Check if there is more than one year aka the values are factored
#   # TODO: Review that sum is OK to use in these cases - otherwise what are
#   #       the alternatives?
#   if (length(unique(abundance$year) != nrow(abundance))) {
#     abundance <- abundance |>
#       dplyr::group_by(year) |>
#       dplyr::summarise(estimate = sum(as.numeric(estimate)))
#   }

#   abundance <- abundance |>
#     dplyr::mutate(estimate = round(as.numeric(estimate), digits = 0)) |>
#     dplyr::rename(abundance = estimate) |>
#     dplyr::select(year, abundance)

#   # Checks
#   if (length(unique(catch$year)) != nrow(catch)) cli::cli_alert_warning("[catch] dataframe needs review.")
#   if (length(unique(biomass$year)) != nrow(biomass)) cli::cli_alert_warning("[biomass] dataframe needs review.")
#   if (length(unique(abundance$year)) != nrow(abundance)) cli::cli_alert_warning("[abundance] dataframe needs review.")

#   sb <- dat |>
#     dplyr::filter(
#       label == "spawning_biomass",
#       module_name %in% c("DERIVED_QUANTITIES", "t.series")
#     ) |>
#     dplyr::mutate(
#       estimate = round(as.numeric(estimate), digits = 2)
#     ) |>
#     dplyr::rename(spawning_biomass = estimate) |>
#     dplyr::select(year, spawning_biomass)

#   # Bring together quantities for table
#   bnc <- biomass |>
#     dplyr::left_join(sb, by = "year") |>
#     dplyr::left_join(abundance, by = "year") |>
#     dplyr::left_join(catch, by = "year") |>
#     dplyr::mutate(year = as.factor(year)) |>
#     # apply table
#     flextable::flextable() |>
#     flextable::set_header_labels(
#       year = "Year",
#       biomass = biomass_label,
#       abundance = "Abundance",
#       total_catch = catch_label,
#       spawning_biomass = sb_label
#     )

#   # add theming to final table
#   final <- suppressWarnings(add_theme(bnc))

#   # export figure to rda if argument = T
#   if (make_rda == TRUE) {
#     # run write_captions.R if its output doesn't exist
#     if (!file.exists(
#       fs::path(getwd(), "captions_alt_text.csv")
#     )
#     ) {
#       stockplotr::write_captions(
#         dat = dat,
#         dir = tables_dir,
#         year = NULL
#       )
#     }

#     # extract this plot's caption and alt text
#     caps_alttext <- extract_caps_alttext(
#       topic_label = topic_label,
#       fig_or_table = fig_or_table,
#       dir = tables_dir
#     )
#
#     export_rda(
#       object = final,
#       caps_alttext = caps_alttext,
#       figures_tables_dir = tables_dir,
#       # get name of function and remove "table_" from it
#       topic_label = gsub("table_", "", as.character(sys.call()[[1]])),
#       fig_or_table = fig_or_table
#     )
#   }
#   # Return finished table
#   final
# }
