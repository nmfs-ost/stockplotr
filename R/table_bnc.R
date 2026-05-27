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
  
  biomass_label <- glue::glue("Biomass ({biomass_unit_label})")
  catch_label <- glue::glue("Catch ({catch_unit_label})")
  sb_label <- glue::glue("Spawning biomass ({sb_unit_label})")
  
  # Filter data for biomass
  prepared_data_b <- filter_data(
    dat = dat,
    label_name = "^biomass$",
    geom = "line",
    era = era,
    module = ifelse("TIME_SERIES" %in% dat$module_name, "TIME_SERIES", 
                    ifelse("t.series" %in% dat$module_name, "t.series", NULL)),
    scale_amount = 1,
    interactive = interactive
  ) |>
    dplyr::mutate(estimate = round(as.numeric(estimate), digits = 0),
                  uncertainty = round(as.numeric(uncertainty), digits = 2))
  
  # Add check if there is any b data
  if (nrow(prepared_data_b) == 0) {
    cli::cli_abort("No biomass data found.")
  }
  
  # Filter data for catch
  labels <- c("catch$", "landings_observed")
  prepared_data_catch <- purrr::map_df(labels, ~ {
    filter_data(
      dat = dat,
      label_name = .x,
      geom = "line",
      era = era,
      module = if ("TIME_SERIES" %in% dat$module_name) "TIME_SERIES" else 
        if ("t.series" %in% dat$module_name) "t.series" else NULL,
      scale_amount = 1,
      interactive = interactive
    )
  }) |>
    dplyr::mutate(
      estimate = round(as.numeric(estimate), digits = 0),
      uncertainty = round(as.numeric(uncertainty), digits = 2)
    )
  
  # Add check if there is any catch data
  if (nrow(prepared_data_catch) == 0) {
    cli::cli_abort("No catch data found.")
  }
  
  # Filter data for abundance
  labels <- c("mature_abundance", "^abundance")
  prepared_data_abun <- purrr::map_df(labels, ~ {
    filter_data(
      dat = dat,
      label_name = .x,
      geom = "line",
      era = era,
      module = if ("TIME_SERIES" %in% dat$module_name) "TIME_SERIES" else 
        if ("t.series" %in% dat$module_name) "t.series" else NULL,
      scale_amount = 1,
      interactive = interactive
    )
  }) |>
    dplyr::mutate(
      estimate = round(as.numeric(estimate), digits = 0),
      uncertainty = round(as.numeric(uncertainty), digits = 2)
    )
  
  # Add check if there is any abundance data
  if (nrow(prepared_data_abun) == 0) {
    cli::cli_abort("No abundance data found.")
  }
  
  # Filter data for spawning biomass
  # TODO: check if the label name shouldn't have carrot and dollar sign; for example data, this includes spawning_biomass and spawning_biomass_unfished
  prepared_data_sb <- filter_data(
    dat = dat,
    label_name = "^spawning_biomass$",
    geom = "line",
    era = era,
    module = ifelse("TIME_SERIES" %in% dat$module_name, "TIME_SERIES", 
                    ifelse("t.series" %in% dat$module_name, "t.series", NULL)),
    scale_amount = 1,
    interactive = interactive
  ) |>
    dplyr::mutate(estimate = round(as.numeric(estimate), digits = 0)) |>
    dplyr::mutate(uncertainty = round(as.numeric(uncertainty), digits = 2))
  
  # Add check if there is any sb data
  if (nrow(prepared_data_sb) == 0) {
    cli::cli_abort("No spawning biomass data found.")
  }
  
  prepared_data <- rbind(prepared_data_b,
                         prepared_data_catch,
                         prepared_data_abun,
                         prepared_data_sb)
  
  # TODO: Decide if uncertainty is necessary for this table
  # it's not present for B or SB but is present for catch in example data
  
  # TODO: check correct to not collect fleet names
  
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

  # merge error and landings columns and rename
  df_list <- merge_error(
    table_data,
    uncert_lab,
    fleets,
    label = "landings",
    unit_label
  )
  
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
}
  
  
  
  
  
  
  
  
  
  


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