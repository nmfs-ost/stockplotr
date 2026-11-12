#' Biomass, abundance, and catch time series table
#'
#' @inheritParams plot_recruitment
#'
#' @param biomass_label String. Units for biomass
#'
#' Default: NULL
#'
#' @param abundance_label String. Units for abundance
#'
#' Default: NULL
#'
#' @param catch_label String. Units for catch
#'
#' Default: NULL
#'
#' @param group A string of a single column that groups the data.
#'
#' Set group = "none" to summarize data over all indexing values.
#'
#' Default: NULL
#' Options: Including, but not limited to: "year", "area", "fleet", "sex", "none", NULL
#'
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
#' table_biomass_abundance_catch(stockplotr::example_data)
#'
#' table_biomass_abundance_catch(stockplotr::example_data,
#'   biomass_label = "kg",
#'   abundance_label = "eggs",
#'   catch_label = "kg",
#'   module = c("TIME_SERIES", "CATCH", "TIME_SERIES"),
#'   interactive = FALSE
#' )
table_biomass_abundance_catch <- function(
  dat,
  biomass_label = NULL,
  abundance_label = NULL,
  catch_label = NULL,
  era = NULL,
  interactive = TRUE,
  group = NULL,
  method = "sum",
  module = NULL,
  label = NULL,
  make_rda = FALSE,
  tables_dir = getwd()
) {
  check_data(dat)

  named_vec <- c(
    "^biomass$" = "biomass",
    "^abundance" = "abundance",
    "catch$|landings_observed" = "catch"
  )
  
  biomass_abundance_catch.biomass.units <- NA
  biomass_abundance_catch.abundance.units <- NA
  biomass_abundance_catch.catch.units <- NA

  # Aiming for totals
  # iterate through label_names
  lab_list <- purrr::map(
    names(named_vec),
    function(x) {
      cli::cli_alert_info(paste0("Processing ", x))
      filtered_data <- filter_data(
        dat = dat |>
          dplyr::filter(is.na(age), is.na(length_bins)),
        label_name = x,
        geom = "line",
        era = "time",
        module = module,
        scale_amount = 1,
        interactive = interactive
      ) 

      uncertainty_label <- ifelse(
        is.na(unique(filtered_data$uncertainty_label)),
        "Uncertainty",
        unique(filtered_data$uncertainty_label)
      )

      if (x == "catch$|landings_observed") {
        extra_grouping <- check_grouping(filtered_data)[-grep("year|fleet", check_grouping(filtered_data))]

        filtered_data <- filtered_data |>
          dplyr::summarise(
            estimate = sum(as.numeric(estimate), na.rm = TRUE),
            estimate_upper = mean(as.numeric(estimate_upper), na.rm = TRUE),
            estimate_lower = mean(as.numeric(estimate_lower), na.rm = TRUE),
            uncertainty_label = unique(uncertainty_label),
            uncertainty = mean(uncertainty, na.rm = TRUE),
            unit = unique(.data$unit),
            .by = c(year, model, label)
          ) # |>
        # dplyr::ungroup()
      } else if (x == "^abundance") {
        extra_grouping <- check_grouping(filtered_data)[-grep("year|sex", check_grouping(filtered_data))]

        filtered_data <- filtered_data |>
          dplyr::summarise(
            estimate = sum(as.numeric(estimate), na.rm = TRUE),
            estimate_upper = mean(as.numeric(estimate_upper), na.rm = TRUE),
            estimate_lower = mean(as.numeric(estimate_lower), na.rm = TRUE),
            uncertainty_label = unique(uncertainty_label),
            uncertainty = mean(uncertainty, na.rm = TRUE),
            unit = unique(.data$unit),
            .by = c(year, model, label)
          ) # |>
        # dplyr::ungroup()
      } else {
        extra_grouping <- check_grouping(filtered_data)[-grep("year", check_grouping(filtered_data))]
      }

      if (length(extra_grouping) > 0) {
        cli::cli_abort(
          "Table not created due to {extra_grouping} variables in {x}."
        )
      }

      # Add check if there is any data
      if (nrow(filtered_data) == 0) {
        cli::cli_abort("No {x} data found.")
      }

      # process to reduce columns
      processed_data <- process_table(
        filtered_data,
        digits = 2
      )

      data <- processed_data[[1]]
      group <- processed_data[[2]]
      
      # Labs
      # get unit label for biomass, abundance, and catch if not NULL
      unit_name <- named_vec[grep(x, names(named_vec), fixed = TRUE)][[1]]
      if (is.null(get(paste0(unit_name, "_label")))) {
        unit_label <- filtered_data |>
          dplyr::filter(grepl(x, unit_name),
                        !is.na(.data$unit)) |>
          dplyr::pull("unit") |>
          unique()
        if (is.na(unit_label) || length(unit_label) == 0) {
          cli::cli_alert_warning("No unit label found in the data for {unit_name}. Please provide a unit label using the '{unit_name}_label' argument.")
          unit_label <- ""
        }
      } else {
        unit_label <- get(paste0(unit_name, "_label"))
      }
      
      # Obtain relevant key quantities for captions/alt text
      idx <- match(x, names(named_vec)[1:3])
      if (identical(idx, 1L)) biomass_abundance_catch.biomass.units <<- unit_label
      if (identical(idx, 2L)) biomass_abundance_catch.abundance.units <<- unit_label
      if (identical(idx, 3L)) biomass_abundance_catch.catch.units <<- unit_label

      # merge data with uncertainty and unit labels
      merge_error(
        table_data = data,
        id_col_vals = group,
        unit_label = unit_label,
        uncert_lab = uncertainty_label
      )[[1]]
    }
  )

  combine_data <- purrr::reduce(
    purrr::compact(lab_list),
    dplyr::full_join,
    by = c("Year")
  ) |>
    gt::gt()

  final <- theme_table(combine_data)
  
  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    # calculate & export key quantities
    export_kqs(
      biomass_abundance_catch.biomass.units,
      biomass_abundance_catch.abundance.units,
      biomass_abundance_catch.catch.units,
      dir=tables_dir
    )

    # Add key quantities to captions/alt text
    insert_kqs(
      biomass_abundance_catch.biomass.units,
      biomass_abundance_catch.abundance.units,
      biomass_abundance_catch.catch.units,
      dir=tables_dir
    )

    create_rda(
      object = final,
      # get name of function and remove "table_" from it
      topic_label = gsub("table_", "", utils::tail(as.character(sys.call()[[1]]), n = 1)),
      fig_or_table = "table",
      dat = dat,
      dir = tables_dir,
      scale_amount = 1,
      table_df = final
    )
  }

  rm(biomass_abundance_catch.biomass.units, biomass_abundance_catch.abundance.units, biomass_abundance_catch.catch.units)
  
  if (exists("selected_module")) {
    rm(selected_module, envir = .GlobalEnv)
  }
  
  final
}
