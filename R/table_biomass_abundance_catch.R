#' Biomass, abundance, and catch time series table
#'
#' @inheritParams plot_recruitment
#' @param biomass_unit_label Abbreviated units for biomass
#' Default: "mt"
#' 
#' @param abundance_unit_label Abbreviated units for abundance
#' Default: "fish"
#' 
#' 
#' @param catch_unit_label Abbreviated units for catch
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
#' table_biomass_abundance_catch(stockplotr::example_data)
#'
#' table_biomass_abundance_catch(stockplotr::example_data,
#'   biomass_unit_label = "b label",
#'   catch_unit_label = "catch label",
#'   module = c("TIME_SERIES", "CATCH", "TIME_SERIES"),
#'   interactive = FALSE
#' )
table_biomass_abundance_catch <- function(
    dat,
    biomass_unit_label = "mt",
    abundance_unit_label = "fish",
    catch_unit_label = "mt",
    era = NULL,
    interactive = TRUE,
    group = NULL,
    method = "sum",
    module = NULL,
    label = NULL,
    make_rda = FALSE,
    tables_dir = getwd()) {
  
  named_vec <- c("^biomass$" = biomass_unit_label,
                 "catch$|landings_observed" = catch_unit_label,
                # "mature_abundance|^abundance" = abundance_unit_label)
  "^abundance" = abundance_unit_label)

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
      ) #|>
        # dplyr::mutate(estimate = round(as.numeric(estimate), digits = 0)) |>
        # dplyr::mutate(uncertainty = round(as.numeric(uncertainty), digits = 2))
      
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
            .by = c(year, model, label))# |>
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
        digits = 2)
      
      data <- processed_data[[1]]
      group <- processed_data[[2]]

      # merge data with uncertainty and unit labels
      merge_error(
        table_data = data,
        id_col_vals = group,
        unit_label = named_vec[grep(x, names(named_vec), fixed = TRUE)][[1]],
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
  
  final_table <- theme_table(combine_data)
  
  # export figure to rda if argument = T
  if (make_rda == TRUE) {
      # Obtain relevant key quantities for captions/alt text
      biomass_abundance_catch.b.units <- biomass_unit_label
      biomass_abundance_catch.catch.units <- catch_unit_label
      biomass_abundance_catch.abundance.units <- abundance_unit_label
      
      # calculate & export key quantities
      export_kqs(biomass_abundance_catch.b.units,
                 biomass_abundance_catch.catch.units,
                 biomass_abundance_catch.abundance.units)
      
      # Add key quantities to captions/alt text
      insert_kqs(biomass_abundance_catch.b.units,
                 biomass_abundance_catch.catch.units,
                 biomass_abundance_catch.abundance.units)
      
      create_rda(
        object = final_table,
        # get name of function and remove "table_" from it
        topic_label = gsub("table_", "", as.character(sys.call()[[1]])),
        fig_or_table = "table",
        dat = dat,
        dir = tables_dir,
        scale_amount = 1,
        table_df = final_table
      )
    }
  
  final_table
}
  
