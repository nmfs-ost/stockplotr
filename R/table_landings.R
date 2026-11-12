#' Landed catch by fleet and year table
#'
#' @inheritParams plot_recruitment
#' @param unit_label String. Abbreviated landings units
#'
#' Default: NULL
#' @param group String. Single column that groups the data.
#'
#' Set group = "none" to summarize data over all indexing values.
#'
#' Default: NULL
#' Options: Including, but not limited to: "year", "area", "fleet", "sex", "none", NULL
#' @param method String. Method for summarizing data when group
#' is set to "none".
#'
#' Default: "sum"
#'
#' Options: "sum" or "mean"
#' @param digits Number. Numeric value indicating the number of digits values in the
#' table will be rounded to.
#'
#' Default: 2
#' @param tables_dir Path. The location of the folder containing the generated table
#' rda files ("tables") that will be created if the argument `make_rda` = TRUE.
#'
#' Default: the working directory (`getwd()`)
#' @param label String. The label that will be chosen from the input file. If unspecified,
#' the function will search the "label" column and use the first matching label
#' in this ordered list: "landings_weight",  "landings_numbers", "landings_expected",
#' "landings_predicted", "landings".
#'
#' Default: NULL
#'
#' @returns A table ready of landed catch by fleet and year.
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a [gt::gt()] object or export an rda object
#' containing a gt-based table, caption, and LaTeX-based table.
#'
#' @seealso [convert_output()], [filter_data()], [process_table()], [export_kqs()], [insert_kqs()], [create_rda()]
#' @export
#'
#' @examples
#' table_landings(stockplotr::example_data,
#'   interactive = FALSE)
#'
#' table_landings(
#'   stockplotr::example_data,
#'   unit_label = "kg",
#'   group = "fleet"
#' )
table_landings <- function(
  dat,
  unit_label = NULL,
  era = NULL,
  interactive = TRUE,
  group = NULL,
  method = "sum",
  module = NULL,
  label = NULL,
  digits = 2,
  scale_amount = 1,
  make_rda = FALSE,
  tables_dir = getwd()
) {
  check_data(dat)

  # Filter data for landings
  prepared_data <- filter_data(
    dat = dat,
    label_name = "landings",
    geom = "line",
    era = era,
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  ) |>
    dplyr::mutate(estimate = round(as.numeric(estimate), digits = digits)) |>
    dplyr::mutate(uncertainty = round(as.numeric(uncertainty), digits = digits))

  # Add check if there is any data
  if (nrow(prepared_data) == 0) {
    cli::cli_abort("No landings data found.")
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

  if (length(uncert_lab) == 0 || is.na(uncert_lab)) uncert_lab <- "uncertainty"

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
    label = label,
    digits = digits
  )
  table_data <- table_data_info[[1]]
  indexed_vars <- table_data_info[[2]]
  id_col_vals <- table_data_info[[3]]
  
  if (is.null(unit_label)) {
    # remove fleet names from table_data colnames
    fleet_pattern <- paste0("_(", paste(fleets, collapse = "|"), ")$")
    cleaned_colnames <- gsub(fleet_pattern, "", colnames(table_data[[1]]))
    unit_label <- prepared_data |>
      dplyr::filter(label %in% cleaned_colnames) |>
      dplyr::select(unit) |>
      unique() |>
      dplyr::pull(unit)
    if (is.na(unit_label)) {
      cli::cli_alert_warning("No unit label found in the data. Please provide a unit label using the 'unit_label' argument.")
      unit_label <- ""
    }
  }
  
  # set unit label if scaled
  landings_label <- label_magnitude(
    label = "",
    unit_label = unit_label,
    scale_amount = scale_amount
  )

    # TODO: add check if there is a landings column for every error column -- if not remove the error (can keep landings)

  # merge error and landings columns and rename
  final_tab <- merge_error(
    table_data,
    id_col_vals,
    landings_label,
    uncert_lab
  )

  final <- final_tab[[1]] |>
    gt::gt() |>
    theme_table()

  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    # Obtain relevant key quantities for captions/alt text
    landings.units <- unit_label
    
    # calculate & export key quantities
    export_kqs(landings.units, dir = tables_dir)
    
    # Add key quantities to captions/alt text
    insert_kqs(landings.units, dir = tables_dir)
    
    create_rda(
      object = final[[1]],
      # get name of function and remove "table_" from it
      topic_label = gsub("table_", "", utils::tail(as.character(sys.call()[[1]]), n = 1)),
      fig_or_table = "table",
      dat = dat,
      dir = tables_dir,
      scale_amount = 1,
      table_df = final
    )
  }
  
  if (exists("selected_module")) {
    rm(selected_module, envir = .GlobalEnv)
  }
  
  final
}
