#' Landed catch by fleet and year table
#'
#' @inheritParams plot_recruitment
#' @param unit_label Abbreviated units of landings
#'
#' Default: "mt"
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
#' @param digits Numeric value indicating the number of digits values in the
#' table will be rounded to.
#'
#' Default: 2
#' @param tables_dir The location of the folder containing the generated table
#' rda files ("tables") that will be created if the argument `make_rda` = TRUE.
#'
#' Default: the working directory (`getwd()`)
#' @param label The label that will be chosen from the input file. If unspecified,
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
#' table_landings(stockplotr::example_data)
#'
#' table_landings(
#'   stockplotr::example_data,
#'   unit_label = "landings label",
#'   group = "fleet"
#' )
table_landings <- function(
  dat,
  unit_label = "mt",
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
  # set unit label if scaled
  unit_label <- label_magnitude(
    label = "",
    unit_label = unit_label,
    scale_amount = scale_amount
  )

  # TODO: do group and facet need to be uncommented and updated?
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

  # id_group_vals <- sapply(id_cols, function(x) unique(prepared_data[[x]]), simplify = FALSE)
  # TODO: add check if there is a landings column for every error column -- if not remove the error (can keep landings)

  # merge error and landings columns and rename
  df_list <- merge_error(
    table_data,
    id_col_vals,
    unit_label,
    uncert_lab
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
      landings.units <- unit_label

      # calculate & export key quantities
      export_kqs(landings.units)

      # Add key quantities to captions/alt text
      insert_kqs(landings.units)

      create_rda(
        object = final$label,
        # get name of function and remove "table_" from it
        topic_label = gsub("table_", "", utils::tail(as.character(sys.call()[[1]]), n = 1)),
        fig_or_table = "table",
        dat = dat,
        dir = tables_dir,
        scale_amount = 1,
        unit_label = unit_label,
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
