#' Landed catch by fleet and year table
#'
#' @inheritParams plot_recruitment
#' @param unit_label Abbreviated units of landings
#' @param group A string identifying the indexing variable of the data. If you
#' want to just summarize the data across all factors, set group = "none".
#' @param method A string describing the method of summarizing data when group
#' is set to "none". Options are "sum" or "mean". Default is "sum".
#' @param tables_dir The location of the folder containing the generated table
#' rda files ("tables") that will be created if the argument `make_rda` = TRUE.
#' @param label The label that will be chosen from the input file. If unspecified,
#' the function will search the "label" column and use the first matching label
#' in this ordered list: "landings_weight",  "landings_numbers", "landings_expected",
#' "landings_predicted", "landings".
#'
#' @return Create a table ready for a stock assessment report of landed catch by
#' fleet and year.
#' @export
#'
#' @examples
#' table_landings(stockplotr::example_data)
#'
#' table_landings(
#'   stockplotr::example_data,
#'   unit_label = "landings label",
#'   group =
#'   )
table_landings <- function(
  dat,
  unit_label = "mt",
  era = NULL,
  interactive = TRUE,
  group = NULL,
  method = "sum",
  module = NULL,
  label = NULL,
  make_rda = FALSE,
  tables_dir = getwd()
) {
  # TODO: do group and facet need to be uncommented and updated?
  # Filter data for landings
  prepared_data <- filter_data(
    dat = dat,
    label_name = "landings",
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
      create_rda(
        object = final$label,
        topic_label = "landings",
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
