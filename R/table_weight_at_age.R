#' Weight at age table
#'
#' @inheritParams plot_recruitment
#' @param unit_label String. Abbreviated weight units
#'
#' Default: "mt"
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
#'
#' @returns A report-ready table of weight by age class and year.
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
#' table_weight_at_age(stockplotr::example_data)
#'
#' table_weight_at_age(
#'   dat = stockplotr::example_data,
#'   unit_label = "grams"
#' )
table_weight_at_age <- function(
  dat,
  unit_label = "mt",
  era = NULL,
  interactive = TRUE,
  group = NULL,
  method = "sum",
  # module = NULL,
  # label = NULL,
  digits = 4,
  scale_amount = 1,
  make_rda = FALSE,
  tables_dir = getwd()
) {
  #TODO: Update to allow for showing other indexing vars like fleet, area, etc
  
  # Filter data for body weight
  prepared_data <- filter_data(
    dat = dat,
    label_name = "body_weight|wgt.klb|weight_hat",
    geom = "point",
    era = NULL,
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  ) |>
    dplyr::distinct(.keep_all = TRUE)

  # Add check if there is any data
  if (nrow(prepared_data) == 0) {
    cli::cli_abort("No weight data found.")
  }

  # TODO: fix this so that fleet names aren't removed if, e.g., group = "fleet"
  table_data_info <- process_table(
    dat = prepared_data,
    # group = group,
    # method = method,
    # label = label,
    digits = digits
  ) |>
    suppressWarnings()
  
  table_data <- table_data_info[[1]]
  indexed_vars <- table_data_info[[2]]
  id_col_vals <- table_data_info[[3]]

  
  if (length(indexed_vars) > 1){
    waa_data <- prepared_data |>
      dplyr::group_by(year, sex, age) |>
      dplyr::summarise(Weight = mean(estimate))
  } else {
    waa_data <- prepared_data |>
      dplyr::select(estimate, year, sex, age) |>
      dplyr::group_by(year, sex, age) |>
      dplyr::summarise(Weight = mean(estimate))
  }
  
  # transform df into table
  final <- waa_data |>
    dplyr::ungroup() |>
    dplyr::mutate(age = as.numeric(age)) |>
    dplyr::arrange(sex, age) |>
    dplyr::select(where(~ !all(is.na(.)))) |>
    dplyr::mutate(Weight = round(Weight, digits = digits)) |>
    tidyr::pivot_wider(names_from = age,
                       values_from = Weight) |>
    dplyr::rename_with(stringr::str_to_title) |>
    gt::gt(
      rowname_col = NULL,
      groupname_col = NULL
      ) |>
      theme_table()

  final
  
  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    if (length(df_list) == 1) {
      # Obtain relevant key quantities for captions/alt text
      waa.units <- unit_label

      # calculate & export key quantities
      export_kqs(waa.units)

      # Add key quantities to captions/alt text
      insert_kqs(waa.units)

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
  # if (!is.data.frame(table_data)) {
  #   for (t in final) {
  #     print(t)
  #   }
  #   # Return table list invisibly
  #   return(invisible(final))
  # } else {
  #   # Return finished table (when only one table)
    return(final)
  # }
}
