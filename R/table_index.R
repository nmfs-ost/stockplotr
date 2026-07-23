#' Index of abundance table
#'
#' @inheritParams plot_recruitment
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
#' in this ordered list: "index_weight",  "index_numbers", "index_expected",
#' "index_predicted", "index".
#'
#' Default: NULL
#'
#' @returns A table of observed annual index of abundance plus error,
#' stratified by fleet.
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a [gt::gt()] object or export an rda object
#' containing a gt-based table, caption, and LaTeX-based table.
#' @seealso [convert_output()], [filter_data()], [process_table()], [export_kqs()], [insert_kqs()], [create_rda()]
#' @export
#'
#' @examples
#' \dontrun{
#' table_index(dat)
#'
#' table_index(
#'   dat,
#'   make_rda = TRUE,
#'   tables_dir = getwd()
#' )
#' }
table_index <- function(
    dat,
    era = NULL,
    interactive = TRUE,
    group = NULL,
    method = "sum",
    module = NULL,
    label = NULL,
    digits = 2,
    make_rda = FALSE,
    tables_dir = getwd()
    ) {
  
  # TODO: do group and facet need to be uncommented and updated?
  # Filter data for landings
  prepared_data <- filter_data(
    dat = dat,
    label_name = "index_observed|index_predicted",
    geom = "line",
    era = era,
    module = module,
    scale_amount = 1,
    interactive = interactive
  ) |>
    dplyr::mutate(estimate = round(as.numeric(estimate), digits = digits),
                  uncertainty = round(as.numeric(uncertainty), digits = digits))
  
  # Add check if there is any data
  if (nrow(prepared_data) == 0) {
    cli::cli_abort("No index data found.")
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
    label = label,
    digits = digits
  )
  table_data <- table_data_info[[1]]
  indexed_vars <- table_data_info[[2]]
  id_col_vals <- table_data_info[[3]]
  
  # id_group_vals <- sapply(id_cols, function(x) unique(prepared_data[[x]]), simplify = FALSE)
  # TODO: add check if there is a index column for every error column -- if not remove the error (can keep index)
  
  # if (uncert_lab != "") uncert_lab <- glue::glue("({uncert_lab})")
  
  # merge error and index columns and rename
  df_list <- merge_error(
    table_data,
    id_col_vals,
    unit_label = "", # should this be CPUE?
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
    
    # Caption contains no key quantities for index table
    # So, export captions/alt text csv if absent
    if (!file.exists(fs::path(getwd(), "captions_alt_text.csv"))) {
      caps_alttext <- utils::read.csv(
        system.file("resources", "captions_alt_text_template.csv", package = "stockplotr")
      )
    # export df with captions and alt text to csv
    utils::write.csv(
      x = caps_alttext,
      file = fs::path(getwd(), "captions_alt_text.csv"),
      row.names = FALSE
      )    
    }
  
    if (length(df_list) == 1) {
      create_rda(
        object = final$label,
        # get name of function and remove "table_" from it
        topic_label = gsub("table_", "", as.character(sys.call()[[1]])),
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
