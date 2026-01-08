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
#' )
table_landings <- function(
    dat,
    unit_label = "mt",
    era = NULL,
    interactive = TRUE,
    group = NULL,
    method = "sum",
    module = NULL,
    scale_amount = 1,
    # Consider moving label out and make it automated - I set foundations for this
    label = NULL,
    make_rda = FALSE,
    tables_dir = getwd()) {
  
  #TODO: do group and facet need to be uncommented and updated?
  # Filter data for landings
  prepared_data <- filter_data(
    dat = dat,
    label_name = "landings",
    geom = "line",
    era = era,
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  )
  
  # Add check for length label >1
  # below method will only work when unqiue(label) == 2
  if (length(unique(prepared_data$label)) > 1){
    cli::cli_alert_info("Multiple labels found in prepared data: {unique(prepared_data$label)}")
    # check if the multiple labels are equal for all indexing
    # all non-indexing variables
    indexing_vars <- colnames(prepared_data)[-grep(
      paste(
        c("year", "estimate", 
          "uncertainty", "uncertainty_label", 
          "label", "module_name", 
          "likelihood", "initial"),
        collapse = "|"), colnames(prepared_data))]

    # compare estimate across all indexing vars and see if they are different over years
    label_differences <- prepared_data |>
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(indexing_vars),
        names_from = label,
        values_from = estimate
      ) |>
      dplyr::mutate(
        diff = .data[[unique(prepared_data$label)[1]]] - .data[[unique(prepared_data$label)[2]]]
      )
    
    if (all(label_differences$diff == 0)){
      cli::cli_alert_info("Labels have identical values. Using only the first label: {unique(prepared_data$label)[1]}")
      prepared_data <- prepared_data |>
        dplyr::filter(label == unique(prepared_data$label)[1])
      multi_label <- FALSE
    } else  {
      multi_label <- TRUE
    }
  }
  
  # order potential labels by applicability
  ordered_labels <- c(
    # "landings_weight",
    # "landings_numbers",
    # "landings_expected",
    # "landings_predicted",
    "landings_observed_weight",
    "landings_predicted_weight",
    "landings_observed_number",
    "landings_predicted_number",
    "landings")
  
  # TODO: evaluate this step and see if it can be condensed with the changes
  if (is.null(label) & multi_label){
    cli::cli_alert_info("`label` not specified.")
    # Choose label to filter by, based on presence in prepared_data
    for (lab in ordered_labels) {
      if (lab %in% prepared_data$label) {
        target_label <- lab
        break
      }
    }
    cli::cli_alert_info("`label` selected as {target_label}.")
  } else if (length(label) > 1 & multi_label){
    cli::cli_alert_info("More than one `label` exists.")
    target_label <- label[1]  
    cli::cli_alert_info("The first `label` value will be selected {target_label}.")
  } else if (!is.null(label)){
    target_label <- label
  } else {
    target_label <- unique(prepared_data$label)
  }

  prepared_data2 <- prepared_data |>
    dplyr::filter(label == target_label) |>
    dplyr::mutate(estimate = round(as.numeric(estimate), digits = 0)) |>
    dplyr::mutate(uncertainty = round(as.numeric(uncertainty), digits = 2))
  
  # get uncertainty label
  uncert_lab <- prepared_data2$uncertainty_label |> 
    unique()
  
  if (length(uncert_lab) > 1){
    cli::cli_alert_warning("More than one value for uncertainty exists: {uncert_lab}")
    uncert_lab <- uncert_lab[[1]]
    cli::cli_alert_warning("The first value ({uncert_lab}) will be chosen.")
  }
  
  # get fleet names
  fleets <- prepared_data2$fleet |>
    unique() |>
    # sort numerically even if fleets are 100% characters
    stringr::str_sort(numeric = TRUE)

  #TODO: fix this so that fleet names aren't removed if, e.g., group = "fleet"
  table_data_info <- process_table(
    dat = prepared_data2,
   # group = group,
    method = method
    )
  table_data <- table_data_info[[1]]
  indexed_vars <- table_data_info[[2]]
  
  # put table_data into a nice table
  capitalized_names <- c("Year" = "year",
                         "Sex" = "sex",
                         "Fleet" = "fleet",
                         "Model" = "model")
  
  landings_colname <- paste0("Landings (", unit_label, ")")
  
  final_df <- table_data |>
    # replace col names from unique(prepared_data2$label) with landings_colname
    dplyr::rename_with(
       ~ gsub(stringr::str_to_title(target_label), landings_colname, .)
    ) |>
    dplyr::rename_with(~ gsub("_", " - ", .))
  
  final <- final_df |>
    gt::gt() |>
    add_theme()
 # final
  # Progress:
    # for bsb, hake, vsnap, and stockplotr::example_data, cols are:
    #    "Year", "Landings (<unit>)", "uncertainty"
    # for am, cols are:
    #    "Landings (mt) - cbn",	"cv - cbn",	"Landings (mt) - cbs",	"cv - cbs", etc
  
  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    create_rda(
      object = final,
      topic_label = "landings",
      fig_or_table = "table",
      dat = dat,
      dir = tables_dir,
      scale_amount = scale_amount,
      unit_label = unit_label,
      table_df = final_df
    )
  }
  # Return finished table
  final
}
