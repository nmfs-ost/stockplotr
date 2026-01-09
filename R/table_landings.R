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

  prepared_data <- prepared_data |>
    dplyr::mutate(estimate = round(as.numeric(estimate), digits = 0)) |>
    dplyr::mutate(uncertainty = round(as.numeric(uncertainty), digits = 2))
  
  # get uncertainty label by model
  uncert_lab <- prepared_data |>
    dplyr::filter(!is.na(uncertainty_label)) |>
      dplyr::group_by(model) |>
      dplyr::summarise(unique_uncert = unique(uncertainty_label))
  uncert_lab <- setNames(uncert_lab$unique_uncert, uncert_lab$model)
  # if (length(unique(uncert_lab)) == 1) uncert_lab <- unique(uncert_lab) # might need this line
  
  # This needs to be adjusted when comparing different models and diff error
  if (length(uncert_lab) > 1 & length(unique(prepared_data$model)) == 1){
    cli::cli_alert_warning("More than one value for uncertainty exists: {uncert_lab}")
    uncert_lab <- uncert_lab[[1]]
    cli::cli_alert_warning("The first value ({uncert_lab}) will be chosen.")
  }
  
  # get fleet names
  # TODO: change from fleets to id_group AFTER the process data step and adjust throughout the table based on indexing
  fleets <- unique(prepared_data$fleet) |>
    # sort numerically even if fleets are 100% characters
    stringr::str_sort(numeric = TRUE)

  #TODO: fix this so that fleet names aren't removed if, e.g., group = "fleet"
  table_data_info <- process_table(
    dat = prepared_data,
    # group = group,
    method = method,
    label = label
  )
  table_data <- table_data_info[[1]]
  indexed_vars <- table_data_info[[2]]
  
  # TODO: add check if there is a landings column for every error column -- if not remove the error (can keep landings)
  
  # TODO: TEST !!!
  if (!is.data.frame(table_data)) {
    # lapply made with the help of Gemini (all recoding names code is original)
    df_list <- lapply(df_list, function(table_data) {
      
      landings_cols_init <- colnames(table_data)[
        grepl("landings", tolower(colnames(table_data)))
      ]
      
      # CONDITION: Only proceed if landings columns actually exist in this data frame
      if (length(landings_cols_init) > 0) {
        # Clean up fleet names and keywords
        landings_cols_new <- stringr::str_remove_all(
          landings_cols_init, 
          paste0("_", fleets, collapse = "|")
        )
        # Drop "weight" or "number" if present
        landings_cols_new <- unique(
          stringr::str_remove_all(landings_cols_new, " Number| Weight")
        )
        # Check if we should simplify to a single "Landings" label
        if (length(unique(landings_cols_new)) == 1) {
          landings_cols_new <- "Landings"
        }
  
        # Add units
        landings_cols_new <- paste0(landings_cols_new, " (", unit_label, ")")
        
        # Re-attach fleet names to the new labels
        cols_fleets <- stringr::str_extract(
          landings_cols_init, 
          paste0(fleets, collapse = "|")
        )
        
        # Final target labels
        final_names <- paste0(landings_cols_new, " - ", cols_fleets)
        
        # Create a named vector for renaming: c(new_name = old_name)
        # This handles the "Rename this specific old name to this specific new name"
        rename_map <- setNames(landings_cols_init, final_names)
        
        # Apply the renaming
        table_data <- table_data |>
          dplyr::rename(any_of(rename_map))
      }
      
      # Apply the general underscore formatting to ALL columns (regardless of landings)
      table_data <- table_data |>
        dplyr::rename_with(~ gsub("_", " - ", .))
      return(table_data)
    })
    # transform dfs into tables
    final <- lapply(table_data, function(df) {
      df |>
        gt::gt() |>
        add_theme()
    })
  } else {
    # Determine target label(s) for landings based on available labels in data
    # If 1 label -> "Landings"
    # if > 1 label -> drop "weight" or "number" if present
    landings_cols_init <- colnames(table_data)[
      grepl("landings", tolower(colnames(table_data)))
    ]
    landings_cols_new <- stringr::str_remove_all(
      landings_cols_init,
      paste0("_", fleets,collapse = "|"))
    # drop "weight" or "number" if present
    # Potential for users to want both?
    landings_cols_new <- unique(
      stringr::str_remove_all(landings_cols_new, " Number| Weight"))
    
    # test if all labels are the same in landings_cols
    if (length(unique(landings_cols_new)) == 1) {
      landings_cols_new <- "Landings"
    }
    # Add unit label to landings colnames
    landings_cols_new <- paste0(
      landings_cols_new,
      " (", unit_label, ")")
    # Extract fleets from landings_cols_init
    cols_fleets <- unlist(stringr::str_extract_all(
      landings_cols_init,
      paste0(fleets, collapse = "|")
    ))
    landings_cols_new <- paste0(landings_cols_new, " - ", cols_fleets)
    
    final_df <- table_data |>
      # replace col names from unique(prepared_data2$label) with landings_colname
      dplyr::rename_with(
        ~ gsub(stringr::str_to_title(target_label), landings_colname, .)
      ) |>
      dplyr::rename_with(~ gsub("_", " - ", .))
    # Turn df into table
    final <- final_df |>
      gt::gt() |>
      add_theme()
  }
  # 
  # if (!is.data.frame(table_data)) {
  #   table_data <- lapply(table_data, function(df) {
  #     df |>
  #       dplyr::rename_with(
  #         ~ gsub(stringr::str_to_title(target_label), landings_colname, .)
  #       ) |>
  #       dplyr::rename_with(~ gsub("_", " - ", .))
  #   })
  #   final <- lapply(table_data, function(df) {
  #     df |>
  #       gt::gt() |>
  #       add_theme()
  #   })
  # } else {
  #   # Determine target label(s) for landings based on available labels in data
  #   # If 1 label -> "Landings"
  #   # if > 1 label -> drop "weight" or "number" if present
  #   landings_cols_init <- colnames(table_data)[
  #     grepl("landings", tolower(colnames(table_data)))
  #   ]
  #   landings_cols_new <- stringr::str_remove_all(
  #     landings_cols_init,
  #     paste0("_", fleets,collapse = "|"))
  #   # drop "weight" or "number" if present
  #   # Potential for users to want both?
  #   landings_cols_new <- unique(
  #     stringr::str_remove_all(landings_cols_new, " Number| Weight"))
  #   
  #   # test if all labels are the same in landings_cols
  #   if (length(unique(landings_cols_new)) == 1) {
  #     landings_cols_new <- "Landings"
  #   }
  #   # Add unit label to landings colnames
  #   landings_cols_new <- paste0(
  #     landings_cols_new,
  #     " (", unit_label, ")")
  #   # Extract fleets from landings_cols_init
  #   cols_fleets <- unlist(stringr::str_extract_all(
  #     landings_cols_init,
  #     paste0(fleets, collapse = "|")
  #   ))
  #   landings_cols_new <- paste0(landings_cols_new, " - ", cols_fleets)
  #   
  #   final_df <- table_data |>
  #     # replace col names from unique(prepared_data2$label) with landings_colname
  #     dplyr::rename_with(
  #       ~ gsub(stringr::str_to_title(target_label), landings_colname, .)
  #     ) |>
  #     dplyr::rename_with(~ gsub("_", " - ", .))
  #   
  #   final <- final_df |>
  #     gt::gt() |>
  #     add_theme()
  # }
  
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
