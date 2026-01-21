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
  ) |>
    dplyr::mutate(estimate = round(as.numeric(estimate), digits = 0)) |>
    dplyr::mutate(uncertainty = round(as.numeric(uncertainty), digits = 2))
  
  # Add check if there is any data
  if (nrow(prepared_data) == 0){
    cli::cli_abort("No landings data found.")
  }
  
  # get uncertainty label by model
  uncert_lab <- prepared_data |>
    dplyr::filter(!is.na(uncertainty_label)) |>
      dplyr::group_by(model) |>
      dplyr::reframe(unique_uncert = unique(uncertainty_label)) # changed to reframe -- may cause errors
  uncert_lab <- setNames(uncert_lab$unique_uncert, uncert_lab$model)
  # if (length(unique(uncert_lab)) == 1) uncert_lab <- unique(uncert_lab) # might need this line
  
  # This needs to be adjusted when comparing different models and diff error
  if (length(uncert_lab) > 1 & length(unique(uncert_lab)) == 1 | length(names(uncert_lab)) == 1){ # prepared_data$model
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

  #TODO: fix this so that fleet names aren't removed if, e.g., group = "fleet"
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
  
  # TODO: TEST !!!
  if (!is.data.frame(table_data)) {
    # lapply made with the help of Gemini (all recoding names code is original)
    df_list <- lapply(table_data, function(tab_dat) {
      
      landings_cols_init <- colnames(tab_dat)[
        grepl("landings", tolower(colnames(tab_dat)))
      ]
      
      # CONDITION: Only proceed if landings columns actually exist in this data frame
      if (length(landings_cols_init) > 0) {
        # Clean up fleet names and keywords
        landings_cols_new <- stringr::str_remove_all(
          landings_cols_init, 
          paste0("_", fleets, collapse = "|")
        ) |> stringr::str_replace_all("_", " ")
        # Drop "weight" or "number" if present
        landings_cols_new <- unique(
          stringr::str_remove_all(tolower(landings_cols_new), " number| weight")
        )
        # Check if we should simplify to a single "Landings" label
        if (length(unique(landings_cols_new)) == 2) {
          matches <- sapply(uncert_lab, function(l) {
            any(stringr::str_detect(landings_cols_new, stringr::str_c("\\b", l, "\\b")))
          })
          id_uncert <- uncert_lab[matches]
          if (length(id_uncert) == 0) id_uncert <- "uncertainty"
          
          landings_cols_new <- c(
            ifelse(
              id_uncert == "uncertainty",
              paste0("Landings (", unit_label, ")"),
              paste0("Landings (", unit_label, ") (", id_uncert, ")")
              ), 
            id_uncert) 
          # Remove (", id_uncert, ")" in the above line if we don't want to combine value and error in one column
        }
  
        # Add units
        # landings_cols_new <- paste0(landings_cols_new, " (", unit_label, ")")
        
        # Re-attach fleet names to the new labels
        cols_fleets <- stringr::str_extract(
          landings_cols_init, 
          paste0("_",fleets, "$", collapse = "|")
        ) |> stringr::str_remove_all("_")
        
        # Final target labels
        final_names <- ifelse(
          is.na(cols_fleets),
          landings_cols_new,
          paste0(landings_cols_new, " - ", cols_fleets)
        )
        
        # Create a named vector for renaming: c(new_name = old_name)
        # This handles the "Rename this specific old name to this specific new name"
        rename_map <- setNames(landings_cols_init, final_names)
        
        # Apply the renaming
        tab_dat <- tab_dat |>
          dplyr::rename(any_of(rename_map))
        
        # Comment out from here to closing brackets if don't want to combine label and uncertainty
        # {{ -------------------------------------------------------------------
        # Use loop to combine label (uncertainty)
        landings_cols <- grep(paste0("Landings \\(", unit_label, "\\)"), names(tab_dat), value = TRUE)
        
        for (l_col in landings_cols) {
          # 1. Extract fleet from current landing column name
          f_id <- stringr::str_extract(l_col, paste0(unique(cols_fleets), collapse = "|"))
          
          # 2. Construct the matching uncertainty column name
          u_col <- paste0(id_uncert, " - ", f_id)
          
          # 3. Only perform the merge if the uncertainty column actually exists
          if (u_col %in% names(tab_dat)) {
            tab_dat[[l_col]] <- paste0(
              tab_dat[[l_col]], 
              " (", tab_dat[[u_col]], ")"
            )
            
            # Optional: Clean up " (NA)" if they appear
            tab_dat[[l_col]] <- stringr::str_remove(tab_dat[[l_col]], " \\(NA\\)")
          }
        }
        # Remove error column(s)
        tab_dat <- tab_dat |>
          dplyr::select(-dplyr::matches(paste0(uncert_lab, " - ", fleets, collapse = "|")))
        # }} -------------------------------------------------------------------
      }
      
      # Apply the general underscore formatting to ALL columns (regardless of landings)
      tab_dat <- tab_dat |>
        dplyr::rename_with(~ gsub("_", " - ", .))
      return(tab_dat)
    })
    # transform dfs into tables
    final <- lapply(df_list, function(df) {
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
    
    # CONDITION: Only proceed if landings columns actually exist in this data frame
    if (length(landings_cols_init) > 0) {
      # Clean up fleet names and keywords
      landings_cols_new <- stringr::str_remove_all(
        landings_cols_init, 
        paste0("_", fleets, collapse = "|")
      ) |> stringr::str_replace_all("_", " ")
      # Drop "weight" or "number" if present
      landings_cols_new <- unique(
        stringr::str_remove_all(tolower(landings_cols_new), " number| weight")
      )
      # Check if we should simplify to a single "Landings" label
      if (length(unique(landings_cols_new)) == 2) {
        matches <- sapply(uncert_lab, function(l) {
          any(stringr::str_detect(landings_cols_new, stringr::str_c("\\b", l, "\\b")))
        })
        id_uncert <- uncert_lab[matches]
        if (length(id_uncert) == 0) id_uncert <- "uncertainty"
      } else {
        id_uncert <- uncert_lab
      }
      
      if (any(grepl("expected|predicted|observed|estimated",landings_cols_new))) {
        landings_lab <- stringr::str_to_title(unique(stringr::str_extract(
          landings_cols_new,
          "landings expected|landings predicted|landings observed|landings estimated")
        ))
        id_uncert_col <- paste0(
          id_uncert, " ", landings_lab)
      } else {
        landings_lab <- "Landings"
      }
      
      if (id_uncert == "uncertainty" || length(id_uncert) == 0) {
        landings_cols_new <- c(paste0(landings_lab, " (", unit_label, ")"), id_uncert)
      } else {
        landings_cols_new <- c(
          paste0(landings_lab, " (", unit_label, ") (", id_uncert, ")"),
          id_uncert_col)
      }
      # Remove (", id_uncert, ")" in the above line if we don't want to combine value and error in one column
      
      # Re-attach fleet names to the new labels
      cols_fleets <- stringr::str_extract(
        landings_cols_init, 
        paste0("_",fleets, "$", collapse = "|")
      ) |> stringr::str_remove_all("_")
      
      # Final target labels
      final_names <- ifelse(
        is.na(cols_fleets),
        landings_cols_new,
        paste0(landings_cols_new, " - ", cols_fleets)
      )
      
      # Create a named vector for renaming: c(new_name = old_name)
      # This handles the "Rename this specific old name to this specific new name"
      rename_map <- setNames(landings_cols_init, final_names)
      
      # Apply the renaming
      table_data <- table_data |>
        dplyr::rename(any_of(rename_map))
      
      # Comment out from here to closing brackets if don't want to combine label and uncertainty
      # {{ --------------------------------------------------------------------
      if (!all(is.na(table_data[[id_uncert]]))) { # only works for 1 column of uncertainty data
        # Use loop to combine label (uncertainty)
        landings_cols <- grep(paste0("Landings.*\\(", unit_label, "\\)"), names(table_data), value = TRUE)
        
        for (col_name in landings_cols) {
          # 2. Extract metadata from the current column name
          # Example: "land exp (mt) (cv) - mrip" -> Type: exp, Fleet: mrip
          type_val  <- stringr::str_extract(col_name, "Expected|Predicted|Observed|Estimated")
          fleet_val <- stringr::str_extract(col_name, glue::glue("{unique(cols_fleets)}$")) # Use your specific fleet list
          
          # 3. Construct the name of the "CV" column that matches
          # This looks for the column starting with "cv", containing the type and the fleet
          cv_col_name <- names(table_data)[
            stringr::str_detect(names(table_data), glue::glue("^{id_uncert}")) & 
              stringr::str_detect(names(table_data), type_val) & 
              stringr::str_detect(names(table_data), na.omit(fleet_val))
          ]
          
          # 4. Update the Landings column with the combined format
          # If a matching CV column was found, merge them
          if (length(cv_col_name) == 1) {
            table_data[[col_name]] <- paste0(
              table_data[[col_name]], " (", table_data[[cv_col_name]], ")"
            )
          }
        }
        
        if (length(landings_cols) > 1) {
          for (l_col in landings_cols) {
            # 1. Extract fleet from current landing column name
            f_id <- stringr::str_extract(l_col, paste0(unique(cols_fleets), collapse = "|"))
            
            # 2. Construct the matching uncertainty column name
            u_col <- paste0(id_uncert, " - ", f_id)
            
            # 3. Only perform the merge if the uncertainty column actually exists
            if (u_col %in% names(table_data)) {
              table_data[[l_col]] <- paste0(
                table_data[[l_col]], 
                " (", table_data[[u_col]], ")"
              )
              
              # Optional: Clean up " (NA)" if they appear
              table_data[[l_col]] <- stringr::str_remove(table_data[[l_col]], " \\(NA\\)")
            }
          }
          # Remove error column(s)
          table_data <- table_data |>
            dplyr::select(-dplyr::matches(paste0(uncert_lab, " - ", fleets, collapse = "|")))
        }
      }
      # }} ---------------------------------------------------------------------
    }
    
    # Apply the general underscore formatting to ALL columns (regardless of landings)
    table_data <- table_data |>
      dplyr::rename_with(~ gsub("_", " - ", .)) |>
      # Remove columns containing all NA
      dplyr::select(where(~!all(is.na(.))))
    
    # Turn df into table
    final <- table_data |>
      gt::gt() |>
      add_theme()
  }
  
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
  # Send table(s) to viewer
  if (!is.data.frame(table_data)) {
    for(t in final) {
      print(t)
    }
    # Return table list invisibly
    return(invisible(final))
  } else {
    # Return finished table (when only one table)
    return(final)
  }
}
