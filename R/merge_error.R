merge_error <- funtion(table_data) {
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
      } else if (any(grepl("expected|predicted|observed|estimated",landings_cols_new))) {
        landings_lab <- stringr::str_to_title(unique(stringr::str_extract(
          landings_cols_new,
          "landings expected|landings predicted|landings observed|landings estimated")
        ))
        id_uncert_col <- paste0(
          id_uncert, " ", landings_lab)
        if (id_uncert == "uncertainty" || length(id_uncert) == 0) {
          landings_cols_new <- c(paste0(landings_lab, " (", unit_label, ")"), id_uncert)
        } else {
          landings_cols_new <- c(
            paste0(landings_lab, " (", unit_label, ") (", id_uncert, ")"),
            id_uncert_col)
        }
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
      landings_cols <- names(tab_dat)[-c(1, grep(glue::glue("^{uncert_lab} "), names(tab_dat)))]
      for (l_col in landings_cols) {
        
        # Identify the error column that contains l_col in the name
        
        
        #############here#####################
        
        
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
}