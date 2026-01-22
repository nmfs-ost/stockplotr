#' Rename column
#'
#' @param table_data list of dataframes that will be eventually turned into tables
#' @param uncert_lab uncertainty label. Typically inherited from another function
#' but is the exact string of the uncertainty in the data (e.g., "sd", "se", "cv",
#' "uncertainty").)
#' @param fleets Vector of fleet names.
#' @param label Label name of target quantity that is being presented by the table.
#' @param unit_label String. The units of the estimate being presented in the table.
#'
#' @return List of formatted dataframes that contain column names formatted
#' for a table along with a merge of values in the estimate and error columns
#' to reduce redundancy in the table.
#' @export
#'
merge_error <- function(table_data, uncert_lab, fleets, label, unit_label) {
  # TODO: change fleets to grouping when the data is indexed by factors other than fleet 
  lapply(table_data, function(tab_dat) {

    label_cols_init <- colnames(tab_dat)[
      grepl(label, tolower(colnames(tab_dat))) # "landings"
    ]

    # CONDITION: Only proceed if label columns actually exist in this data frame
    if (length(label_cols_init) > 0) {
      # Clean up fleet names and keywords
      label_cols_new <- stringr::str_remove_all(
        label_cols_init,
        paste0("_", fleets, collapse = "|")
      ) |> stringr::str_replace_all("_", " ")
      # Drop "weight" or "number" if present
      label_cols_new <- unique(
        stringr::str_remove_all(tolower(label_cols_new), " number| weight")
      )
      # Check if we should simplify to a single "Landings" label
      if (length(unique(label_cols_new)) == 2) {
        matches <- sapply(uncert_lab, function(l) {
          any(stringr::str_detect(label_cols_new, stringr::str_c("\\b", l, "\\b")))
        })
        id_uncert <- uncert_lab[matches]
        if (length(id_uncert) == 0) id_uncert <- "uncertainty"

        label_cols_final <- c(
          ifelse(
            id_uncert == "uncertainty",
            paste0(stringr::str_to_title(label), " (", unit_label, ")"),
            paste0(stringr::str_to_title(label), " (", unit_label, ") (", id_uncert, ")")
          ),
          id_uncert)
        # Remove (", id_uncert, ")" in the above line if we don't want to combine value and error in one column
      } else if (any(grepl("expected|predicted|observed|estimated",label_cols_new))) {
        label_lab <- stringr::str_to_title(unique(stringr::str_extract(
          label_cols_new,
          paste(label, c("expected", "predicted", "observed", "estimated"), collapse = "|"))
        ))
        id_uncert_col <- paste0(
          uncert_lab, " ", label_lab)
        if (uncert_lab == "uncertainty" || length(uncert_lab) == 0) {
          label_cols_final <- c(paste0(label_lab, " (", unit_label, ")"), uncert_lab)
        } else {
          label_cols_final <- c(
            paste0(label_lab, " (", unit_label, ") (", uncert_lab, ")"),
            id_uncert_col)
        }
      }

      # Re-attach fleet names to the new labels
      cols_fleets <- stringr::str_extract(
        label_cols_init,
        paste0("_",fleets, "$", collapse = "|")
      ) |> stringr::str_remove_all("_")

      # Target labels for next step
      final_names <- ifelse(
        is.na(cols_fleets),
        label_cols_new,
        paste0(label_cols_new, " - ", cols_fleets)
      )

      # Assign previous names with new identifying ones
      rename_map <- setNames(label_cols_init, final_names)

      # rename cols for final df
      rename_map_final <- setNames(
        final_names, 
        ifelse(
          is.na(cols_fleets),
          label_cols_final,
          paste0(label_cols_final, " - ", cols_fleets)
        ))

      # Apply the renaming
      tab_dat <- tab_dat |>
        dplyr::rename(any_of(rename_map))

      # Identify lestimate and uncertainty columns for loop and other reference
      label_cols <- names(tab_dat)[-c(1, grep(glue::glue("^{uncert_lab} "), names(tab_dat)))]
      uncert_cols <- names(tab_dat)[grep(glue::glue("^{uncert_lab} "), names(tab_dat))]
      # Comment out from here to closing brackets if don't want to combine label and uncertainty
      # {{ -------------------------------------------------------------------
      # Use loop to combine label (uncertainty)
      for (l_col in label_cols) {

        # Identify the error column that contains l_col in the name
        uncert_col <- uncert_cols[grepl(l_col, uncert_cols)]

        # adjust tab dat to combine the uncert_col value into the l_col = l_col (uncert_col)
        tab_dat <- tab_dat |>
          dplyr::mutate(
            !!l_col := ifelse(
              !is.na(.data[[uncert_col]]),
              paste0(.data[[l_col]], " (", .data[[uncert_col]], ")"),
              # maybe not good practice to insert dash?
              ifelse(
                is.na(.data[[l_col]]),
                "-",
                as.character(.data[[l_col]])
                )
            )
          ) |>
          # Remove uncertainty colummn id'd in this step of the loop
          dplyr::select(-dplyr::all_of(uncert_col)) 
      } # close loop combining label and uncertainty
      # }} -------------------------------------------------------------------
      
      # Rename final df with cleaned names
      tab_dat <- tab_dat |>
        dplyr::rename(any_of(rename_map_final)) |>
        dplyr::rename_with(~ gsub("_", " - ", .)) # |>
        # not sure if we want to keep this or not
        # dplyr::select(where(~!all(is.na(.)) | !all(. == "-"))) # remove columns that are all NA or all "-"))
    } else {
      cli::cli_alert_info(
        "No {label} columns found in data; skipping renaming step."
      )
    } # close if statement on label column
    return(tab_dat)
  }) # close and end lapply
}
