##############################
# Utility functions for tables
##############################

#' Create the rda package for a plot or table
#'
#' @param data A dataframe-based table
#' @param caption A string comprising the table caption
#' @param label A string comprising the table label
#'
#' @returns A table based in LaTeX.
#' @export
#'
#' @examples
#' create_latex_table(
#'  data = as.data.frame(head(mtcars, 6)),
#'  caption = "My caption",
#'  label = "My label"
#' )
create_latex_table <- function(data,
                         caption,
                         label) {
  
  # Essential latex packages:
  # \usepackage{hyperref}
  # \usepackage{bookmark}
  # \usepackage{booktabs} 
  # \usepackage{tagpdf}
  # \usepackage{caption}
  
  latex_tbl <- knitr::kable(data, 
                            format = "latex", 
                            booktabs = TRUE,
                            linesep = "")
  
  # add number of header rows
  nheaders <- 1
  
  init_line <- paste0("\\tagpdfsetup{table/header-rows={", nheaders, "}}")

  # add caption, label
  cap <- paste0("\\captionof{table}{", caption, "}\n")
  lab <- paste0("\\label{", label, "}\n")
  
  # put together table
  table <- paste0(init_line,
                  "\n",
                  "\\begin{center}\n",
                  cap,
                  lab,
                  latex_tbl,
                  "\n",
                  "\\end{center}")
  
  table
  
  # ncols <- ncol(data)
  # column_names <- paste(colnames(data), collapse = " & ")
  # latex_format_data <- paste(
  #   column_names, "\\\\", "\n"
  # )
  # 
  # # c signifies all cols will be centered
  # alignment <- strrep("c", ncols)
  # 
  # for (i in 1:nrow(data)) {
  #   row_data <- stringr::str_replace_all(
  #     paste(data[i,], collapse = " & "),
  #     "NA",
  #     "-")
  #   
  #   # ends up adding a space and two forward slashes to the end of each line
  #   row_data_with_linebreak <- paste0(row_data, " \\\\")
  #   
  #   latex_format_data <- paste(
  #     latex_format_data,
  #     ifelse(i == 1, "\\midrule\n", ""),
  #     row_data_with_linebreak, "\n",
  #  #   row_data, "\n",
  #     sep = "",
  #     collapse = "\n"
  #   )
  # }
  # 
  # table <- paste0(
  #  # "\\begin{document}\n",
  #   "\\begin{tabular}{", alignment, "}\n",
  #   cap,
  #   lab,
  #   "\\toprule\n",
  #   # multicolumn needs to = number of columns in the data set
  #   "\\multicolumn{", ncols, "}{c}{Example} \\\\ \n",# example is the first header - not sure we want bc it's a merged cell
  #   # headers and values separated by '&' ending with 2 trailing forward slashes
  #   latex_format_data,
  #   "\\bottomrule\n",
  #   "\\end{tabular}\n",
  #   collapse = "\n"
  # )
}

#-------------------------------------------------------------------------------

#' Create loop to test for differences in column values
#' @param dat input data into process_table
#' @param index_variables the index_variables vector created within process_table
#' @param id_group the identifying index variable as a string

check_label_differences <- function(dat, index_variables, id_group = NULL) {
  # Loop over model to perform checks if the model columns are identical
  for (mod in unique(dat$model)){
    mod_index_variables <- unique(index_variables[names(index_variables) == mod])
    mod_data <- dplyr::filter(dat, model == mod)
    mod_id_group <- unique(id_group[names(id_group) == mod])
    
    if (length(unique(mod_data$label)) == 1) {
      # only one label - nothing to edit for this model
      next
    } else if (length(unique(mod_data$label)) == 2) {
      label_differences <- mod_data |>
        tidyr::pivot_wider(
          id_cols = dplyr::all_of(mod_index_variables),
          names_from = label,
          values_from = estimate
        ) |>
        dplyr::mutate(
          diff = .data[[unique(mod_data$label)[1]]] - .data[[unique(mod_data$label)[2]]]
        )
      
      if (all(label_differences$diff == 0)){
        # Modify dat to only include one label from model
        cli::cli_alert_info("Labels in {mod} model have identical values. Using only: {unique(mod_data$label)[2]}")
        dat <- dat |>
          dplyr::filter(label != unique(mod_data$label)[1])
      }
    } else {
      label_differences <- mod_data |>
        tidyr::pivot_wider(
          id_cols = dplyr::all_of(unique(mod_index_variables)),
          names_from = label,
          values_from = estimate
        )
      
      # Identify if any of the aligned columns contain ID group -- if so warn user and remove id_group labels from table
      empty_check <- label_differences |>
        dplyr::filter(dplyr::if_all(dplyr::any_of(mod_id_group), ~ !is.na(.))) |>
        dplyr::summarise(dplyr::across(unique(mod_data$label), ~ all(is.na(.))))
      col_to_remove <- names(empty_check)[which(as.logical(empty_check))]
      mod_data2 <- dplyr::filter(mod_data, label %notin% col_to_remove)
      # Identify if any of the columns are identical then remove one of the identical columns
      if (length(unique(mod_data2$label)) == 2){
        # compare estimate across all indexing vars and see if they are different over years
        label_differences <- mod_data2 |>
          tidyr::pivot_wider(
            id_cols = dplyr::all_of(c(mod_index_variables)),
            names_from = label,
            values_from = estimate
          ) |>
          dplyr::mutate(
            diff = .data[[unique(mod_data2$label)[1]]] - .data[[unique(mod_data2$label)[2]]]
          )
        
        if (all(label_differences$diff == 0)){
          cli::cli_alert_info("Labels in {mod} model have identical values. Using only one label: {unique(prepared_data$label)[2]}")
          col_to_remove <- c(col_to_remove, unique(mod_data2$label)[1])
        }
        dat <- dplyr::filter(dat, label %notin% col_to_remove)
      } else {
        cli::cli_alert_danger("Multiple labels with differing values detected. Function may not work as intended. Please leave an issue on GitHub.")
      }
    }
  }
  dat
}

#-------------------------------------------------------------------------------

#' Rename columns and merge estimate and uncertainty columns for table presentation
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
        if (id_uncert != uncert_lab) uncert_lab <- id_uncert
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
      rename_map <- stats::setNames(label_cols_init, final_names)
      
      # rename cols for final df
      rename_map_final <- stats::setNames(
        final_names, 
        ifelse(
          is.na(cols_fleets),
          label_cols_final,
          paste0(label_cols_final, " - ", cols_fleets)
        ))
      
      # Apply the renaming
      tab_dat <- tab_dat |>
        dplyr::rename(dplyr::any_of(rename_map))
      
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

