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
#'   data = as.data.frame(head(mtcars, 6)),
#'   caption = "My caption",
#'   label = "My label"
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
    linesep = ""
  )

  # add number of header rows
  nheaders <- 1

  init_line <- paste0("\\tagpdfsetup{table/header-rows={", nheaders, "}}")

  # add caption, label
  cap <- paste0("\\captionof{table}{", caption, "}\n")
  lab <- paste0("\\label{", label, "}\n")

  # put together table
  table <- paste0(
    init_line,
    "\n",
    "\\begin{center}\n",
    cap,
    lab,
    latex_tbl,
    "\n",
    "\\end{center}"
  )

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
#'
#' Default: NULL

check_label_differences <- function(dat, index_variables, id_group = NULL) {
  # Loop over model to perform checks if the model columns are identical
  for (mod in unique(dat$model)) {
    mod_index_variables <- unique(index_variables[names(index_variables) == mod])
    mod_data <- dplyr::filter(dat, model == mod)
    mod_id_group <- unique(id_group[names(id_group) == mod])

    if (length(unique(mod_data$label)) == 1) {
      # only one label - nothing to edit for this model
      next
    } else if (length(unique(mod_data$label)) == 2) {
      label_differences <- mod_data |>
        dplyr::mutate(estimate = as.numeric(estimate)) |>
        tidyr::pivot_wider(
          id_cols = dplyr::all_of(mod_index_variables),
          names_from = label,
          values_from = estimate
        ) |>
        dplyr::mutate(
          diff = .data[[unique(mod_data$label)[1]]] - .data[[unique(mod_data$label)[2]]]
        )

      if (all(label_differences$diff == 0)) {
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
      if (length(unique(mod_data2$label)) == 2) {
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

        if (all(label_differences$diff == 0)) {
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
#' @returns List of formatted dataframes that contain column names formatted
#' for a table along with a merge of values in the estimate and error columns
#' to reduce redundancy in the table.
#'
# merge_error <- function(table_data, uncert_lab, fleets, label, unit_label) {
#   # TODO: change fleets to grouping when the data is indexed by factors other than fleet
#   lapply(table_data, function(tab_dat) {
#     label_cols_init <- colnames(tab_dat)[
#       grepl(label, tolower(colnames(tab_dat))) # "landings"
#     ]
# 
#     # CONDITION: Only proceed if label columns actually exist in this data frame
#     if (length(label_cols_init) > 0) {
#       # Clean up fleet names and keywords
#       label_cols_new <- stringr::str_remove_all(
#         label_cols_init,
#         paste0("_", fleets[order(nchar(fleets), decreasing = TRUE)], collapse = "|")
#       ) |> stringr::str_replace_all("_", " ")
#       # Drop "weight" or "number" if present
#       if (any(grepl("numbers|weight", tolower(label_cols_new)))) {
#         # add check if both numbers and weight are present
#         # TRUE -- select weight
#         if (any(grepl("numbers", tolower(label_cols_new))) & any(grepl("weight", tolower(label_cols_new)))) {
#           cli::cli_alert_info("Both numbers and weight exist. Selecting only weight.")
#           label_cols_new <- label_cols_new[grepl("weight", tolower(label_cols_new))]
#         }
#         label_cols_new <- unique(
#           stringr::str_remove_all(tolower(label_cols_new), " numbers| weight")
#         )
#       }
#       
#       # Check if there are other groupings left in label_cols_new
#       if (any(
#         !stringr::str_detect(
#           label_cols_new,
#           paste0(label,
#                  "(",
#                  paste(c(" predicted", " weight", " observed", " numbers"), collapse = "|"),
#                  ")$")
#           )
#       )) {
#         other_grouping <- stringr::str_trim(stringr::str_match(
#           label_cols_new,
#           paste0("(?<=", label, "(", paste(c(" predicted", " weight", " observed", " numbers"), collapse = "|"), "))(.*)")
#         )[, 3]) |> unique()
#         label_cols_new <- stringr::str_remove_all(
#           label_cols_new,
#           paste0(" ", other_grouping, collapse = "|")
#         ) |> unique()
#       } else {
#         other_grouping <- NULL
#       }
#       
#       # Check if we should simplify to a single "Landings" label
#       if (length(unique(label_cols_new)) == 2) {
#         matches <- sapply(uncert_lab, function(l) {
#           any(stringr::str_detect(label_cols_new, stringr::str_c("\\b", l, "\\b")))
#         })
#         id_uncert <- uncert_lab[matches]
#         if (length(id_uncert) == 0) id_uncert <- "uncertainty"
# 
#         label_cols_final <- c(
#           ifelse(
#             id_uncert == "uncertainty",
#             paste0(stringr::str_to_title(label), " ", unit_label),
#             paste0(stringr::str_to_title(label), " ", unit_label, " (", id_uncert, ")")
#           ),
#           id_uncert
#         )
#         if (id_uncert != uncert_lab) uncert_lab <- id_uncert
#         # Remove (", id_uncert, ")" in the above line if we don't want to combine value and error in one column
#       } else if (any(grepl("expected|predicted|observed|estimated", label_cols_new))) {
#         # simplify to unique terms with upper case
#         # not including error
#         label_lab <- stringr::str_to_title(unique(stringr::str_extract(
#           label_cols_new,
#           paste(label, c("expected", "predicted", "observed", "estimated"), collapse = "|")
#         )))
#         # ID the uncertainty columns and also make upper case
#         id_uncert_col <- paste0(
#           uncert_lab, " ", label_lab
#         )
#         # create final column names (unique) -- contains no grouping identifiers
#         if (uncert_lab == "uncertainty" || length(uncert_lab) == 0) {
#           label_cols_final <- c(paste0(label_lab, " ", unit_label), uncert_lab)
#         } else {
#           label_cols_final <- c(
#             # TODO: works when unit_label = "", but not when it's set
#             paste0(label_lab, unit_label, " (", uncert_lab, ")"),
#             id_uncert_col
#           )
#         }
#       }
#       
#       # Re-attach fleet names to the new labels
#       # extract fleet names -- order matters
#       # want to assign labels to fleet names in the fleet name order
#       # cols_fleets <- stringr::str_extract(
#       #   label_cols_init,
#       #   paste0("_", fleets, "$", collapse = "|")
#       # ) |> stringr::str_remove_all("_")
#       cols_fleets <- stringr::str_extract(
#         label_cols_init,
#         paste0("_", fleets, "$", collapse = "|")
#       ) |> stringr::str_remove_all("_")
#       # check if no fleets
#       if (all(is.na(cols_fleets))) {
#         cols_fleets <- stringr::str_extract(
#           label_cols_init,
#           paste0("_", fleets[order(nchar(fleets), decreasing = TRUE)], collapse = "|")
#         ) |> stringr::str_replace("_", "")
#           # stringr::str_replace_all("_", " ")
#       }
#       
#       # Combine groupings again
#       if (!is.null(other_grouping)) {
#         # all_col_group_idfers <- tidyr::expand_grid(label = other_grouping, fleet = unique(cols_fleets)) |>
#         #   dplyr::mutate(final_string = paste0(fleet, " ", other_grouping)) |>
#         #   dplyr::pull(final_string)
#         # match the ones that are in the initial column names
#         # col_group_idfers <- all_col_group_idfers[sapply(
#         #   stringr::str_replace_all(all_col_group_idfers, " ", "_"),
#         #   function(x) any(grepl(x, label_cols_init, fixed = TRUE))
#         # )]
#         # Repeat the matches the number of times it occurs to match later
#         # counts <- sapply(
#         #   stringr::str_replace_all(col_group_idfers, " ", "_"),
#         #   function(p) sum(grepl(p, label_cols_init, fixed = TRUE))
#         # )
#         # group_idfers <- rep(col_group_idfers, times = counts)
#         
#         all_col_group_idfers <- unlist(stringr::str_extract_all(
#           label_cols_init, paste0("(", paste0(fleets, collapse = "|"), ").*")
#           )) |> unique()
#         
#         # Target labels for next step
#         final_names <- ifelse(
#           is.na(cols_fleets),
#           label_cols_new,
#           tidyr::expand_grid(label = label_cols_new, fleet = all_col_group_idfers) |>
#               dplyr::mutate(final_string = paste0(label, " - ", fleet)) |>
#               dplyr::pull(final_string)
#         )
#       } else {
#         final_names <- ifelse(
#           is.na(cols_fleets),
#           label_cols_final,
#           tidyr::expand_grid(label = label_cols_final, fleet = unique(cols_fleets)) |> # col_group_idfers if other option
#             dplyr::mutate(final_string = stringr::str_c(label, " - ", fleet)) |> 
#             dplyr::pull(final_string)
#         )
#       }
#       
#       # Assign previous names with new identifying ones
#       rename_map <- stats::setNames(sort(label_cols_init), sort(final_names))
# 
#       # # rename cols for final df
#       # rename_map_final <- stats::setNames(
#       #   final_names,
#       #   ifelse(
#       #     is.na(cols_fleets),
#       #     label_cols_final,
#       #     paste0(label_cols_final, " - ", cols_fleets)
#       #   )
#       # )
# 
#       # Apply the renaming
#       tab_dat <- tab_dat |>
#         dplyr::rename(dplyr::any_of(rename_map))
# 
#       # Identify lestimate and uncertainty columns for loop and other reference
#       label_cols <- names(tab_dat)[-c(1, grep(glue::glue("^{gsub('_', ' ', uncert_lab)} "), names(tab_dat)))]
#       uncert_cols <- names(tab_dat)[grep(glue::glue("^{gsub('_', ' ', uncert_lab)} "), names(tab_dat))]
#       # Comment out from here to closing brackets if don't want to combine label and uncertainty
#       # {{ -------------------------------------------------------------------
#       # Use loop to combine label (uncertainty)
#       for (l_col in label_cols) {
#         # Identify the error column that contains l_col in the name
#         # Extract the fleet suffix (e.g., "cl") by grabbing whatever is after the dash
#         fleet_suffix <- stringr::str_extract(tolower(l_col), "(?<=- ).*$") #(?<=- )\\w+$
#         
#         # Determine if it's observed or predicted
#         is_observed  <- stringr::str_detect(tolower(l_col), "observed")
#         is_predicted <- stringr::str_detect(tolower(l_col), "predicted")
#         
#         # 3. Filter the choices vector based on those exact components
#         if (all(is.na(fleet_suffix))) {
#           uncert_col <- uncert_cols[
#             stringr::str_detect(tolower(uncert_cols), "observed") == is_observed &
#             stringr::str_detect(tolower(uncert_cols), "predicted") == is_predicted
#           ]
#         } else {
#           uncert_col <- uncert_cols[
#             stringr::str_detect(tolower(uncert_cols), paste0("- ", fleet_suffix, "$")) & # may not word when > 1 grouping
#               stringr::str_detect(tolower(uncert_cols), "observed") == is_observed &
#               stringr::str_detect(tolower(uncert_cols), "predicted") == is_predicted
#           ]
#         }
#         
#         # uncert_col <- uncert_cols[grepl(paste0(l_col, "$"), uncert_cols)]
# 
#         # adjust tab dat to combine the uncert_col value into the l_col = l_col (uncert_col)
#         tab_dat <- tab_dat |>
#           dplyr::mutate(
#             !!l_col := ifelse(
#               !is.na(.data[[uncert_col]]),
#               paste0(.data[[l_col]], " (", .data[[uncert_col]], ")"),
#               # maybe not good practice to insert dash?
#               ifelse(
#                 is.na(.data[[l_col]]),
#                 "-",
#                 as.character(.data[[l_col]])
#               )
#             )
#           ) |>
#           # Remove uncertainty colummn id'd in this step of the loop
#           dplyr::select(-dplyr::all_of(uncert_col))
#       } # close loop combining label and uncertainty
#       # }} -------------------------------------------------------------------
# 
#       # Rename final df with cleaned names
#       tab_dat2 <- tab_dat |>
#         # dplyr::rename(dplyr::any_of(rename_map_final)) #|>
#       dplyr::rename_with(~ gsub("_", " - ", .)) # |>
#       # not sure if we want to keep this or not
#       # dplyr::select(where(~!all(is.na(.)) | !all(. == "-"))) # remove columns that are all NA or all "-"))
#     } else {
#       cli::cli_alert_info(
#         "No {label} columns found in data; skipping renaming step."
#       )
#     } # close if statement on label column
#     return(tab_dat)
#   }) # close and end lapply
# }


merge_error <- function(
    table_data,
    id_col_vals,
    unit_label,
    uncert_lab
    ) {
  # TODO: change fleets to grouping when the data is indexed by factors other than fleet
  lapply(table_data, function(tab_dat) {
    label_cols <- names(tab_dat)[-c(1, grep(glue::glue("^{uncert_lab}"), names(tab_dat)))]
    uncert_cols <- names(tab_dat)[grep(glue::glue("^{uncert_lab}"), names(tab_dat))]
    # Comment out from here to closing brackets if don't want to combine label and uncertainty
    # {{ -------------------------------------------------------------------
    # Use loop to combine label (uncertainty)
    for (l_col in label_cols) {
      # Identify the error column that contains l_col in the name
      # Extract the fleet suffix (e.g., "cl") by grabbing whatever is after the dash
      index_suffix <- stringr::str_extract(
        tolower(l_col),
        paste(
          stringr::str_escape(unlist(id_col_vals, use.names = FALSE)),
          collapse = "|")
        )
      
      # Identify which uncert col aligns with l_col
      uncert_col <- uncert_cols[grep(l_col, uncert_cols)]
      
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
    
    # Adjust all header label names now
    header_labs <- stringr::str_replace_all(colnames(tab_dat), "_", " ") |>
      stringr::str_to_title()
    header_labs2 <- glue::glue("{header_labs[-1]}{ifelse(unit_label!='', paste0('(', unit_label,')'), ' ')}({uncert_lab})")
    colnames(tab_dat) <- c(header_labs[1], header_labs2)
    
    return(tab_dat)
  }) # close and end lapply
}
