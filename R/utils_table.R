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

check_label_differences <- function(dat, index_variables, id_group = NULL) {
  # Loop over model to perform checks if the model columns are identical
  for (mod in unique(dat$model)){
    mod_index_variables <- unique(index_variables[names(index_variables) == mod])
    mod_data <- dplyr::filter(dat, model == mod)
    mod_id_group <- unique(id_group[names(id_group) == mod])
    
    if (length(unique(mod_data$label)) == 2) {
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
        dplyr::summarise(across(unique(mod_data$label), ~ all(is.na(.))))
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
