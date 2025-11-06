##############################
# Utility functions for tables
##############################

create_table <- function(data) {
  ncols <- ncol(data)
  column_names <- paste(colnames(data), collapse = " & ")
  latex_format_data <- paste(
    column_names, "\n"
  )
  for (i in 1:nrow(data)) {
    row_data <- stringr::str_replace_all(
      paste(data[i,], collapse = " & "),
      "NA",
      "-")
    latex_format_data <- paste(
      latex_format_data,
      row_data, "\n",
      sep = "",
      collapse = "\n"
    )
  }
  
  table <- paste0(
    "\\begin{document}\n",
    "\\begin{tabular}{lr}\n",
    # multicolumn needs to = number of columns in the data set
    "\\multicolumn{", ncols, "}{c}{Example}\n",# example is the first header - not sure we want bc it's a merged cell
    # headers and values separated by '&' ending with 2 trailing forward slashes
    latex_format_data,
    "\\end{tabular}\n",
    collapse = "\n"
  )
}