##############################
# Utility functions for tables
##############################

create_table <- function(data) {
  ncols <- ncol(data)
  column_names <- paste(colnames(data), collapse = " & ")
  latex_format_data <- paste(
    column_names, "\\\\", "\n"
  )
  
  # c signifies all cols will be centered
  alignment <- strrep("c", ncols)
  
  for (i in 1:nrow(data)) {
    row_data <- stringr::str_replace_all(
      paste(data[i,], collapse = " & "),
      "NA",
      "-")
    
    # ends up adding a space and two forward slashes to the end of each line
    row_data_with_linebreak <- paste0(row_data, " \\\\")
    
    latex_format_data <- paste(
      latex_format_data,
      row_data_with_linebreak, "\n",
   #   row_data, "\n",
      sep = "",
      collapse = "\n"
    )
  }
  
  table <- paste0(
   # "\\begin{document}\n",
    "\\begin{tabular}{", alignment, "}\n",
    # multicolumn needs to = number of columns in the data set
    "\\multicolumn{", ncols, "}{c}{Example} \\\\ \n",# example is the first header - not sure we want bc it's a merged cell
    # headers and values separated by '&' ending with 2 trailing forward slashes
    latex_format_data,
    "\\end{tabular}\n",
    collapse = "\n"
  )
}