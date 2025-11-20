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
                  "\n",
                  lab,
                  "\n",
                  latex_tbl,
                  "\n",
                  "\\end{center}")
  
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
  # 
  cat(table)
}
