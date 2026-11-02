#' Check the structure of converted model output
#'
#' @param dat Data frame from [convert_output()] containing `label` and
#'   `estimate` columns.
#' @returns Invisibly returns `NULL` if the data has the expected structure.
#' @noRd
check_data <- function(dat) {
  if (!is.data.frame(dat) || !all(c("label", "estimate") %in% colnames(dat))) {
    stop("dat is not the appropriate format. Have you run `convert_output()`?")
  }
  invisible(NULL)
}
