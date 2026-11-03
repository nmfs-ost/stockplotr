#' Check the structure of converted model output
#'
#' @param dat Data frame from [convert_output()] containing `label` and
#'   `estimate` columns, or a non-empty list of such data frames.
#' @returns Invisibly returns `NULL` if the data has the expected structure.
#' @noRd
check_data <- function(dat) {
  if (is.data.frame(dat) || !is.list(dat) || length(dat) == 0) {
    dat <- list(dat)
  }
  for (model_dat in dat) {
    if (!is.data.frame(model_dat) || !all(c("label", "estimate") %in% colnames(model_dat))) {
      stop("dat is not the appropriate format. Have you run `convert_output()`?")
    }
  }
  invisible(NULL)
}
