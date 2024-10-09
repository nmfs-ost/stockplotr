# Comma: format table numbers

#' Function to format numbers to text with 1,000s of commas.
#' This function was copied with permission from Ben Williams,
#' a NOAA Research Fisheries Biologist, as part of the AFSC SAFE
#' reporting tool.
#'
#' @param number value to format
#'
#' @export comma
#'
comma <- function( number ) {
  format(round(as.numeric( number ), digits = 0), big.mark = ",")
}
