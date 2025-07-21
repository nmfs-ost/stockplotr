# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Utility functions to read and support SS3
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Before converted development

SS3_extract_df <- function(dat, label) {
  # Read SS3 report file

  # Locate the row containing the specified value from the df
  value_row <- which(apply(dat, 1, function(row) any(row == label)))[2]

  # If the parameter value is not found, return NA
  if (length(value_row) == 0) {
    cli::cli_alert_danger("Label not found in data frame.")
    return(NA)
  }
  # Search for the next blank row after the value
  next_blank <- which(apply(dat, 1, function(row) all(is.na(row) | row == "" | row == "-" | row == "#")) & (seq_len(nrow(dat)) > value_row))[1]

  # Combine the rows surrounding the selected metric from the output table
  rows <- c(value_row, next_blank)

  # Extract the metric using the rows from above as a guide and clean up empty columns
  clean_df <- dat[rows[1]:(rows[2] - 1), ] |>
    naniar::replace_with_na_all(condition = ~ .x == "")
  clean_df <- Filter(function(x) !all(is.na(x)), clean_df)

clean_df
}


SS3_extract_bio_info <- function(
    dat,
    parameter = c("SPB", "Recr", "SPRratio", "F", "Bratio", "SSB", "TotBio", "SmryBio", "SPR", "Fstd", "TotYield", "RetYield", "ForeCatch", "OFLCatch", "ForeCatchret", "Bzero"),
    reference.points = TRUE) {
  # dat parameter is already extracted df from the Report.sso file

  parameter <- match.arg(parameter, several.ok = FALSE)

  # Convert SS3 output to table
  get_ncol <- function(file, skip = 0) {
    nummax <- max(utils::count.fields(file,
      skip = skip, quote = "",
      comment.char = ""
    )) + 1
  nummax
  }

  output <- utils::read.table(
    file = dat, col.names = 1:get_ncol(dat), fill = TRUE, quote = "",
    colClasses = "character", nrows = -1, comment.char = "",
    blank.lines.skip = FALSE
  )

  # Extract derived quantities specifically
  bio_info <- SS3_extract_df(output, "DERIVED_QUANTITIES")[-c(1:4), ]
  colnames(bio_info) <- bio_info[1, ]
  bio_info <- bio_info[-1, ] |>
    tidyr::separate_wider_delim(cols = Label, delim = "_", names = c("label", "year")) |>
    dplyr::filter(label == parameter)
  bio_info <- Filter(function(x) !all(is.na(x)), bio_info)

  if (isTRUE(reference.points)) {

  }
bio_info
}
