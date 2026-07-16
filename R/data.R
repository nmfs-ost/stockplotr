#' SS3 Example data
#'
#' Included data set that represents a Report.sso file converted using
#' convert_output(). This example is from the 2022 Petrale sole stock assessment.
#'
#' @format A tibble with 591109 rows and 33 variables:
#' \describe{
#'   \item{label}{Standard name for estimate or value name}
#'   \item{estimate}{Actual value of the label}
#'   \item{year}{Annual indexing value}
#'   \item{fleet}{Names of fleets or surveys indexed by the data}
#'   \item{sex}{Native reference to male, female, unknown, or none}
#'   \item{area}{Specified areas by the model}
#'   \item{growth_pattern}{Indexing column of data}
#'   \item{uncertainty}{Value of uncertainty associated with the label column}
#'   \item{uncertainty_label}{Uncertainty label or name associated with the label columns}
#'   \item{module_name}{Name of keyword (SS3) or list indexing in the original model output for tracking purposes}
#'   \item{time}{Time sometimes referenced in decimals to the year and month}
#'   \item{era}{"time" for current time series in model; "fore" representing the projected or forecasted years of the model}
#'   \item{month}{Month factor}
#'   \item{season}{Season usually associated with year}
#'   \item{subseason}{Subseason when used}
#'   \item{birthseas}{Birthseason found in SS3}
#'   \item{initial}{Initial input value in the model when available associated with the label column}
#'   \item{likelihood}{Likelihood value for the data point}
#'   \item{platoon}{Platoon also an SS3 indexing value}
#'   \item{age}{Age of fish}
#'   \item{bio_pattern}{Indexing column of data}
#'   \item{settlement}{Indexing column of data}
#'   \item{morph}{Indexing column of data}
#'   \item{type}{Indexing column of data}
#'   \item{factor}{Indexing column of data}
#'   \item{part}{Indexing column of data}
#'   \item{kind}{Indexing column of data}
#'   \item{nsim}{Indexing column of data}
#'   \item{bin}{Indexing column of data}
#'   \item{age_a}{Alternative age column}
#'   \item{length_bins}{Length bins for composition or other length based data}
#'   \item{count}{Indexing column of data}
#'   \item{block}{Indexing column of data}
#' }
#'
"example_data"
