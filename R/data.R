#' SS3 Example data
#'
#' Included data set that represents a Report.sso file converted using
#' convert_output(). This example is from the 2022 Petrale sole stock assessment.
#'
#' @format A tibble (\code{tbl_df}, \code{tbl}, \code{data.frame}) with 591109 rows and 33 variables:
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

#' 2018 Gulf of Alaska Pollock stock assessment output
#'
#' Output of the 2018 Gulf of Alaska Pollock stock assessment produced from Rceattle, standardized 
#' using `stockplotr::convert_output()`. The example can be found and run from the Rceattle repository.
#'
#' @docType data
#'
#' @usage data(pollock)
#'
#' @format A tibble (\code{tbl_df}, \code{tbl}, \code{data.frame}) with 9,983 rows and 35 variables:
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
#' @keywords datasets
#'
#' @references Adams, G. D., Holsman, K. K., Barbeaux, S. J., Dorn, M. W., Ianelli, J. N., Spies, I., … Punt, A. E. (2022)
#' (\doi{10.1016/j.fishres.2022.106303})
#'
#' @source \href{https://github.com/afsc-assessments/Rceattle/blob/main/examples/Fit_2018_GOA_single-species_models.R}{Rceattle Example}
#'
#' @examples
#' data(pollock)
#' head(pollock)
#' unique(pollock$label)
"pollock"

#' SEDAR 55 South Atlantic Vermilion Snapper
#'
#' Standardized output of the 2018 South Atlantic Vermilion Snapper stock assessment 
#' produced from the Beaufort Assessment Model (BAM).
#'
#' @docType data
#'
#' @usage data(vermilion_snapper)
#'
#' @format A tibble (\code{tbl_df}, \code{tbl}, \code{data.frame}) with 35,881 rows and 35 variables:
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
#' @keywords datasets
#' 
#' @references SEDAR (2018)
#' (\url{https://sedarweb.org/documents/sedar-55-stock-assessment-report-south-atlantic-vermilion-snapper/})
#'
#' @examples
#' data(vermilion_snapper)
#' head(vermilion_snapper)
#' unique(vermilion_snapper$label)
"vermilion_snapper"

#' SS3 Simple stock assessment output
#'
#' Standardized output of a generic simple example of SS3 found from the `r4ss` 
#' repository examples. The raw output is converted to an R readable object 
#' using `r4ss::ss_output` then standardized using `stockplotr::convert_output()`. 
#'
#' @docType data
#'
#' @usage data(ss3_simple)
#'
#' @format A tibble (\code{tbl_df}, \code{tbl}, \code{data.frame}) with 38,195 rows and 35 variables:
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
#' @keywords datasets
#'
#' @source \href{https://github.com/r4ss/r4ss/}{r4ss Repository}
#'
#' @examples
#' data(ss3_simple)
#' head(ss3_simple)
#' unique(ss3_simple$label)
"ss3_simple"

#' Big Skate length composition data
#'
#' Length compositions and fits from the 2019 Big Skate stock assessment for
#' the U.S. West Coast (`BigSkate_2019` in the Stock Synthesis test models),
#' the model used for the r4ss example plots at
#' \url{https://r4ss.github.io/r4ss/BigSkate/}. The data are the `lendbase`
#' rows of [convert_output()] applied to the output of `r4ss::SS_output()`.
#' See `data-raw/bigskate_comps.R`.
#'
#' Compositions are sex-specific, with female and male proportions summing to
#' one within each year, fleet, and partition. `Fishery_current` has separate
#' retained and discarded compositions.
#'
#' @format A tibble in the standard [convert_output()] format with 14504 rows
#' and 35 columns. The columns used for compositions are:
#' \describe{
#'   \item{label}{"length_composition_observed", "length_composition_predicted",
#'   "length_composition_pearson_residual" (as reported by Stock Synthesis),
#'   or "length_composition_sample_size" (adjusted input sample size,
#'   `Nsamp_adj` in r4ss)}
#'   \item{estimate}{Value of the label}
#'   \item{module_name}{`"lendbase"`}
#'   \item{year}{Year of the observation}
#'   \item{fleet}{Fleet name: "Fishery_current", "WCGBTS", or "Triennial"}
#'   \item{sex}{"female" or "male"}
#'   \item{part}{Partition: 0 = whole catch, 1 = discard, 2 = retained}
#'   \item{length_bins}{Lower edge of the length bin (cm)}
#' }
#' @source Taylor, I.G., Gertseva, V., Stephens, A., and Bizzarro, J. (2019). Status of
#' Big Skate (Beringraja binoculata) off the U.S. Pacific Coast in 2019.
#' Pacific Fishery Management Council, Portland, OR.
#' @seealso [bigskate_caal], [prepare_composition_data()], [plot_composition_distribution()]
"bigskate_lencomp"

#' Big Skate conditional age-at-length data
#'
#' Conditional age-at-length data and fits from the 2019 Big Skate stock
#' assessment (see [bigskate_lencomp]). The data are the `condbase` rows of
#' [convert_output()] applied to the output of `r4ss::SS_output()`. Each age
#' composition is for one sex and one length bin, and sums to one. See
#' `data-raw/bigskate_comps.R`.
#'
#' @format A tibble in the standard [convert_output()] format with 26240 rows
#' and 35 columns. The columns used for compositions are:
#' \describe{
#'   \item{label}{"conditional_age_composition_observed",
#'   "conditional_age_composition_predicted",
#'   "conditional_age_composition_pearson_residual", or
#'   "conditional_age_composition_sample_size"}
#'   \item{estimate}{Value of the label}
#'   \item{module_name}{`"condbase"`}
#'   \item{year}{Year of the observation}
#'   \item{fleet}{Fleet name: "Fishery_current" or "WCGBTS"}
#'   \item{sex}{"female" or "male"}
#'   \item{part}{Partition: 0 = whole catch, 1 = discard, 2 = retained}
#'   \item{length_bins}{Length bin the ages are conditioned on (`Lbin_lo` in r4ss)}
#'   \item{age}{Age}
#' }
#' @source Taylor, I.G., Gertseva, V., Stephens, A., and Bizzarro, J. (2019). Status of
#' Big Skate (Beringraja binoculata) off the U.S. Pacific Coast in 2019.
#' Pacific Fishery Management Council, Portland, OR.
#' @seealso [bigskate_lencomp], [prepare_composition_data()], [plot_composition_andre()]
"bigskate_caal"
