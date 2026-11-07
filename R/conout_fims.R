#' Convert FIMS output to the standard output format
#'
#' Reshape the estimates from a FIMS model (`FIMS::get_estimates()`) into the
#' standardized long format returned by [convert_output()].
#'
#' @param file A tibble returned by `FIMS::get_estimates()`, a `FIMSFit`
#' object, or the path to an .rds file containing either.
#' @param out_new The empty standard output data frame from
#' [convert_output()].
#' @param start_year Number. First year of the model. FIMS reports years as
#' indices (`year_i`, 1 = first model year), which are converted to years as
#' `year_i + start_year - 1`. NULL keeps the indices.
#'
#' @returns A data frame with the columns of `out_new`.
#'
#' @details Each data point (index, landings, and age and length compositions)
#' is one row in FIMS, with the observed value in `observed` and the expected
#' value in `estimated`. These are split into two rows, labeled
#' "*_observed" and "*_predicted" as for other models (e.g., "index_observed"
#' and "index_predicted"). Observed rows hold the observation error in
#' `uncertainty` and the negative log-likelihood of the observation in
#' `likelihood`. FIMS reports the observation error as `log_sd`, the log of
#' its standard deviation. For lognormal data this standard deviation
#' (\eqn{\sigma}) is on the log scale, so it is converted to the coefficient
#' of variation of the observation, \eqn{\sqrt{\exp(\sigma^2) - 1}}, and
#' labeled "cv"; for other data `exp(log_sd)` is labeled "se". Observations
#' coded as missing (-999) are not observations and are not included.
#' Compositions are kept as counts, as reported by FIMS.
#'
#' All other estimates keep one row each, with the value in `estimate` and
#' its standard error in `uncertainty`. FIMS codes estimates that could not
#' be calculated as -999; these are NA. Rows in years after the last year of
#' data (e.g., spawning biomass at the start of the year after the model
#' ends) have `era` "fore"; other rows with a year have `era` "time".
#'
#' @noRd
conout_fims <- function(file, out_new, start_year = NULL) {
  dat <- read_fims_estimates(file)
  names(dat) <- tolower(names(dat))
  if (!all(c("label", "estimated") %in% names(dat))) {
    cli::cli_abort(c(
      "{.arg file} is not FIMS output.",
      "i" = "Supply the output of {.code FIMS::get_estimates()} or a {.cls FIMSFit} object."
    ))
  }
  col <- function(name) {
    if (name %in% names(dat)) dat[[name]] else rep(NA, nrow(dat))
  }
  year_i <- suppressWarnings(as.numeric(col("year_i")))
  if (is.null(start_year)) {
    cli::cli_alert_info(
      "FIMS reports years as indices (1 = first model year); use {.arg start_year} to convert them to years."
    )
    year <- year_i
  } else {
    year <- year_i + start_year - 1
  }
  observed <- suppressWarnings(as.numeric(col("observed")))
  observed[observed %in% -999] <- NA
  # The last year of data; later years are after the model ends
  data_years <- year_i[!is.na(observed)]
  last_year <- if (length(data_years) > 0) max(data_years) else max(year_i, na.rm = TRUE)
  era <- dplyr::case_when(
    is.na(year_i) ~ NA_character_,
    year_i > last_year ~ "fore",
    TRUE ~ "time"
  )
  uncertainty <- dplyr::na_if(suppressWarnings(as.numeric(col("uncertainty"))), -999)
  base <- data.frame(
    module_name = as.character(col("module_name")),
    label = as.character(dat$label),
    era = era,
    year = year,
    initial = suppressWarnings(as.numeric(col("input"))),
    estimate = dplyr::na_if(suppressWarnings(as.numeric(dat$estimated)), -999),
    uncertainty = uncertainty,
    uncertainty_label = ifelse(is.na(uncertainty), NA_character_, "se"),
    likelihood = NA_real_,
    fleet = as.character(col("fleet")),
    age = suppressWarnings(as.numeric(col("age"))),
    len_bins = suppressWarnings(as.numeric(col("length"))),
    stringsAsFactors = FALSE
  )

  # Data points: split into observed and predicted rows
  stream <- dplyr::case_when(
    grepl("^index_?expected$", base$label, ignore.case = TRUE) ~ "index",
    grepl("^(catch|landings)_?expected$", base$label, ignore.case = TRUE) ~ "landings",
    grepl("^age_?comp(osition)?_?expected$", base$label, ignore.case = TRUE) ~ "age_composition",
    grepl("^length_?comp(osition)?_?expected$", base$label, ignore.case = TRUE) ~ "length_composition",
    TRUE ~ NA_character_
  )
  # Fleets with data for a stream; expected values for other fleets (e.g.,
  # the expected index of a fishery without an index) keep their FIMS label
  has_data <- stats::ave(!is.na(observed), stream, base$fleet, FUN = any)
  is_data <- !is.na(stream) & has_data %in% TRUE
  obs_rows <- base[is_data & !is.na(observed), , drop = FALSE]
  if (nrow(obs_rows) > 0) {
    is_obs <- is_data & !is.na(observed)
    sigma <- exp(suppressWarnings(as.numeric(col("log_sd")))[is_obs])
    # A lognormal sigma is on the log scale; report the CV of the observation
    lognormal <- grepl("^(d?lnorm|log_?normal)$", col("distribution")[is_obs], ignore.case = TRUE)
    obs_rows$label <- paste0(stream[is_obs], "_observed")
    obs_rows$estimate <- observed[is_obs]
    obs_rows$uncertainty <- ifelse(lognormal, sqrt(exp(sigma^2) - 1), sigma)
    obs_rows$uncertainty_label <- dplyr::case_when(
      is.na(sigma) ~ NA_character_,
      lognormal ~ "cv",
      TRUE ~ "se"
    )
    obs_rows$initial <- NA_real_
    # Stock Synthesis reports the negative log-likelihood of each observation
    obs_rows$likelihood <- -suppressWarnings(as.numeric(col("likelihood")))[is_obs]
  }
  pred_rows <- base[is_data, , drop = FALSE]
  pred_rows$label <- paste0(stream[is_data], "_predicted")
  out <- rbind(base[!is_data, , drop = FALSE], obs_rows, pred_rows)

  missing <- setdiff(names(out_new), names(out))
  out[missing] <- NA
  out[, names(out_new)]
}

# The estimates of a FIMS model from get_estimates() output, a FIMSFit
# object, or an .rds file with either
read_fims_estimates <- function(file) {
  if (is.character(file) && length(file) == 1) {
    file <- readRDS(file)
  }
  if (inherits(file, "FIMSFit")) {
    get_estimates <- tryCatch(
      getExportedValue("FIMS", "get_estimates"),
      error = function(e) {
        cli::cli_abort("The {.pkg FIMS} package is needed to read a {.cls FIMSFit} object.")
      }
    )
    file <- get_estimates(file)
  }
  if (!is.data.frame(file)) {
    cli::cli_abort("{.arg file} must be FIMS output from {.code FIMS::get_estimates()} or a {.cls FIMSFit} object.")
  }
  as.data.frame(file)
}
