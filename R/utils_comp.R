#' Prepare composition data for plotting
#'
#' Reshape age or length composition data (observed and, optionally,
#' expected proportions) into the tidy format used by the composition
#' plotting functions ([plot_comp()], [plot_comp_bubble()],
#' [plot_comp_residuals()], [plot_comp_sex_ratio()], and [plot_comp_francis()]).
#'
#' @param dat A data frame of composition data. The following formats are
#' recognized automatically:
#' \describe{
#'   \item{Tidy composition data}{Columns `year`, `bin`, and `observed`, with
#'   optional columns `fleet`, `sex`, `part` (partition: 0 = whole catch,
#'   1 = discard, 2 = retained), `expected`, `n` (input or adjusted sample
#'   size), `pearson`, and `comp_type`. See [bigskate_lencomp].}
#'   \item{FIMS output}{Output from FIMS (e.g., `FIMS::get_estimates()` or
#'   [convert_output()] with `model = "fims"`) that contains `observed` and
#'   `expected` columns. Rows whose `label` contains "comp" are kept and the
#'   bin is taken from the `age` or `length` column.}
#'   \item{stockplotr standard output}{Long-format output from
#'   [convert_output()] where observed and expected compositions are stored in
#'   the `estimate` column with labels such as
#'   "age_composition_proportion"/"age_composition_observed" and
#'   "age_composition_expected"/"age_composition_predicted" (and the
#'   corresponding "length_" labels).}
#'   \item{r4ss output}{A list returned by `r4ss::SS_output()` (length and
#'   age compositions are combined and fleet names are added) or its
#'   `lendbase` or `agedbase` elements (columns `Yr`, `Fleet`, `Sex`, `Bin`,
#'   `Obs`, `Exp`, `Pearson`, `Nsamp_adj`, ...). Conditional
#'   age-at-length rows are dropped from `agedbase` so that only marginal age
#'   compositions remain, unless `conditional = TRUE`.}
#' }
#' For conditional age-at-length data (`conditional = TRUE`), tidy data also
#' need a `length_bin` column giving the length bin that each age
#' composition is conditioned on (see [bigskate_caal]).
#' @param comp_type String. Type of composition to keep when `dat` contains
#' more than one. Options: "age", "length", or NULL (keep all).
#'
#' Default: NULL
#'
#' @param fleet Character or numeric vector. Fleet(s) to keep, matching
#' either the fleet name (e.g., "Fishery_current") or the fleet name with its
#' partition (e.g., "Fishery_current (retained)"). NULL keeps all fleets.
#'
#' Default: NULL
#'
#' @param normalize Logical. TRUE/FALSE; rescale observed and expected values
#' so that they sum to one within each year, fleet, and composition type
#' (and length bin for conditional data). Sex-specific compositions are
#' normalized jointly across sexes, unless each sex already sums to one on
#' its own (single-sex compositions). Use TRUE when `dat` contains counts
#' rather than proportions.
#'
#' Default: TRUE
#'
#' @param conditional Logical. TRUE/FALSE; keep conditional age-at-length
#' data instead of marginal compositions. Conditional compositions are
#' normalized within each length bin. Used by [plot_comp_andre()] and
#' [plot_comp_bubble()].
#'
#' Default: FALSE
#'
#' @returns A tibble with one row per year, fleet, sex, and bin and columns
#' `comp_type`, `fleet`, `year`, `sex`, `bin`, `observed`, `expected`, `n`,
#' and `pearson` (plus `length_bin` when `conditional = TRUE`). Columns that
#' cannot be determined from `dat` are `NA`.
#'
#' @details When Pearson residuals are not supplied, they are calculated
#' assuming a multinomial distribution as
#' \deqn{r = (o - e) / \sqrt{e (1 - e) / N}}
#' where \eqn{o} and \eqn{e} are the observed and expected proportions and
#' \eqn{N} is the sample size in the `n` column. If `n` is not available,
#' residuals are left as `NA`.
#'
#' @seealso [plot_comp()], [plot_comp_bubble()], [plot_comp_residuals()],
#' [plot_comp_sex_ratio()], [plot_comp_francis()]
#' @export
#'
#' @examples
#' comps <- prepare_comp_data(bigskate_lencomp, fleet = "WCGBTS")
#' head(comps)
#'
#' # Data in the stockplotr long format are reshaped automatically
#' long <- data.frame(
#'   label = rep(c("age_composition_proportion", "age_composition_expected"), each = 3),
#'   year = 2020,
#'   fleet = "fleet1",
#'   age = rep(1:3, 2),
#'   estimate = c(0.2, 0.5, 0.3, 0.25, 0.45, 0.3)
#' )
#' prepare_comp_data(long)
prepare_comp_data <- function(
  dat,
  comp_type = NULL,
  fleet = NULL,
  normalize = TRUE,
  conditional = FALSE
) {
  # Full r4ss::SS_output() list: combine length and marginal age comps, or
  # use the conditional age-at-length data
  if (!is.data.frame(dat) && is.list(dat) &&
    any(c("lendbase", "agedbase", "condbase") %in% names(dat))) {
    dbase <- if (conditional) {
      dat[["condbase"]]
    } else {
      dplyr::bind_rows(dat[["lendbase"]], dat[["agedbase"]])
    }
    if (is.null(dbase) || nrow(dbase) == 0) {
      cli::cli_abort("No {if (conditional) 'conditional age-at-length' else 'composition'} data found in the r4ss output.")
    }
    if (!is.null(dat[["FleetNames"]]) && nrow(dbase) > 0) {
      dbase$Fleet_Name <- dat[["FleetNames"]][dbase$Fleet]
    }
    dat <- dbase
  }
  if (!is.data.frame(dat)) {
    cli::cli_abort("{.arg dat} must be a data frame or an {.code r4ss::SS_output()} list.")
  }
  # Return early if the data are already prepared
  prepared <- inherits(dat, "stockplotr_comp")
  if (prepared) {
    out <- dat
    if (conditional != ("length_bin" %in% names(out))) {
      cli::cli_abort("{.arg dat} was prepared with {.code conditional = {!conditional}}.")
    }
  } else {
    out <- switch(detect_comp_format(dat),
      "r4ss" = comp_from_r4ss(dat, conditional = conditional),
      "obs_exp_cols" = comp_from_obs_exp_cols(dat),
      "long" = comp_from_long(dat),
      "tidy" = comp_from_tidy(dat),
      cli::cli_abort(c(
        "Could not find composition data in {.arg dat}.",
        "i" = "Supply a data frame with columns {.field year}, {.field bin}, and {.field observed} (see {.code ?prepare_comp_data})."
      ))
    )
    has_lbin <- any(!is.na(out$length_bin))
    if (conditional && !has_lbin) {
      cli::cli_abort(c(
        "No conditional age-at-length data found in {.arg dat}.",
        "i" = "Tidy data need a {.field length_bin} column."
      ))
    }
    if (!conditional && has_lbin && length(unique(stats::na.omit(out$length_bin))) > 1) {
      cli::cli_abort(c(
        "{.arg dat} contains conditional age-at-length data ({.field length_bin}).",
        "i" = "Use {.code conditional = TRUE}, e.g., with {.fn plot_comp_andre}."
      ))
    }
    if (!conditional) out$length_bin <- NULL
  }

  # Filter to comp type and fleet
  if (!is.null(comp_type)) {
    keep_type <- match.arg(comp_type, c("age", "length"))
    out$comp_type[is.na(out$comp_type)] <- keep_type
    out <- out[out$comp_type == keep_type, , drop = FALSE]
  } else if (length(unique(stats::na.omit(out$comp_type))) > 1) {
    cli::cli_alert_info(
      "Both age and length compositions found; use {.arg comp_type} to plot one type at a time."
    )
  }
  if (!is.null(fleet)) {
    # Match either the fleet name or the fleet name with its partition,
    # e.g., "Fishery_current" or "Fishery_current (retained)"
    fleet_base <- base_fleet(out$fleet)
    keep <- out$fleet %in% as.character(fleet) | fleet_base %in% as.character(fleet)
    out <- out[keep, , drop = FALSE]
  }
  if (nrow(out) == 0) {
    cli::cli_abort("No composition data remain after filtering by {.arg comp_type} and {.arg fleet}.")
  }

  if (normalize && !prepared) {
    # Compositions are normalized jointly across sexes unless each sex
    # already sums to one on its own (single-sex compositions, as is common
    # for conditional age-at-length data)
    out <- out |>
      dplyr::group_by(dplyr::across(dplyr::any_of(
        c("comp_type", "fleet", "year", "length_bin")
      ))) |>
      dplyr::mutate(
        sex_group = if (length(unique(.data$sex)) > 1 &&
          all(abs(tapply(.data$observed, .data$sex, sum, na.rm = TRUE) - 1) < 1e-3)) {
          .data$sex
        } else {
          "joint"
        }
      ) |>
      dplyr::group_by(.data$sex_group, .add = TRUE) |>
      dplyr::mutate(
        observed = .data$observed / sum(.data$observed, na.rm = TRUE),
        expected = .data$expected / sum(.data$expected, na.rm = TRUE)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-"sex_group") |>
      # 0/0 when a year has no observations
      dplyr::mutate(
        observed = dplyr::if_else(is.nan(.data$observed), NA_real_, .data$observed),
        expected = dplyr::if_else(is.nan(.data$expected), NA_real_, .data$expected)
      )
  }

  # Calculate multinomial Pearson residuals if not supplied
  need_pearson <- is.na(out$pearson) & !is.na(out$expected) & !is.na(out$n)
  if (any(need_pearson)) {
    out$pearson[need_pearson] <- with(
      out[need_pearson, ],
      (observed - expected) / sqrt(expected * (1 - expected) / n)
    )
    out$pearson[!is.finite(out$pearson)] <- NA_real_
  }

  out <- out |>
    dplyr::arrange(dplyr::across(dplyr::any_of(
      c("comp_type", "fleet", "year", "length_bin", "sex", "bin")
    )))
  class(out) <- unique(c("stockplotr_comp", class(tibble::as_tibble(out))))
  out
}

#------------------------------------------------------------------------------
# Internal helpers for composition data
#------------------------------------------------------------------------------

# Columns returned by prepare_comp_data()
comp_cols <- c(
  "comp_type", "fleet", "year", "sex", "bin",
  "observed", "expected", "n", "pearson"
)

# Find a column in dat ignoring case; returns NULL if absent
find_col <- function(dat, options) {
  nm <- names(dat)
  for (o in options) {
    hit <- nm[tolower(nm) == tolower(o)]
    if (length(hit) > 0) {
      return(hit[1])
    }
  }
  NULL
}

# Pull a column (by any of several names) or return a default vector
pull_col <- function(dat, options, default = NA) {
  col <- find_col(dat, options)
  if (is.null(col)) {
    return(rep(default, nrow(dat)))
  }
  dat[[col]]
}

detect_comp_format <- function(dat) {
  if (!is.null(find_col(dat, "Yr")) && !is.null(find_col(dat, "Bin")) &&
    !is.null(find_col(dat, "Obs"))) {
    return("r4ss")
  }
  has_obs <- !is.null(find_col(dat, "observed"))
  has_bin <- !is.null(find_col(dat, c("bin", "age", "length", "length_bins")))
  if (has_obs && !is.null(find_col(dat, "label")) &&
    any(grepl("comp", dat[[find_col(dat, "label")]], ignore.case = TRUE)) && has_bin) {
    return("obs_exp_cols")
  }
  if (!is.null(find_col(dat, "label")) && !is.null(find_col(dat, "estimate")) &&
    any(grepl("comp", dat[[find_col(dat, "label")]], ignore.case = TRUE) &
      grepl("observed|proportion|expected|predicted", dat[[find_col(dat, "label")]], ignore.case = TRUE))) {
    return("long")
  }
  if (has_obs && has_bin && !is.null(find_col(dat, "year"))) {
    return("tidy")
  }
  "unknown"
}

# Standardize sex codes to "female", "male", or "combined"
standardize_sex <- function(x) {
  if (all(is.na(x))) {
    return(rep("combined", length(x)))
  }
  x <- tolower(as.character(x))
  dplyr::case_when(
    is.na(x) ~ "combined",
    x %in% c("1", "f", "female", "females") ~ "female",
    x %in% c("2", "m", "male", "males") ~ "male",
    x %in% c("0", "3", "both", "combined", "unsexed", "u", "all") ~ "combined",
    TRUE ~ x
  )
}

comp_type_from_label <- function(label) {
  dplyr::case_when(
    grepl("len|size", label, ignore.case = TRUE) ~ "length",
    grepl("age", label, ignore.case = TRUE) ~ "age",
    TRUE ~ NA_character_
  )
}

finish_comp <- function(out) {
  out <- tibble::as_tibble(out)
  out$fleet <- as.character(out$fleet)
  out$sex <- standardize_sex(out$sex)
  if (!"length_bin" %in% names(out)) out$length_bin <- NA_real_
  for (col in c("year", "bin", "observed", "expected", "n", "pearson", "length_bin")) {
    out[[col]] <- suppressWarnings(as.numeric(out[[col]]))
  }
  out$fleet[is.na(out$fleet)] <- "All"
  out <- dplyr::filter(out, !is.na(.data$bin), !is.na(.data$year))
  out[, c(comp_cols, "length_bin")]
}

comp_from_tidy <- function(dat) {
  finish_comp(data.frame(
    comp_type = pull_col(dat, "comp_type", NA_character_),
    fleet = label_partition(
      as.character(pull_col(dat, c("fleet", "fleet_name"), "All")),
      pull_col(dat, "part")
    ),
    year = pull_col(dat, "year"),
    sex = pull_col(dat, "sex"),
    bin = pull_col(dat, c("bin", "age", "length", "length_bins")),
    observed = pull_col(dat, "observed"),
    expected = pull_col(dat, c("expected", "predicted")),
    n = pull_col(dat, c("n", "sample_size", "nsamp", "nsamp_adj")),
    pearson = pull_col(dat, c("pearson", "pearson_residual", "residual")),
    length_bin = pull_col(dat, c("length_bin", "lbin", "lbin_lo"))
  ))
}

comp_from_obs_exp_cols <- function(dat) {
  label_col <- find_col(dat, "label")
  dat <- dat[grepl("comp", dat[[label_col]], ignore.case = TRUE), , drop = FALSE]
  type <- comp_type_from_label(dat[[label_col]])
  age <- pull_col(dat, "age")
  len <- pull_col(dat, c("length", "length_bins"))
  bin <- pull_col(dat, "bin")
  bin <- dplyr::case_when(
    !is.na(bin) ~ suppressWarnings(as.numeric(bin)),
    type == "length" ~ suppressWarnings(as.numeric(len)),
    TRUE ~ suppressWarnings(as.numeric(age))
  )
  finish_comp(data.frame(
    comp_type = type,
    fleet = pull_col(dat, c("fleet", "fleet_name", "module_id"), "All"),
    year = pull_col(dat, c("year", "year_i")),
    sex = pull_col(dat, "sex"),
    bin = bin,
    observed = pull_col(dat, "observed"),
    expected = pull_col(dat, c("expected", "predicted")),
    n = pull_col(dat, c("n", "sample_size", "nsamp", "nsamp_adj")),
    pearson = pull_col(dat, c("pearson", "pearson_residual"))
  ))
}

comp_from_long <- function(dat) {
  label_col <- find_col(dat, "label")
  dat <- dat[grepl("comp", dat[[label_col]], ignore.case = TRUE), , drop = FALSE]
  lab <- dat[[label_col]]
  value_type <- dplyr::case_when(
    grepl("expected|predicted", lab, ignore.case = TRUE) ~ "expected",
    grepl("observed|proportion", lab, ignore.case = TRUE) ~ "observed",
    TRUE ~ NA_character_
  )
  type <- comp_type_from_label(lab)
  bin <- dplyr::if_else(
    type == "length",
    suppressWarnings(as.numeric(pull_col(dat, c("length_bins", "length", "bin")))),
    suppressWarnings(as.numeric(pull_col(dat, c("age", "bin"))))
  )
  long <- data.frame(
    comp_type = type,
    fleet = as.character(pull_col(dat, c("fleet", "fleet_name"), "All")),
    year = suppressWarnings(as.numeric(pull_col(dat, "year"))),
    sex = as.character(pull_col(dat, "sex")),
    bin = bin,
    value_type = value_type,
    value = suppressWarnings(as.numeric(dat[[find_col(dat, "estimate")]]))
  ) |>
    dplyr::filter(!is.na(.data$value_type))
  wide <- long |>
    tidyr::pivot_wider(
      names_from = "value_type",
      values_from = "value",
      values_fn = function(x) sum(x, na.rm = TRUE)
    )
  if (!"observed" %in% names(wide)) {
    cli::cli_abort("No observed compositions found in {.arg dat}.")
  }
  if (!"expected" %in% names(wide)) wide$expected <- NA_real_
  wide$n <- NA_real_
  wide$pearson <- NA_real_
  finish_comp(wide)
}

comp_from_r4ss <- function(dat, conditional = FALSE) {
  dat <- as.data.frame(dat)
  # Drop conditional age-at-length data, keeping marginal age comps
  lo <- find_col(dat, "Lbin_lo")
  hi <- find_col(dat, "Lbin_hi")
  kind <- pull_col(dat, "Kind")
  if (!is.null(lo) && !is.null(hi)) {
    is_age <- if (all(is.na(kind))) rep(TRUE, nrow(dat)) else toupper(kind) %in% "AGE"
    range <- dat[[hi]] - dat[[lo]]
    multiple_ranges <- any(is_age) && length(unique(range[is_age])) > 1
    if (conditional) {
      # Keep age rows conditioned on a length bin narrower than the full range
      keep <- is_age & (if (multiple_ranges) range < max(range[is_age], na.rm = TRUE) else TRUE)
      dat <- dat[keep, , drop = FALSE]
      kind <- kind[keep]
    } else if (multiple_ranges) {
      cli::cli_alert_info("Conditional age-at-length data removed; only marginal age compositions are kept.")
      keep <- !is_age | range == max(range[is_age], na.rm = TRUE)
      dat <- dat[keep, , drop = FALSE]
      kind <- kind[keep]
    }
  }
  type <- dplyr::case_when(
    toupper(kind) %in% c("LEN", "SIZE") ~ "length",
    toupper(kind) == "AGE" ~ "age",
    !is.null(lo) ~ "age",
    TRUE ~ "length"
  )
  fleet <- as.character(pull_col(dat, c("Fleet_Name", "Fleet_name", "Fleet"), "All"))
  # Add partition to fleet name when a fleet has more than one partition
  fleet <- label_partition(fleet, pull_col(dat, "Part"))
  out <- finish_comp(data.frame(
    comp_type = type,
    fleet = fleet,
    year = pull_col(dat, "Yr"),
    sex = r4ss_sex(dat),
    bin = pull_col(dat, "Bin"),
    observed = pull_col(dat, "Obs"),
    expected = pull_col(dat, "Exp"),
    # Dirichlet-multinomial sample size when available, as in r4ss
    n = dplyr::coalesce(
      suppressWarnings(as.numeric(pull_col(dat, "Nsamp_DM"))),
      suppressWarnings(as.numeric(pull_col(dat, c("Nsamp_adj", "N"))))
    ),
    pearson = pull_col(dat, "Pearson"),
    length_bin = if (conditional && !is.null(lo)) dat[[lo]] else NA_real_
  ))
  # Single-sex male compositions are reported twice in the Stock Synthesis
  # composition database (once for each internal sex code), with identical
  # values, so keep one copy
  dplyr::distinct(out)
}

# Add the partition to fleet names, as in r4ss, when a fleet has data from
# more than one partition (0 = whole catch, 1 = discard, 2 = retained)
label_partition <- function(fleet, part) {
  fleet <- as.character(fleet)
  if (all(is.na(part))) {
    return(fleet)
  }
  part_name <- c("0" = "whole catch", "1" = "discard", "2" = "retained")[as.character(part)]
  multi_part <- tapply(part, fleet, function(x) length(unique(stats::na.omit(x))) > 1)
  ifelse(
    !is.na(part_name) & multi_part[fleet] %in% TRUE,
    paste0(fleet, " (", part_name, ")"),
    fleet
  )
}

# Fleet name without the partition added by label_partition()
base_fleet <- function(fleet) {
  sub(" \\((whole catch|discard|retained)\\)$", "", fleet)
}

# Partition name added by label_partition(), or NA
fleet_partition <- function(fleet) {
  pattern <- "^.* \\((whole catch|discard|retained)\\)$"
  ifelse(grepl(pattern, fleet), sub(pattern, "\\1", fleet), NA_character_)
}

# Sex from an r4ss composition database. r4ss adds a lowercase "sex"
# column (0 = unknown, 1 = female, 2 = male); the uppercase "Sex" column is
# an internal Stock Synthesis code where 1 means female or unknown, so
# single-sex male compositions (Sexes = 2) can have Sex = 1.
r4ss_sex <- function(dat) {
  if ("sex" %in% names(dat)) {
    return(dat[["sex"]])
  }
  sexes <- pull_col(dat, "Sexes")
  sex <- pull_col(dat, "Sex")
  if (all(is.na(sexes))) {
    return(sex)
  }
  dplyr::if_else(sexes == 3, sex, sexes)
}

# Axis label for the bin variable
comp_bin_label <- function(comps) {
  types <- unique(stats::na.omit(comps$comp_type))
  if (length(types) == 1 && types == "age") {
    "Age"
  } else if (length(types) == 1 && types == "length") {
    "Length"
  } else {
    "Bin"
  }
}

# Jeffreys interval for a binomial proportion
jeffreys_interval <- function(p, n, level = 0.75) {
  alpha <- (1 - level) / 2
  x <- p * n
  lower <- ifelse(x <= 0, 0, stats::qbeta(alpha, x + 0.5, n - x + 0.5))
  upper <- ifelse(x >= n, 1, stats::qbeta(1 - alpha, x + 0.5, n - x + 0.5))
  list(lower = lower, upper = upper)
}

# Aggregate compositions across years within fleet (and sex). Years are
# weighted by sample size when available.
aggregate_comp_years <- function(comps) {
  comps |>
    dplyr::group_by(.data$comp_type, .data$fleet, .data$year) |>
    dplyr::mutate(w = dplyr::if_else(is.na(.data$n), 1, .data$n)) |>
    dplyr::group_by(.data$comp_type, .data$fleet, .data$sex, .data$bin) |>
    dplyr::summarise(
      observed = sum(.data$observed * .data$w, na.rm = TRUE),
      expected = sum(.data$expected * .data$w, na.rm = TRUE),
      has_exp = any(!is.na(.data$expected)),
      .groups = "drop"
    ) |>
    dplyr::mutate(n = NA_real_) |>
    dplyr::group_by(.data$comp_type, .data$fleet) |>
    dplyr::mutate(
      observed = .data$observed / sum(.data$observed),
      expected = dplyr::if_else(.data$has_exp, .data$expected / sum(.data$expected), NA_real_)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-"has_exp")
}

# Facet a composition plot by the variables that vary in the data
comp_facet <- function(vars, ncol = NULL, scales = "fixed") {
  if (length(vars) == 0) {
    return(NULL)
  }
  list(
    ggplot2::facet_wrap(
      ggplot2::vars(!!!rlang::syms(vars)),
      ncol = ncol,
      scales = scales,
      labeller = ggplot2::labeller(.multi_line = FALSE)
    ),
    # Fleet names with partitions and weights make long panel labels
    ggplot2::theme(strip.text = ggplot2::element_text(size = 10))
  )
}
