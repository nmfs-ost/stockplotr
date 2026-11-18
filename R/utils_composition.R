#' Prepare composition data for plotting
#'
#' Reshape age or length composition data (observed and, optionally,
#' expected proportions) from [convert_output()] into the tidy format used by
#' the composition plotting functions ([plot_composition_distribution()],
#' [plot_composition_bubble()], [plot_composition_residuals()],
#' [plot_composition_sexratio()], [plot_composition_francis()],
#' [plot_composition_samples()], [plot_composition_residual_distribution()],
#' [plot_composition_residual_pattern()], and [plot_composition_andre()]).
#' Each of these functions calls `prepare_composition_data()` itself, so it
#' only needs to be called directly to inspect or modify the data.
#'
#' @param dat A tibble returned from [convert_output()] (see
#' [bigskate_lencomp]). Compositions are the rows whose `label` contains
#' "composition", for example:
#' \describe{
#'   \item{Stock Synthesis (from an `r4ss::SS_output()` list)}{
#'   "length_composition_observed", "length_composition_predicted",
#'   "length_composition_pearson_residual", and
#'   "length_composition_sample_size" (and the "age_" equivalents), with
#'   conditional age-at-length data labeled "conditional_age_composition_*".}
#'   \item{FIMS}{"age_composition_proportion" and "age_composition_expected"
#'   (and the "length_" equivalents).}
#'   \item{BAM and Rceattle}{"age_composition_observed" and
#'   "age_composition_predicted", or "composition_observed" and
#'   "composition_predicted".}
#' }
#' Bins are taken from the `age` or `length_bins` column, and partitions from
#' the `part` column. Other formats are also recognized:
#' \describe{
#'   \item{Tidy composition data}{Columns `year`, `bin`, and `observed`, with
#'   optional columns `fleet`, `sex`, `part` (partition: 0 = whole catch,
#'   1 = discard, 2 = retained), `expected`, `n` (input or adjusted sample
#'   size), `pearson`, `comp_type`, and `length_bin` (for conditional
#'   age-at-length data).}
#'   \item{FIMS estimates}{Output from `FIMS::get_estimates()` that contains
#'   `observed` and `expected` columns. Rows whose `label` contains "comp" are
#'   kept and the bin is taken from the `age` or `length` column.}
#'   \item{r4ss output}{A list returned by `r4ss::SS_output()` (length and
#'   age compositions are combined and fleet names are added) or its
#'   `lendbase` or `agedbase` elements (columns `Yr`, `Fleet`, `Sex`, `Bin`,
#'   `Obs`, `Exp`, `Pearson`, `Nsamp_adj`, ...). Conditional
#'   age-at-length rows are dropped from `agedbase` so that only marginal age
#'   compositions remain, unless `conditional = TRUE`.}
#' }
#' For conditional age-at-length data (`conditional = TRUE`), see
#' [bigskate_caal].
#' @param comp_type String. Type of composition to keep when `dat` contains
#' more than one. Options: "age", "length", or NULL (keep all). Default: NULL
#'
#' @param fleet Character or numeric vector. Fleet(s) to keep, matching
#' either the fleet name (e.g., "Fishery_current") or the fleet name with its
#' partition (e.g., "Fishery_current (retained)"). NULL keeps all fleets. Default: NULL
#'
#' @param normalize Logical. TRUE/FALSE; rescale observed and expected values
#' so that they sum to one within each year, fleet, and composition type
#' (and length bin for conditional data). Sex-specific compositions are
#' normalized jointly across sexes, unless each sex already sums to one on
#' its own (single-sex compositions). Use TRUE when `dat` contains counts
#' rather than proportions. Default: TRUE
#'
#' @param conditional Logical. TRUE/FALSE; keep conditional age-at-length
#' data instead of marginal compositions. Conditional compositions are
#' normalized within each length bin. Used by [plot_composition_andre()] and
#' [plot_composition_bubble()]. Default: FALSE
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
#' @seealso [plot_composition_distribution()], [plot_composition_bubble()], [plot_composition_residuals()],
#' [plot_composition_sexratio()], [plot_composition_francis()]
#' @export
#'
#' @examples
#' comps <- prepare_composition_data(bigskate_lencomp, fleet = "WCGBTS")
#' head(comps)
#'
#' # Conditional age-at-length data
#' prepare_composition_data(bigskate_caal, conditional = TRUE)
#'
#' # Data in the stockplotr long format are reshaped automatically
#' long <- data.frame(
#'   label = rep(c("age_composition_proportion", "age_composition_expected"), each = 3),
#'   year = 2020,
#'   fleet = "fleet1",
#'   age = rep(1:3, 2),
#'   estimate = c(0.2, 0.5, 0.3, 0.25, 0.45, 0.3)
#' )
#' prepare_composition_data(long)
prepare_composition_data <- function(
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
      "long" = comp_from_long(dat, conditional = conditional),
      "tidy" = comp_from_tidy(dat),
      cli::cli_abort(c(
        "Could not find composition data in {.arg dat}.",
        "i" = "Supply a data frame with columns {.field year}, {.field bin}, and {.field observed} (see {.code ?prepare_composition_data})."
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
        "i" = "Use {.code conditional = TRUE}, e.g., with {.fn plot_composition_andre}."
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

# Columns returned by prepare_composition_data()
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

comp_from_long <- function(dat, conditional = FALSE) {
  label_col <- find_col(dat, "label")
  lab <- dat[[label_col]]
  # Conditional age-at-length rows are labeled "conditional_age_composition_*"
  is_comp <- grepl("comp", lab, ignore.case = TRUE)
  is_conditional <- grepl("^conditional_", lab)
  if (!conditional && !any(is_comp & !is_conditional) && any(is_conditional)) {
    cli::cli_abort(c(
      "{.arg dat} contains only conditional age-at-length data.",
      "i" = "Use {.code conditional = TRUE}, e.g., with {.fn plot_composition_andre}."
    ))
  }
  keep <- is_comp & is_conditional == conditional
  dat <- dat[keep, , drop = FALSE]
  lab <- dat[[label_col]]
  value_type <- dplyr::case_when(
    grepl("pearson", lab, ignore.case = TRUE) ~ "pearson",
    grepl("sample_size", lab, ignore.case = TRUE) ~ "n",
    grepl("expected|predicted", lab, ignore.case = TRUE) ~ "expected",
    grepl("observed|proportion", lab, ignore.case = TRUE) ~ "observed",
    TRUE ~ NA_character_
  )
  type <- if (conditional) rep("age", length(lab)) else comp_type_from_label(lab)
  bin <- dplyr::if_else(
    type %in% "length",
    suppressWarnings(as.numeric(pull_col(dat, c("length_bins", "len_bins", "length", "bin")))),
    suppressWarnings(as.numeric(pull_col(dat, c("age", "bin"))))
  )
  long <- data.frame(
    comp_type = type,
    fleet = label_partition(
      as.character(pull_col(dat, c("fleet", "fleet_name"), "All")),
      pull_col(dat, "part")
    ),
    year = suppressWarnings(as.numeric(pull_col(dat, "year"))),
    sex = as.character(pull_col(dat, "sex")),
    length_bin = if (conditional) {
      suppressWarnings(as.numeric(pull_col(dat, c("length_bins", "len_bins"))))
    } else {
      NA_real_
    },
    bin = bin,
    value_type = value_type,
    value = suppressWarnings(as.numeric(dat[[find_col(dat, "estimate")]]))
  ) |>
    dplyr::filter(!is.na(.data$value_type))
  if (nrow(long) == 0) {
    cli::cli_abort("No {if (conditional) 'conditional age-at-length' else 'composition'} data found in {.arg dat}.")
  }
  # Proportions in repeated rows (e.g., seasons) are summed; sample sizes
  # and residuals are averaged
  wide <- long |>
    dplyr::summarise(
      value = if (dplyr::first(.data$value_type) %in% c("observed", "expected")) {
        sum(.data$value, na.rm = TRUE)
      } else {
        mean(.data$value, na.rm = TRUE)
      },
      .by = c("comp_type", "fleet", "year", "sex", "length_bin", "bin", "value_type")
    ) |>
    tidyr::pivot_wider(names_from = "value_type", values_from = "value")
  if (!"observed" %in% names(wide)) {
    cli::cli_abort("No observed compositions found in {.arg dat}.")
  }
  for (col in c("expected", "n", "pearson")) {
    if (!col %in% names(wide)) wide[[col]] <- NA_real_
  }
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

# Default colors for sex-specific compositions: the two ends of
# viridis::viridis(2, option = "viridis", end = 0.75), i.e., "#440154" for
# females and "#5DC863" for males, with gray for other values (e.g.,
# "combined"). Use with ggplot2::scale_fill_manual() and
# ggplot2::scale_color_manual().
comp_sex_colors <- function() {
  sex_pal <- scales::pal_viridis(option = "viridis", end = 0.75)(2)
  c(female = sex_pal[1], male = sex_pal[2], combined = "gray50", unknown = "gray50")
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
comp_facet <- function(vars, ncol = NULL, scales = "fixed", direction = "v") {
  if (length(vars) == 0) {
    return(NULL)
  }
  list(
    ggplot2::facet_wrap(
      ggplot2::vars(!!!rlang::syms(vars)),
      ncol = ncol,
      dir = match.arg(direction, c("v", "h")),
      scales = scales,
      labeller = ggplot2::labeller(.multi_line = FALSE)
    ),
    # Fleet names with partitions and weights make long panel labels
    ggplot2::theme(strip.text = ggplot2::element_text(size = 10))
  )
}

#------------------------------------------------------------------------------
# Internal helpers
#------------------------------------------------------------------------------

# Prepare comps and keep rows with residuals, or abort with a hint
comp_residuals_or_abort <- function(dat, comp_type = NULL, fleet = NULL) {
  comps <- prepare_composition_data(dat, comp_type = comp_type, fleet = fleet)
  comps <- dplyr::filter(comps, is.finite(.data$pearson))
  if (nrow(comps) == 0) {
    cli::cli_abort(c(
      "No Pearson residuals available.",
      "i" = "Supply a {.field pearson} column, or {.field expected} values and sample sizes ({.field n}) so residuals can be calculated."
    ))
  }
  comps
}

# One-sided Wald-Wolfowitz runs test on the signs of x (in order). Returns
# the p-value for fewer runs than expected (positive autocorrelation).
runs_test <- function(x) {
  s <- sign(x[x != 0 & !is.na(x)])
  n1 <- sum(s > 0)
  n2 <- sum(s < 0)
  n <- n1 + n2
  if (n < 3) {
    return(NA_real_)
  }
  # A single run: probability that all n signs are the same by chance
  if (n1 == 0 || n2 == 0) {
    return(2 * 0.5^n)
  }
  runs <- 1 + sum(s[-1] != s[-length(s)])
  mu <- 2 * n1 * n2 / n + 1
  v <- 2 * n1 * n2 * (2 * n1 * n2 - n) / (n^2 * (n - 1))
  if (v <= 0) {
    return(NA_real_)
  }
  stats::pnorm((runs - mu) / sqrt(v))
}

# Input and effective (McAllister-Ianelli) sample size by composition
comp_effn_table <- function(comps) {
  comps |>
    dplyr::filter(!is.na(.data$expected)) |>
    dplyr::group_by(.data$comp_type, .data$fleet, .data$year) |>
    dplyr::summarise(
      n = dplyr::first(.data$n),
      effn = sum(.data$expected * (1 - .data$expected)) /
        sum((.data$observed - .data$expected)^2),
      .groups = "drop"
    )
}

# Does dat contain observed and expected compositions by bin in the
# standard output format?
has_bin_comps <- function(dat) {
  is.data.frame(dat) && "label" %in% names(dat) &&
    any(grepl("^(length|age)_composition_(predicted|expected)$", dat$label))
}

# Does dat contain Stock Synthesis FIT_*_COMPS tables from convert_output()?
is_ss3_fit_summary <- function(dat) {
  is.data.frame(dat) &&
    all(c("module_name", "label", "estimate") %in% names(dat)) &&
    any(dat$module_name %in% c("FIT_LEN_COMPS", "FIT_AGE_COMPS", "FIT_SIZE_COMPS") &
          dat$label %in% "effn")
}

# Sample sizes from the Stock Synthesis FIT_*_COMPS tables in
# convert_output() output. Each composition is identified by its module,
# fleet, year, and other indexing columns; compositions that cannot be
# separated (e.g., conditional age-at-length, which lack the length bin in
# the converted output) are dropped.
comp_sample_size_from_ss3 <- function(dat, comp_type = NULL) {
  keys <- intersect(
    c("module_name", "fleet", "year", "sex", "season", "subseason", "month", "time"),
    names(dat)
  )
  fit <- dat[dat$module_name %in% c("FIT_LEN_COMPS", "FIT_AGE_COMPS", "FIT_SIZE_COMPS") &
               dat$label %in% c("nsamp_adj", "effn", "part", "ageerr"), c(keys, "label", "estimate")]
  # convert_output() keeps the values from each row of the SS3 table
  # together, in order, so a new composition starts at each "part" (or
  # "nsamp_adj" in output without partitions)
  first_label <- if ("part" %in% fit$label) "part" else "nsamp_adj"
  fit <- fit |>
    dplyr::group_by(.data$module_name) |>
    dplyr::mutate(comp_id = cumsum(.data$label == first_label)) |>
    dplyr::ungroup()
  wide <- fit |>
    dplyr::group_by(.data$module_name, .data$comp_id) |>
    dplyr::filter(all(c("nsamp_adj", "effn") %in% .data$label), !any(duplicated(.data$label))) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = "label", values_from = "estimate") |>
    dplyr::filter(!is.na(.data$year), !is.na(.data$fleet))
  if (!"part" %in% names(wide)) wide$part <- NA_real_
  if (!"ageerr" %in% names(wide)) wide$ageerr <- NA_real_
  wide$comp_type <- dplyr::if_else(wide$module_name == "FIT_AGE_COMPS", "age", "length")
  if (!is.null(comp_type)) wide <- wide[wide$comp_type == comp_type, , drop = FALSE]
  # Several compositions for the same fleet, year, sex, and partition are
  # conditional age-at-length data, which lack the length bin in the
  # converted output
  wide <- wide |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(keys, "part", "ageerr")))) |>
    dplyr::mutate(n_comps = dplyr::n()) |>
    dplyr::ungroup()
  if (any(wide$n_comps > 1)) {
    cli::cli_alert_info(
      "Dropped {sum(wide$n_comps > 1)} composition{?s} that appear to be conditional age-at-length data."
    )
  }
  wide |>
    dplyr::filter(.data$n_comps == 1) |>
    dplyr::mutate(
      year = suppressWarnings(as.numeric(.data$year)),
      n = .data$nsamp_adj
    ) |>
    dplyr::group_by(.data$comp_type) |>
    dplyr::mutate(fleet = label_partition(.data$fleet, .data$part)) |>
    dplyr::ungroup() |>
    dplyr::select("comp_type", "fleet", "year", "n", "effn")
}


# Composition data by bin from an r4ss::SS_output() list, as rows of
# convert_output()'s standard output. Each composition database becomes one
# module ("lendbase", "agedbase", "condbase") with labels
# "<type>_composition_<value>", where <type> is "length", "age", or
# "conditional_age" and <value> is "observed", "predicted",
# "pearson_residual", or "sample_size" (adjusted input sample size, or the
# Dirichlet-multinomial sample size when available).
ss3_composition_rows <- function(replist, fleet_names, start_year, end_year, columns) {
  dbases <- c(lendbase = "length", agedbase = "age", condbase = "conditional_age")
  out <- list()
  for (db in names(dbases)) {
    d <- replist[[db]]
    if (!is.data.frame(d) || nrow(d) == 0) next
    prefix <- dbases[[db]]
    sex <- standardize_sex(r4ss_sex(d))
    bin <- as.character(d[["Bin"]])
    base <- data.frame(
      module_name = db,
      year = as.character(d[["Yr"]]),
      month = as.character(pull_col(d, "Month")),
      season = as.character(pull_col(d, "Seas")),
      fleet = fleet_names[d[["Fleet"]]],
      sex = sex,
      part = as.character(pull_col(d, "Part")),
      kind = as.character(pull_col(d, "Kind")),
      age = if (prefix == "length") NA_character_ else bin,
      len_bins = if (prefix == "length") {
        bin
      } else if (prefix == "conditional_age") {
        as.character(d[["Lbin_lo"]])
      } else {
        NA_character_
      }
    )
    n <- dplyr::coalesce(
      suppressWarnings(as.numeric(pull_col(d, "Nsamp_DM"))),
      suppressWarnings(as.numeric(pull_col(d, "Nsamp_adj")))
    )
    values <- list(
      observed = d[["Obs"]],
      predicted = d[["Exp"]],
      pearson_residual = suppressWarnings(as.numeric(d[["Pearson"]])),
      sample_size = n
    )
    for (v in names(values)) {
      rows <- base
      rows$label <- paste0(prefix, "_composition_", v)
      rows$estimate <- as.numeric(values[[v]])
      out[[paste(db, v)]] <- rows
    }
  }
  if (length(out) == 0) {
    return(NULL)
  }
  out <- dplyr::bind_rows(out) |>
    # Single-sex male compositions are reported twice in the composition
    # database (once for each internal sex code) with identical values
    dplyr::distinct() |>
    dplyr::mutate(
      era = dplyr::case_when(
        as.numeric(.data$year) < start_year ~ "init",
        as.numeric(.data$year) <= end_year ~ "time",
        TRUE ~ "fore"
      )
    )
  out[setdiff(columns, names(out))] <- NA
  as.data.frame(out[, columns])
}
