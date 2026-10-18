#' Plot input versus effective sample size of compositions
#'
#' Compare the input (or adjusted input) sample size of each composition with
#' its effective sample size, the sample size implied by how well the model
#' fits the composition. This is the diagnostic behind the McAllister and
#' Ianelli (1997) data weighting method.
#'
#' @inheritParams plot_comp
#' @param dat Composition data in any format accepted by
#' [prepare_comp_data()], or [convert_output()] output from Stock Synthesis
#' that contains the `FIT_LEN_COMPS` or `FIT_AGE_COMPS` tables (as in
#' `stockplotr::example_data`), from which the adjusted input (`nsamp_adj`)
#' and effective (`effn`) sample sizes are taken.
#' @param show_years Logical. TRUE/FALSE; label each point with its year.
#'
#' Default: FALSE
#'
#' @returns A `ggplot2` object.
#'
#' @details For each composition (year and fleet), the effective sample size
#' is
#' \deqn{N_{eff} = \sum_b e_b (1 - e_b) / \sum_b (o_b - e_b)^2}
#' where \eqn{o_b} and \eqn{e_b} are the observed and expected proportions in
#' bin \eqn{b} (McAllister and Ianelli 1997), the same calculation used by
#' Stock Synthesis.
#'
#' Each panel shows a 1:1 line, a loess smoother (dashed red, when there are
#' more than six distinct input sample sizes), the arithmetic mean of the
#' input sample sizes (vertical dashed line), and the harmonic mean of the
#' effective sample sizes (horizontal dashed line). The ratio of the harmonic
#' mean effective sample size to the mean input sample size, the
#' McAllister-Ianelli weight, is included in each panel label. Ratios below
#' one suggest that input sample sizes are too large relative to the fit. As
#' in Stock Synthesis, the weight is calculated across all partitions of a
#' fleet; multiplying it by the current variance adjustment gives the
#' `Recommend_var_adj` reported by Stock Synthesis.
#'
#' Effective sample sizes from this formula are not appropriate for
#' compositions fit with a Dirichlet-multinomial likelihood.
#'
#' @references McAllister, M.K., and Ianelli, J.N. (1997). Bayesian stock
#' assessment using catch-age data and the sampling-importance resampling
#' algorithm. Canadian Journal of Fisheries and Aquatic Sciences 54: 284-300.
#'
#' @section r4ss equivalent: `r4ss::SSplotComps()` subplot 7 ("N-EffN
#' comparison").
#'
#' @seealso [prepare_comp_data()], [plot_comp_francis()]
#' @export
#'
#' @examples
#' plot_comp_sample_size(bigskate_lencomp)
#'
#' # Stock Synthesis output converted with convert_output()
#' plot_comp_sample_size(stockplotr::example_data, comp_type = "length")
plot_comp_sample_size <- function(
  dat,
  comp_type = NULL,
  fleet = NULL,
  show_years = FALSE,
  ncol = NULL
) {
  if (is_ss3_fit_summary(dat)) {
    if (!is.null(comp_type)) comp_type <- match.arg(comp_type, c("age", "length"))
    sizes <- comp_sample_size_from_ss3(dat, comp_type = comp_type)
    if (!is.null(fleet)) {
      sizes <- sizes[sizes$fleet %in% as.character(fleet), , drop = FALSE]
    }
    if (nrow(sizes) == 0) {
      cli::cli_abort("No sample sizes remain after filtering by {.arg comp_type} and {.arg fleet}.")
    }
  } else {
    comps <- prepare_comp_data(dat, comp_type = comp_type, fleet = fleet)
    sizes <- comp_effn_table(comps)
  }
  sizes <- dplyr::filter(sizes, is.finite(.data$n), is.finite(.data$effn))
  if (nrow(sizes) == 0) {
    cli::cli_abort(c(
      "Effective sample sizes could not be calculated.",
      "i" = "Supply {.field expected} values and sample sizes ({.field n})."
    ))
  }

  facet_vars <- c(if (length(unique(sizes$comp_type)) > 1) "comp_type", "fleet")
  # Harmonic mean effective N and mean input N by panel, and the
  # McAllister-Ianelli weight by fleet. As in Stock Synthesis, partitions of
  # a fleet share one weight.
  sizes$base_fleet <- base_fleet(sizes$fleet)
  mi <- sizes |>
    dplyr::group_by(.data$comp_type, .data$base_fleet) |>
    dplyr::summarise(
      n_parts = length(unique(.data$fleet)),
      mi_weight = (1 / mean(1 / .data$effn)) / mean(.data$n),
      .groups = "drop"
    )
  summ <- sizes |>
    dplyr::group_by(dplyr::across(dplyr::all_of(unique(c("comp_type", facet_vars, "base_fleet"))))) |>
    dplyr::summarise(
      mean_n = mean(.data$n),
      hmean_effn = 1 / mean(1 / .data$effn),
      .groups = "drop"
    ) |>
    dplyr::left_join(mi, by = c("comp_type", "base_fleet")) |>
    dplyr::mutate(
      panel = paste0(
        .data$fleet, "\nMcAllister-Ianelli weight",
        dplyr::if_else(.data$n_parts > 1, " (all partitions)", ""),
        " = ", signif(.data$mi_weight, 3)
      )
    )
  sizes <- dplyr::left_join(sizes, summ, by = unique(c("comp_type", facet_vars, "base_fleet")))

  # Loess smoother by panel when there are enough distinct sample sizes
  smooth <- lapply(split(sizes, sizes$panel), function(d) {
    if (length(unique(d$n)) <= 6 || diff(range(d$n)) <= 2) {
      return(NULL)
    }
    fit <- suppressWarnings(stats::loess(effn ~ n, data = d, degree = 1))
    d <- d[order(d$n), ]
    d$fit <- stats::predict(fit, newdata = data.frame(n = d$n))
    d
  })
  smooth <- dplyr::bind_rows(smooth)

  plt <- ggplot2::ggplot(sizes, ggplot2::aes(x = .data$n, y = .data$effn)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "black") +
    ggplot2::geom_vline(
      data = summ,
      ggplot2::aes(xintercept = .data$mean_n),
      linetype = "22",
      color = "#0085CA"
    ) +
    ggplot2::geom_hline(
      data = summ,
      ggplot2::aes(yintercept = .data$hmean_effn),
      linetype = "22",
      color = "#0085CA"
    ) +
    ggplot2::geom_point(color = "gray30", size = 2)
  if (nrow(smooth) > 0) {
    plt <- plt +
      ggplot2::geom_line(
        data = smooth,
        ggplot2::aes(x = .data$n, y = .data$fit),
        color = "#D55E00",
        linetype = "dashed"
      )
  }
  if (show_years) {
    plt <- plt +
      ggplot2::geom_text(
        ggplot2::aes(label = .data$year),
        hjust = -0.2,
        size = 2.8
      )
  }
  plt +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::labs(
      x = "Adjusted input sample size (N adj.)",
      y = "Effective sample size (N eff.)",
      caption = "Solid line: 1:1. Dashed blue lines: arithmetic mean N adj. and harmonic mean N eff."
    ) +
    theme_noaa() +
    comp_facet(c(if ("comp_type" %in% facet_vars) "comp_type", "panel"), ncol = ncol, scales = "free")
}

#------------------------------------------------------------------------------

#' Plot the distribution of composition residuals
#'
#' Quantile-quantile plot or histogram of Pearson residuals for each fleet,
#' to check for heavy tails, skewness, and outliers.
#'
#' @inheritParams plot_comp
#' @param type String. Type of plot. Options: "qq" (normal quantile-quantile
#' plot) or "histogram" (with a standard normal density overlaid).
#'
#' Default: "qq"
#'
#' @returns A `ggplot2` object.
#'
#' @details The standard deviation of the normalized residuals (SDNR) for
#' each fleet is included in the panel label. Values well above one suggest
#' that the compositions are given more weight (larger sample sizes) than the
#' fit supports. Pearson residuals for compositions are not expected to be
#' exactly normally distributed, especially for bins with small expected
#' proportions or small sample sizes, so treat departures in the tails with
#' some caution.
#'
#' @seealso [plot_comp_residuals()], [plot_comp_residual_pattern()],
#' [prepare_comp_data()]
#' @export
#'
#' @examples
#' plot_comp_residual_dist(bigskate_lencomp)
#' plot_comp_residual_dist(bigskate_lencomp, type = "histogram")
plot_comp_residual_dist <- function(
  dat,
  type = c("qq", "histogram"),
  comp_type = NULL,
  fleet = NULL,
  ncol = NULL
) {
  type <- match.arg(type)
  comps <- comp_residuals_or_abort(dat, comp_type = comp_type, fleet = fleet)
  facet_vars <- c(if (length(unique(comps$comp_type)) > 1) "comp_type", "fleet")
  comps <- comps |>
    dplyr::group_by(dplyr::across(dplyr::all_of(facet_vars))) |>
    dplyr::mutate(
      panel = paste0(.data$fleet, " (SDNR = ", format(round(stats::sd(.data$pearson), 2), nsmall = 2), ")")
    ) |>
    dplyr::ungroup()

  if (type == "qq") {
    plt <- ggplot2::ggplot(comps, ggplot2::aes(sample = .data$pearson)) +
      ggplot2::geom_qq_line(color = "#0085CA", linewidth = 0.8) +
      ggplot2::geom_qq(size = 1, alpha = 0.5) +
      ggplot2::labs(x = "Theoretical quantiles (standard normal)", y = "Pearson residual")
  } else {
    plt <- ggplot2::ggplot(comps, ggplot2::aes(x = .data$pearson)) +
      ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(.data$density)),
        bins = 30,
        fill = "gray60",
        color = "white"
      ) +
      ggplot2::stat_function(fun = stats::dnorm, color = "#0085CA", linewidth = 0.8) +
      ggplot2::labs(x = "Pearson residual", y = "Density")
  }
  plt +
    theme_noaa() +
    comp_facet(c(if ("comp_type" %in% facet_vars) "comp_type", "panel"), ncol = ncol, scales = "free")
}

#------------------------------------------------------------------------------

#' Plot patterns in composition residuals by bin or year
#'
#' Plot the mean Pearson residual for each bin (across years) or each year
#' (across bins), with a runs test for non-random patterns in the signs of
#' the residuals. These plots summarize the stripes that can appear in
#' residual bubble plots, for example, consistently negative residuals at
#' older ages caused by mis-specified selectivity.
#'
#' @inheritParams plot_comp
#' @param by String. Summarize residuals by "bin" (age or length) or by
#' "year".
#'
#' Default: "bin"
#'
#' @returns A `ggplot2` object.
#'
#' @details Vertical lines are plus and minus two standard errors of the mean
#' residual. The runs test (Wald-Wolfowitz) is applied to the signs of the
#' residuals ordered by year (when `by = "bin"`) or by bin (when
#' `by = "year"`). Filled points have fewer runs than expected by chance
#' (one-sided p < 0.05), which indicates a non-random pattern of residuals.
#' When all residuals have the same sign, the p-value is the probability of
#' that happening by chance, \eqn{2 \times 0.5^n}.
#' Residuals are summarized separately for each fleet and sex.
#'
#' @seealso [plot_comp_residuals()], [plot_comp_residual_dist()],
#' [prepare_comp_data()]
#' @export
#'
#' @examples
#' plot_comp_residual_pattern(bigskate_lencomp)
#' plot_comp_residual_pattern(bigskate_lencomp, by = "year")
plot_comp_residual_pattern <- function(
  dat,
  by = c("bin", "year"),
  comp_type = NULL,
  fleet = NULL,
  ncol = NULL
) {
  by <- match.arg(by)
  comps <- comp_residuals_or_abort(dat, comp_type = comp_type, fleet = fleet)
  bin_label <- comp_bin_label(comps)
  order_var <- if (by == "bin") "year" else "bin"

  summ <- comps |>
    dplyr::arrange(.data[[order_var]]) |>
    dplyr::group_by(.data$comp_type, .data$fleet, .data$sex, .data[[by]]) |>
    dplyr::summarise(
      mean_resid = mean(.data$pearson),
      se = stats::sd(.data$pearson) / sqrt(dplyr::n()),
      runs_p = runs_test(.data$pearson),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      runs = factor(
        dplyr::if_else(!is.na(.data$runs_p) & .data$runs_p < 0.05, "Non-random (p < 0.05)", "Random"),
        levels = c("Non-random (p < 0.05)", "Random")
      )
    )

  facet_vars <- c(
    if (length(unique(summ$comp_type)) > 1) "comp_type",
    "fleet",
    if (length(unique(summ$sex)) > 1) "sex"
  )

  ggplot2::ggplot(summ, ggplot2::aes(x = .data[[by]], y = .data$mean_resid)) +
    ggplot2::geom_hline(yintercept = 0, color = "gray50") +
    ggplot2::geom_linerange(
      ggplot2::aes(
        ymin = .data$mean_resid - 2 * .data$se,
        ymax = .data$mean_resid + 2 * .data$se
      ),
      color = "gray40",
      na.rm = TRUE
    ) +
    ggplot2::geom_point(ggplot2::aes(fill = .data$runs), shape = 21, size = 2.5) +
    ggplot2::scale_fill_manual(
      values = c("Non-random (p < 0.05)" = "gray10", "Random" = "white"),
      drop = FALSE
    ) +
    ggplot2::labs(
      x = if (by == "bin") bin_label else "Year",
      y = "Mean Pearson residual",
      fill = paste("Runs test across", if (by == "bin") "years" else "bins")
    ) +
    theme_noaa() +
    comp_facet(facet_vars, ncol = ncol)
}

#------------------------------------------------------------------------------

#' Plot Andre's mean age at length for conditional age-at-length data
#'
#' For conditional age-at-length data, plot the observed and expected mean
#' age in each length bin, and the standard error of the mean age, for each
#' year. These are "Andre's plots" from `r4ss::SSplotComps()` (subplot 12).
#'
#' @inheritParams plot_comp
#' @param dat Conditional age-at-length data in any format accepted by
#' [prepare_comp_data()] with `conditional = TRUE`: a tidy data frame with a
#' `length_bin` column (see [bigskate_caal]), or an
#' `r4ss::SS_output()` list (its `condbase` element is used).
#' @param interval Number. Coverage of the intervals around the observed
#' values.
#'
#' Default: 0.90
#'
#' @returns A `ggplot2` object with one row of panels per year: mean age at
#' length on the left and standard error of the mean age on the right.
#'
#' @details Within each year and length bin, the observed and expected mean
#' ages are \eqn{\sum_a a\, o_a} and \eqn{\sum_a a\, e_a}, and the standard
#' errors are the corresponding standard deviations of age divided by
#' \eqn{\sqrt{N}}, where \eqn{N} is the number of fish aged in the length
#' bin. Following r4ss, the interval for the mean is the observed mean plus or
#' minus the normal quantile times the observed standard error, and the
#' interval for the standard error is based on the chi-square distribution.
#' Observed values outside the intervals, or expected values that
#' consistently differ from the observed values, suggest mis-specified growth
#' or variability in length at age.
#'
#' Length bins with no observations or no sample size are omitted. Plot one
#' fleet at a time with `fleet`, and subset years by filtering `dat`.
#'
#' @section r4ss equivalent: `r4ss::SSplotComps()` subplot 12 (Andre's plots of
#' mean age and standard deviation for conditional age-at-length data).
#'
#' @seealso [prepare_comp_data()], [plot_comp_bubble()]
#' @export
#'
#' @examples
#' plot_comp_andre(
#'   dplyr::filter(bigskate_caal, year %in% 2016:2018),
#'   fleet = "WCGBTS"
#' )
plot_comp_andre <- function(
  dat,
  interval = 0.90,
  fleet = NULL,
  ncol = NULL
) {
  comps <- prepare_comp_data(dat, comp_type = "age", fleet = fleet, conditional = TRUE)
  if (length(unique(comps$fleet)) > 1) {
    cli::cli_alert_info(
      "Plotting {length(unique(comps$fleet))} fleets; use {.arg fleet} to plot one fleet at a time."
    )
  }
  z <- stats::qnorm(1 - (1 - interval) / 2)
  stats_df <- comps |>
    dplyr::group_by(.data$fleet, .data$year, .data$sex, .data$length_bin) |>
    dplyr::filter(
      max(.data$observed, na.rm = TRUE) > 1e-4,
      !is.na(dplyr::first(.data$n)),
      dplyr::first(.data$n) > 0
    ) |>
    dplyr::summarise(
      N = dplyr::first(.data$n),
      obs_mean = sum(.data$bin * .data$observed) / sum(.data$observed),
      obs_sd = sqrt(max(0, sum(.data$bin^2 * .data$observed) / sum(.data$observed) - .data$obs_mean[1]^2)),
      exp_mean = sum(.data$bin * .data$expected) / sum(.data$expected),
      exp_sd = sqrt(max(0, sum(.data$bin^2 * .data$expected) / sum(.data$expected) - .data$exp_mean[1]^2)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      obs_se = .data$obs_sd / sqrt(.data$N),
      exp_se = .data$exp_sd / sqrt(.data$N)
    )
  if (nrow(stats_df) == 0) {
    cli::cli_abort("No conditional age-at-length data with observations and sample sizes found.")
  }

  mean_df <- stats_df |>
    dplyr::transmute(
      .data$fleet, .data$year, .data$sex, .data$length_bin,
      statistic = "Mean age",
      observed = .data$obs_mean,
      expected = .data$exp_mean,
      lower = .data$obs_mean - z * .data$obs_se,
      upper = .data$obs_mean + z * .data$obs_se
    )
  se_df <- stats_df |>
    dplyr::transmute(
      .data$fleet, .data$year, .data$sex, .data$length_bin,
      statistic = "SE of mean age",
      observed = .data$obs_se,
      expected = .data$exp_se,
      lower = dplyr::if_else(
        .data$N > 1,
        .data$obs_se * sqrt(pmax(.data$N - 1, 0) / stats::qchisq(1 - (1 - interval) / 2, .data$N)),
        NA_real_
      ),
      upper = dplyr::if_else(
        .data$N > 1,
        .data$obs_se * sqrt(pmax(.data$N - 1, 0) / stats::qchisq((1 - interval) / 2, .data$N)),
        NA_real_
      )
    )
  plot_df <- dplyr::bind_rows(mean_df, se_df) |>
    dplyr::mutate(statistic = factor(.data$statistic, levels = c("Mean age", "SE of mean age")))

  row_vars <- c(
    if (length(unique(plot_df$fleet)) > 1) "fleet",
    if (length(unique(plot_df$sex)) > 1) "sex",
    "year"
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$length_bin)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      fill = "gray90",
      na.rm = TRUE
    ) +
    ggplot2::geom_line(ggplot2::aes(y = .data$lower), linetype = "dotted", na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .data$upper), linetype = "dotted", na.rm = TRUE) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$expected, linetype = "Expected"),
      color = "#0085CA",
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(ggplot2::aes(y = .data$observed, shape = "Observed"), size = 1.5) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(!!!rlang::syms(row_vars)),
      cols = ggplot2::vars(.data$statistic),
      scales = "free_y",
      labeller = ggplot2::labeller(.multi_line = FALSE)
    ) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(
      x = "Length bin",
      y = NULL,
      shape = NULL,
      linetype = NULL,
      caption = paste0(
        "Left: mean age at length (obs. and exp.) with ", round(interval * 100),
        "% intervals based on adding ", round(z, 2), " SE of mean to the data.\n",
        "Right: SE of mean age at length (obs. and exp.) with ", round(interval * 100),
        "% intervals based on the chi-square distribution."
      )
    ) +
    theme_noaa() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8)
    )
}

#------------------------------------------------------------------------------
# Internal helpers
#------------------------------------------------------------------------------

# Prepare comps and keep rows with residuals, or abort with a hint
comp_residuals_or_abort <- function(dat, comp_type = NULL, fleet = NULL) {
  comps <- prepare_comp_data(dat, comp_type = comp_type, fleet = fleet)
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
