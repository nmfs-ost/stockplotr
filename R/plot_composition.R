#' Plot composition distributions
#'
#' Plot age or length compositions as distributions (proportion by bin), one
#' panel per year, optionally overlaying the model-expected values. Setting
#' `aggregate = TRUE` instead sums the compositions across years so that each
#' fleet is summarized in a single panel.
#'
#' @param dat A tibble returned from [convert_output()] that contains
#' composition data (see [bigskate_lencomp]), or composition data in any other
#' format accepted by [prepare_composition_data()].
#' @param show_expected Logical. TRUE/FALSE; overlay the expected values as
#' lines. Ignored if `dat` contains no expected values.
#'
#' Default: TRUE
#'
#' @param aggregate Logical. TRUE/FALSE; aggregate compositions across years
#' within each fleet. Years are weighted by their sample size when sample
#' sizes are available.
#'
#' Default: FALSE
#'
#' @param show_n Logical. TRUE/FALSE; label each panel with its adjusted
#' input sample size ("N adj.") and, when expected values are shown, its
#' effective sample size ("N eff."), as in r4ss. Not used when
#' `aggregate = TRUE`.
#'
#' Default: TRUE
#'
#' @param ncol Number. Number of facet columns. NULL lets `ggplot2` choose.
#'
#' Default: NULL
#'
#' @inheritParams prepare_composition_data
#'
#' @returns A `ggplot2` object.
#'
#' @details When compositions are sex-specific (both "female" and "male"
#' values in the `sex` column), females are plotted above zero and males
#' below zero. Bars show observed proportions and lines show expected
#' proportions.
#'
#' If more than one fleet is plotted without aggregating, panels are
#' created for every fleet and year combination; use `fleet` to focus on one
#' fleet at a time.
#'
#' @section Equivalent plots in other packages:
#' r4ss: `r4ss::SSplotComps()` subplot 1 (by year) and
#' subplot 21 (aggregated across years).
#'
#' @seealso [prepare_composition_data()], [plot_composition_bubble()],
#' [plot_composition_residuals()], [plot_composition_sexratio()], [plot_composition_francis()]
#' @export
#'
#' @examples
#' # Observations only
#' plot_composition_distribution(bigskate_lencomp, fleet = "WCGBTS", show_expected = FALSE)
#'
#' # Observed and expected
#' plot_composition_distribution(bigskate_lencomp, fleet = "Triennial")
#'
#' # All fleets aggregated across years
#' plot_composition_distribution(bigskate_lencomp, aggregate = TRUE)
plot_composition_distribution <- function(
  dat,
  show_expected = TRUE,
  aggregate = FALSE,
  comp_type = NULL,
  fleet = NULL,
  show_n = TRUE,
  ncol = NULL
) {
  comps <- prepare_composition_data(dat, comp_type = comp_type, fleet = fleet)
  bin_label <- comp_bin_label(comps)

  if (aggregate) {
    comps <- aggregate_comp_years(comps)
  }
  show_expected <- show_expected && any(!is.na(comps$expected))
  two_sex <- all(c("female", "male") %in% comps$sex)

  # Mirror males below zero for sex-specific comps
  comps <- comps |>
    dplyr::mutate(
      direction = dplyr::if_else(two_sex & .data$sex == "male", -1, 1),
      obs_plot = .data$observed * .data$direction,
      exp_plot = .data$expected * .data$direction
    )

  bins <- sort(unique(comps$bin))
  width <- if (length(bins) > 1) min(diff(bins)) else 1

  plt <- ggplot2::ggplot(comps, ggplot2::aes(x = .data$bin))
  if (length(unique(comps$sex)) > 1) {
    plt <- plt +
      ggplot2::geom_col(
        ggplot2::aes(y = .data$obs_plot, fill = .data$sex),
        width = width * 0.9,
        alpha = 0.6,
        position = if (two_sex) "identity" else "dodge"
      )
  } else {
    plt <- plt +
      ggplot2::geom_col(
        ggplot2::aes(y = .data$obs_plot),
        width = width * 0.9,
        alpha = 0.6,
        fill = "gray50"
      )
  }
  if (show_expected) {
    exp_dat <- dplyr::filter(comps, !is.na(.data$exp_plot))
    if (length(unique(comps$sex)) > 1) {
      plt <- plt +
        ggplot2::geom_line(
          data = exp_dat,
          ggplot2::aes(y = .data$exp_plot, group = .data$sex, color = .data$sex),
          linewidth = 0.7
        )
    } else {
      plt <- plt +
        ggplot2::geom_line(
          data = exp_dat,
          ggplot2::aes(y = .data$exp_plot, group = .data$sex),
          color = "#0085CA",
          linewidth = 0.7
        )
    }
  }
  if (two_sex) {
    plt <- plt + ggplot2::geom_hline(yintercept = 0, linewidth = 0.3)
  }

  # Sample size labels, as in r4ss: adjusted input (N adj.) and effective
  # (N eff.) sample sizes
  if (show_n && !aggregate && any(!is.na(comps$n))) {
    n_lab <- comps |>
      dplyr::filter(!is.na(.data$n)) |>
      dplyr::distinct(.data$comp_type, .data$fleet, .data$year, .data$n) |>
      dplyr::mutate(label = paste0("N adj.=", signif(.data$n, 3)))
    if (show_expected) {
      n_lab <- n_lab |>
        dplyr::left_join(
          dplyr::select(comp_effn_table(comps), "comp_type", "fleet", "year", "effn"),
          by = c("comp_type", "fleet", "year")
        ) |>
        dplyr::mutate(label = paste0(.data$label, "\nN eff.=", signif(.data$effn, 3)))
    }
    plt <- plt +
      ggplot2::geom_text(
        data = n_lab,
        ggplot2::aes(x = Inf, y = Inf, label = .data$label),
        hjust = 1.1,
        vjust = 1.5,
        size = 2.8,
        inherit.aes = FALSE
      )
  }

  ylab <- if (two_sex) "Proportion (females above, males below)" else "Proportion"
  plt <- plt +
    ggplot2::scale_y_continuous(labels = function(x) abs(x)) +
    ggplot2::labs(
      x = bin_label,
      y = ylab,
      fill = if (length(unique(comps$sex)) > 1) "Sex",
      color = if (length(unique(comps$sex)) > 1 && show_expected) "Sex"
    ) +
    theme_noaa()

  # Facets
  facet_vars <- if (aggregate) "fleet" else c("fleet", "year")
  if (!aggregate && length(unique(comps$fleet)) == 1) facet_vars <- "year"
  if (length(unique(comps$comp_type)) > 1) facet_vars <- c("comp_type", facet_vars)
  if (!aggregate && length(unique(comps$fleet)) > 1) {
    cli::cli_alert_info(
      "Plotting {length(unique(comps$fleet))} fleets by year; use {.arg fleet} to plot one fleet at a time."
    )
  }
  plt <- plt + comp_facet(facet_vars, ncol = ncol)
  if (!aggregate) {
    plt <- plt + ggplot2::theme(
      strip.text = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8)
    )
  }
  plt
}

#------------------------------------------------------------------------------

#' Plot composition bubble plots
#'
#' Bubble plot of compositions by year (x-axis) and bin (y-axis). Bubbles can
#' show the observed (or expected) proportions, which makes strong and weak
#' cohorts easy to follow through time, or the Pearson residuals, which show
#' blocks or stripes of misfit among years or bins.
#'
#' @inheritParams plot_composition_distribution
#' @param value String. Value to plot. Options: "observed", "expected", or
#' "pearson".
#'
#' Default: "observed"
#'
#' @param max_size Number. Size of the largest bubble.
#'
#' Default: 6
#'
#' @param conditional Logical. TRUE/FALSE; plot conditional age-at-length
#' data (see [prepare_composition_data()]), with age on the y-axis, length bin on
#' the x-axis, and one panel per year.
#'
#' Default: FALSE
#'
#' @returns A `ggplot2` object.
#'
#' @details Panels are created for each fleet, and for each sex when
#' compositions are sex-specific. For Pearson residuals, filled bubbles are
#' positive residuals (observed > expected) and open bubbles are negative
#' residuals (observed < expected), with bubble area proportional to the
#' absolute value of the residual. The maximum absolute residual for each
#' panel is included in the panel label.
#'
#' @section Equivalent plots in other packages:
#' r4ss: `r4ss::SSplotComps()` subplot 2 (bubble plot of
#' observations or residuals) and subplot 3 (conditional age-at-length).
#'
#' @seealso [prepare_composition_data()], [plot_composition_residuals()], [plot_composition_distribution()]
#' @export
#'
#' @examples
#' # Observed proportions
#' plot_composition_bubble(bigskate_lencomp)
#'
#' # Pearson residuals
#' plot_composition_bubble(bigskate_lencomp, value = "pearson")
#'
#' # Residuals for conditional age-at-length data
#' plot_composition_bubble(
#'   dplyr::filter(bigskate_caal, year %in% 2016:2018),
#'   value = "pearson",
#'   fleet = "WCGBTS",
#'   conditional = TRUE
#' )
plot_composition_bubble <- function(
  dat,
  value = c("observed", "pearson", "expected"),
  comp_type = NULL,
  fleet = NULL,
  max_size = 6,
  ncol = NULL,
  conditional = FALSE
) {
  value <- match.arg(value)
  comps <- prepare_composition_data(
    dat,
    comp_type = if (conditional) "age" else comp_type,
    fleet = fleet,
    conditional = conditional
  )
  bin_label <- comp_bin_label(comps)
  # Conditional age-at-length: age by length bin, one panel per year
  x_var <- if (conditional) "length_bin" else "year"
  comps$value <- comps[[value]]
  comps <- dplyr::filter(comps, !is.na(.data$value))
  if (nrow(comps) == 0) {
    if (value == "pearson") {
      cli::cli_abort(c(
        "No Pearson residuals available.",
        "i" = "Supply a {.field pearson} column, or {.field expected} values and sample sizes ({.field n}) so residuals can be calculated."
      ))
    }
    cli::cli_abort("No {value} values found in {.arg dat}.")
  }

  facet_vars <- c(
    if (length(unique(comps$comp_type)) > 1) "comp_type",
    if (!conditional || length(unique(comps$fleet)) > 1) "fleet",
    if (length(unique(comps$sex)) > 1) "sex",
    if (conditional) "year"
  )
  if (value == "pearson") {
    # Add the max absolute residual to a single panel label, ordered by
    # comp type, fleet, and sex
    comps <- comps |>
      dplyr::arrange(.data$comp_type, .data$fleet, .data$sex, .data$year) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(facet_vars))) |>
      dplyr::mutate(
        panel = paste0(
          paste(unlist(dplyr::cur_group()), collapse = ", "),
          "\n(max = ", format(round(max(abs(.data$value)), 2), nsmall = 2), ")"
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(panel = factor(.data$panel, levels = unique(.data$panel))) |>
      dplyr::mutate(
        sign = factor(
          dplyr::if_else(.data$value >= 0, "Observed > expected", "Observed < expected"),
          levels = c("Observed > expected", "Observed < expected")
        )
      )
    plt <- ggplot2::ggplot(
      comps,
      ggplot2::aes(x = .data[[x_var]], y = .data$bin, size = abs(.data$value), fill = .data$sign)
    ) +
      ggplot2::geom_point(shape = 21, color = "black", alpha = 0.8) +
      ggplot2::scale_fill_manual(
        values = c("Observed > expected" = "gray20", "Observed < expected" = "white"),
        drop = FALSE
      ) +
      ggplot2::labs(size = "|Pearson residual|", fill = NULL) +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size = 4)))
    facet_vars <- "panel"
  } else {
    plt <- ggplot2::ggplot(
      comps,
      ggplot2::aes(x = .data[[x_var]], y = .data$bin, size = .data$value)
    ) +
      ggplot2::geom_point(shape = 21, color = "black", fill = "gray40", alpha = 0.5) +
      ggplot2::labs(size = cap_first_letter(paste(value, "proportion")))
  }

  plt +
    ggplot2::scale_size_area(max_size = max_size) +
    ggplot2::scale_x_continuous(
      breaks = if (conditional) ggplot2::waiver() else axis_breaks(comps$year),
      guide = ggplot2::guide_axis(minor.ticks = TRUE)
    ) +
    ggplot2::labs(x = if (conditional) "Length bin" else "Year", y = bin_label) +
    theme_noaa() +
    comp_facet(facet_vars, ncol = ncol)
}

#------------------------------------------------------------------------------

#' Plot composition Pearson residuals
#'
#' A convenience wrapper around [plot_composition_bubble()] with
#' `value = "pearson"`. Residuals are taken from `dat` when supplied or
#' calculated by [prepare_composition_data()] assuming a multinomial distribution.
#'
#' @inheritParams plot_composition_bubble
#' @param ... Additional arguments passed to [plot_composition_bubble()].
#'
#' @returns A `ggplot2` object.
#' @section Equivalent plots in other packages:
#' r4ss: `r4ss::SSplotComps()` subplot 2 for residuals by
#' fleet and subplot 24 (residuals compared across fleets).
#'
#' @seealso [plot_composition_bubble()], [prepare_composition_data()]
#' @export
#'
#' @examples
#' plot_composition_residuals(bigskate_lencomp)
#'
#' # Compare residuals across fleets for females
#' plot_composition_residuals(
#'   dplyr::filter(bigskate_lencomp, sex == "female"),
#'   ncol = 1
#' )
plot_composition_residuals <- function(dat, ...) {
  plot_composition_bubble(dat, value = "pearson", ...)
}

#------------------------------------------------------------------------------

#' Plot sex ratios implied by composition data
#'
#' Plot the proportion female, female / (female + male), by bin for the
#' observed and expected compositions. Trends in the lack of fit to sex ratios
#' can be easier to see here than in residuals, especially for bins with
#' small proportions.
#'
#' @inheritParams plot_composition_distribution
#' @param interval Number. Coverage of the interval around the observed sex
#' ratio, calculated as a Jeffreys interval using the sample size in each bin
#' (the sample size multiplied by the proportion of the composition in the
#' bin). Set to NULL to omit intervals. Intervals are only shown when sample
#' sizes are available.
#'
#' Default: 0.75
#'
#' @returns A `ggplot2` object.
#'
#' @details Requires sex-specific compositions with both "female" and
#' "male" values in the `sex` column of `dat`. Points show observed ratios
#' and lines show expected ratios. As in r4ss, the smallest observed
#' proportion in each composition (such as the small constant Stock Synthesis
#' adds to every bin) is subtracted from the observed and expected values
#' first, so bins with no fish observed are omitted. When `aggregate = TRUE`, female and male
#' compositions are summed across years (weighted by sample size when
#' available) before calculating ratios.
#'
#' @section Equivalent plots in other packages:
#' r4ss: `r4ss::SSplotSexRatio()` with
#' `sexratio.option = 2` (female:total).
#'
#' @seealso [prepare_composition_data()], [plot_composition_distribution()]
#' @export
#'
#' @examples
#' plot_composition_sexratio(bigskate_lencomp, fleet = "WCGBTS")
#' plot_composition_sexratio(bigskate_lencomp, aggregate = TRUE)
plot_composition_sexratio <- function(
  dat,
  aggregate = FALSE,
  interval = 0.75,
  comp_type = NULL,
  fleet = NULL,
  ncol = NULL
) {
  comps <- prepare_composition_data(dat, comp_type = comp_type, fleet = fleet)
  bin_label <- comp_bin_label(comps)
  if (!all(c("female", "male") %in% comps$sex)) {
    cli::cli_abort("Sex ratios require compositions with both {.val female} and {.val male} values in the {.field sex} column.")
  }

  # Females and males side by side, weighted by sample size when available.
  # As in r4ss, the smallest observation in each composition (e.g., the
  # constant Stock Synthesis adds to every bin) is subtracted first, so that
  # bins without fish do not appear as 50:50 sex ratios.
  wide <- comps |>
    dplyr::filter(.data$sex %in% c("female", "male")) |>
    dplyr::group_by(.data$comp_type, .data$fleet, .data$year) |>
    dplyr::mutate(
      min_obs = min(.data$observed, na.rm = TRUE),
      observed = pmax(.data$observed - .data$min_obs, 0),
      expected = pmax(.data$expected - .data$min_obs, 0)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(w = dplyr::if_else(is.na(.data$n), 1, .data$n)) |>
    dplyr::select("comp_type", "fleet", "year", "sex", "bin", "observed", "expected", "n", "w") |>
    tidyr::pivot_wider(names_from = "sex", values_from = c("observed", "expected"))
  group_vars <- if (aggregate) c("comp_type", "fleet", "bin") else c("comp_type", "fleet", "year", "bin")

  ratios <- wide |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      f_obs = sum(.data$observed_female * .data$w, na.rm = TRUE),
      m_obs = sum(.data$observed_male * .data$w, na.rm = TRUE),
      f_exp = sum(.data$expected_female * .data$w, na.rm = TRUE),
      m_exp = sum(.data$expected_male * .data$w, na.rm = TRUE),
      has_n = all(!is.na(.data$n)),
      has_exp = any(!is.na(.data$expected_female)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      ratio_obs = .data$f_obs / (.data$f_obs + .data$m_obs),
      ratio_exp = dplyr::if_else(.data$has_exp, .data$f_exp / (.data$f_exp + .data$m_exp), NA_real_),
      # weighted sums equal the number of fish in the bin when w is the sample size
      # (at least 1, as in r4ss)
      n_bin = dplyr::if_else(
        .data$has_n & .data$f_obs + .data$m_obs > 0,
        pmax(.data$f_obs + .data$m_obs, 1),
        NA_real_
      )
    )
  ratios$ratio_obs[!is.finite(ratios$ratio_obs)] <- NA_real_
  ratios$ratio_exp[!is.finite(ratios$ratio_exp)] <- NA_real_

  show_interval <- !is.null(interval) && any(!is.na(ratios$n_bin))
  if (show_interval) {
    ci <- jeffreys_interval(ratios$ratio_obs, ratios$n_bin, level = interval)
    ratios$lower <- ci$lower
    ratios$upper <- ci$upper
  }

  plt <- ggplot2::ggplot(ratios, ggplot2::aes(x = .data$bin)) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray60")

  if (show_interval) {
    plt <- plt +
      ggplot2::geom_linerange(
        data = dplyr::filter(ratios, !is.na(.data$ratio_obs), !is.na(.data$lower)),
        ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
        color = "gray50"
      )
  }
  plt <- plt +
    ggplot2::geom_point(
      data = dplyr::filter(ratios, !is.na(.data$ratio_obs)),
      ggplot2::aes(y = .data$ratio_obs, shape = "Observed"),
      size = 1.5
    )
  if (any(!is.na(ratios$ratio_exp))) {
    plt <- plt +
      ggplot2::geom_line(
        data = dplyr::filter(ratios, !is.na(.data$ratio_exp)),
        ggplot2::aes(y = .data$ratio_exp, linetype = "Expected"),
        color = "#0085CA",
        linewidth = 0.8
      )
  }

  facet_vars <- if (aggregate) "fleet" else c("fleet", "year")
  if (!aggregate && length(unique(ratios$fleet)) == 1) facet_vars <- "year"
  if (length(unique(ratios$comp_type)) > 1) facet_vars <- c("comp_type", facet_vars)

  plt <- plt +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
    ggplot2::labs(
      x = bin_label,
      y = "Fraction female",
      shape = NULL,
      linetype = NULL,
      caption = if (!is.null(interval) && any(!is.na(ratios$n_bin))) {
        paste0("Vertical lines: ", round(interval * 100), "% Jeffreys intervals for observed ratios")
      }
    ) +
    theme_noaa() +
    comp_facet(facet_vars, ncol = ncol)
  if (!aggregate) {
    plt <- plt + ggplot2::theme(
      strip.text = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 8)
    )
  }
  plt
}

#------------------------------------------------------------------------------

#' Plot mean age or length with Francis (TA1.8) data weights
#'
#' Time series of the mean age or length in the observed and expected
#' compositions. Developed as a diagnostic for the Francis (2011) data
#' weighting method (TA1.8), the plot is also a useful summary of trends in
#' compositions and lack of fit among years.
#'
#' @inheritParams plot_composition_distribution
#' @param interval Number. Coverage of the interval around the observed mean.
#'
#' Default: 0.95
#'
#' @param show_francis Logical. TRUE/FALSE; add the Francis TA1.8 weight for
#' each fleet to the panel label and show thinner, capped intervals
#' representing sample sizes after applying that weight.
#'
#' Default: TRUE
#'
#' @returns A `ggplot2` object.
#'
#' @details For each year and fleet (with sexes combined), the observed mean
#' is \eqn{\bar{x}_o = \sum_b x_b o_b} and the expected mean is
#' \eqn{\bar{x}_e = \sum_b x_b e_b}, where \eqn{x_b} is the bin and \eqn{o_b}
#' and \eqn{e_b} are the observed and expected proportions. Assuming a
#' multinomial distribution, the standard error of the mean is
#' \eqn{\sqrt{v_e / N}}, where \eqn{v_e = \sum_b x_b^2 e_b - \bar{x}_e^2} is
#' the variance of the expected composition and \eqn{N} is the sample size.
#' Intervals are drawn around the observed means.
#'
#' The Francis weight is \eqn{1 / \mathrm{var}(r)} where
#' \eqn{r = (\bar{x}_o - \bar{x}_e) / SE} are the standardized residuals of
#' the mean across years. As in Stock Synthesis and
#' `r4ss::SSMethod.TA1.8()`, all partitions of a fleet (e.g., retained and
#' discarded) share one weight, and its 95% interval is calculated by
#' bootstrapping the standardized residuals (1000 resamples), so use
#' [set.seed()] for reproducible intervals. The weight assumes a multinomial likelihood and has not been
#' adapted for Dirichlet-multinomial or other likelihoods.
#'
#' Without expected values, only the observed means are plotted. Without
#' sample sizes, no intervals or weights are shown.
#'
#' @references Francis, R.I.C.C. (2011). Data weighting in statistical
#' fisheries stock assessment models. Canadian Journal of Fisheries and
#' Aquatic Sciences 68: 1124-1138.
#'
#' @section Equivalent plots in other packages:
#' r4ss: `r4ss::SSplotComps()` subplots 8 and 9, which
#' use `r4ss::SSMethod.TA1.8()`.
#'
#' @seealso [prepare_composition_data()], [plot_composition_distribution()]
#' @export
#'
#' @examples
#' plot_composition_francis(bigskate_lencomp)
plot_composition_francis <- function(
  dat,
  interval = 0.95,
  show_francis = TRUE,
  comp_type = NULL,
  fleet = NULL,
  ncol = NULL
) {
  comps <- prepare_composition_data(dat, comp_type = comp_type, fleet = fleet)
  bin_label <- comp_bin_label(comps)
  means <- comp_mean_table(comps)
  z <- stats::qnorm(1 - (1 - interval) / 2)
  has_exp <- any(!is.na(means$mean_exp))
  has_se <- any(!is.na(means$se))

  # Francis weights by fleet. As in Stock Synthesis and
  # r4ss::SSMethod.TA1.8(), partitions of a fleet share one weight.
  weights <- NULL
  if (show_francis && has_exp && has_se) {
    means$base_fleet <- base_fleet(means$fleet)
    weights <- means |>
      dplyr::filter(!is.na(.data$std_resid)) |>
      dplyr::group_by(.data$comp_type, .data$base_fleet) |>
      dplyr::filter(dplyr::n() > 1) |>
      dplyr::summarise(
        n_parts = length(unique(.data$fleet)),
        weight = 1 / stats::var(.data$std_resid),
        weight_ci = list(francis_boot_ci(.data$std_resid)),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        weight_label = paste0(
          "TA1.8 weight", dplyr::if_else(.data$n_parts > 1, " (all partitions)", ""),
          " = ", signif(.data$weight, 3), " (",
          signif(purrr::map_dbl(.data$weight_ci, 1), 3), "-",
          signif(purrr::map_dbl(.data$weight_ci, 2), 3), ")"
        )
      )
    means <- means |>
      dplyr::left_join(
        dplyr::select(weights, "comp_type", "base_fleet", "weight", "weight_label"),
        by = c("comp_type", "base_fleet")
      ) |>
      dplyr::mutate(
        fleet = dplyr::if_else(
          is.na(.data$weight_label),
          .data$fleet,
          paste0(.data$fleet, "\n", .data$weight_label)
        ),
        se_adj = .data$se / sqrt(.data$weight)
      )
  }

  plt <- ggplot2::ggplot(means, ggplot2::aes(x = .data$year))
  if (has_se) {
    plt <- plt +
      ggplot2::geom_linerange(
        ggplot2::aes(
          ymin = .data$mean_obs - z * .data$se,
          ymax = .data$mean_obs + z * .data$se
        ),
        color = "gray40",
        linewidth = 0.8
      )
    if (!is.null(weights) && nrow(weights) > 0) {
      plt <- plt +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = .data$mean_obs - z * .data$se_adj,
            ymax = .data$mean_obs + z * .data$se_adj
          ),
          color = "gray20",
          linewidth = 0.3,
          width = 0.4
        )
    }
  }
  plt <- plt +
    ggplot2::geom_point(ggplot2::aes(y = .data$mean_obs, shape = "Observed"), size = 2)
  if (has_exp) {
    plt <- plt +
      ggplot2::geom_line(
        data = dplyr::filter(means, !is.na(.data$mean_exp)),
        ggplot2::aes(y = .data$mean_exp, linetype = "Expected"),
        color = "#0085CA",
        linewidth = 0.8
      )
  }

  caption <- NULL
  if (has_se) {
    caption <- paste0(round(interval * 100), "% intervals based on current sample sizes")
    if (!is.null(weights) && nrow(weights) > 0) {
      caption <- paste0(
        caption,
        "; thinner intervals (with capped ends) show result of further adjusting sample sizes (Francis TA1.8)"
      )
    }
  }

  facet_vars <- c(if (length(unique(means$comp_type)) > 1) "comp_type", "fleet")
  plt +
    ggplot2::scale_x_continuous(
      breaks = axis_breaks(means$year),
      guide = ggplot2::guide_axis(minor.ticks = TRUE)
    ) +
    ggplot2::labs(
      x = "Year",
      y = paste("Mean", tolower(bin_label)),
      shape = NULL,
      linetype = NULL,
      caption = caption
    ) +
    theme_noaa() +
    comp_facet(facet_vars, ncol = ncol, scales = "free_y")
}

# Bootstrap 95% interval for the Francis weight, following
# r4ss::SSMethod.TA1.8() (1000 resamples of the standardized residuals)
francis_boot_ci <- function(std_resid, n_boot = 1000) {
  boot <- matrix(
    sample(std_resid, n_boot * length(std_resid), replace = TRUE),
    nrow = length(std_resid)
  )
  w <- apply(boot, 2, function(x) 1 / stats::var(x))
  as.vector(stats::quantile(w, c(0.025, 0.975), na.rm = TRUE))
}

# Mean bin of observed and expected compositions by year and fleet. As in
# r4ss::SSMethod.TA1.8(), sexes and partitions of a fleet in the same year
# are combined into one composition, with the mean sample size.
comp_mean_table <- function(comps) {
  comps <- comps |>
    dplyr::mutate(
      part_name = fleet_partition(.data$fleet),
      fleet = base_fleet(.data$fleet)
    )
  comps |>
    dplyr::group_by(.data$comp_type, .data$fleet) |>
    dplyr::mutate(
      fleet = if (length(unique(stats::na.omit(.data$part_name))) > 1) {
        paste0(.data$fleet, " (", paste(sort(unique(stats::na.omit(.data$part_name))), collapse = " & "), ")")
      } else {
        .data$fleet
      }
    ) |>
    dplyr::group_by(.data$comp_type, .data$fleet, .data$year, .data$bin) |>
    dplyr::summarise(
      observed = sum(.data$observed, na.rm = TRUE),
      expected = if (all(is.na(.data$expected))) NA_real_ else sum(.data$expected, na.rm = TRUE),
      n = mean(.data$n),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data$comp_type, .data$fleet, .data$year) |>
    dplyr::summarise(
      mean_obs = sum(.data$bin * .data$observed) / sum(.data$observed),
      mean_exp = sum(.data$bin * .data$expected) / sum(.data$expected),
      # Use the expected distribution for the variance when available
      var_bin = if (all(is.na(.data$expected))) {
        sum(.data$bin^2 * .data$observed) / sum(.data$observed) - .data$mean_obs[1]^2
      } else {
        sum(.data$bin^2 * .data$expected) / sum(.data$expected) - .data$mean_exp[1]^2
      },
      n = mean(.data$n),
      .groups = "drop"
    ) |>
    dplyr::filter(is.finite(.data$mean_obs)) |>
    dplyr::mutate(
      se = sqrt(.data$var_bin / .data$n),
      std_resid = (.data$mean_obs - .data$mean_exp) / .data$se
    )
}
