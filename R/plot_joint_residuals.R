#' Plot joint residuals of the fits to indices of abundance
#'
#' Plot the residuals of the fits to all indices over time with a loess
#' smoother and a fit statistic, as in `JABBA` (`jbplot_residuals()` and
#' `jbplot_stdresiduals()`) and `ss3diags` (`SSplotJABBAres()`). A trend in
#' the smoother shows a pattern in the residuals; a large statistic shows a
#' poor fit.
#'
#' @inheritParams plot_runs_test
#' @param scaled Logical. TRUE/FALSE; plot standardized residuals (log
#' residuals divided by their standard error) instead of log residuals.
#'
#' Default: FALSE
#' @param overlay Logical. TRUE/FALSE; plot all indices on one panel, with a
#' boxplot of the residuals in each year. FALSE plots each index on its own
#' panel.
#'
#' Default: TRUE
#' @param show_stat Logical. TRUE/FALSE; show the fit statistic in the top
#' right corner of each panel: the root mean square error (RMSE) of log
#' residuals, or the standard deviation of normalized residuals (SDNR) of
#' scaled residuals. When `overlay = TRUE`, the statistic of each index is
#' also shown in the legend.
#'
#' Default: TRUE
#' @param ncol Number. Number of columns of panels when `overlay = FALSE`.
#'
#' Default: NULL
#'
#' @returns A `ggplot2` object.
#'
#' @details Log residuals are \eqn{r = \log(o) - \log(p)} for observed
#' \eqn{o} and predicted \eqn{p}, as in [plot_runs_test()]; scaled
#' residuals are \eqn{z = r / s} for the log-scale standard error \eqn{s} of
#' the observation. The RMSE is \eqn{100 \sqrt{\overline{r^2}}} percent. The
#' SDNR is \eqn{\sqrt{\sum z^2 / (n - 1)}}, shown with its critical value
#' \eqn{\sqrt{\chi^2_{0.95, n - 1} / (n - 1)}}; an SDNR above the critical
#' value suggests the standard errors are too small (Francis 2011).
#'
#' The smoother is a loess regression (span 0.75) of the mean residual of
#' each year on year, for all indices together or for each index on its own
#' panel. It is not drawn for fewer than five years of residuals, or when
#' the years are too sparse for a stable fit; a message names the indices
#' without one.
#'
#' Observations are treated as in [plot_runs_test()]: a fleet observed in
#' more than one area, season, subseason, month, or sex becomes separate
#' series, and observations without a positive observed and predicted value
#' are reported in a message. Scaled residuals also need a standard error;
#' observations without one are reported in a message.
#'
#' @references Francis, R.I.C.C. 2011. Data weighting in statistical
#' fisheries stock assessment models. Canadian Journal of Fisheries and
#' Aquatic Sciences 68: 1124-1138.
#'
#' @seealso [plot_runs_test()], [plot_index()], [convert_output()]
#' @export
#'
#' @examples
#' plot_joint_residuals(stockplotr::example_data, interactive = FALSE)
#' plot_joint_residuals(
#'   stockplotr::example_data,
#'   scaled = TRUE,
#'   overlay = FALSE,
#'   interactive = FALSE
#' )
plot_joint_residuals <- function(
  dat,
  fleet = NULL,
  module = NULL,
  scaled = FALSE,
  overlay = TRUE,
  show_stat = TRUE,
  ncol = NULL,
  interactive = TRUE,
  make_rda = FALSE,
  figures_dir = getwd()
) {
  check_data(dat)
  resid <- index_residuals(dat, fleet = fleet, module = module, interactive = interactive)
  if (scaled) {
    no_se <- is.na(resid$se) | resid$se <= 0
    if (any(no_se)) {
      cli::cli_alert_info(
        "{sum(no_se)} residual{?s} without a standard error {?is/are} not shown when {.arg scaled} is TRUE."
      )
      resid <- resid[!no_se, , drop = FALSE]
      if (nrow(resid) == 0) {
        cli::cli_abort("No residuals have a standard error to scale by.")
      }
    }
    resid$residual <- resid$residual / resid$se
  }

  stat <- residual_stat(resid, scaled = scaled)
  by_series <- stat[stat$series != "All indices", , drop = FALSE]
  resid$index <- factor(
    resid$series,
    levels = by_series$series,
    # The legend shows each index's statistic; panels show it in the corner
    labels = if (show_stat && overlay) {
      paste0(by_series$series, " (", by_series$value, ")")
    } else {
      by_series$series
    }
  )
  smooth <- residual_smooth(resid, by_series = !overlay)

  plt <- ggplot2::ggplot(resid, ggplot2::aes(x = .data$year, y = .data$residual)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40")
  if (overlay) {
    n_series <- nrow(by_series)
    dodge <- ggplot2::position_dodge(width = if (n_series > 1) 0.6 else 0)
    # Boxplots only where a year has more than one residual
    boxes <- resid[resid$year %in% resid$year[duplicated(resid$year)], , drop = FALSE]
    plt <- plt +
      ggplot2::geom_boxplot(
        data = boxes,
        ggplot2::aes(group = .data$year),
        fill = "gray85",
        color = "gray60",
        outliers = FALSE,
        width = 0.8
      )
  } else {
    dodge <- "identity"
  }
  plt <- plt +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = 0, ymax = .data$residual, color = .data$index),
      position = dodge
    ) +
    ggplot2::geom_point(
      ggplot2::aes(fill = .data$index),
      shape = 21,
      size = 2,
      position = dodge
    )
  if (!is.null(smooth)) {
    plt <- plt +
      ggplot2::geom_line(
        data = smooth,
        ggplot2::aes(x = .data$year, y = .data$smooth),
        linewidth = 1
      )
  }
  if (show_stat) {
    labels <- if (overlay) {
      stat[stat$series == "All indices", , drop = FALSE]
    } else {
      by_series
    }
    labels$index <- if (overlay) NA else resid$index[match(labels$series, resid$series)]
    plt <- plt +
      ggplot2::geom_label(
        data = labels,
        ggplot2::aes(x = Inf, y = Inf, label = .data$text),
        hjust = 1.05, vjust = 1.3,
        fill = "white", alpha = 0.8, label.size = 0,
        inherit.aes = FALSE
      )
  }
  legend_ncol <- if (max(nchar(levels(resid$index))) > 30) 1 else 2
  plt <- plt +
    ggplot2::labs(
      x = "Year",
      y = if (scaled) "Standardized residual" else "Residual (log observed / predicted)",
      color = "Index",
      fill = "Index"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(ncol = legend_ncol),
      fill = ggplot2::guide_legend(ncol = legend_ncol)
    ) +
    theme_noaa() +
    ggplot2::theme(legend.position = "bottom")
  if (!overlay) {
    plt <- plt +
      ggplot2::facet_wrap(ggplot2::vars(.data$index), ncol = ncol) +
      ggplot2::theme(legend.position = "none")
  }

  if (make_rda) {
    fit_export_rda(
      plt,
      topic_label = "joint_residuals",
      tags = joint_residuals_tags(resid, stat, smooth, scaled = scaled, overlay = overlay),
      kqs = joint_residuals_kqs(resid, stat, scaled = scaled),
      dat = dat,
      figures_dir = figures_dir
    )
  }
  plt
}

# Fit statistic of the residuals of each series and of all residuals
# together ("All indices", the last row): the RMSE (percent) of log
# residuals, as in ss3diags::SSrmse(), or the SDNR of scaled residuals, with
# its critical value, as in JABBA::jbplot_stdresiduals()
residual_stat <- function(resid, scaled = FALSE) {
  stat_fun <- function(r) {
    n <- length(r)
    if (scaled) {
      sdnr <- if (n > 1) sqrt(sum(r^2) / (n - 1)) else NA_real_
      critical <- if (n > 1) sqrt(stats::qchisq(0.95, df = n - 1) / (n - 1)) else NA_real_
      tibble::tibble(stat = sdnr, critical = critical, n = n)
    } else {
      tibble::tibble(stat = 100 * sqrt(mean(r^2)), critical = NA_real_, n = n)
    }
  }
  series <- unique(resid$series)
  out <- dplyr::bind_rows(
    lapply(series, function(s) stat_fun(resid$residual[resid$series == s])),
    stat_fun(resid$residual)
  )
  out <- tibble::tibble(series = c(series, "All indices"), out)
  if (scaled) {
    out$value <- ifelse(
      is.na(out$stat),
      "NA",
      paste0(format(round(out$stat, 2), nsmall = 2, trim = TRUE))
    )
    out$text <- paste0(
      "SDNR = ", out$value,
      ifelse(is.na(out$critical), "", paste0(" (critical ", format(round(out$critical, 2), nsmall = 2, trim = TRUE), ")"))
    )
  } else {
    out$value <- paste0(format(round(out$stat, 1), nsmall = 1, trim = TRUE), "%")
    out$text <- paste0("RMSE = ", out$value)
  }
  out
}

# Loess smoother of the mean residual of each year, for all residuals or for
# each series; NULL (or no rows for a series) for fewer than five years
residual_smooth <- function(resid, by_series = FALSE) {
  groups <- if (by_series) c("index", "series") else character(0)
  yearly <- resid |>
    dplyr::summarise(mean = mean(.data$residual), .by = dplyr::all_of(c(groups, "year"))) |>
    dplyr::arrange(.data$year)
  # NULL for fewer than five years, or when loess cannot fit them stably
  # (it warns when the years are too sparse for the span)
  fit_one <- function(d) {
    if (nrow(d) < 5) {
      return(NULL)
    }
    tryCatch(
      {
        fit <- stats::loess(mean ~ year, data = d, span = 0.75)
        d$smooth <- stats::predict(fit, newdata = d)
        d
      },
      warning = function(w) NULL
    )
  }
  parts <- if (by_series) lapply(split(yearly, yearly$series), fit_one) else list(fit_one(yearly))
  short <- if (by_series) setdiff(unique(yearly$series), names(Filter(Negate(is.null), parts))) else NULL
  out <- dplyr::bind_rows(parts)
  if (nrow(out) == 0) {
    cli::cli_alert_info("Too few years of residuals for a loess smoother; it is not drawn.")
    return(NULL)
  }
  if (length(short) > 0) {
    cli::cli_alert_info("Too few years of residuals for a loess smoother for {short}; none is drawn for {?it/them}.")
  }
  out
}

# Tags for the caption and alternative text (joint.residuals.*)
joint_residuals_tags <- function(resid, stat, smooth, scaled, overlay) {
  series <- stat[stat$series != "All indices", , drop = FALSE]
  all <- stat[stat$series == "All indices", , drop = FALSE]
  worst <- series[which.max(series$stat), , drop = FALSE]
  stat_name <- if (scaled) {
    "standard deviation of normalized residuals (SDNR)"
  } else {
    "root mean square error (RMSE)"
  }
  trend <- if (is.null(smooth)) {
    "There are too few years to draw a smoother."
  } else {
    range <- range(smooth$smooth)
    paste0(
      "The smoother through the mean residual of each year ranges from ",
      round(range[1], 2), " to ", round(range[2], 2), "."
    )
  }
  tags <- list(
    type = if (scaled) "Standardized residuals" else "Residuals",
    index.list = fit_list_text(series$series),
    layout = paste0(
      if (overlay) {
        "together over time, with a boxplot of the residuals in each year (gray) and a loess smoother through the mean residual of each year (black line)"
      } else {
        "over time, each index on its own panel with a loess smoother (black line)"
      },
      ". Residuals are the log of the observed index divided by the predicted index",
      if (scaled) ", divided by the standard error of the observation" else "",
      ", and the ", stat_name, " of each index is ",
      if (overlay) "in the legend" else "in the corner of its panel",
      if (scaled) "; an SDNR above its critical value suggests the standard errors are too small" else ""
    ),
    start.year = min(resid$year),
    end.year = max(resid$year),
    summary = paste0(
      "The ", stat_name, " of all residuals is ", all$value,
      if (nrow(series) > 1) paste0(", and is largest for ", worst$series, " (", worst$value, ")"),
      ". ", trend
    )
  )
  stats::setNames(tags, paste0("joint.residuals.", names(tags)))
}

# Key quantities recorded in key_quantities.csv (joint.residuals.*)
joint_residuals_kqs <- function(resid, stat, scaled) {
  by_index <- stat[stat$series != "All indices", , drop = FALSE]
  value <- function(rows) {
    if (scaled) sub("^SDNR = ", "", rows$text) else rows$value
  }
  kqs <- list(
    start.year = min(resid$year),
    end.year = max(resid$year),
    n.indices = nrow(by_index),
    residual.type = if (scaled) "standardized" else "log",
    stat.all = stat$text[stat$series == "All indices"],
    stat.by.index = paste0(by_index$series, ": ", value(by_index), collapse = "; ")
  )
  stats::setNames(kqs, paste0("joint.residuals.", names(kqs)))
}
