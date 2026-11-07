#' Plot residual runs tests for indices of abundance
#'
#' Plot the residuals of the fit to each index over time with three-sigma
#' control limits and a runs test for non-random residuals (Carvalho et al.
#' 2017), as in JABBA and ss3diags (`SSplotRunstest()`).
#'
#' @inheritParams plot_index
#' @param dat A tibble from [convert_output()] with labels "index_observed"
#' and "index_predicted".
#' @param fleet Character vector. Fleet(s) to plot. Default: NULL (all fleets)
#' @param ncol Number. Number of columns of panels. Default: NULL
#'
#' @returns A `ggplot2` object.
#'
#' @details Residuals are \eqn{\log(o) - \log(p)} for observed \eqn{o} and
#' predicted \eqn{p}, assuming lognormal errors. The one-sided runs test
#' checks whether residual signs are random over time (normal
#' approximation, as in ss3diags). The shaded band spans the three-sigma
#' limits, with sigma from the average moving range; it is green when the
#' test passes (p >= 0.05) and red when it fails. Residuals outside the
#' limits are filled red.
#'
#' No observations are dropped silently: a fleet observed in more than one
#' area, season, subseason, month, or sex becomes separate series (e.g.,
#' "Survey (season 2)"), observations without a positive observed and
#' predicted value are reported in a message, and output from more than one
#' model or with values that cannot be told apart is an error. Years with no
#' observed index (BAM reports every year) are not observations.
#'
#' @references Carvalho, F., et al. 2017. Can diagnostic tests help identify
#' model misspecification in integrated stock assessments? Fisheries Research
#' 192: 28-40.
#'
#' @seealso [plot_index()], [convert_output()]
#' @export
#'
#' @examples
#' plot_runs_test(stockplotr::example_data, interactive = FALSE)
plot_runs_test <- function(
  dat,
  fleet = NULL,
  module = NULL,
  interactive = TRUE,
  ncol = NULL,
  make_rda = FALSE,
  figures_dir = getwd()
) {
  check_data(dat)
  resid <- index_residuals(dat, fleet = fleet, module = module, interactive = interactive)

  ctl <- resid |>
    dplyr::arrange(.data$series, .data$year) |>
    dplyr::group_by(.data$series) |>
    dplyr::summarise(
      lcl = residual_control(.data$residual)$lcl,
      ucl = residual_control(.data$residual)$ucl,
      p = residual_control(.data$residual)$p,
      xmin = min(.data$year) - 0.5,
      xmax = max(.data$year) + 0.5,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      result = factor(
        dplyr::case_when(
          is.na(.data$p) ~ "Too few residuals",
          .data$p < 0.05 ~ "Failed (p < 0.05)",
          TRUE ~ "Passed"
        ),
        levels = c("Passed", "Failed (p < 0.05)", "Too few residuals")
      ),
      panel = paste0(
        wrap_label(.data$series), "\n",
        dplyr::if_else(
          is.na(.data$p),
          "p = NA",
          paste0("p = ", format.pval(round(.data$p, 3), eps = 0.001))
        )
      )
    )
  resid <- resid |>
    dplyr::left_join(ctl[, c("series", "lcl", "ucl", "panel")], by = "series") |>
    dplyr::mutate(
      outside = !is.na(.data$lcl) & (.data$residual < .data$lcl | .data$residual > .data$ucl)
    )

  plt <- ggplot2::ggplot(resid, ggplot2::aes(x = .data$year, y = .data$residual)) +
    ggplot2::geom_rect(
      data = ctl,
      ggplot2::aes(
        xmin = .data$xmin, xmax = .data$xmax,
        ymin = .data$lcl, ymax = .data$ucl,
        fill = .data$result
      ),
      inherit.aes = FALSE,
      alpha = 0.35,
      na.rm = TRUE
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    ggplot2::geom_segment(ggplot2::aes(xend = .data$year, yend = 0), color = "gray40") +
    ggplot2::geom_point(fill = "white", shape = 21, size = 2, stroke = 0.8) +
    ggplot2::geom_point(data = resid[resid$outside, ], color = "#B2182B", size = 2) +
    ggplot2::scale_fill_manual(
      values = c(
        "Passed" = "#4DAC26",
        "Failed (p < 0.05)" = "#D7191C",
        "Too few residuals" = "gray80"
      ),
      drop = TRUE
    ) +
    ggplot2::labs(x = "Year", y = "Residual (log observed / predicted)", fill = "Runs test") +
    theme_noaa() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::facet_wrap(ggplot2::vars(.data$panel), ncol = ncol, scales = "free")

  if (make_rda) {
    fit_export_rda(
      plt,
      topic_label = "runs_test",
      tags = runs_test_tags(resid, ctl),
      dat = dat,
      figures_dir = figures_dir
    )
  }
  plt
}

#------------------------------------------------------------------------------
# Index residuals
#------------------------------------------------------------------------------

# Columns that distinguish observations of a fleet in the same year
index_series_cols <- c("area", "season", "subseason", "month", "sex")

# Observed and predicted index with log residuals, one row per series and
# year. A series is a fleet, or a fleet in one area, season, subseason,
# month, or sex when it has observations in more than one of these.
index_residuals <- function(dat, fleet = NULL, module = NULL, interactive = TRUE) {
  idx <- filter_data(
    dat,
    label_name = "^index_(observed|predicted)$",
    module = module,
    geom = "point",
    interactive = interactive
  )
  models <- unique(stats::na.omit(as.character(idx$model)))
  if (length(models) > 1) {
    cli::cli_abort(c(
      "{.arg dat} has output from {length(models)} models.",
      "i" = "Diagnostic plots show one model at a time; supply the output of one model."
    ))
  }
  if (!is.null(fleet)) {
    idx <- idx[idx$fleet %in% fleet, , drop = FALSE]
    if (nrow(idx) == 0) {
      cli::cli_abort("No index found for fleet {.val {fleet}}.")
    }
  }
  ids <- intersect(index_series_cols, names(idx))
  type <- tolower(as.character(idx$uncertainty_label))
  long <- tibble::tibble(
    fleet = as.character(idx$fleet),
    year = as.numeric(idx$year),
    kind = sub("^index_", "", idx$label),
    value = as.numeric(idx$estimate),
    se = dplyr::case_when(
      type %in% c("se", "sd", "log_sd", "logsd", "stddev") ~ as.numeric(idx$uncertainty),
      type == "cv" ~ sqrt(log(1 + as.numeric(idx$uncertainty)^2)),
      TRUE ~ NA_real_
    )
  )
  for (col in ids) long[[col]] <- as.character(idx[[col]])
  long$fleet[is.na(long$fleet)] <- "All"
  # Rows that are exact copies are combined; other duplicates are an error
  long <- dplyr::distinct(long)
  keys <- c("fleet", "year", "kind", ids)
  dups <- long |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup() |>
    dplyr::distinct(.data$fleet, .data$year)
  if (nrow(dups) > 0) {
    examples <- utils::head(paste(dups$fleet, dups$year), 3)
    cli::cli_abort(c(
      "{.arg dat} has more than one index value for {nrow(dups)} fleet{?s} and year{?s} that cannot be told apart (e.g., {examples}).",
      "i" = "Filter {.arg dat} to one value per fleet and year, or add a column (e.g., {.field season}) that distinguishes them."
    ))
  }
  wide <- tidyr::pivot_wider(
    long,
    id_cols = dplyr::all_of(c("fleet", "year", ids)),
    names_from = "kind",
    values_from = c("value", "se")
  )
  if (!"value_predicted" %in% names(wide)) wide$value_predicted <- NA_real_
  if (!"se_observed" %in% names(wide)) wide$se_observed <- NA_real_
  out <- tibble::tibble(
    fleet = wide$fleet,
    year = wide$year,
    observed = wide$value_observed,
    predicted = wide$value_predicted,
    se = wide$se_observed
  )
  # Name series after the columns that vary within a fleet
  extra <- rep("", nrow(out))
  for (col in ids) {
    vals <- wide[[col]]
    varies <- tapply(vals, out$fleet, function(v) length(unique(stats::na.omit(v))) > 1)
    use <- out$fleet %in% names(varies)[varies %in% TRUE] & !is.na(vals)
    extra[use] <- paste0(extra[use], ifelse(extra[use] == "", "", ", "), col, " ", vals[use])
  }
  out$series <- ifelse(extra == "", out$fleet, paste0(out$fleet, " (", extra, ")"))
  out <- out[!is.na(out$observed), , drop = FALSE]
  out$residual <- suppressWarnings(log(out$observed) - log(out$predicted))
  out$residual[!is.finite(out$residual)] <- NA_real_
  no_resid <- is.na(out$residual)
  if (any(no_resid)) {
    cli::cli_alert_info(
      "{sum(no_resid)} observation{?s} without a positive observed and predicted index {?has/have} no residual and {?is/are} not shown."
    )
  }
  out <- out[!no_resid, , drop = FALSE]
  if (nrow(out) == 0) {
    cli::cli_abort("No index residuals found in {.arg dat}.")
  }
  dplyr::arrange(out, .data$series, .data$year)
}

# Wrap long labels for panel titles at spaces or underscores (fleet names
# such as "Survey_1_shelikof_acoustic" often have no spaces), keeping every
# character of the label
wrap_label <- function(x, width = 28) {
  vapply(x, function(lab) {
    if (nchar(lab) <= width) {
      return(lab)
    }
    # Split after each space or underscore, keeping the separator
    pieces <- regmatches(lab, gregexpr("[^ _]+[ _]?", lab))[[1]]
    lines <- character(0)
    current <- ""
    for (piece in pieces) {
      if (nchar(current) > 0 && nchar(current) + nchar(piece) > width) {
        lines <- c(lines, trimws(current, "right"))
        current <- piece
      } else {
        current <- paste0(current, piece)
      }
    }
    paste(c(lines, trimws(current, "right")), collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

#------------------------------------------------------------------------------
# Runs test and control limits
#------------------------------------------------------------------------------

# Three-sigma control limits and runs test for a series of residuals
# (Carvalho et al. 2017; Winker et al. 2018). The standard deviation is
# estimated from the average moving range (Montgomery 2009), after removing
# moving ranges greater than the upper limit for moving ranges.
residual_control <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2) {
    return(list(lcl = NA_real_, ucl = NA_real_, p = residual_runs_test(x)))
  }
  mr <- abs(diff(x))
  amr <- mean(mr)
  mr <- mr[mr < 3.267 * amr]
  stdev <- if (length(mr) > 0) mean(mr) / 1.128 else NA_real_
  list(
    lcl = -3 * stdev,
    ucl = 3 * stdev,
    p = residual_runs_test(x)
  )
}

# One-sided Wald-Wolfowitz runs test on the signs of x (in order). Returns
# the p-value for fewer runs than expected (positive autocorrelation), using
# the normal approximation.
residual_runs_test <- function(x) {
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

#------------------------------------------------------------------------------
# Caption and alternative text tags
#------------------------------------------------------------------------------

# `resid` has one row per residual and `ctl` one row per series
runs_test_tags <- function(resid, ctl) {
  passed <- ctl$series[!is.na(ctl$p) & ctl$p >= 0.05]
  failed <- ctl$series[!is.na(ctl$p) & ctl$p < 0.05]
  outside <- resid[resid$outside, , drop = FALSE]
  outside_text <- paste0(outside$series, " in ", outside$year)
  if (length(outside_text) > 5) {
    outside_text <- c(outside_text[1:5], paste(length(outside_text) - 5, "more"))
  }
  tags <- list(
    panel.desc = fit_panel_text(ctl$series, what = "index"),
    start.year = min(resid$year),
    end.year = max(resid$year),
    summary = paste0(
      "The runs test passed for ", length(passed), " of ", nrow(ctl), " indices",
      if (length(failed) > 0) paste0(" and failed for ", fit_list_text(failed)), ". ",
      if (nrow(outside) == 0) {
        "No residuals are outside the control limits."
      } else {
        paste0(
          if (nrow(outside) == 1) "One residual is" else paste(nrow(outside), "residuals are"),
          " outside the control limits: ", fit_list_text(outside_text), "."
        )
      }
    )
  )
  stats::setNames(tags, paste0("runs.test.", names(tags)))
}
