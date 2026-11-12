fims_converted <- function() {
  est <- readRDS(test_path("fixtures", "fims_output", "fims_estimates.rds"))
  suppressMessages(convert_output(est, model = "fims", start_year = 2001))
}

test_that("residual_control estimates limits and the runs test", {
  ctl <- residual_control(c(1, -1, 1, -1, 1, -1))
  # All moving ranges are 2, so sigma = 2 / 1.128
  expect_equal(ctl$ucl, 3 * 2 / 1.128)
  expect_equal(ctl$lcl, -ctl$ucl)
  # Alternating signs have the most runs; long runs of one sign the fewest
  expect_gt(ctl$p, 0.9)
  expect_lt(residual_control(c(-3:-1, 1:3))$p, 0.05)
  expect_true(is.na(residual_control(1)$lcl))
  expect_equal(residual_runs_test(rep(1, 5)), 2 * 0.5^5)
  # Normal approximation: 6 positive, 4 negative, 5 runs
  x <- c(1, 1, -1, 1, -1, -1, -1, 1, 1, 1)
  mu <- 2 * 6 * 4 / 10 + 1
  v <- 2 * 6 * 4 * (2 * 6 * 4 - 10) / (10^2 * 9)
  expect_equal(residual_runs_test(x), stats::pnorm((5 - mu) / sqrt(v)))
})

test_that("index residuals match each example model output", {
  datasets <- list(
    example_data = stockplotr::example_data,
    pollock = stockplotr::pollock,
    ss3_simple = stockplotr::ss3_simple,
    vermilion_snapper = stockplotr::vermilion_snapper,
    fims = fims_converted()
  )
  for (name in names(datasets)) {
    dat <- datasets[[name]]
    # ss3_simple also has catch labeled as an index in the "catch" module
    module <- if (name == "ss3_simple") "cpue" else NULL
    res <- suppressMessages(index_residuals(dat, module = module, interactive = FALSE))
    rows <- dat[dat$label %in% c("index_observed", "index_predicted"), ]
    if (!is.null(module)) rows <- rows[rows$module_name == module, ]
    obs <- rows[rows$label == "index_observed" & !is.na(rows$estimate), ]
    pred <- rows[rows$label == "index_predicted", ]
    obs <- obs[order(obs$fleet, obs$year), ]
    pred <- pred[match(paste(obs$fleet, obs$year), paste(pred$fleet, pred$year)), ]
    expect_equal(nrow(res), nrow(obs), label = name)
    expect_equal(res$residual, log(obs$estimate) - log(pred$estimate), label = name)
    # Log-scale standard errors, with CVs (BAM and FIMS) converted
    se <- ifelse(obs$uncertainty_label == "cv", sqrt(log(1 + obs$uncertainty^2)), obs$uncertainty)
    expect_equal(res$se, se, label = name)

    p <- suppressMessages(plot_runs_test(dat, module = module, interactive = FALSE))
    expect_no_error(ggplot2::ggplot_build(p))
  }
})

test_that("index residuals keep every observation", {
  two_seasons <- data.frame(
    label = rep(c("index_observed", "index_predicted"), each = 6),
    fleet = "Survey",
    year = rep(rep(2001:2003, 2), 2),
    season = rep(rep(1:2, each = 3), 2),
    estimate = c(1, 2, 3, 10, 20, 30, 1.1, 2.1, 2.9, 11, 19, 31),
    uncertainty = 0.2,
    uncertainty_label = "se"
  )
  res <- index_residuals(two_seasons, interactive = FALSE)
  expect_setequal(unique(res$series), c("Survey (season 1)", "Survey (season 2)"))
  # Exact copies are combined; values that cannot be told apart are an error
  expect_equal(nrow(index_residuals(rbind(two_seasons, two_seasons), interactive = FALSE)), 6)
  two_seasons$season <- NULL
  expect_error(index_residuals(two_seasons, interactive = FALSE), "cannot be told apart")

  # Observations without a predicted value are reported
  no_pred <- data.frame(
    label = c(rep("index_observed", 3), rep("index_predicted", 2)),
    fleet = "a", year = c(1:3, 1:2), estimate = c(1, 2, 3, 1, 2),
    uncertainty = NA, uncertainty_label = NA
  )
  expect_message(res <- index_residuals(no_pred, interactive = FALSE), "1 observation")
  expect_equal(res$year, 1:2)

  expect_error(
    index_residuals(list(stockplotr::example_data, stockplotr::example_data), interactive = FALSE),
    "2 models"
  )
  expect_error(index_residuals(stockplotr::example_data, fleet = "none", interactive = FALSE), "No index found")
})

test_that("plot_runs_test flags outliers and failed runs tests", {
  p <- plot_runs_test(stockplotr::example_data, interactive = FALSE)
  # The Triennial index in 2004 is outside the control limits
  expect_equal(p$data$year[p$data$outside], 2004)
  expect_equal(p$data$fleet[p$data$outside], "Triennial")

  p <- suppressMessages(plot_runs_test(stockplotr::pollock, interactive = FALSE))
  ctl <- p$layers[[1]]$data
  expect_true(all(c("Passed", "Failed (p < 0.05)") %in% ctl$result))
  # Long fleet names are wrapped, not cut
  expect_true(all(grepl("\n", ctl$panel)))
})

test_that("plot_runs_test saves a caption and alt text that describe the tests", {
  rda <- fit_rda(plot_runs_test(stockplotr::example_data, interactive = FALSE, make_rda = TRUE), "runs_test")
  expect_tags_filled(rda, "runs.test.")
  expect_match(rda$alt_text, "passed for 2 of 2 indices")
  expect_match(rda$alt_text, "One residual is outside the control limits: Triennial in 2004.")

  # save_all_plots() saves to a figures folder other than the working directory
  figs <- tempfile("figs")
  on.exit(unlink(figs, recursive = TRUE), add = TRUE)
  rda <- fit_rda(
    plot_runs_test(stockplotr::example_data, interactive = FALSE, make_rda = TRUE, figures_dir = figs),
    "runs_test",
    dir = figs
  )
  expect_tags_filled(rda, "runs.test.")
})
