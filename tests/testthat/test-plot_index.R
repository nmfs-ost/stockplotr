test_that("plot_index generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_index(
      stockplotr::example_data,
      interactive = FALSE
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    plot_index(
      stockplotr::example_data,
      interactive = FALSE
    ),
    "gg"
  )
})

# test_that("rda file made when indicated", {
#   # export rda
#   plot_index(
#     stockplotr::example_data,
#     make_rda = TRUE,
#     unit_label = "metric tons",
#     interactive = FALSE,
#     figures_dir = getwd()
#   )

#   # expect that both figures dir and the index_figure.rda file exist
#   check_and_remove_tmp_files("index_figure.rda")
# })

test_that("plot_index draws lognormal intervals around the observed index", {
  p <- plot_index(stockplotr::example_data, interval = 0.95, interactive = FALSE)
  # The intervals are the first layer, beneath the points and lines
  expect_s3_class(p$layers[[1]]$geom, "GeomLinerange")
  ints <- p$layers[[1]]$data
  obs <- stockplotr::example_data[
    stockplotr::example_data$label %in% "index_observed",
  ]
  obs <- obs[order(obs$fleet, obs$year), ]
  ints <- ints[order(ints$fleet, ints$year), ]
  expect_equal(nrow(ints), nrow(obs))
  expect_equal(ints$estimate_lower, obs$estimate * exp(-stats::qnorm(0.975) * obs$uncertainty))
  expect_equal(ints$estimate_upper, obs$estimate * exp(stats::qnorm(0.975) * obs$uncertainty))
  expect_no_error(ggplot2::ggplot_build(p))

  # 95% intervals by default, and none with interval = NULL
  p_default <- plot_index(stockplotr::example_data, interactive = FALSE)
  expect_equal(p_default$layers[[1]]$data, p$layers[[1]]$data)
  p <- plot_index(stockplotr::example_data, interval = NULL, interactive = FALSE)
  expect_false(any(sapply(p$layers, function(l) inherits(l$geom, "GeomLinerange"))))

  expect_error(plot_index(stockplotr::example_data, interval = 95, interactive = FALSE), "between 0 and 1")
})

test_that("plot_index intervals use log-scale SDs and CVs", {
  dat <- data.frame(
    label = "index_observed", year = 1:3, fleet = "a",
    estimate = c(10, 20, 30), uncertainty = c(0.2, 0.2, NA),
    uncertainty_label = c("cv", "log_sd", "cv")
  )
  out <- index_observed_interval(dat, interval = 0.9)
  z <- stats::qnorm(0.95)
  expect_equal(out$estimate_lower[1], 10 * exp(-z * sqrt(log(1 + 0.2^2))))
  expect_equal(out$estimate_upper[2], 20 * exp(z * 0.2))
  # No uncertainty, no interval
  expect_true(is.na(out$estimate_lower[3]))

  # Rceattle (log_sd) and BAM (cv) output
  for (dat in list(stockplotr::pollock, stockplotr::vermilion_snapper)) {
    p <- suppressMessages(plot_index(dat, interval = 0.95, interactive = FALSE))
    expect_gt(nrow(p$layers[[1]]$data), 0)
    expect_no_error(ggplot2::ggplot_build(p))
  }
})

test_that("plot_index can use a log scale", {
  p <- plot_index(stockplotr::example_data, interval = 0.95, log = TRUE, interactive = FALSE)
  expect_equal(p$scales$get_scales("y")$trans$name, "log-10")
  expect_equal(p$labels$y, "Estimated Index (log scale)")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_index intervals work for FIMS output", {
  est <- readRDS(test_path("fixtures", "fims_output", "fims_estimates.rds"))
  fims <- suppressMessages(convert_output(est, model = "fims", start_year = 2001))
  p <- suppressMessages(plot_index(fims, interval = 0.95, interactive = FALSE))
  ints <- p$layers[[1]]$data
  rows <- est[est$label == "index_expected" & !is.na(est$observed), ]
  expect_equal(ints$estimate_lower, rows$observed * exp(-stats::qnorm(0.975) * exp(rows$log_sd)))
})
