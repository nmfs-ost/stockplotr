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

test_that("plot_index can plot all indices relative to their means", {
  p <- plot_index(stockplotr::example_data, relative = TRUE, interval = 0.95, interactive = FALSE)
  b <- ggplot2::ggplot_build(p)
  # One panel, with fleets shown by color
  expect_equal(length(unique(b$layout$layout$PANEL)), 1)
  expect_equal(p$labels$y, "Index relative to its mean")
  expect_s3_class(p$layers[[1]]$geom, "GeomHline")

  # Observed values average one for each fleet, and the predicted index and
  # intervals are scaled by the same mean
  pts <- p$layers[[3]]$data
  obs <- pts[pts$label == "index_observed", ]
  expect_equal(as.vector(tapply(obs$estimate, obs$fleet, mean)), c(1, 1))
  raw <- stockplotr::example_data[stockplotr::example_data$label %in% c("index_observed", "index_predicted"), ]
  tri <- raw[raw$fleet == "Triennial", ]
  tri_mean <- mean(tri$estimate[tri$label == "index_observed"])
  pred <- p$layers[[4]]$data
  expect_equal(
    sort(pred$estimate[pred$fleet == "Triennial"]),
    sort(tri$estimate[tri$label == "index_predicted"] / tri_mean)
  )
  # Intervals keep their width relative to each observation
  ints <- p$layers[[2]]$data
  ints0 <- plot_index(stockplotr::example_data, interval = 0.95, interactive = FALSE)$layers[[1]]$data
  ints <- ints[order(ints$fleet, ints$year), ]
  ints0 <- ints0[order(ints0$fleet, ints0$year), ]
  expect_equal(ints$estimate_lower / ints$estimate, ints0$estimate_lower / ints0$estimate)
  expect_no_error(b)

  # Survey months that are nested within fleets do not split the panel
  p <- suppressMessages(plot_index(stockplotr::pollock, relative = TRUE, interactive = FALSE))
  expect_equal(length(unique(ggplot2::ggplot_build(p)$layout$layout$PANEL)), 1)

  # Works on a log scale, with one fleet, and with FIMS output
  p <- plot_index(stockplotr::example_data, relative = TRUE, log = TRUE, interactive = FALSE)
  expect_equal(p$labels$y, "Index relative to its mean (log scale)")
  expect_no_error(ggplot2::ggplot_build(p))
  expect_no_error(ggplot2::ggplot_build(suppressMessages(plot_index(stockplotr::ss3_simple, relative = TRUE, interactive = FALSE))))
  est <- readRDS(test_path("fixtures", "fims_output", "fims_estimates.rds"))
  fims <- suppressMessages(convert_output(est, model = "fims", start_year = 2001))
  p <- suppressMessages(plot_index(fims, relative = TRUE, interactive = FALSE))
  expect_equal(mean(p$layers[[2]]$data$estimate[p$layers[[2]]$data$label == "index_observed"]), 1)
})
