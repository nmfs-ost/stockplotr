test_that("plot_joint_residuals works with each example model output and option", {
  est <- readRDS(test_path("fixtures", "fims_output", "fims_estimates.rds"))
  datasets <- list(
    example_data = stockplotr::example_data,
    pollock = stockplotr::pollock,
    ss3_simple = stockplotr::ss3_simple,
    vermilion_snapper = stockplotr::vermilion_snapper,
    fims = suppressMessages(convert_output(est, model = "fims", start_year = 2001))
  )
  for (name in names(datasets)) {
    # ss3_simple also has catch labeled as an index in the "catch" module
    module <- if (name == "ss3_simple") "cpue" else NULL
    for (scaled in c(FALSE, TRUE)) {
      p <- suppressMessages(plot_joint_residuals(
        datasets[[name]],
        module = module, scaled = scaled, overlay = !scaled, interactive = FALSE
      ))
      expect_no_error(ggplot2::ggplot_build(p))
    }
  }
})

test_that("residual_stat gives the RMSE and the SDNR with its critical value", {
  resid <- data.frame(series = rep(c("A", "B"), c(3, 1)), residual = c(0.1, -0.2, 0.3, 0.4))
  rmse <- residual_stat(resid)
  expect_equal(rmse$series, c("A", "B", "All indices"))
  expect_equal(rmse$stat, 100 * sqrt(c(mean(c(0.01, 0.04, 0.09)), 0.16, mean(resid$residual^2))))
  expect_equal(rmse$text[2], "RMSE = 40.0%")
  sdnr <- residual_stat(resid, scaled = TRUE)
  expect_equal(sdnr$stat[1], sqrt(0.14 / 2))
  expect_equal(sdnr$critical[1], sqrt(stats::qchisq(0.95, df = 2) / 2))
  # One residual has no SDNR
  expect_equal(sdnr$text[2], "SDNR = NA")
})

test_that("plot_joint_residuals overlays indices with the statistic, boxplots, and smoother", {
  resid <- index_residuals(stockplotr::example_data, interactive = FALSE)
  stat <- residual_stat(resid)
  p <- plot_joint_residuals(stockplotr::example_data, interactive = FALSE)
  layer <- function(p, geom) p$layers[[which(sapply(p$layers, function(l) inherits(l$geom, geom)))]]$data
  expect_setequal(levels(p$data$index), paste0(stat$series[1:2], " (", stat$value[1:2], ")"))
  expect_equal(layer(p, "GeomLabel")$text, stat$text[3])
  # Boxplots only for years with more than one residual
  expect_true(all(table(resid$year)[as.character(layer(p, "GeomBoxplot")$year)] > 1))
  smooth <- layer(p, "GeomLine")
  expect_equal(smooth$mean, as.numeric(tapply(resid$residual, resid$year, mean)[as.character(smooth$year)]))

  p <- plot_joint_residuals(stockplotr::example_data, show_stat = FALSE, interactive = FALSE)
  expect_false(any(sapply(p$layers, function(l) inherits(l$geom, "GeomLabel"))))
  expect_setequal(levels(p$data$index), stat$series[1:2])

  short <- stockplotr::example_data[stockplotr::example_data$year %in% 1995:1998, ]
  expect_message(p <- plot_joint_residuals(short, interactive = FALSE), "Too few years")
  expect_false(any(sapply(p$layers, function(l) inherits(l$geom, "GeomLine"))))
})

test_that("plot_joint_residuals scales residuals and plots indices on panels", {
  resid <- index_residuals(stockplotr::example_data, interactive = FALSE)
  resid <- resid[order(resid$series, resid$year), ]
  p <- plot_joint_residuals(stockplotr::example_data, scaled = TRUE, overlay = FALSE, interactive = FALSE)
  plotted <- p$data[order(p$data$series, p$data$year), ]
  expect_equal(plotted$residual, resid$residual / resid$se)
  expect_s3_class(p$facet, "FacetWrap")
  expect_false(any(sapply(p$layers, function(l) inherits(l$geom, "GeomBoxplot"))))
  # One statistic per panel
  labels <- p$layers[[which(sapply(p$layers, function(l) inherits(l$geom, "GeomLabel")))]]$data
  expect_equal(labels$text, residual_stat(plotted, scaled = TRUE)$text[1:2])

  # Observations without a standard error are reported, not dropped silently
  dat <- stockplotr::example_data
  dat$uncertainty[which(dat$label == "index_observed")[1]] <- NA
  expect_message(
    p <- plot_joint_residuals(dat, scaled = TRUE, interactive = FALSE),
    "1 residual without a standard error"
  )
  expect_equal(nrow(p$data), nrow(resid) - 1)
})

test_that("plot_joint_residuals saves a caption and alt text", {
  rda <- fit_rda(
    plot_joint_residuals(stockplotr::example_data, scaled = TRUE, interactive = FALSE, make_rda = TRUE),
    "joint_residuals"
  )
  expect_tags_filled(rda, "joint.residuals.")
  expect_match(rda$caption, "Standardized residuals of the fits to Triennial", fixed = TRUE)
  expect_match(rda$alt_text, "(SDNR) of all residuals is", fixed = TRUE)
})
