# FIMS::get_estimates() output for the FIMS data_big example
# (see fixtures/fims_output/README.md)
fims_estimates <- function() {
  readRDS(test_path("fixtures", "fims_output", "fims_estimates.rds"))
}
convert_fims <- function(dat = fims_estimates(), ...) {
  suppressMessages(convert_output(dat, models = "fims", ...))
}

test_that("FIMS data points are split into observed and predicted rows", {
  est <- fims_estimates()
  out <- convert_fims(est, start_year = 2001)
  counts <- table(out$label[grepl("_(observed|predicted)$", out$label)])
  expect_equal(
    as.vector(counts[c("index_observed", "index_predicted", "landings_observed", "landings_predicted")]),
    c(30, 30, 30, 30)
  )
  expect_equal(
    as.vector(counts[c("age_composition_observed", "length_composition_observed")]),
    c(sum(est$label == "agecomp_expected" & !is.na(est$observed)), sum(est$label == "lengthcomp_expected" & !is.na(est$observed)))
  )

  rows <- est[est$label == "index_expected" & !is.na(est$observed), ]
  obs <- out[out$label == "index_observed", ]
  pred <- out[out$label == "index_predicted", ]
  expect_equal(obs$estimate, rows$observed)
  expect_equal(pred$estimate, rows$estimated)
  # Observation error and negative log-likelihood of each observation
  # Lognormal observation error is reported as a CV
  expect_equal(obs$uncertainty, sqrt(exp(exp(rows$log_sd)^2) - 1))
  expect_equal(unique(obs$uncertainty_label), "cv")
  expect_equal(obs$likelihood, -rows$likelihood)
  expect_equal(unique(obs$fleet), "survey1")
  expect_equal(obs$year, rows$year_i + 2000)

  # Age compositions keep their bins and counts
  comp <- out[out$label == "age_composition_observed", ]
  expect_true(all(!is.na(comp$age)))
  expect_equal(sum(comp$estimate), sum(est$observed[est$label == "agecomp_expected"], na.rm = TRUE))
  expect_true(all(!is.na(out$length_bins[out$label == "length_composition_observed"])))
})

test_that("FIMS expected values for fleets without data keep their FIMS label", {
  out <- convert_fims()
  # fleet1 is the fishery, which has no index
  expect_false(any(out$label == "index_predicted" & out$fleet == "fleet1"))
  expect_true(any(out$label == "index_expected" & out$fleet == "fleet1"))
})

test_that("FIMS observations coded as missing are not observations", {
  est <- fims_estimates()
  idx <- which(est$label == "index_expected" & !is.na(est$observed))
  est$observed[idx[1:2]] <- -999
  out <- convert_fims(est)
  expect_equal(sum(out$label == "index_observed"), length(idx) - 2)
  expect_equal(sum(out$label == "index_predicted"), length(idx))
})

test_that("FIMS lognormal intervals are on the original scale", {
  est <- fims_estimates()
  est <- est[est$label == "landings_expected" | est$label == "catch_expected", ]
  row <- which(!is.na(est$observed))[1]
  est$observed[row] <- 100
  est$log_sd[row] <- log(0.2)
  out <- convert_fims(est)
  obs <- out[out$label == "landings_observed", ][1, ]
  expect_equal(obs$uncertainty_label, "cv")
  # The CV gives back the lognormal sigma, and so the lognormal interval
  sigma <- sqrt(log(1 + obs$uncertainty^2))
  expect_equal(sigma, 0.2)
  expect_equal(100 * exp(c(-1.96, 1.96) * sigma), c(67.57, 147.99), tolerance = 1e-4)
})

test_that("FIMS estimates that could not be calculated are NA", {
  est <- fims_estimates()
  ssb <- which(est$label == "spawning_biomass")
  est$estimated[ssb[1]] <- -999
  est$uncertainty[ssb[1]] <- -999
  out <- convert_fims(est)
  out_ssb <- out[out$label == "spawning_biomass", ]
  # The row is kept, with a missing estimate
  expect_equal(nrow(out_ssb), length(ssb))
  expect_true(is.na(out_ssb$estimate[1]) && is.na(out_ssb$uncertainty[1]))
  expect_false(any(out$estimate %in% -999))
})

test_that("FIMS estimates are relabeled and get years and eras", {
  est <- fims_estimates()
  out <- convert_fims(est, start_year = 2001)
  ssb <- out[out$label == "spawning_biomass", ]
  expect_equal(ssb$estimate, est$estimated[est$label == "spawning_biomass"])
  expect_equal(ssb$uncertainty, est$uncertainty[est$label == "spawning_biomass"])
  expect_equal(range(ssb$year), c(2001, 2031))
  # Spawning biomass at the start of the year after the model ends
  expect_equal(ssb$era[ssb$year == 2031], "fore")
  expect_true(all(ssb$era[ssb$year <= 2030] == "time"))

  expect_equal(sum(out$label == "recruitment"), sum(est$label == "expected_recruitment"))
  expect_equal(sum(out$label == "log_recruitment_deviations"), sum(est$label == "log_devs"))
  expect_false(any(c("expected_recruitment", "log_devs") %in% out$label))

  # Without start_year, years are the FIMS year indices
  expect_message(convert_output(est, model = "fims"), "start_year")
  expect_equal(range(convert_fims(est)$year[convert_fims(est)$label == "spawning_biomass"]), c(1, 31))
})

test_that("FIMS output is read from an rds file", {
  path <- tempfile(fileext = ".rds")
  saveRDS(fims_estimates(), path)
  on.exit(unlink(path), add = TRUE)
  expect_equal(nrow(convert_fims(path)), nrow(convert_fims()))
})

test_that("a FIMSFit object needs FIMS", {
  skip_if(requireNamespace("FIMS", quietly = TRUE))
  methods::setClass("FIMSFit", representation(x = "numeric"), where = environment())
  expect_error(
    suppressMessages(convert_output(methods::new("FIMSFit", x = 1))),
    "FIMS"
  )
})

test_that("plots work with converted FIMS output", {
  out <- convert_fims(start_year = 2001)
  p <- suppressMessages(plot_index(out, interactive = FALSE))
  b <- ggplot2::ggplot_build(p)
  expect_equal(nrow(b$data[[1]]), 30)
  expect_equal(range(b$data[[1]]$x), c(2001, 2030))
  for (f in list(plot_landings, plot_spawning_biomass, plot_biomass, plot_recruitment, plot_recruitment_deviations)) {
    p <- suppressMessages(suppressWarnings(f(out, interactive = FALSE)))
    expect_s3_class(p, "ggplot")
    expect_gt(sum(sapply(ggplot2::ggplot_build(p)$data, nrow)), 0)
  }
})

test_that("plot_index plots only observed and predicted indices", {
  # pollock (Rceattle) has other labels that contain "index", such as
  # index_log_q, and ss3_simple has a single index fleet
  for (dat in list(stockplotr::pollock, stockplotr::ss3_simple, stockplotr::example_data)) {
    p <- suppressMessages(plot_index(dat, interactive = FALSE))
    expect_no_error(ggplot2::ggplot_build(p))
  }
})
