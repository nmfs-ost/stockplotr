r4ss_output <- readRDS(
  test_path("fixtures", "r4ss_output", "simple_small", "simple_r4ss.rds")
)

has_layer <- function(p, geom_class) {
  any(vapply(p$layers, function(l) inherits(l$geom, geom_class), logical(1)))
}

# prepare_comp_data ----------------------------------------------------------
test_that("prepare_comp_data returns standard columns from tidy data", {
  comps <- prepare_comp_data(stockplotr::bigskate_lencomp)
  expect_s3_class(comps, "stockplotr_comp")
  expect_named(
    comps,
    c("comp_type", "fleet", "year", "sex", "bin", "observed", "expected", "n", "pearson")
  )
  # proportions sum to one across sexes within year, fleet, and partition
  sums <- comps |>
    dplyr::summarise(total = sum(observed), .by = c(fleet, year))
  expect_true(all(abs(sums$total - 1) < 1e-6))
  # Stock Synthesis Pearson residuals are kept
  expect_equal(sort(comps$pearson), sort(stockplotr::bigskate_lencomp$pearson))
})

test_that("prepare_comp_data calculates multinomial Pearson residuals", {
  comps <- prepare_comp_data(
    dplyr::select(stockplotr::bigskate_lencomp, -pearson)
  )
  i <- 10
  expect_equal(
    comps$pearson[i],
    (comps$observed[i] - comps$expected[i]) /
      sqrt(comps$expected[i] * (1 - comps$expected[i]) / comps$n[i])
  )
})

test_that("prepare_comp_data filters by fleet and comp_type", {
  comps <- prepare_comp_data(stockplotr::bigskate_lencomp, fleet = "WCGBTS")
  expect_equal(unique(comps$fleet), "WCGBTS")
  expect_error(
    prepare_comp_data(stockplotr::bigskate_lencomp, fleet = "not a fleet"),
    "No composition data"
  )
  expect_error(
    prepare_comp_data(stockplotr::bigskate_lencomp, comp_type = "age"),
    "No composition data"
  )
})

test_that("partitions are added to fleet names as in r4ss", {
  comps <- prepare_comp_data(stockplotr::bigskate_lencomp)
  expect_setequal(
    unique(comps$fleet),
    c("Fishery_current (discard)", "Fishery_current (retained)", "WCGBTS", "Triennial")
  )
  # a fleet can be selected with or without its partition
  both <- prepare_comp_data(stockplotr::bigskate_lencomp, fleet = "Fishery_current")
  expect_equal(length(unique(both$fleet)), 2)
  retained <- prepare_comp_data(stockplotr::bigskate_lencomp, fleet = "Fishery_current (retained)")
  expect_equal(unique(retained$fleet), "Fishery_current (retained)")
})

test_that("single-sex compositions are normalized separately", {
  one_sex <- data.frame(
    year = 2000,
    sex = rep(c("female", "male"), each = 3),
    bin = rep(1:3, 2),
    observed = c(0.2, 0.3, 0.5, 0.1, 0.1, 0.8)
  )
  comps <- prepare_comp_data(one_sex)
  expect_equal(comps$observed, one_sex$observed)
  # joint (two-sex) compositions are normalized together
  joint <- one_sex
  joint$observed <- joint$observed / 2
  expect_equal(prepare_comp_data(joint)$observed, joint$observed)
})

test_that("prepare_comp_data normalizes counts", {
  counts <- data.frame(year = 2000, bin = 1:4, observed = c(10, 20, 30, 40))
  comps <- prepare_comp_data(counts)
  expect_equal(comps$observed, c(0.1, 0.2, 0.3, 0.4))
  expect_equal(unique(comps$sex), "combined")
  expect_true(all(is.na(comps$expected)))
})

test_that("prepare_comp_data reshapes stockplotr long format", {
  long <- data.frame(
    label = rep(c("age_composition_proportion", "age_composition_expected"), each = 3),
    year = 2020,
    fleet = "fleet1",
    age = rep(1:3, 2),
    estimate = c(0.2, 0.5, 0.3, 0.25, 0.45, 0.3)
  )
  comps <- prepare_comp_data(long)
  expect_equal(nrow(comps), 3)
  expect_equal(comps$observed, c(0.2, 0.5, 0.3))
  expect_equal(comps$expected, c(0.25, 0.45, 0.3))
  expect_equal(unique(comps$comp_type), "age")
})

test_that("prepare_comp_data reads FIMS-style observed/expected columns", {
  fims <- data.frame(
    module_name = "Fleet",
    label = rep(c("agecomp_expected", "lengthcomp_expected"), each = 4),
    fleet = "survey1",
    year = 1,
    age = c(1:4, rep(NA, 4)),
    length = c(rep(NA, 4), seq(10, 40, by = 10)),
    observed = c(1, 2, 3, 4, 4, 3, 2, 1),
    expected = c(1.5, 2, 3, 3.5, 4, 3, 2, 1)
  )
  comps <- prepare_comp_data(fims)
  expect_setequal(comps$comp_type, c("age", "length"))
  len <- prepare_comp_data(fims, comp_type = "length")
  expect_equal(len$bin, c(10, 20, 30, 40))
  expect_equal(sum(len$observed), 1)
})

test_that("prepare_comp_data reads r4ss output", {
  comps <- prepare_comp_data(r4ss_output)
  expect_setequal(comps$comp_type, c("age", "length"))
  expect_true("FISHERY" %in% comps$fleet)
  expect_setequal(unique(comps$sex), c("female", "male"))
  # r4ss Pearson residuals are kept rather than recalculated
  len <- prepare_comp_data(r4ss_output$lendbase)
  expect_equal(
    sort(len$pearson),
    sort(as.numeric(r4ss_output$lendbase$Pearson))
  )
})

# plot_comp ------------------------------------------------------------------
test_that("plot_comp makes observation-only and fit plots", {
  p_obs <- plot_comp(stockplotr::bigskate_lencomp, fleet = "WCGBTS", show_expected = FALSE)
  expect_s3_class(p_obs, "ggplot")
  expect_true(has_layer(p_obs, "GeomCol"))
  expect_false(has_layer(p_obs, "GeomLine"))

  p_fit <- plot_comp(stockplotr::bigskate_lencomp, fleet = "Triennial")
  expect_true(has_layer(p_fit, "GeomLine"))
  # males are mirrored below zero
  expect_true(any(p_fit$data$obs_plot < 0))
  # r4ss-style sample size labels
  text_layer <- p_fit$layers[vapply(p_fit$layers, function(l) inherits(l$geom, "GeomText"), logical(1))][[1]]
  expect_true(all(grepl("^N adj\\.=.*N eff\\.=", text_layer$data$label)))
})

test_that("plot_comp aggregates across years", {
  p <- plot_comp(stockplotr::bigskate_lencomp, aggregate = TRUE)
  expect_s3_class(p, "ggplot")
  expect_false("year" %in% names(p$data))
  sums <- p$data |>
    dplyr::summarise(total = sum(observed), .by = fleet)
  expect_true(all(abs(sums$total - 1) < 1e-8))
})

test_that("plot_comp works with r4ss output", {
  expect_no_error(plot_comp(r4ss_output, comp_type = "length", fleet = "FISHERY"))
})

# plot_comp_bubble / plot_comp_residuals --------------------------------------
test_that("bubble plots show observations and residuals", {
  p <- plot_comp_bubble(stockplotr::bigskate_lencomp)
  expect_s3_class(p, "ggplot")
  expect_true(has_layer(p, "GeomPoint"))

  r <- plot_comp_residuals(stockplotr::bigskate_lencomp)
  expect_s3_class(r, "ggplot")
  expect_true(all(c("Observed > expected", "Observed < expected") %in% levels(r$data$sign)))
  expect_true(any(grepl("max =", r$data$panel)))
})

test_that("residual plot errors without expected values or sample sizes", {
  obs_only <- dplyr::select(stockplotr::bigskate_lencomp, -expected, -n, -pearson)
  expect_error(plot_comp_residuals(obs_only), "No Pearson residuals")
})

# plot_comp_sex_ratio ---------------------------------------------------------
test_that("plot_comp_sex_ratio calculates ratios and intervals", {
  p <- plot_comp_sex_ratio(stockplotr::bigskate_lencomp, fleet = "WCGBTS")
  expect_s3_class(p, "ggplot")
  expect_true(has_layer(p, "GeomLinerange"))
  ratios <- p$data
  ok <- !is.na(ratios$ratio_obs)
  expect_true(all(ratios$ratio_obs[ok] >= 0 & ratios$ratio_obs[ok] <= 1))
  expect_true(all(ratios$lower[ok] <= ratios$ratio_obs[ok] + 1e-12))
  expect_true(all(ratios$upper[ok] >= ratios$ratio_obs[ok] - 1e-12))

  # bins without fish (only the constant Stock Synthesis adds) are omitted
  # rather than shown as 50:50
  expect_false(any(ratios$ratio_obs[ok] == 0.5 & ratios$bin[ok] > 180))

  expect_no_error(plot_comp_sex_ratio(stockplotr::bigskate_lencomp, aggregate = TRUE))
})

test_that("plot_comp_sex_ratio requires two sexes", {
  combined <- dplyr::filter(stockplotr::bigskate_lencomp, sex == "female")
  expect_error(plot_comp_sex_ratio(combined), "both")
})

# plot_comp_francis --------------------------------------------------------------
test_that("plot_comp_francis matches r4ss Francis weights", {
  # Values from r4ss::SSMethod.TA1.8() for the simple_small model
  means <- comp_mean_table(
    prepare_comp_data(r4ss_output, comp_type = "length", fleet = "FISHERY")
  )
  expect_equal(1 / stats::var(means$std_resid), 1.3108142, tolerance = 1e-6)
  means <- comp_mean_table(
    prepare_comp_data(r4ss_output, comp_type = "age", fleet = "SURVEY1")
  )
  expect_equal(1 / stats::var(means$std_resid), 2.667424, tolerance = 1e-6)
  # Big Skate: partitions of a fleet are combined, as in r4ss
  means <- comp_mean_table(
    prepare_comp_data(stockplotr::bigskate_lencomp, fleet = "Fishery_current")
  )
  expect_equal(1 / stats::var(means$std_resid), 1.0019480, tolerance = 1e-6)
  expect_equal(unique(means$fleet), "Fishery_current (discard & retained)")
  means <- comp_mean_table(
    prepare_comp_data(stockplotr::bigskate_lencomp, fleet = "WCGBTS")
  )
  expect_equal(1 / stats::var(means$std_resid), 1.0000112, tolerance = 1e-6)
})

test_that("plot_comp_francis plots observed and expected means", {
  p <- plot_comp_francis(stockplotr::bigskate_lencomp)
  expect_s3_class(p, "ggplot")
  expect_true(has_layer(p, "GeomLine"))
  expect_true(has_layer(p, "GeomErrorbar"))
  expect_true(all(grepl("TA1.8 weight", p$data$fleet)))

  no_francis <- plot_comp_francis(stockplotr::bigskate_lencomp, show_francis = FALSE)
  expect_false(has_layer(no_francis, "GeomErrorbar"))
})
