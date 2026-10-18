r4ss_output <- readRDS(
  test_path("fixtures", "r4ss_output", "simple_small", "simple_r4ss.rds")
)

has_layer <- function(p, geom_class) {
  any(vapply(p$layers, function(l) inherits(l$geom, geom_class), logical(1)))
}

# plot_comp_sample_size -------------------------------------------------------
test_that("effective sample sizes match Stock Synthesis", {
  comps <- prepare_comp_data(r4ss_output, comp_type = "length")
  effn <- comp_effn_table(comps)
  ss3 <- r4ss_output$lendbase |>
    dplyr::distinct(Yr, Fleet, effN) |>
    dplyr::mutate(fleet = r4ss_output$FleetNames[Fleet])
  joined <- dplyr::inner_join(effn, ss3, by = c("year" = "Yr", "fleet"))
  expect_gt(nrow(joined), 0)
  expect_equal(joined$effn, joined$effN, tolerance = 1e-3)
})

test_that("plot_comp_sample_size works with composition data", {
  p <- plot_comp_sample_size(stockplotr::bigskate_lencomp)
  expect_s3_class(p, "ggplot")
  expect_true(has_layer(p, "GeomAbline"))
  expect_true(all(grepl("McAllister-Ianelli weight", p$data$panel)))
  # Matches Stock Synthesis: Recommend_var_adj = Curr_Var_Adj * weight, with
  # partitions of a fleet combined (Length_Comp_Fit_Summary for Big Skate)
  weights <- dplyr::distinct(p$data, base_fleet, mi_weight)
  ss3 <- data.frame(
    base_fleet = c("Fishery_current", "WCGBTS", "Triennial"),
    curr_var_adj = c(0.239885, 0.067420, 1),
    recommend_var_adj = c(0.107271, 0.637793, 0.870899)
  )
  joined <- dplyr::inner_join(weights, ss3, by = "base_fleet")
  expect_equal(nrow(joined), 3)
  # (tolerance allows for the precision of values printed by Stock Synthesis)
  expect_equal(joined$mi_weight * joined$curr_var_adj, joined$recommend_var_adj, tolerance = 1e-3)
  expect_no_error(plot_comp_sample_size(r4ss_output, comp_type = "age", show_years = TRUE))
})

test_that("plot_comp_sample_size works with converted SS3 output", {
  p <- plot_comp_sample_size(stockplotr::example_data, comp_type = "length")
  expect_s3_class(p, "ggplot")
  expect_true(all(p$data$comp_type == "length"))
  # retained and discard compositions are separate panels
  expect_true(any(grepl("retained", p$data$fleet)))
  expect_true(any(grepl("discard", p$data$fleet)))
  # conditional age-at-length comps are dropped from age comps
  expect_message(
    plot_comp_sample_size(stockplotr::example_data, comp_type = "age"),
    "conditional age-at-length"
  )
})

test_that("plot_comp_sample_size errors without expected values", {
  obs_only <- dplyr::select(stockplotr::bigskate_lencomp, -expected)
  expect_error(plot_comp_sample_size(obs_only), "could not be calculated")
})

# residual distribution and patterns -----------------------------------------
test_that("plot_comp_residual_dist makes qq plots and histograms", {
  qq <- plot_comp_residual_dist(stockplotr::bigskate_lencomp)
  expect_s3_class(qq, "ggplot")
  expect_true(has_layer(qq, "GeomPoint"))
  expect_true(all(grepl("SDNR", qq$data$panel)))

  hist <- plot_comp_residual_dist(stockplotr::bigskate_lencomp, type = "histogram")
  expect_true(has_layer(hist, "GeomBar"))
})

test_that("plot_comp_residual_pattern summarizes by bin and year", {
  p_bin <- plot_comp_residual_pattern(stockplotr::bigskate_lencomp)
  expect_s3_class(p_bin, "ggplot")
  expect_true("bin" %in% names(p_bin$data))
  # mean residual by bin matches a direct calculation
  direct <- stockplotr::bigskate_lencomp |>
    dplyr::filter(fleet == "WCGBTS", sex == "female", bin == 50)
  check <- dplyr::filter(p_bin$data, fleet == "WCGBTS", sex == "female", bin == 50)
  expect_equal(check$mean_resid, mean(direct$pearson))

  p_year <- plot_comp_residual_pattern(stockplotr::bigskate_lencomp, by = "year")
  expect_true("year" %in% names(p_year$data))
})

test_that("residual plots error without residuals", {
  obs_only <- dplyr::select(stockplotr::bigskate_lencomp, -expected, -n, -pearson)
  expect_error(plot_comp_residual_dist(obs_only), "No Pearson residuals")
  expect_error(plot_comp_residual_pattern(obs_only), "No Pearson residuals")
})

test_that("runs_test detects patterns", {
  expect_lt(runs_test(c(rep(1, 10), rep(-1, 10))), 0.05)
  expect_gt(runs_test(rep(c(1, -1), 10)), 0.5)
  expect_equal(runs_test(rep(-1, 8)), 2 * 0.5^8)
  expect_true(is.na(runs_test(c(1, -1))))
})

# conditional age-at-length --------------------------------------------------
test_that("prepare_comp_data handles conditional age-at-length data", {
  caal <- prepare_comp_data(stockplotr::bigskate_caal, conditional = TRUE)
  expect_true("length_bin" %in% names(caal))
  # single-sex compositions stay normalized within each sex and length bin
  sums <- caal |>
    dplyr::summarise(total = sum(observed), .by = c(fleet, year, sex, length_bin))
  expect_true(all(abs(sums$total - 1) < 1e-6))
  expect_equal(caal$observed, stockplotr::bigskate_caal$observed[
    order(stockplotr::bigskate_caal$fleet, stockplotr::bigskate_caal$year,
      stockplotr::bigskate_caal$length_bin, stockplotr::bigskate_caal$sex,
      stockplotr::bigskate_caal$bin)
  ], tolerance = 1e-6)
  # marginal plots refuse conditional data
  expect_error(prepare_comp_data(stockplotr::bigskate_caal), "conditional = TRUE")
  expect_error(plot_comp(stockplotr::bigskate_caal), "conditional = TRUE")
  # and conditional plots need a length bin
  expect_error(
    prepare_comp_data(stockplotr::bigskate_lencomp, conditional = TRUE),
    "length_bin"
  )
})

test_that("prepare_comp_data reads conditional data from r4ss dbase", {
  dbase <- r4ss_output$agedbase
  # make half of the age comps conditional on a narrow length bin
  dbase$Lbin_hi[seq_len(nrow(dbase) / 2)] <- dbase$Lbin_lo[seq_len(nrow(dbase) / 2)]
  caal <- prepare_comp_data(dbase, conditional = TRUE)
  expect_equal(nrow(caal), nrow(dbase) / 2)
  marginal <- suppressMessages(prepare_comp_data(dbase))
  expect_equal(nrow(marginal), nrow(dbase) / 2)
  # r4ss output without conditional data
  expect_error(prepare_comp_data(r4ss_output, conditional = TRUE), "No conditional")
})

test_that("plot_comp_andre shows mean and SE of age at length", {
  dat <- dplyr::filter(stockplotr::bigskate_caal, year %in% 2017:2018)
  p <- plot_comp_andre(dat, fleet = "WCGBTS")
  expect_s3_class(p, "ggplot")
  expect_setequal(as.character(unique(p$data$statistic)), c("Mean age", "SE of mean age"))
  expect_true(has_layer(p, "GeomRibbon"))
  # observed mean ages increase with length
  means <- dplyr::filter(p$data, statistic == "Mean age", year == 2018, sex == "female")
  expect_gt(stats::cor(means$length_bin, means$observed), 0.8)
  # one value per year, sex, and length bin, calculated as in r4ss
  z <- dplyr::filter(
    stockplotr::bigskate_caal,
    fleet == "WCGBTS", year == 2018, sex == "female", length_bin == 60
  )
  check <- dplyr::filter(p$data, year == 2018, sex == "female", length_bin == 60)
  obs_mean <- sum(z$bin * z$observed) / sum(z$observed)
  expect_equal(check$observed[check$statistic == "Mean age"], obs_mean)
  expect_equal(
    check$expected[check$statistic == "SE of mean age"],
    sqrt(sum(z$bin^2 * z$expected) / sum(z$expected) -
      (sum(z$bin * z$expected) / sum(z$expected))^2) / sqrt(z$n[1])
  )
})

test_that("conditional bubble plots use length bins on the x-axis", {
  dat <- dplyr::filter(stockplotr::bigskate_caal, year %in% 2017:2018)
  p <- plot_comp_residuals(dat, fleet = "WCGBTS", conditional = TRUE)
  expect_s3_class(p, "ggplot")
  expect_true(any(grepl("^female, 2017\n\\(max", levels(p$data$panel))))
  expect_no_error(plot_comp_bubble(dat, fleet = "WCGBTS", conditional = TRUE))
})

test_that("r4ss sex codes are read correctly", {
  # single-sex male compositions have the Stock Synthesis code Sex = 1 but
  # r4ss sex = 2, and appear twice in the composition database
  dbase <- data.frame(
    Yr = 2000, Fleet = 1, Kind = "AGE", Part = 0,
    Sexes = 2, Sex = c(1, 1, 2, 2), sex = 2,
    Lbin_lo = 50, Lbin_hi = 50, Bin = c(1, 2, 1, 2),
    Obs = c(0.4, 0.6, 0.4, 0.6), Exp = c(0.5, 0.5, 0.5, 0.5),
    Pearson = c(-0.2, 0.2, -0.2, 0.2), Nsamp_adj = 10
  )
  comps <- prepare_comp_data(dbase, conditional = TRUE)
  expect_equal(nrow(comps), 2)
  expect_equal(unique(comps$sex), "male")
  expect_equal(comps$observed, c(0.4, 0.6))
  # without the r4ss sex column, sex is derived from Sexes and Sex
  comps <- prepare_comp_data(dplyr::select(dbase, -sex), conditional = TRUE)
  expect_equal(unique(comps$sex), "male")
})
