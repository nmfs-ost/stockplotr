make_utils_plot_data <- function() {
  tibble::tibble(
    label = c("biomass", "biomass", "biomass", "biomass_msy", "biomass_unfished"),
    estimate = c(100, 120, 140, 110, 200),
    module_name = c("TIME_SERIES", "TIME_SERIES", "TIME_SERIES", "TIME_SERIES", "TIME_SERIES"),
    year = c(2000, 2001, 2002, NA, NA),
    fleet = c("fleet_a", "fleet_a", "fleet_b", NA, NA),
    age = c(1, 2, 3, NA, NA),
    season = c("spring", "spring", "fall", NA, NA),
    uncertainty = c(10, 10, 10, NA, NA),
    uncertainty_label = c("se", "se", "se", NA, NA),
    era = c("time", "time", "time", "time", "time")
  )
}

test_that("filter_data handles data frames and lists", {
  dat <- make_utils_plot_data()

  filtered <- filter_data(
    dat = dat,
    label_name = "biomass",
    geom = "line",
    interactive = FALSE
  )
  expect_true(is.data.frame(filtered))
  expect_true(all(filtered$group_var == "solid"))
  expect_equal(unique(filtered$model), "1")

  filtered_list <- filter_data(
    dat = list(model_one = dat, model_two = dat),
    label_name = "biomass",
    geom = "point",
    interactive = FALSE
  )
  expect_setequal(unique(filtered_list$model), c("model_one", "model_two"))
  expect_setequal(unique(filtered_list$group_var), c("model_one", "model_two"))
})

test_that("plot_timeseries and plot_error return expected layers", {
  dat <- filter_data(
    dat = make_utils_plot_data(),
    label_name = "biomass",
    geom = "line",
    interactive = FALSE
  )

  expect_s3_class(
    plot_timeseries(dat, geom = "line"),
    "gg"
  )

  point_plot <- plot_timeseries(dat, geom = "point", facet = "fleet")
  expect_s3_class(point_plot, "gg")
  expect_true(!is.null(point_plot$facet$params$facets))

  error_plot_hline <- plot_error(dat, hline = TRUE)
  error_plot_no_hline <- plot_error(dat, hline = FALSE)
  has_hline <- function(p) {
    any(vapply(p$layers, function(layer) inherits(layer$geom, "GeomHline"), logical(1)))
  }
  expect_true(has_hline(error_plot_hline))
  expect_false(has_hline(error_plot_no_hline))
})

test_that("plot_aa and helper line builders return expected outputs", {
  aa_dat <- tibble::tibble(
    label = "abundance",
    estimate = c(10, 20, 15, 40, 30, 20),
    module_name = "AGE_SERIES",
    year = c(2000, 2000, 2001, 2001, 2002, 2002),
    fleet = c("a", "a", "a", "b", "b", "b"),
    age = c(1, 2, 1, 2, 1, 2),
    season = "spring",
    uncertainty = 1,
    uncertainty_label = "se",
    era = "time",
    model = "1",
    group_var = "1"
  )

  proportional_plot <- plot_aa(aa_dat, proportional = TRUE)
  expect_s3_class(proportional_plot, "gg")
  expect_equal(proportional_plot$theme$legend.position, "none")

  avg_layer <- stockplotr:::average_age_line(aa_dat, facet = "fleet")
  expect_type(avg_layer, "list")
  expect_s3_class(avg_layer[[1]], "LayerInstance")
  expect_true("avg" %in% names(avg_layer[[1]]$data))

  cohort_dat <- tibble::tibble(
    year = c(2000, 2001, 2002, 2000, 2001, 2002),
    age = c(1, 2, 3, 1, 2, 3),
    estimate = c(100, 120, 140, 5, 5, 5)
  )
  cohort_layer <- stockplotr:::cohort_line(cohort_dat)
  expect_type(cohort_layer, "list")
  expect_true("cohort" %in% names(cohort_layer[[1]]$data))
  expect_equal(length(unique(cohort_layer[[1]]$data$cohort)), 1)
})

test_that("reference and scalar utility helpers behave as expected", {
  dat <- make_utils_plot_data()
  ts_dat <- filter_data(
    dat = dat,
    label_name = "biomass",
    geom = "line",
    interactive = FALSE
  )
  base_plot <- plot_timeseries(ts_dat, geom = "line")

  ref_plot <- reference_line(
    plot = base_plot,
    dat = dat,
    label_name = "biomass",
    reference = c(msy = 110)
  )
  has_hline <- function(p) {
    any(vapply(p$layers, function(layer) inherits(layer$geom, "GeomHline"), logical(1)))
  }
  expect_true(has_hline(ref_plot))

  missing_ref_plot <- reference_line(
    plot = base_plot,
    dat = dat,
    label_name = "biomass",
    reference = "target"
  )
  expect_equal(length(missing_ref_plot$layers), length(base_plot$layers))

  expect_true(is.numeric(stockplotr:::axis_breaks(2000:2005)))
  expect_equal(stockplotr:::cap_first_letter("biomass"), "Biomass")
  expect_equal(stockplotr:::cap_first_letter(""), "")

  expect_equal(stockplotr:::get_id(list(1, 2, 3)), 1:3)
  expect_equal(stockplotr:::get_id(list(a = 1, b = 2)), c("a", "b"))

  expect_equal(
    stockplotr:::calculate_reference_point(dat, "biomass msy"),
    110
  )
  expect_equal(
    round(stockplotr:::calculate_reference_point(dat, "biomass msy", lbs = TRUE), 3),
    round(110 * 2.20462, 3)
  )
  expect_null(stockplotr:::calculate_reference_point(dat, "not_here"))

  expect_equal(
    stockplotr:::label_magnitude("Biomass", scale_amount = 1),
    "Biomass (mt)"
  )
  expect_match(
    stockplotr:::label_magnitude("Biomass", scale_amount = 1e6, unit_label = "kg"),
    "millions of kg"
  )
  expect_match(
    stockplotr:::label_magnitude("Biomass", legend = TRUE),
    "\n\\("
  )
  expect_error(stockplotr:::label_magnitude("Biomass", scale_amount = 1e10))
})

test_that("check_grouping finds indexing columns with multiple values", {
  dat <- tibble::tibble(
    year = c(2000, 2001, 2002),
    age = c(1, 1, 1),
    fleet = c("a", "a", "b")
  )
  grouped <- stockplotr:::check_grouping(dat)
  expect_true("year" %in% grouped)
  expect_true("fleet" %in% grouped)
  expect_false("age" %in% grouped)
})
