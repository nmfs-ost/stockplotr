# load sample dataset
load(file.path(
  "fixtures", "ss3_models_converted", "Hake_2018",
  "std_output.rda"
))

# TODO: Fix tests
test_that("plot_abundance_at_age generates plots without errors", {

  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_abundance_at_age(out_new)
  )

  # expect error-free plot with many arguments
  expect_no_error(
    plot_abundance_at_age(
      out_new,
      facet = "area",
      unit_label = "fish",
      scale_amount = 1000,
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    plot_abundance_at_age(
      out_new,
      unit_label = "fish",
      scale_amount = 1,
      make_rda = FALSE,
      figures_dir = getwd()
    ),
    "gg"
  )
})

test_that("rda file made when indicated", {

  # export rda
  plot_abundance_at_age(
    out_new,
    figures_dir = getwd(),
    make_rda = TRUE
  )

  # expect that both figures dir and the pop.naa_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "figures")))
  expect_true(file.exists(fs::path(getwd(), "figures", "pop.naa_figure.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})

test_that("plot_abundance_at_age generates error with scaling <1", {

  # expect error
  expect_error(
    plot_abundance_at_age(
      out_new,
      unit_label = "fish",
      scale_amount = 0.1,
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )
})

test_that("plot_abundance_at_age generates error when abundance label is not found", {
  # generate test data set
  years <- 2000:2020
  fleets <- c("fleet_A", "fleet_B")
  seasons <- c("spring", "fall")
  ages <- 1:5 # Max age for AGE_SERIES
  labels <- c("biomass", "spawning_biomass")
  module_names <- c("TIME_SERIES", "AGE_SERIES")

  # Create a full grid for TIME_SERIES
  time_series_data <- tidyr::expand_grid(
    label = labels,
    module_name = "TIME_SERIES",
    year = years,
    fleet = fleets,
    season = seasons
  )  |>
    dplyr::mutate(
      # 'age' is NA for TIME_SERIES
      age = NA_integer_,
      # Simulate estimate values for time series
      estimate = rnorm(dplyr::n(), mean = 1000, sd = 200),
      # Simulate uncertainty (standard error)
      uncertainty = runif(dplyr::n(), min = 50, max = 150),
      uncertainty_label = "se"
    )

  # Create a full grid for AGE_SERIES
  age_series_data <- tidyr::expand_grid(
    label = labels,
    module_name = "AGE_SERIES",
    year = years,
    fleet = fleets,
    season = seasons,
    age = ages
  )  |>
    dplyr::mutate(
      # Simulate estimate values for age series (potentially age-dependent)
      estimate = rnorm(dplyr::n(), mean = 500 / age, sd = 100 / age), # Example: older ages have lower biomass
      # Simulate uncertainty (standard error)
      uncertainty = runif(dplyr::n(), min = 10, max = 50),
      uncertainty_label = "se"
    )

  # Combine the two datasets
  sample_data <- dplyr::bind_rows(time_series_data, age_series_data)  |>
    # Arrange for better readability (optional)
    dplyr::arrange(label, module_name, year, fleet, season, age)  |>
    # Ensure column order matches the request
    dplyr::select(label, estimate, module_name, year, fleet, age, season, uncertainty, uncertainty_label) |>
    # Add era so code doesn't break for now
    dplyr::mutate(era = "time") |>
    rbind(data.frame(label = "spawning_biomass_msy", 
                      estimate = 1024, 
                      module_name = "TIME_SERIES", # DERIVED_QUANTITIES
                      year = NA_integer_, 
                      fleet = NA, 
                      age = NA_integer_, 
                      season = NA, 
                      uncertainty = NA_real_, 
                      uncertainty_label = NA, 
                      era = "time"))

  # expect error
  expect_error(
    plot_abundance_at_age(
      sample_data,
      unit_label = "fish",
      scale_amount = 1,
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )
})

test_that("plot is made proportional and does not contain a legend", {
  plot <- plot_abundance_at_age(
    out_new,
    unit_label = "fish",
    proportional = TRUE,
    scale_amount = 1,
    make_rda = FALSE,
    figures_dir = getwd()
  )
  expect_true(plot$theme$legend.position == "none")
})

test_that("plot contains a legend", {
  plot <- plot_abundance_at_age(
    out_new,
    proportional = FALSE,
    unit_label = "fish",
    scale_amount = 1,
    make_rda = FALSE,
    figures_dir = getwd()
  )
  expect_true(is.null(plot$theme$legend.position))
})
