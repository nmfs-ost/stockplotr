test_that("plot_biomass_at_age generates plots without errors", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # expect error-free plot with minimal arguments
  expect_no_error(
    stockplotr::plot_biomass_at_age(dat)
  )

  # expect error-free plot with many arguments
  expect_no_error(
    stockplotr::plot_biomass_at_age(
      dat,
      unit_label = "metric tons",
      scale_amount = 10,
      end_year = 2024,
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    stockplotr::plot_biomass_at_age(
      dat,
      unit_label = "metric tons",
      scale_amount = 1,
      end_year = 2024,
      make_rda = FALSE,
      figures_dir = getwd()
    ),
    "gg"
  )
})

test_that("rda file made when indicated", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # export rda
  stockplotr::plot_biomass_at_age(
    dat,
    figures_dir = getwd(),
    make_rda = TRUE,
    end_year = 2023
  )

  # expect that both figures_tables dir and the pop.baa_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "figures_tables")))
  expect_true(file.exists(fs::path(getwd(), "figures_tables", "pop.baa_figure.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "figures_tables"), recursive = T)
})

test_that("plot_biomass_at_age generates error with future end_year", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # expect error
  expect_error(
    stockplotr::plot_biomass_at_age(
      dat,
      unit_label = "metric tons",
      scale_amount = 10,
      end_year = 2035,
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )
})
