test_that("plot_biomass generates plots without errors", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # expect error-free plot with minimal arguments
  expect_no_error(
    stockplotr::plot_biomass(dat)
  )

  # expect plot with warning message if ref_point not indicated
  expect_message(
    stockplotr::plot_biomass(dat)
  )

  # expect error-free plot with many arguments
  expect_no_error(
    stockplotr::plot_biomass(
      dat,
      ref_point = 18000,
      unit_label = "metric tons",
      scale_amount = 1,
      end_year = 2024,
      relative = FALSE,
      make_rda = FALSE,
      rda_dir = getwd()
    )
  )

  # expect error-free plot when setting relative to T
  expect_no_error(
    stockplotr::plot_biomass(
      dat,
      ref_point = 18000,
      unit_label = "metric tons",
      scale_amount = 1,
      end_year = 2024,
      relative = TRUE,
      make_rda = FALSE,
      rda_dir = getwd()
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    stockplotr::plot_biomass(
      dat,
      ref_point = 18000,
      unit_label = "metric tons",
      scale_amount = 1,
      end_year = 2024,
      relative = TRUE,
      make_rda = FALSE,
      rda_dir = getwd()
    ),
    "gg"
  )
})

test_that("plot_biomass plots contain reference point when indicated", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )


  # expect plot with a reference point (horizontal line) contains 4 layers while
  # plot w/o ref pt contains only 3 layers

  # make b plot with reference point
  b_ref <- stockplotr::plot_biomass(dat,
    ref_point = 18000
  )
  # extract number of layers (should be 4)
  b_ref_layers <- b_ref[["layers"]] |>
    length()

  # make b plot without reference point
  b_no_ref <- stockplotr::plot_biomass(dat)
  # extract number of layers (should be 3)
  b_no_ref_layers <- b_no_ref[["layers"]] |>
    length()

  expect_equal(
    (b_ref_layers - 1),
    b_no_ref_layers
  )
})

test_that("rda file made when indicated", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # export rda
  stockplotr::plot_biomass(
    dat,
    rda_dir = getwd(),
    make_rda = TRUE,
    end_year = 2023,
    ref_point = 18000
  )

  # expect that both rda_files dir and the biomass_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "rda_files")))
  expect_true(file.exists(fs::path(getwd(), "rda_files", "biomass_figure.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "rda_files"), recursive = T)
})

test_that("plot_biomass generates error with future end_year", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # expect error
  expect_error(
    stockplotr::plot_biomass(
      dat,
      rda_dir = getwd(),
      make_rda = TRUE,
      end_year = 2035,
      ref_point = 18000
    )
  )
})
