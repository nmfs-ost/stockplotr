test_that("plot_landings generates plots without errors", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # expect error-free plot with minimal arguments
  expect_no_error(
    stockplotr::plot_landings(dat)
  )


  # expect ggplot object is returned
  expect_s3_class(
    stockplotr::plot_landings(dat),
    "gg"
  )
})

test_that("rda file made when indicated", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # export rda
  plot_landings(dat,
    end_year = 2024,
    make_rda = TRUE,
    unit_label = "metric tons",
    rda_dir = getwd()
  )

  # expect that both rda_files dir and the landings_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "rda_files")))
  expect_true(file.exists(fs::path(getwd(), "rda_files", "landings_figure.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "rda_files"), recursive = T)
})

test_that("plot_landings generates error with future end_year", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # expect error
  expect_error(
    plot_landings(dat,
      end_year = 2035,
      make_rda = TRUE,
      unit_label = "metric tons",
      rda_dir = getwd()
    )
  )
})
