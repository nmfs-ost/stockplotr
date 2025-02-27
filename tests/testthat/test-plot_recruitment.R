test_that("plot_recruitment generates plots without errors", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # expect error-free plot with minimal arguments
  expect_no_error(
    stockplotr::plot_recruitment(dat,
      end_year = 2022
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    stockplotr::plot_recruitment(
      dat,
      unit_label = "mt",
      scale_amount = 10,
      end_year = 2025,
      n_projected_years = 3,
      relative = FALSE,
      make_rda = FALSE,
      rda_dir = getwd()
    )
  )

  # expect error-free plot when setting relative to T
  expect_no_error(
    stockplotr::plot_recruitment(
      dat,
      unit_label = "mt",
      scale_amount = 10,
      end_year = 2025,
      n_projected_years = 3,
      relative = T,
      make_rda = FALSE,
      rda_dir = getwd()
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    stockplotr::plot_recruitment(
      dat,
      unit_label = "mt",
      scale_amount = 10,
      end_year = 2025,
      n_projected_years = 3,
      relative = T,
      make_rda = FALSE,
      rda_dir = getwd()
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
  plot_recruitment(
    dat,
    rda_dir = getwd(),
    make_rda = TRUE
  )

  # expect that both rda_files dir and the recruitment_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "rda_files")))
  expect_true(file.exists(fs::path(getwd(), "rda_files", "recruitment_figure.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "rda_files"), recursive = T)
})
