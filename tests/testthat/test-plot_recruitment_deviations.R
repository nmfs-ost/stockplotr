test_that("plot_recruitment_deviations generates plots without errors", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # expect error-free plot with minimal arguments
  expect_no_error(
    stockplotr::plot_recruitment_deviations(dat,
      end_year = 2022
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    stockplotr::plot_recruitment_deviations(
      dat,
      end_year = 2022,
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    stockplotr::plot_recruitment_deviations(
      dat,
      end_year = 2022,
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
  plot_recruitment_deviations(
    dat,
    end_year = 2022,
    make_rda = TRUE,
    figures_dir = getwd()
  )

  # expect that both figures dir and the recruitment.deviations_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "figures")))
  expect_true(file.exists(fs::path(getwd(), "figures", "recruitment.deviations_figure.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})

test_that("plot_recruitment_deviations generates error with future end_year", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # expect error
  expect_error(
    plot_recruitment_deviations(
      dat,
      end_year = 2035,
      make_rda = TRUE,
      figures_dir = getwd()
    )
  )
})
