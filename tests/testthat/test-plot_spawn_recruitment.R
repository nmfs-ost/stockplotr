test_that("plot_spawn_recruitment generates plots without errors", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # expect error-free plot with minimal arguments
  expect_no_error(
    stockplotr::plot_spawn_recruitment(dat)
  )

  # expect error-free plot with many arguments
  expect_no_error(
    stockplotr::plot_spawn_recruitment(
      dat,
      spawning_biomass_label = "mt",
      recruitment_label = "mt",
      end_year = 2026,
      make_rda = FALSE,
      rda_dir = getwd()
    )
  )


  # expect ggplot object is returned
  expect_s3_class(
    stockplotr::plot_spawn_recruitment(
      dat,
      spawning_biomass_label = "mt",
      recruitment_label = "mt",
      end_year = 2026,
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
  plot_spawn_recruitment(
    dat,
    spawning_biomass_label = "mt",
    recruitment_label = "mt",
    end_year = 2026,
    make_rda = TRUE,
    rda_dir = getwd()
  )

  # expect that both rda_files dir and the sr_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "rda_files")))
  expect_true(file.exists(fs::path(getwd(), "rda_files", "sr_figure.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "rda_files"), recursive = T)
})
