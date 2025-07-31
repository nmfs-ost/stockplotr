# load sample dataset
load(file.path(
  "fixtures", "ss3_models_converted", "Hake_2018",
  "std_output.rda"
))

test_that("plot_spawn_recruitment generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    stockplotr::plot_spawn_recruitment(
      dat = out_new,
      end_year = 2022
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    stockplotr::plot_spawn_recruitment(
      out_new,
      spawning_biomass_label = "mt",
      recruitment_label = "mt",
      end_year = 2024,
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )


  # expect ggplot object is returned
  expect_s3_class(
    stockplotr::plot_spawn_recruitment(
      out_new,
      spawning_biomass_label = "mt",
      recruitment_label = "mt",
      end_year = 2024,
      make_rda = FALSE,
      figures_dir = getwd()
    ),
    "gg"
  )
})

test_that("plot_spawn_recruitment doesn't generate plots with erraneous end year", {
  # expect error when attempt to use end_year past current year
  expect_error(
    stockplotr::plot_spawn_recruitment(
      out_new,
      spawning_biomass_label = "mt",
      recruitment_label = "mt",
      end_year = 2029,
      make_rda = TRUE, # TRUE
      figures_dir = getwd()
    )
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})

test_that("rda file made when indicated", {
  # export rda
  plot_spawn_recruitment(
    out_new,
    spawning_biomass_label = "mt",
    recruitment_label = "mt",
    end_year = 2024,
    make_rda = TRUE,
    figures_dir = getwd()
  )

  # expect that both figures dir and the sr_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "figures")))
  expect_true(file.exists(fs::path(getwd(), "figures", "sr_figure.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})

test_that("plot_spawn_recruitment generates error with future end_year", {
  # expect error
  expect_error(
    stockplotr::plot_spawn_recruitment(
      out_new,
      spawning_biomass_label = "mt",
      recruitment_label = "mt",
      end_year = 2029,
      make_rda = FALSE, # FALSE
      figures_dir = getwd()
    )
  )
})
