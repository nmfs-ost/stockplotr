# read in sample dataset
dat <- asar::convert_output(
  file = fs::path("fixtures", "ss3_models", "models", "Hake_2018", "Report.sso"),
  model = "ss3"
)

test_that("plot_spawning_biomass generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    stockplotr::plot_spawning_biomass(dat)
  )

  # expect plot with warnings if ref_point not indicated
  expect_message(
    stockplotr::plot_spawning_biomass(dat)
  )

  # expect error-free plot with many arguments
  expect_no_error(
    stockplotr::plot_spawning_biomass(
      dat,
      unit_label = "metric tons",
      scale_amount = 1,
      ref_line = "target",
      end_year = 2024
    )
  )

  # expect error-free plot when setting relative to T
  expect_no_error(
    stockplotr::plot_spawning_biomass(
      dat,
      unit_label = "metric tons",
      scale_amount = 1,
      ref_point = 100,
      end_year = 2024,
      relative = TRUE
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    stockplotr::plot_spawning_biomass(
      dat,
      unit_label = "metric tons",
      scale_amount = 1,
      ref_point = 100,
      end_year = 2024,
      relative = TRUE
    ),
    "gg"
  )
})

test_that("plot_spawning_biomass plots contain reference point when indicated", {
  # expect plot with a reference point (horizontal line) contains 4 layers while
  # plot w/o ref pt contains only 3 layers

  # make sb plot with reference point
  sb_ref <- stockplotr::plot_spawning_biomass(dat,
    ref_point = 18000
  )
  # extract number of layers (should be 4)
  sb_ref_layers <- sb_ref[["layers"]] |>
    length()

  # make sb plot without reference point
  sb_no_ref <- stockplotr::plot_spawning_biomass(dat)
  # extract number of layers (should be 3)
  sb_no_ref_layers <- sb_no_ref[["layers"]] |>
    length()

  expect_equal(
    (sb_ref_layers - 1),
    sb_no_ref_layers
  )
})

test_that("rda file made when indicated", {
  # export rda
  plot_spawning_biomass(
    dat,
    unit_label = "metric tons",
    scale_amount = 1,
    ref_line = "msy",
    end_year = 2024,
    make_rda = TRUE,
    figures_dir = getwd()
  )

  # expect that both figures dir and the spawning.biomass_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "figures")))
  expect_true(file.exists(fs::path(getwd(), "figures", "spawning.biomass_figure.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})

test_that("plot_spawning_biomass generates error with future end_year", {
  # expect error
  expect_error(
    stockplotr::plot_spawning_biomass(
      dat,
      unit_label = "metric tons",
      end_year = 2055,
      scale_amount = 1,
      ref_point = 100,
      relative = TRUE
    )
  )
})
