# load sample dataset
load(file.path(
  "fixtures", "ss3_models_converted", "Hake_2018",
  "std_output.rda"
))

test_that("plot_spawning_biomass generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_spawning_biomass(out_new)
  )

  # expect plot with warnings if ref_point not indicated
  expect_message(
    plot_spawning_biomass(out_new)
  )

  # expect error-free plot with many arguments
  expect_no_error(
    plot_spawning_biomass(
      out_new,
      unit_label = "metric tons",
      scale_amount = 1,
      ref_line = "msy"
    )
  )

  # expect error-free plot when setting relative to T
  expect_no_error(
    plot_spawning_biomass(
      out_new,
      unit_label = "metric tons",
      scale_amount = 1,
      ref_line = "msy",
      relative = TRUE
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    plot_spawning_biomass(
      out_new,
      unit_label = "metric tons",
      scale_amount = 1,
      ref_line = c("msy" = 2000000),
      relative = TRUE
    ),
    "gg"
  )
})

# Test now mute bc will always plot like when reference point is present
# test_that("plot_spawning_biomass plots contain reference point when indicated", {
#   # expect plot with a reference point (horizontal line) contains 4 layers while
#   # plot w/o ref pt contains only 3 layers

#   # make sb plot with reference point
#   sb_ref <- plot_spawning_biomass(out_new,
#     ref_line = c("msy" = 1800099),
#     module = "TIME_SERIES"
#   )
#   # extract number of layers (should be 4)
#   sb_ref_layers <- sb_ref[["layers"]] |>
#     length()

#   # make sb plot without reference point
#   sb_no_ref <- plot_spawning_biomass(
#     out_new,
#     module = "TIME_SERIES")
#   # extract number of layers (should be 3)
#   sb_no_ref_layers <- sb_no_ref[["layers"]] |>
#     length()

#   expect_equal(
#     (sb_ref_layers - 1),
#     sb_no_ref_layers
#   )
# })

test_that("rda file made when indicated", {
  # export rda
  plot_spawning_biomass(
    out_new,
    unit_label = "metric tons",
    scale_amount = 1,
    ref_line = "msy",
    module = "TIME_SERIES",
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

# Test no longer applicable bc end_year is not an argument
# test_that("plot_spawning_biomass generates error with future end_year", {
#   # expect error
#   expect_error(
#     plot_spawning_biomass(
#       out_new,
#       unit_label = "metric tons",
#       end_year = 2055,
#       scale_amount = 1,
#       ref_point = 100,
#       relative = TRUE
#     )
#   )
# })
