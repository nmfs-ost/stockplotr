test_that("plot_spawning_biomass generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_spawning_biomass(
      stockplotr::example_data,
      interactive = FALSE
    )
  )

  # expect plot with warnings if ref_point not indicated
  expect_message(
    plot_spawning_biomass(
      stockplotr::example_data,
      interactive = FALSE
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    plot_spawning_biomass(
      stockplotr::example_data,
      unit_label = "metric tons",
      scale_amount = 1,
      ref_line = "msy",
      interactive = FALSE
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    plot_spawning_biomass(
      stockplotr::example_data,
      unit_label = "metric tons",
      scale_amount = 1,
      ref_line = c("msy" = 2000000),
      relative = FALSE,
      interactive = FALSE
    ),
    "gg"
  )
})

# test_that("rda file made when indicated", {
#   # export rda
#   plot_spawning_biomass(
#     stockplotr::example_data,
#     unit_label = "metric tons",
#     scale_amount = 1,
#     ref_line = "msy",
#     module = "TIME_SERIES",
#     interactive = FALSE,
#     make_rda = TRUE,
#     figures_dir = getwd()
#   )

#   # expect that both figures dir and the spawning_biomass_figure.rda file exist
#   check_and_remove_tmp_files("spawning_biomass_figure.rda")
# })

test_that("plot_spawning_biomass generates plots for multiple models", {
    dat <- convert_output(list(
      "model1" = r4ss::SS_output(test_path("fixtures", "ss3_models", "models", "platoons")),
      "model2" = r4ss::SS_output(test_path("fixtures", "ss3_models", "models", "Empirical_Wtatage_Age_Selex"))
    ))

    expect_no_error(plt <- plot_spawning_biomass(dat, module="derived_quants"))
    expect_s3_class(plt, "gg")

    p <- ggplot2::ggplot_build(plt)
    expect_equal(length(unique(p$data[[1]]$group)), 2)

})
