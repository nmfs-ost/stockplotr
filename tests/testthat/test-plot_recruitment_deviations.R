test_that("plot_recruitment_deviations generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_recruitment_deviations(
      stockplotr::example_data,
      module = "SPAWN_RECRUIT",
      interactive = FALSE
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    plot_recruitment_deviations(
      stockplotr::example_data,
      module = "SPAWN_RECRUIT",
      interactive = FALSE,
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    plot_recruitment_deviations(
      stockplotr::example_data,
      module = "SPAWN_RECRUIT",
      interactive = FALSE,
      make_rda = FALSE,
      figures_dir = getwd()
    ),
    "gg"
  )
})

# test_that("rda file made when indicated", {
#   # export rda
#   plot_recruitment_deviations(
#     stockplotr::example_data,
#     module = "SPAWN_RECRUIT",
#     interactive = FALSE,
#     make_rda = TRUE,
#     figures_dir = getwd()
#   )

#   # expect that both figures dir and the recruitment_deviations_figure.rda file exist
#   check_and_remove_tmp_files("recruitment_deviations_figure.rda")
# })
