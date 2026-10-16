test_that("plot_index generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_index(
      stockplotr::example_data,
      interactive = FALSE
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    plot_index(
      stockplotr::example_data,
      interactive = FALSE
    ),
    "gg"
  )
})

# test_that("rda file made when indicated", {
#   # export rda
#   plot_index(
#     stockplotr::example_data,
#     make_rda = TRUE,
#     unit_label = "metric tons",
#     interactive = FALSE,
#     figures_dir = getwd()
#   )

#   # expect that both figures dir and the index_figure.rda file exist
#   check_and_remove_tmp_files("index_figure.rda")
# })
