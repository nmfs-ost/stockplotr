test_that("plot_landings generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_landings(
      stockplotr::example_data,
      interactive = FALSE
    )
  )


  # expect ggplot object is returned
  expect_s3_class(
    plot_landings(
      stockplotr::example_data,
      interactive = FALSE
    ),
    "gg"
  )
})

test_that("rda file made when indicated", {
  # export rda
  plot_landings(
    stockplotr::example_data,
    interactive = FALSE,
    make_rda = TRUE,
    unit_label = "metric tons",
    figures_dir = getwd()
  )

  # expect that both figures dir and the landings_figure.rda file exist
  check_and_remove_tmp_files("landings_figure.rda")
})
