test_that("plot_recruitment generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_recruitment(
      stockplotr::example_data,
      unit_label = "kg",
      module = "TIME_SERIES",
      interactive = FALSE
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    plot_recruitment(
      stockplotr::example_data,
      unit_label = "mt",
      scale_amount = 10,
      module = "TIME_SERIES",
      interactive = FALSE,
      # relative = FALSE,
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )

  # expect error-free plot when setting relative to T
  # Relative is no longer an option
  # expect_no_error(
  #   plot_recruitment(
  #     stockplotr::example_data,
  #     unit_label = "mt",
  #     scale_amount = 10,
  #     # relative = TRUE,
  #     module = "TIME_SERIES",
  #     relative = T,
  #     make_rda = FALSE,
  #     figures_dir = getwd()
  #   )
  # )

  # expect ggplot object is returned
  expect_s3_class(
    plot_recruitment(
      stockplotr::example_data,
      unit_label = "mt",
      scale_amount = 10,
      # relative = TRUE,
      module = "TIME_SERIES",
      interactive = FALSE,
      make_rda = FALSE,
      figures_dir = getwd()
    ),
    "gg"
  )
})

