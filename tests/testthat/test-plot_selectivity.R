test_that("plot_selectivity generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_selectivity(
      stockplotr::example_data,
      interactive = FALSE
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    plot_selectivity(
      stockplotr::example_data |>
        dplyr::filter(year == 2020),
      group = NULL,
      facet = NULL,
      interactive = FALSE,
      module = "AGE_SELEX",
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )

  expect_no_error(
    plot_selectivity(
      stockplotr::example_data |>
        dplyr::filter(year == 2008),
      unit_label = "mm",
      group = NULL,
      facet = NULL,
      interactive = FALSE,
      module = "LEN_SELEX",
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    plot_selectivity(
      stockplotr::example_data,
      interactive = FALSE
    ),
    "gg"
  )
})
