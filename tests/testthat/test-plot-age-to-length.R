test_that("plot_age_to_length_conversion generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_age_to_length_conversion(
      stockplotr::ss3_simple,
      interactive = FALSE
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    plot_age_to_length_conversion(
      stockplotr::ss3_simple,
      group = NULL,
      facet = NULL,
      interactive = FALSE,
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )

    expect_no_error(
    plot_age_to_length_conversion(
      stockplotr::ss3_simple |>
      dplyr::filter(age < 10),
      group = NULL,
      facet = NULL,
      interactive = FALSE,
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    plot_age_to_length_conversion(
      stockplotr::ss3_simple,
      interactive = FALSE
    ),
    "ggplot"
  )
})
