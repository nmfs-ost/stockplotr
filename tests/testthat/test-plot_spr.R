test_that("plot_spr generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_spr(stockplotr::example_data, interactive = FALSE)
  )
  
  # expect error-free plot with many arguments
  expect_no_error(
    plot_spr(
      stockplotr::example_data,
      # quantity = "spr_ratio",
      interactive = FALSE,
      ref_line = "biomass_target"
    )
  )

  
  # expect ggplot object is returned
  expect_s3_class(
    plot_spr(
      stockplotr::example_data,
      interactive = FALSE,
      ref_line = c("target" = 0.80)
    ),
    "gg"
  )
})
