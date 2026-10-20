test_that("plot_stock_recruitment generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    stockplotr::plot_stock_recruitment(
      dat = stockplotr::example_data,
      interactive = FALSE
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    stockplotr::plot_stock_recruitment(
      stockplotr::example_data,
      spawning_biomass_label = "mt",
      recruitment_label = "mt",
      interactive = FALSE,
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )


  # expect ggplot object is returned
  expect_s3_class(
    stockplotr::plot_stock_recruitment(
      stockplotr::example_data,
      spawning_biomass_label = "mt",
      recruitment_label = "mt",
      interactive = FALSE,
      make_rda = FALSE,
      figures_dir = getwd()
    ),
    "gg"
  )
})

# test_that("rda file made when indicated", {
#   # export rda
#   plot_stock_recruitment(
#     stockplotr::example_data,
#     spawning_biomass_label = "mt",
#     recruitment_label = "mt",
#     interactive = FALSE,
#     make_rda = TRUE,
#     figures_dir = getwd()
#   )

#   # expect that both figures dir and the sr_figure.rda file exist
#   check_and_remove_tmp_files("stock_recruitment_figure.rda")
# })
