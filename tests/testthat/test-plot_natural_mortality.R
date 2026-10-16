test_that("plot_natural_mortality generate without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_natural_mortality(
      stockplotr::example_data, 
      module = "Natural_Mortality",
      interactive = FALSE
    )
  )

  # This check is moot with the more flextible approach and labelling?
  # expect eror when not interactive since it's choosing the first module in ex
  # expect_error(
  #   plot_natural_mortality(stockplotr::example_data)
  # )

  # expect ggplot object is returned
  expect_s3_class(
    plot_natural_mortality(
      stockplotr::example_data,
      module = "Natural_Mortality",
      interactive = FALSE
    ),
    "gg"
  )
})

# test_that("rda file made when indicated", {
#   # export rda
#   plot_natural_mortality(
#     stockplotr::example_data,
#     module = "Natural_Mortality",
#     interactive = FALSE,
#     make_rda = TRUE,
#     figures_dir = getwd()
#   )

#   # expect that both figures dir and the natural_mortality_figure.rda file exist
#   check_and_remove_tmp_files("natural_mortality_figure.rda")
# })
