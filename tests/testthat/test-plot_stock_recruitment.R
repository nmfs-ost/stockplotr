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

test_that("stock recruitment uses the selected models for both series", {
  dat <- dplyr::bind_rows(
    dplyr::mutate(stockplotr::example_data, model = "model1"),
    dplyr::mutate(stockplotr::example_data, model = "model2")
  )
  plot <- plot_stock_recruitment(
    dat, model = "model2", interactive = FALSE,
    spawning_biomass_label = "mt", recruitment_label = "mt"
  )
  expect_equal(unique(plot$layers[[1]]$data$model), "model2")
  expect_true(any(!is.na(plot$layers[[1]]$data$predicted_recruitment)))
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
