test_that("plot_indices generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_indices(stockplotr::example_data)
  )


  # expect ggplot object is returned
  expect_s3_class(
    plot_indices(stockplotr::example_data),
    "gg"
  )
})

test_that("rda file made when indicated", {
  # export rda
  plot_indices(stockplotr::example_data,
    make_rda = TRUE,
    unit_label = "metric tons",
    figures_dir = getwd()
  )

  # expect that both figures dir and the index_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "figures")))
  expect_true(file.exists(fs::path(getwd(), "figures", "index_figure.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})
