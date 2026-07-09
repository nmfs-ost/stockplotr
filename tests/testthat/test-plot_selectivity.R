test_that("plot_selectivity generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_selectivity(stockplotr::example_data
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    plot_selectivity(
      stockplotr::example_data |>
        dplyr::filter(year == 2020),
      era = NULL,
      group = NULL,
      facet = NULL,
      interactive = TRUE,
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
      era = NULL,
      group = NULL,
      facet = NULL,
      interactive = TRUE,
      module = "LEN_SELEX",
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    plot_selectivity(
      stockplotr::example_data
    ),
    "gg"
  )
})

test_that("rda file made when indicated", {
  # export rda
  plot_selectivity(
    stockplotr::example_data |>
      dplyr::filter(year == 2021),
    figures_dir = getwd(),
    make_rda = TRUE
  )

  # expect that both figures dir and the selectivity_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "figures")))
  expect_true(file.exists(fs::path(getwd(), "figures", "selectivity_figure.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})
