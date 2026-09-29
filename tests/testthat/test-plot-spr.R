test_that("plot_spr generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_spr(stockplotr::example_data, quantity="spr"),
  )
  
  expect_no_error(
    plot_spr(stockplotr::example_data, quantity="fishing_intensity")
  )
  
  expect_no_error(
    plot_spr(stockplotr::example_data, quantity="spr_ratio")
  )
  
  
  # expect error-free plot with many arguments
  expect_no_error(
    plot_spr(
      stockplotr::example_data,
      quantity = "spr_ratio",
      ref_line = "biomass_target"
    )
  )

  
  # expect ggplot object is returned
  expect_s3_class(
    plot_spr(
      stockplotr::example_data,
      ref_line = c("target" = 0.80)
    ),
    "gg"
  )
})

test_that("rda file made when indicated", {
  # export rda
  plot_spr(
    stockplotr::example_data,
    ref_line = "msy",
    module = "DERIVED_QUANTITIES",
    make_rda = TRUE,
    figures_dir = getwd()
  )
  
  # expect that both figures dir and the spawning_biomass_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "figures")))
  expect_true(file.exists(fs::path(getwd(), "figures", "spr_figure.rda")))
  
  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})
