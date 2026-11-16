test_that("table_projections generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    table_projections(
      stockplotr::example_data,
      interactive = FALSE
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    table_projections(
      dat = stockplotr::example_data,
      catch_label = "kg",
      spawning_biomass_label = "kg",
      interactive = FALSE,
      make_rda = FALSE,
      tables_dir = getwd()
    )
  )
})

test_that("table_projections generates error with incorrect module", {
  # expect error
  # Need to test this -- not exactly the right test/result
  expect_error(
    table_projections(
      dat = stockplotr::example_data,
      interactive = FALSE,
      module = "INCORRECT",
      spawning_biomass_label = "kg",
      make_rda = FALSE,
      tables_dir = getwd()
    )
  )
})
