test_that("table_biomass_abundance_catch generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    table_biomass_abundance_catch(stockplotr::example_data,
      module = c("TIME_SERIES", "CATCH", "TIME_SERIES"),
      interactive = FALSE
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    table_biomass_abundance_catch(stockplotr::example_data,
      biomass_label = "kg",
      abundance_label = "g",
      catch_label = "t",
      module = c("TIME_SERIES", "CATCH", "TIME_SERIES"),
      era = "fore",
      interactive = FALSE,
      make_rda = FALSE,
      tables_dir = getwd()
    )
  )

  # expect gt object is returned
  expect_s3_class(
    table_biomass_abundance_catch(
      dat = stockplotr::example_data,
      era = NULL,
      interactive = FALSE,
      module = c("TIME_SERIES", "CATCH", "TIME_SERIES"),
      make_rda = FALSE,
      tables_dir = getwd()
    ),
    "gt_tbl"
  )
})

test_that("table_biomass_abundance_catch generates error with incorrect module", {
  # expect error
  expect_error(
    table_biomass_abundance_catch(
      dat = stockplotr::example_data,
      interactive = FALSE,
      module = "DERIVED_QUANTITIES",
      make_rda = FALSE,
      tables_dir = getwd()
    )
  )
})
