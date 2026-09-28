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
                                  unit_label = c("biomass" = "kg", "abundance" = "hundreds of fish", "catch" = "kg"),
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

test_that("rda file made when indicated", {
  # export rda
  table_biomass_abundance_catch(
    dat = stockplotr::example_data,
    module = c("TIME_SERIES", "CATCH", "TIME_SERIES"),
    make_rda = TRUE,
    tables_dir = getwd()
  )
  
  # expect that both tables dir and the biomass_abundance_catch_table.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "tables")))
  expect_true(file.exists(fs::path(getwd(), "tables", "biomass_abundance_catch_table.rda")))
  
  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
})

test_that("table_biomass_abundance_catch generates error with incorrect module", {
  # expect error
  expect_error(
    table_biomass_abundance_catch(
      dat = stockplotr::example_data,
      interactive = FALSE,
      module = "SPR_SERIES",
      make_rda = FALSE,
      tables_dir = getwd()
    )
  )
})
