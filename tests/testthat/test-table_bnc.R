test_that("table_bnc generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    table_bnc(
      stockplotr::example_data,
      interactive = FALSE
    )
  )
  
  # expect error-free plot with many arguments
  expect_no_error(
    table_bnc(
      dat = stockplotr::example_data,
      biomass_unit_label = "mt",
      catch_unit_label = "mt",
      sb_unit_label = "mt",
      era = "fore",
      interactive = FALSE,
     # group = NULL,
     # method = "sum",
     # module = NULL,
     # label = NULL,
      make_rda = FALSE,
      tables_dir = getwd()
    )
  )
  
  
  # expect gt object is returned
  # adjust this test to work for multiple output tables
  # expect_s3_class(
  #   table_bnc(
  #     dat = stockplotr::example_data,
  #     unit_label = "mt",
  #     era = NULL,
  #     interactive = FALSE,
  #     module = "CATCH",
  #     make_rda = FALSE,
  #     tables_dir = getwd()
  #   ),
  #   "gt_tbl"
  # )
})

test_that("rda file made when indicated", {
  # export rda
  table_bnc(
    dat = stockplotr::example_data,
    unit_label = "mt",
    interactive = FALSE,
    make_rda = TRUE,
    tables_dir = getwd()
  )
  
  # expect that both tables dir and the bnc_table.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "tables")))
  expect_true(file.exists(fs::path(getwd(), "tables", "bnc_table.rda")))
  
  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
})

test_that("table_bnc generates error with incorrect module", {
  # expect error
  # Need to test this -- not exactly the right test/result
  expect_error(
    table_bnc(
      dat = stockplotr::example_data,
      unit_label = "mt",
      era = NULL,
      interactive = FALSE,
      module = "SPR_SERIES",
      make_rda = FALSE,
      tables_dir = getwd()
    )
  )
})
