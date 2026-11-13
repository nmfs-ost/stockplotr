test_that("table_landings generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    table_landings(
      stockplotr::example_data,
      interactive = FALSE,
      module = "CATCH"
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    table_landings(
      dat = stockplotr::example_data,
      unit_label = "mt",
      interactive = FALSE,
      module = "CATCH",
      make_rda = FALSE,
      tables_dir = getwd()
    )
  )


  # expect gt object is returned
  # adjust this test to work for multiple output tables
  # expect_s3_class(
  #   table_landings(
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

test_that("table_landings generates error with incorrect module", {
  # expect error
  # Need to test this -- not exactly the right test/result
  expect_error(
    table_landings(
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