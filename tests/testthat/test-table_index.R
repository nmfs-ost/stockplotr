test_that("table_index generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    table_index(
      stockplotr::example_data,
      interactive = FALSE
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    table_index(
      dat = stockplotr::example_data,
      make_rda = FALSE,
      tables_dir = getwd()
    )
  )


  # expect gt object is returned
  # adjust this test to work for multiple output tables
  # expect_s3_class(
  #   table_index(
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
