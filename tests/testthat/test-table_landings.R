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

test_that("table_landings tabulates the observed landings, or the labels given", {
  # vermilion_snapper also has predicted landings, whose labels contain
  # "landings"
  dat <- stockplotr::vermilion_snapper
  tbl <- suppressMessages(table_landings(dat, interactive = FALSE))
  cols <- names(tbl[["_data"]])
  expect_true(all(grepl("^Year$|Observed", cols)))
  expect_false(any(grepl("Predicted", cols)))
  # A label given is used, even when observed landings exist
  tbl <- suppressMessages(table_landings(
    stockplotr::example_data,
    label = "landings_predicted_weight", interactive = FALSE
  ))
  cols <- names(tbl[["_data"]])
  expect_true(any(grepl("Predicted Weight", cols)))
  expect_false(any(grepl("Observed", cols)))
  # cod reports its observed landings as "catch"
  tbl <- suppressMessages(table_landings(stockplotr::cod, interactive = FALSE))
  expect_s3_class(tbl, "gt_tbl")
})
