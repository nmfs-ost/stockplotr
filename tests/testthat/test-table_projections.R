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
      interactive = FALSE,
      make_rda = FALSE,
      tables_dir = getwd()
    )
  )

})

test_that("rda file made when indicated", {
  # export rda
  table_projections(
    dat = stockplotr::example_data,
    interactive = FALSE,
    make_rda = TRUE,
    tables_dir = getwd()
  )

  # expect that both tables dir and the projections_table.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "tables")))
  expect_true(file.exists(fs::path(getwd(), "tables", "projections_table.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
})

test_that("table_projections generates error with incorrect module", {
  # expect error
  # Need to test this -- not exactly the right test/result
  expect_error(
    table_projections(
      dat = stockplotr::example_data,
      interactive = FALSE,
      module = "INCORRECT",
      make_rda = FALSE,
      tables_dir = getwd()
    )
  )
})
