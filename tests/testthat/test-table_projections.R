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
  check_and_remove_tmp_files("projections_table.rda", figs_or_tables="tables")
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
