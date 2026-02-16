# load sample dataset
load(file.path(
  "fixtures", "ss3_models_converted", "Hake_2018",
  "std_output.rda"
))

test_that("table_landings generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    table_landings(
      out_new,
      interactive = FALSE,
      module = "CATCH"
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    table_landings(
      dat = out_new,
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
  #     dat = out_new,
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
  table_landings(
    dat = out_new,
    unit_label = "mt",
    era = NULL,
    interactive = FALSE,
    module = "CATCH",
    make_rda = TRUE,
    tables_dir = getwd()
  )

  # expect that both tables dir and the landings_table.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "tables")))
  expect_true(file.exists(fs::path(getwd(), "tables", "landings_table.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
})

test_that("table_landings generates error with incorrect module", {
  # expect error
  # Need to test this -- not exactly the right test/result
  expect_error(
    table_landings(
      dat = out_new,
      unit_label = "mt",
      era = NULL,
      interactive = FALSE,
      module = "SPR_SERIES",
      make_rda = FALSE,
      tables_dir = getwd()
    )
  )
})
