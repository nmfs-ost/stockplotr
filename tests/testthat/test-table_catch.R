test_that("table_catch generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    table_catch(
      stockplotr::example_data,
      interactive = FALSE,
      module = "TIME_SERIES"
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    table_catch(
      dat = stockplotr::example_data,
      unit_label = "mt",
      interactive = FALSE,
      module = "TIME_SERIES",
      make_rda = FALSE,
      tables_dir = getwd()
    )
  )


  # expect gt object is returned
  # adjust this test to work for multiple output tables
  # expect_s3_class(
  #   table_catch(
  #     dat = stockplotr::example_data,
  #     unit_label = "mt",
  #     era = NULL,
  #     interactive = FALSE,
  #     module = "TIME_SERIES",
  #     make_rda = FALSE,
  #     tables_dir = getwd()
  #   ),
  #   "gt_tbl"
  # )
})

test_that("rda file made when indicated", {
  # export rda
  table_catch(
    dat = stockplotr::example_data,
    unit_label = "mt",
    era = NULL,
    interactive = FALSE,
    module = "TIME_SERIES",
    make_rda = TRUE,
    tables_dir = getwd()
  )

  # expect that both tables dir and the catch_table.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "tables")))
  expect_true(file.exists(fs::path(getwd(), "tables", "catch_table.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
})

test_that("table_catch generates error with incorrect module", {
  # expect error
  # Need to test this -- not exactly the right test/result
  expect_error(
    table_catch(
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


test_that("rda file made when indicated", {
  # export rda
  table_catch(
    dat = stockplotr::example_data,
    module = "TIME_SERIES",
    make_rda = TRUE,
    tables_dir = getwd()
  )
  
  # expect that both tables dir and the catch_table.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "tables")))
  expect_true(file.exists(fs::path(getwd(), "tables", "catch_table.rda")))
  
  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)

  
  # export rda
  table_catch(
    dat = stockplotr::example_data,
    module = "TIME_SERIES",
    make_rda = TRUE,
    tables_dir = getwd()
  )
  
  # load the rda file and check that it contains the expected object
  load(fs::path(getwd(), "tables", "catch_table.rda"))
  # expect rda contains three objects: table, caption, and latex table
  expect_false(
    is.null(rda$table)
  )
  expect_false(
    is.null(rda$caption)
  )
  expect_false(
    is.null(rda$latex_table)
  )
  
  expect_true(rda$caption == "Total catch over time.")
  
  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
})

