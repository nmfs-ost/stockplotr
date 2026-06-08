# load sample dataset
load(file.path(
  "fixtures", "ss3_models_converted", "Hake_2018",
  "std_output.rda"
))

test_that("table_index generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    table_index(
      out_new,
      interactive = FALSE
    )
  )
  
  # expect error-free plot with many arguments
  expect_no_error(
    table_index(
      dat = out_new,
      make_rda = FALSE,
      tables_dir = getwd()
    )
  )
  
  
  # expect gt object is returned
  # adjust this test to work for multiple output tables
  # expect_s3_class(
  #   table_index(
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
  table_index(
    dat = out_new,
    make_rda = TRUE,
    tables_dir = getwd()
  )
  
  # expect that both tables dir and the index_table.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "tables")))
  expect_true(file.exists(fs::path(getwd(), "tables", "index_table.rda")))
  
  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
})
