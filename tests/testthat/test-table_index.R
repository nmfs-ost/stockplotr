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

# test_that("rda file made when indicated", {
#   # export rda
#   table_index(
#     dat = stockplotr::example_data,
#     interactive = FALSE,
#     make_rda = TRUE,
#     tables_dir = getwd()
#   )

#   # load the rda file and check that it contains the expected object
#   load(fs::path(getwd(), "tables", "index_table.rda"))
#   # expect rda contains three objects: table, caption, and latex table
#   expect_false(
#     is.null(rda$table)
#   )
#   expect_false(
#     is.null(rda$caption)
#   )
#   expect_false(
#     is.null(rda$latex_table)
#   )

#   expect_true(rda$caption == "Calculated index of abundance and corresponding CVs for the fleets and surveys identified in the column headers. ")

#   # erase temporary testing files
#   check_and_remove_tmp_files("index_table.rda", figs_or_tables="tables")
# })
