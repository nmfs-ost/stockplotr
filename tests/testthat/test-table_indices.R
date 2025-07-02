# read in sample dataset
dat <- asar::convert_output(file = fs::path("fixtures", "ss3_models", "models", "Hake_2018", "Report.sso"),
                              model = "ss3")

test_that("table_indices generates plots without errors", {

  # expect error-free plot with minimal arguments
  expect_no_error(
    stockplotr::table_indices(dat)
  )

  # expect error-free plot with many arguments
  expect_no_error(
    stockplotr::table_indices(
      dat,
      end_year = 2024,
      make_rda = FALSE,
      tables_dir = getwd()
    )
  )


  # expect flextable object is returned
  expect_s3_class(
    stockplotr::table_indices(
      dat,
      make_rda = FALSE,
      tables_dir = getwd()
    ),
    "flextable"
  )
})

test_that("rda file made when indicated", {

  # export rda
  table_indices(
    dat,
    end_year = 2024,
    make_rda = TRUE,
    tables_dir = getwd()
  )

  # expect that both tables dir and the indices.abundance_table.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "tables")))
  expect_true(file.exists(fs::path(getwd(), "tables", "indices.abundance_table.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
})

test_that("table_indices generates error with future end_year", {

  # expect error
  expect_error(
    stockplotr::table_indices(
      dat,
      end_year = 2035,
      make_rda = TRUE,
      tables_dir = getwd()
    )
  )
})
