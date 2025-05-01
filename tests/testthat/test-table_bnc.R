test_that("table_bnc generates plots without errors", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # expect error-free plot with minimal arguments
  expect_no_error(
    stockplotr::table_bnc(dat,
      end_year = 2022
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    stockplotr::table_bnc(
      dat,
      end_year = 2025,
      biomass_unit_label = "mt",
      catch_unit_label = "mt",
      sb_unit_label = "mt",
      make_rda = FALSE,
      rda_dir = getwd()
    )
  )


  # expect flextable object is returned
  expect_s3_class(
    stockplotr::table_bnc(
      dat,
      end_year = 2025,
      biomass_unit_label = "mt",
      catch_unit_label = "mt",
      sb_unit_label = "mt",
      make_rda = FALSE,
      rda_dir = getwd()
    ),
    "flextable"
  )
})

test_that("rda file made when indicated", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # export rda
  table_bnc(
    dat,
    end_year = 2025,
    biomass_unit_label = "mt",
    catch_unit_label = "mt",
    sb_unit_label = "mt",
    make_rda = TRUE,
    rda_dir = getwd()
  )

  # expect that both rda_files dir and the bnc_table.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "rda_files")))
  expect_true(file.exists(fs::path(getwd(), "rda_files", "bnc_table.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "rda_files"), recursive = T)
})

test_that("table_bnc generates error with future end_year", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # expect error
  expect_error(
    stockplotr::table_bnc(
      dat,
      end_year = 2035,
      biomass_unit_label = "mt",
      catch_unit_label = "mt",
      sb_unit_label = "mt",
      make_rda = TRUE,
      rda_dir = getwd()
    )
  )
})
