test_that("extract_sis_data() runs with all possible rdas present", {
  save_all_plots(stockplotr::example_data)
  table_total_catch(stockplotr::example_data,
                    module = "TIME_SERIES",
                    interactive = FALSE,
                    make_rda = TRUE)

  files <- c(
    fs::path("figures",
             c("biomass_figure.rda", "spawning_biomass_figure.rda", "abundance_at_age_figure.rda", "fishing_mortality_figure.rda",  "index_figure.rda")
             ),
    fs::path("tables",
             "total_catch_table.rda")
  )

  expect_true(all(file.exists(files)))

  extract_sis_data()

  # erase temporary testing files
  file.remove(fs::path(getwd(), "sis_assmt_template.csv"))
  file.remove(fs::path(getwd(), "sis_ts_template.csv"))
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
  unlink(fs::path(getwd(), "tables"), recursive = T)
})

test_that("extract_sis_data() runs with some of all possible rdas present", {
  plot_biomass(stockplotr::example_data, module = "TIME_SERIES", interactive = FALSE, make_rda = TRUE)
  plot_fishing_mortality(stockplotr::example_data, interactive = FALSE, make_rda = TRUE)
  expect_no_error(extract_sis_data())

  # test that extract_sis_data() exports csvs
  expect_true(file.exists(fs::path(getwd(), "sis_assmt_template.csv")))
  expect_true(file.exists(fs::path(getwd(), "sis_ts_template.csv")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "sis_assmt_template.csv"))
  file.remove(fs::path(getwd(), "sis_ts_template.csv"))
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})

test_that("extract_sis_data() fails with no rdas present", {
  expect_error(extract_sis_data())
})
