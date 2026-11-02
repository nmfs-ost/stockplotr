test_that("extract_sis_data() runs with all possible rdas present", {

  savedir <- fs::path(getwd(), "test-extract_sis_data")
  if(!dir.exists(savedir))
    dir.create(savedir)

  save_all_plots(stockplotr::example_data, figures_tables_dir=savedir)
  # table_total_catch(stockplotr::example_data,
  #                   module = "TIME_SERIES",
  #                   interactive = FALSE,
  #                   make_rda = TRUE)

  files <- c(
    fs::path(savedir, "figures",
             c("biomass_figure.rda", "spawning_biomass_figure.rda", "abundance_at_age_figure.rda", "fishing_mortality_figure.rda",  "index_figure.rda")
             ),
    fs::path(savedir, "tables",
             "total_catch_table.rda")
  )

  expect_true(all(file.exists(files)))

  extract_sis_data(sis_data_dir=savedir, key_quantities_dir=savedir, figures_tables_dir=savedir)

  # erase temporary testing files
  # file.remove(fs::path(savedir, "sis_assmt_template.csv"))
  # file.remove(fs::path(savedir, "sis_ts_template.csv"))
  # file.remove(fs::path(savedir, "captions_alt_text.csv"))
  # file.remove(fs::path(savedir, "key_quantities.csv"))
  # unlink(fs::path(savedir, "figures"), recursive = T)
  # unlink(fs::path(savedir, "tables"), recursive = T)
  unlink(fs::path(savedir), recursive=TRUE)
})

test_that("extract_sis_data() runs with some of all possible rdas present", {

  savedir <- fs::path(getwd(), "test-extract_sis_data")
  if(!dir.exists(savedir))
    dir.create(savedir)

  plot_biomass(stockplotr::example_data, module = "TIME_SERIES", interactive = FALSE, make_rda = TRUE, figures_dir=savedir)
  plot_fishing_mortality(stockplotr::example_data, interactive = FALSE, make_rda = TRUE, figures_dir=savedir)
  expect_no_error(extract_sis_data(sis_data_dir=savedir, key_quantities_dir=savedir, figures_tables_dir=savedir))

  # test that extract_sis_data() exports csvs
  expect_true(file.exists(fs::path(savedir, "sis_assmt_template.csv")))
  expect_true(file.exists(fs::path(savedir, "sis_ts_template.csv")))

  # erase temporary testing files
  # file.remove(fs::path(savedir, "sis_assmt_template.csv"))
  # file.remove(fs::path(savedir, "sis_ts_template.csv"))
  # file.remove(fs::path(savedir, "captions_alt_text.csv"))
  # file.remove(fs::path(savedir, "key_quantities.csv"))
  unlink(fs::path(savedir), recursive = TRUE)
})

test_that("extract_sis_data() fails with no rdas present", {
  savedir <- fs::path(getwd(), "test-extract_sis_data")
  if(!dir.exists(savedir))
    dir.create(savedir)
    
  expect_error(extract_sis_data(sis_data_dir=savedir, key_quantities_dir=savedir, figures_tables_dir=savedir))
  unlink(fs::path(savedir), recursive = TRUE)
})
