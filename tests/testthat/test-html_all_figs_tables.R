# load sample dataset
load(file.path(
  "fixtures", "ss3_models_converted", "Hake_2018",
  "std_output.rda"
))

test_that("html_all_figs_tables makes qmd and html files with default figures_tables_dir argument", {
  stockplotr::exp_all_figs_tables(
    out_new,
    end_year = 2024,
    recruitment_unit_label = "mt",
    recruitment_scale_amount = 1,
    ref_line = "unfished",
    ref_point = 1000,
    ref_line_sb = "msy",
    indices_unit_label = "CPUE",
    figures_tables_dir = getwd()
  ) |>
    suppressWarnings()


  html_all_figs_tables()

  testthat::expect(file.exists(fs::path(getwd(), "all_tables_figures", "all_tables_figures.qmd")),
    failure_message = "Test fail: 'all_tables_figures.qmd' does not exist"
  )
  testthat::expect(file.exists(fs::path(getwd(), "all_tables_figures", "all_tables_figures.html")),
    failure_message = "Test fail: 'all_tables_figures.html' does not exist"
  )

  # erase temporary testing files
  unlink(fs::path(getwd(), "all_tables_figures"), recursive = T)
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "08_tables.qmd"))
  file.remove(fs::path(getwd(), "09_figures.qmd"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
  unlink(fs::path(getwd(), "tables"), recursive = T)
})

test_that("html_all_figs_tables makes qmd and html files with non-default figures_tables_dir argument", {
  rda_path <- fs::path(getwd(), "data")
  dir.create(rda_path)

  withr::with_dir(
    rda_path,
    stockplotr::exp_all_figs_tables(
      out_new,
      end_year = 2024,
      recruitment_unit_label = "mt",
      recruitment_scale_amount = 1,
      ref_line = "unfished",
      ref_point = 1000,
      ref_line_sb = "msy",
      indices_unit_label = "CPUE",
      figures_tables_dir = getwd()
    ) |>
      suppressWarnings()
  )

  html_all_figs_tables(figures_tables_dir = rda_path)

  testthat::expect(file.exists(fs::path(rda_path, "all_tables_figures", "all_tables_figures.qmd")),
    failure_message = "Test fail: 'all_tables_figures.qmd' does not exist"
  )
  testthat::expect(file.exists(fs::path(rda_path, "all_tables_figures", "all_tables_figures.html")),
    failure_message = "Test fail: 'all_tables_figures.html' does not exist"
  )

  # erase temporary testing files
  # unlink(fs::path(getwd(), "all_tables_figures"), recursive = T)
  # unlink(fs::path(getwd(), "figures_tables"), recursive = T)
  unlink(fs::path(getwd(), "data"), recursive = T)
})

test_that("html_all_figs_tables triggers message (question) when all_tables_figures folder already present", {
  stockplotr::exp_all_figs_tables(
    out_new,
    end_year = 2024,
    recruitment_unit_label = "mt",
    recruitment_scale_amount = 1,
    ref_line = "unfished",
    ref_point = 1000,
    ref_line_sb = "msy",
    indices_unit_label = "CPUE",
    figures_tables_dir = getwd()
  ) |>
    suppressWarnings()

  dir.create(fs::path(getwd(), "all_tables_figures"))

  testthat::expect_warning(html_all_figs_tables())

  # erase temporary testing files
  unlink(fs::path(getwd(), "all_tables_figures"), recursive = T)
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "08_tables.qmd"))
  file.remove(fs::path(getwd(), "09_figures.qmd"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
  unlink(fs::path(getwd(), "tables"), recursive = T)
})
