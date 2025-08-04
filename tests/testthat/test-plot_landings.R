# load sample dataset
load(file.path(
  "fixtures", "ss3_models_converted", "Hake_2018",
  "std_output.rda"
))

test_that("plot_landings generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_landings(out_new)
  )


  # expect ggplot object is returned
  expect_s3_class(
    plot_landings(out_new),
    "gg"
  )
})

test_that("rda file made when indicated", {
  # export rda
  plot_landings(out_new,
    end_year = 2024,
    make_rda = TRUE,
    unit_label = "metric tons",
    figures_dir = getwd()
  )

  # expect that both figures dir and the landings_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "figures")))
  expect_true(file.exists(fs::path(getwd(), "figures", "landings_figure.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})

test_that("plot_landings generates error with future end_year", {
  # expect error
  expect_error(
    plot_landings(out_new,
      end_year = 2035,
      make_rda = TRUE,
      unit_label = "metric tons",
      figures_dir = getwd()
    )
  )
})
