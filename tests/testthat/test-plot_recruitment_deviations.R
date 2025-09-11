# load sample dataset
load(file.path(
  "fixtures", "ss3_models_converted", "Hake_2018",
  "std_output.rda"
))

test_that("plot_recruitment_deviations generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_recruitment_deviations(out_new,
      module = "SPAWN_RECRUIT")
  )

  # expect error-free plot with many arguments
  expect_no_error(
    plot_recruitment_deviations(
      out_new,
      module = "SPAWN_RECRUIT",
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    plot_recruitment_deviations(
      out_new,
      module = "SPAWN_RECRUIT",
      make_rda = FALSE,
      figures_dir = getwd()
    ),
    "gg"
  )
})

test_that("rda file made when indicated", {
  # export rda
  plot_recruitment_deviations(
    out_new,
    module = "SPAWN_RECRUIT",
    make_rda = TRUE,
    figures_dir = getwd()
  )

  # expect that both figures dir and the recruitment.deviations_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "figures")))
  expect_true(file.exists(fs::path(getwd(), "figures", "recruitment.deviations_figure.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})

