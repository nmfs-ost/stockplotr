# load sample dataset
load(file.path(
  "fixtures", "ss3_models_converted", "Hake_2018",
  "std_output.rda"
))

test_that("plot_recruitment generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_recruitment(out_new,
                     module = "TIME_SERIES"
    )
  )

  # expect error-free plot with many arguments
  expect_no_error(
    plot_recruitment(
      out_new,
      unit_label = "mt",
      scale_amount = 10,
      module = "TIME_SERIES",
      # relative = FALSE,
      make_rda = FALSE,
      figures_dir = getwd()
    )
  )

  # expect error-free plot when setting relative to T
  # Relative is no longer an option
  # expect_no_error(
  #   plot_recruitment(
  #     out_new,
  #     unit_label = "mt",
  #     scale_amount = 10,
  #     # relative = TRUE,
  #     module = "TIME_SERIES",
  #     relative = T,
  #     make_rda = FALSE,
  #     figures_dir = getwd()
  #   )
  # )

  # expect ggplot object is returned
  expect_s3_class(
    plot_recruitment(
      out_new,
      unit_label = "mt",
      scale_amount = 10,
      # relative = TRUE,
      module = "TIME_SERIES",
      make_rda = FALSE,
      figures_dir = getwd()
    ),
    "gg"
  )
})

test_that("rda file made when indicated", {
  # export rda
  plot_recruitment(
    out_new,
    module = "TIME_SERIES",
    figures_dir = getwd(),
    make_rda = TRUE
  )

  # expect that both figures dir and the recruitment_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "figures")))
  expect_true(file.exists(fs::path(getwd(), "figures", "recruitment_figure.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})
