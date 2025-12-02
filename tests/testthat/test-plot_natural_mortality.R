# load sample dataset
load(file.path(
  "fixtures", "ss3_models_converted", "Hake_2018",
  "std_output.rda"
))

test_that("plot_natural_mortality generate without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_natural_mortality(out_new, module = "Natural_Mortality")
  )

  # expect eror when not interactive since it's choosing the first module in ex
  expect_error(
    plot_natural_mortality(out_new)
  )

  # expect ggplot object is returned
  expect_s3_class(
    plot_natural_mortality(
      out_new,
      module = "Natural_Mortality"
    ),
    "gg"
  )
})

test_that("rda file made when indicated", {
  # export rda
  plot_natural_mortality(
    out_new,
    module = "Natural_Mortality",
    make_rda = TRUE,
    figures_dir = getwd()
  )

  # expect that both figures dir and the spawning.biomass_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "figures")))
  expect_true(file.exists(fs::path(getwd(), "figures", "natural.mortality_figure.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})
