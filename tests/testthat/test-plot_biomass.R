# Below is now moot bc relative is coming from model results
# Make another sample dataset for testing relative
# n <- 448
#
# sim_df <- data.frame(
#   label = "biomass",
#   estimate = rlnorm(n, meanlog = 6.2, sdlog = 1.6),
#   module_name = "B.age",
#   time = NA,
#   season = NA,
#   subseason = NA,
#   fleet = NA,
#   sex = NA,
#   area = NA,
#   uncertainty_label = NA,
#   uncertainty = NA,
#   growth_pattern = NA,
#   age = sample(c(0, 1, 2, 3, 4, 5, 6), n, replace = TRUE),
#   era = sample(c("time", "fore"), n, replace = TRUE),
#   year = as.numeric(sample(2000:2024, n, replace = TRUE)),
#   nsim = as.numeric(1:n)
# ) |>
#   dplyr::full_join(data.frame(
#     label = "biomass_msy",
#     estimate = 30000
#   ))


test_that("plot_biomass generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_biomass(
      stockplotr::example_data,
      interactive = FALSE
    )
  )

  # TODO: Update test
  # expect plot with warning message if ref_point not indicated
  # expect_message(
  #   plot_biomass(stockplotr::example_data)
  # )

  # expect error-free plot with many arguments
  expect_no_error(
    plot_biomass(
      stockplotr::example_data,
      ref_line = c("target" = 18000),
      unit_label = "metric tons",
      scale_amount = 1,
      relative = FALSE,
      interactive = FALSE,
      make_rda = FALSE,
      figures_dir = getwd(),
      module = "TIME_SERIES"
    )
  )

  # expect error-free plot when setting relative to T
  expect_no_error(
    plot_biomass(
      stockplotr::example_data,
      unit_label = "mt",
      # scale_amount = 10,
      relative = TRUE,
      interactive = FALSE
    )
  )

  # expect ggplot object is returned
  expect_s3_class(
    plot_biomass(
      stockplotr::example_data,
      unit_label = "metric tons",
      scale_amount = 1,
      interactive = FALSE,
      make_rda = FALSE,
      figures_dir = getwd(),
      module = "TIME_SERIES"
    ),
    "gg"
  )
})

# test_that("rda file made when indicated", {
#   # export rda
#   plot_biomass(
#     stockplotr::example_data,
#     figures_dir = getwd(),
#     interactive = FALSE,
#     make_rda = TRUE,
#     module = "TIME_SERIES"
#   )

#   # expect that both figures dir and the biomass_figure.rda file exist
#   check_and_remove_tmp_files("biomass_figure.rda")
# })
