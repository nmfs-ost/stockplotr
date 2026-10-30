test_that("check_data accepts converted-data structures", {
  dat <- data.frame(label = "biomass", estimate = 1)

  expect_no_error(check_data(dat))
  expect_no_error(check_data(tibble::as_tibble(dat)))
  expect_no_error(check_data(example_data))
  expect_no_error(check_data(dat[0, ]))
  expect_identical(dat, data.frame(label = "biomass", estimate = 1))
})

test_that("check_data rejects objects without the required structure", {
  invalid_data <- list(
    NULL,
    1,
    "biomass",
    list(label = "biomass", estimate = 1),
    matrix(c("biomass", "1"), nrow = 1,
           dimnames = list(NULL, c("label", "estimate"))),
    data.frame(),
    data.frame(label = "biomass"),
    data.frame(estimate = 1)
  )

  for (dat in invalid_data) {
    expect_error(
      check_data(dat),
      "dat is not the appropriate format. Have you run `convert_output()`?",
      fixed = TRUE
    )
  }
})

test_that("plotting and table entry points reject invalid data before other work", {
  function_names <- c(
    "save_all_plots",
    "plot_abundance_at_age",
    "plot_biomass",
    "plot_biomass_at_age",
    "plot_catch_comp",
    "plot_discard",
    "plot_fishing_mortality",
    "plot_index",
    "plot_landings",
    "plot_natural_mortality",
    "plot_recruitment",
    "plot_recruitment_deviations",
    "plot_selectivity",
    "plot_spawning_biomass",
    "plot_spawning_potential_ratio",
    "plot_stock_recruitment",
    "table_biomass_abundance_catch",
    "table_index",
    "table_landings",
    "table_projections",
    "table_total_catch"
  )

  for (function_name in function_names) {
    fun <- get(function_name, mode = "function")
    for (dat in list(
      list(label = "biomass", estimate = 1),
      data.frame(label = "biomass"),
      data.frame(estimate = 1)
    )) {
      expect_output(
        expect_message(
          expect_error(
            fun(dat),
            "dat is not the appropriate format. Have you run `convert_output()`?",
            fixed = TRUE
          ),
          NA
        ),
        NA
      )
    }
  }
})
