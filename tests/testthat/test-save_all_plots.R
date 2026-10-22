# expect that the figures are all created with expected names
fig_base_temp_files <- c(
  "abundance_at_age_figure.rda",
  "biomass_figure.rda",
  "fishing_mortality_figure.rda",
  "index_figure.rda",
  "landings_figure.rda",
  "natural_mortality_figure.rda",
  "biomass_at_age_figure.rda",
  "catch_comp_figure.rda",
  "recruitment_deviations_figure.rda",
  "recruitment_figure.rda",
  "selectivity_figure.rda",
  "spawning_biomass_figure.rda",
  "spr_figure.rda",
  "stock_recruitment_figure.rda",
  "mod.fit.discards_figure.rda"
)

tab_base_temp_files <- c(
    "biomass_abundance_catch_table.rda",
    "total_catch_table.rda", # comment out bc as is this needs to be interactive to select correct module
    "index_table.rda",
    "landings_table.rda",
    "projections_table.rda"
  )

test_that("save_all_plots works when all figures/tables are plotted", {
  save_all_plots(
    stockplotr::example_data,
    figures_tables_dir = getwd(),
    interactive = FALSE
  )

  # expect that the figures and tables dirs exist
  expect_true(dir.exists(fs::path(getwd(), "figures")))
  expect_true(dir.exists(fs::path(getwd(), "tables")))

  expect_equal(
    sort(list.files(fs::path(getwd(), "figures"))),
    sort(fig_base_temp_files)
  )

  # expect that the tables are all created with expected names
  expect_equal(
    sort(list.files(fs::path(getwd(), "tables"))),
    sort(tab_base_temp_files)
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  file.remove(fs::path(getwd(), "key_quantities.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
  unlink(fs::path(getwd(), "tables"), recursive = T)
})

test_that("save_all_plots with custom plot options", {
    save_all_plots(
      stockplotr::example_data,
      figures_tables_dir = getwd(),
      interactive = FALSE,
      plot_spawning_biomass_options = list(
        ref_line = "target"
      ),
      plot_abundance_at_age_options = list(
        scale_amount = 1000
      )
    )

    # expect that the figures and tables dirs exist
    expect_true(dir.exists(fs::path(getwd(), "figures")))
    expect_true(dir.exists(fs::path(getwd(), "tables")))

    # expect that the figures are all created with expected names
    
    expect_equal(
      sort(list.files(fs::path(getwd(), "figures"))),
      sort(fig_base_temp_files)
    )

    # expect that the tables are all created with expected names
    expect_equal(
      sort(list.files(fs::path(getwd(), "tables"))),
      sort(tab_base_temp_files)
    )

    # erase temporary testing files
    file.remove(fs::path(getwd(), "captions_alt_text.csv"))
    file.remove(fs::path(getwd(), "key_quantities.csv"))
    unlink(fs::path(getwd(), "figures"), recursive = T)
    unlink(fs::path(getwd(), "tables"), recursive = T)
})

test_that("save_all_plots returns error when plot not made", {
    save_all_plots(
      stockplotr::example_data,
      figures_tables_dir = getwd(),
      interactive = FALSE,
      plot_natural_mortality_options = list(
        module = NULL
      )
    )

    # expect that the figures and tables dirs exist
    expect_true(dir.exists(fs::path(getwd(), "figures")))
    expect_true(dir.exists(fs::path(getwd(), "tables")))

    # expect that the figures are all created with expected names
    # no natural_mortality_figure.rda as that throws and error
    # and is not generated in this test.
    expect_equal(
      sort(list.files(fs::path(getwd(), "figures"))),
      sort(fig_base_temp_files[which(!grepl("natural_mortality", fig_base_temp_files))])
    )

    # expect that the tables are all created with expected names
    expect_equal(
      sort(list.files(fs::path(getwd(), "tables"))),
      sort(tab_base_temp_files)
    )

    # erase temporary testing files
    file.remove(fs::path(getwd(), "captions_alt_text.csv"))
    file.remove(fs::path(getwd(), "key_quantities.csv"))
    unlink(fs::path(getwd(), "figures"), recursive = T)
    unlink(fs::path(getwd(), "tables"), recursive = T)
})
