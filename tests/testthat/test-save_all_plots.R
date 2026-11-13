# expect that the figures are all created with expected names
fig_base_temp_files <- c(
  "abundance_at_age_figure.rda",
  "biomass_figure.rda",
  "fishing_mortality_figure.rda",
  "index_figure.rda",
  "landings_figure.rda",
  "natural_mortality_figure.rda",
  "biomass_at_age_figure.rda",
  "catch_at_age_figure.rda",
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

  savedir <- fs::path(getwd(), "test-save_all_plots")
  if(!dir.exists(savedir))
    dir.create(savedir)

  save_all_plots(
    stockplotr::example_data,
    figures_tables_dir = savedir,
    interactive = FALSE
  )

  # expect that the figures and tables dirs exist
  expect_true(dir.exists(fs::path(savedir, "figures")))
  expect_true(dir.exists(fs::path(savedir, "tables")))

  expect_equal(
    sort(list.files(fs::path(savedir, "figures"))),
    sort(fig_base_temp_files)
  )

  # expect that the tables are all created with expected names
  expect_equal(
    sort(list.files(fs::path(savedir, "tables"))),
    sort(tab_base_temp_files)
  )

  # erase temporary testing files
  unlink(fs::path(savedir), recursive=TRUE)
  
})

test_that("save_all_plots with custom plot options", {
    
    savedir <- fs::path(getwd(), "test-save_all_plots")
    if(!dir.exists(savedir))
      dir.create(savedir)

    
    save_all_plots(
      stockplotr::example_data,
      figures_tables_dir = savedir,
      interactive = FALSE,
      plot_spawning_biomass_options = list(
        ref_line = "target"
      ),
      plot_abundance_at_age_options = list(
        scale_amount = 1000
      )
    )

    # expect that the figures and tables dirs exist
    expect_true(dir.exists(fs::path(savedir, "figures")))
    expect_true(dir.exists(fs::path(savedir, "tables")))

    # expect that the figures are all created with expected names
    
    expect_equal(
      sort(list.files(fs::path(savedir, "figures"))),
      sort(fig_base_temp_files)
    )

    # expect that the tables are all created with expected names
    expect_equal(
      sort(list.files(fs::path(savedir, "tables"))),
      sort(tab_base_temp_files)
    )

    # erase temporary testing files
    unlink(fs::path(savedir), recursive=TRUE)
})

test_that("save_all_plots returns error when plot not made", {

    savedir <- fs::path(getwd(), "test-save_all_plots")
    if(!dir.exists(savedir))
      dir.create(savedir)

    save_all_plots(
      stockplotr::example_data,
      figures_tables_dir = savedir,
      interactive = FALSE,
      plot_natural_mortality_options = list(
        module = NULL
      )
    )

    # expect that the figures and tables dirs exist
    expect_true(dir.exists(fs::path(savedir, "figures")))
    expect_true(dir.exists(fs::path(savedir, "tables")))

    # expect that the figures are all created with expected names
    # no natural_mortality_figure.rda as that throws and error
    # and is not generated in this test.
    expect_equal(
      sort(list.files(fs::path(savedir, "figures"))),
      sort(fig_base_temp_files[which(!grepl("natural_mortality", fig_base_temp_files))])
    )

    # expect that the tables are all created with expected names
    expect_equal(
      sort(list.files(fs::path(savedir, "tables"))),
      sort(tab_base_temp_files)
    )

    # erase temporary testing files
    unlink(fs::path(savedir), recursive=TRUE)
})

test_that("save_all_plots returns error when dat is not valid", {
    savedir <- fs::path(getwd(), "test-save_all_plots")
    if(!dir.exists(savedir))
      dir.create(savedir)

    expect_error(
      save_all_plots(
        r4ss::SS_output(file.path(test_path("fixtures", "ss3_models"), "models", "Hake_2018", "Report.sso")),
        figures_tables_dir = savedir,
        interactive = FALSE,
        plot_natural_mortality_options = list(
          module = NULL
        )
      )
    )

    expect_error(
      save_all_plots(
        data.frame(),
        figures_tables_dir = savedir,
        interactive = FALSE,
        plot_natural_mortality_options = list(
          module = NULL
        )
      )
    )

    expect_error(
      save_all_plots(
        data.frame(label="label"),
        figures_tables_dir = savedir,
        interactive = FALSE,
        plot_natural_mortality_options = list(
          module = NULL
        )
      )
    )
    
    # erase temporary testing files
    unlink(fs::path(savedir), recursive=TRUE)
})
