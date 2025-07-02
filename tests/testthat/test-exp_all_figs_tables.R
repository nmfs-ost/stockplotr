# read in sample dataset
dat <- asar::convert_output(
  file = fs::path("fixtures", "ss3_models", "models", "Hake_2018", "Report.sso"),
  model = "ss3"
)

test_that("exp_all_figs_tables works when all figures/tables are plotted", {
  stockplotr::exp_all_figs_tables(
    dat,
    end_year = 2022,
    recruitment_unit_label = "mt",
    recruitment_scale_amount = 1,
    ref_line = "unfished",
    ref_point = 1000,
    ref_line_sb = "target",
    indices_unit_label = "CPUE",
    figures_tables_dir = getwd()
  )

  # expect that the figures and tables dirs exist
  expect_true(dir.exists(fs::path(getwd(), "figures")))
  expect_true(dir.exists(fs::path(getwd(), "tables")))

  # expect that the figures are all created with expected names
  fig_base_temp_files <- c(
    "biomass_figure.rda",
    "landings_figure.rda",
    # "pop.baa_figure.rda",
    # "pop.naa_figure.rda",
    "recruitment.deviations_figure.rda",
    "recruitment_figure.rda",
    "spawning.biomass_figure.rda"
  )
  expect_equal(
    list.files(fs::path(getwd(), "figures")),
    fig_base_temp_files
  )

  # expect that the figures are all created with expected names
  tab_base_temp_files <- c(
    "bnc_table.rda",
    "indices.abundance_table.rda",
    "landings_table.rda"
  )
  expect_equal(
    list.files(fs::path(getwd(), "tables")),
    tab_base_temp_files
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
  unlink(fs::path(getwd(), "tables"), recursive = T)
})

test_that("exp_all_figs_tables works when some figures/tables are not plotted", {
  # plot all figs/tables except for plot_biomass
  stockplotr::exp_all_figs_tables(dat,
    end_year = 2022,
    # add an unreal ref_line so plot_biomass doesn't work
    ref_line = "not_a_real_ref_line",
    ref_line_sb = "target",
    indices_unit_label = "CPUE",
    figures_tables_dir = getwd()
  )

  # expect that the figures and tables dirs exist
  expect_true(dir.exists(fs::path(getwd(), "figures")))
  expect_true(dir.exists(fs::path(getwd(), "tables")))

  # expect that the figures are all created with expected names
  # except for biomass_figure
  fig_base_temp_files <- c(
    # "biomass_figure.rda",
    "landings_figure.rda",
    # "pop.baa_figure.rda",
    # "pop.naa_figure.rda",
    "recruitment.deviations_figure.rda",
    "recruitment_figure.rda",
    "spawning.biomass_figure.rda"
  )
  expect_equal(
    list.files(fs::path(getwd(), "figures")),
    fig_base_temp_files
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
  unlink(fs::path(getwd(), "tables"), recursive = T)
})
