test_that("exp_all_figs_tables works when all figures/tables are plotted", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  stockplotr::exp_all_figs_tables(
    dat,
    end_year = 2022,
    n_projected_years = 3,
    recruitment_unit_label = "mt",
    recruitment_scale_amount = 1,
    ref_line = "unfished",
    ref_point = 1000,
    ref_line_sb = "target",
    indices_unit_label = "CPUE",
    rda_dir = getwd()
  )

  # expect that the rda_files dir exists
  expect_true(dir.exists(fs::path(getwd(), "rda_files")))

  # expect that the rda_files are all created with expected names
  base_temp_files <- c(
    "biomass_figure.rda",
    "bnc_table.rda",
    "indices.abundance_table.rda",
    "landings_figure.rda",
    "pop.baa_figure.rda",
    "recruitment.deviations_figure.rda",
    "recruitment_figure.rda",
    "spawning.biomass_figure.rda"
  )
  expect_equal(
    list.files(fs::path(getwd(), "rda_files")),
    base_temp_files
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "rda_files"), recursive = T)
})

test_that("exp_all_figs_tables works when some figures/tables are not plotted", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file("resources", "sample_data", "petrale_sole-after_2020.csv", package = "stockplotr")
  )

  # plot all figs/tables except for plot_biomass
  stockplotr::exp_all_figs_tables(dat,
    end_year = 2022,
    # add an unreal ref_line so plot_biomass doesn't work
    ref_line = "not_a_real_ref_line",
    ref_line_sb = "target",
    indices_unit_label = "CPUE",
    rda_dir = getwd()
  )

  # expect that the rda_files dir exists
  expect_true(dir.exists(fs::path(getwd(), "rda_files")))

  # expect that the rda_files are all created with expected names
  # except for biomass_figure
  base_temp_files <- c(
    # "biomass_figure.rda",
    "bnc_table.rda",
    "indices.abundance_table.rda",
    "landings_figure.rda",
    "pop.baa_figure.rda",
    "recruitment.deviations_figure.rda",
    "recruitment_figure.rda",
    "spawning.biomass_figure.rda"
  )
  expect_equal(
    list.files(fs::path(getwd(), "rda_files")),
    base_temp_files
  )

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "rda_files"), recursive = T)
})
