# load sample dataset
load(file.path(
  "fixtures", "ss3_models_converted", "Hake_2018",
  "std_output.rda"
))

# TODO: Fix tests
# test_that("plot_abundance_at_age generates plots without errors", {
#
#   # expect error-free plot with minimal arguments
#   expect_no_error(
#     plot_abundance_at_age(out_new)
#   )
#
#   # expect error-free plot with many arguments
#   expect_no_error(
#     plot_abundance_at_age(
#       out_new,
#       unit_label = "metric tons",
#       scale_amount = 10,
#       end_year = 2024,
#       make_rda = FALSE,
#       figures_dir = getwd()
#     )
#   )
#
#   # expect ggplot object is returned
#   expect_s3_class(
#     plot_abundance_at_age(
#       out_new,
#       unit_label = "metric tons",
#       scale_amount = 1,
#       end_year = 2024,
#       make_rda = FALSE,
#       figures_dir = getwd()
#     ),
#     "gg"
#   )
# })
#
# test_that("rda file made when indicated", {
#
#   # export rda
#   plot_abundance_at_age(
#     out_new,
#     figures_dir = getwd(),
#     make_rda = TRUE,
#     end_year = 2023
#   )
#
#   # expect that both figures dir and the pop.naa_figure.rda file exist
#   expect_true(dir.exists(fs::path(getwd(), "figures")))
#   expect_true(file.exists(fs::path(getwd(), "figures", "pop.naa_figure.rda")))
#
#   # erase temporary testing files
#   file.remove(fs::path(getwd(), "captions_alt_text.csv"))
#   unlink(fs::path(getwd(), "figures"), recursive = T)
# })
#
# test_that("plot_abundance_at_age generates error with future end_year", {
#
#   # expect error
#   expect_error(
#     plot_abundance_at_age(
#       out_new,
#       unit_label = "metric tons",
#       scale_amount = 10,
#       end_year = 2035,
#       make_rda = FALSE,
#       figures_dir = getwd()
#     )
#   )
# })
