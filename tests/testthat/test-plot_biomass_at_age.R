# # read in sample dataset
# dat <- asar::convert_output(file = fs::path("fixtures", "ss3_models", "models", "Hake_2018", "Report.sso"),
#                             model = "ss3")
#
# test_that("plot_biomass_at_age generates plots without errors", {
#
#   # expect error-free plot with minimal arguments
#   expect_no_error(
#     stockplotr::plot_biomass_at_age(dat)
#   )
#
#   # expect error-free plot with many arguments
#   expect_no_error(
#     stockplotr::plot_biomass_at_age(
#       dat,
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
#     stockplotr::plot_biomass_at_age(
#       dat,
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
#   stockplotr::plot_biomass_at_age(
#     dat,
#     figures_dir = getwd(),
#     make_rda = TRUE,
#     end_year = 2023
#   )
#
#   # expect that both figures dir and the pop.baa_figure.rda file exist
#   expect_true(dir.exists(fs::path(getwd(), "figures")))
#   expect_true(file.exists(fs::path(getwd(), "figures", "pop.baa_figure.rda")))
#
#   # erase temporary testing files
#   file.remove(fs::path(getwd(), "captions_alt_text.csv"))
#   unlink(fs::path(getwd(), "figures"), recursive = T)
# })
#
# test_that("plot_biomass_at_age generates error with future end_year", {
#
#   # expect error
#   expect_error(
#     stockplotr::plot_biomass_at_age(
#       dat,
#       unit_label = "metric tons",
#       scale_amount = 10,
#       end_year = 2035,
#       make_rda = FALSE,
#       figures_dir = getwd()
#     )
#   )
# })
