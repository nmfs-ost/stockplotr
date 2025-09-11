# # load sample dataset
# load(file.path(
#   "fixtures", "ss3_models_converted", "Hake_2018",
#   "std_output.rda"
# ))

# test_that("table_bnc generates plots without errors", {
#   # expect error-free plot with minimal arguments
#   expect_no_error(
#     stockplotr::table_bnc(out_new,
#       end_year = 2022
#     )
#   )

#   # expect error-free plot with many arguments
#   expect_no_error(
#     stockplotr::table_bnc(
#       out_new,
#       end_year = 2025,
#       biomass_unit_label = "mt",
#       catch_unit_label = "mt",
#       sb_unit_label = "mt",
#       make_rda = FALSE,
#       tables_dir = getwd()
#     )
#   )


#   # expect flextable object is returned
#   expect_s3_class(
#     stockplotr::table_bnc(
#       out_new,
#       end_year = 2025,
#       biomass_unit_label = "mt",
#       catch_unit_label = "mt",
#       sb_unit_label = "mt",
#       make_rda = FALSE,
#       tables_dir = getwd()
#     ),
#     "flextable"
#   )
# })

# test_that("rda file made when indicated", {
#   # export rda
#   table_bnc(
#     out_new,
#     end_year = 2025,
#     biomass_unit_label = "mt",
#     catch_unit_label = "mt",
#     sb_unit_label = "mt",
#     make_rda = TRUE,
#     tables_dir = getwd()
#   )

#   # expect that both tables dir and the bnc_table.rda file exist
#   expect_true(dir.exists(fs::path(getwd(), "tables")))
#   expect_true(file.exists(fs::path(getwd(), "tables", "bnc_table.rda")))

#   # erase temporary testing files
#   file.remove(fs::path(getwd(), "captions_alt_text.csv"))
#   unlink(fs::path(getwd(), "tables"), recursive = T)
# })

# test_that("table_bnc generates error with future end_year", {
#   # expect error
#   expect_error(
#     stockplotr::table_bnc(
#       out_new,
#       end_year = 2035,
#       biomass_unit_label = "mt",
#       catch_unit_label = "mt",
#       sb_unit_label = "mt",
#       make_rda = TRUE,
#       tables_dir = getwd()
#     )
#   )
# })
