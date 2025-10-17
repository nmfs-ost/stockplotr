## code to prepare `DATASET` dataset goes here
# Path to SS3 output file
EX_REPORT_PATH <- file.path("inst", "extdata", "Report.sso")
# Install asar
devtools::install_github("nmfs-ost/asar")
# Convert output
example_data <- asar::convert_output(EX_REPORT_PATH)
# Save object as rda into raw_data
usethis::use_data(example_data, overwrite = TRUE)
