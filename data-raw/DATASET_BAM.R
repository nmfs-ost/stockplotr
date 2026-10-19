## code to prepare `VermilionSnapper` dataset goes here
# Path to SS3 output file
EX_REPORT_PATH <- file.path("inst", "extdata", "VermilionSnapper.rdat")
# Install stockplotr
pak::local_install(".")
# Convert output
VermilionSnapper <- stockplotr::convert_output(EX_REPORT_PATH)
VermilionSnapper <- subset(VermilionSnapper, year > 1980)
# Save object as rda into raw_data
usethis::use_data(VermilionSnapper, overwrite = TRUE)
