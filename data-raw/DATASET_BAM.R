## code to prepare `vermilion_snapper` dataset goes here
# Path to SS3 output file
EX_REPORT_PATH <- file.path("inst", "extdata", "VermilionSnapper.rdat")
# Install stockplotr
pak::local_install(".")
# Convert output
vermilion_snapper <- stockplotr::convert_output(EX_REPORT_PATH)
vermilion_snapper <- subset(vermilion_snapper, year > 1980)
# Save object as rda into raw_data
usethis::use_data(vermilion_snapper, overwrite = TRUE)
# Overwrite as compressed data
save(vermilion_snapper, file = "data/vermilion_snapper.rda", compress = "xz", compression_level = 9)
