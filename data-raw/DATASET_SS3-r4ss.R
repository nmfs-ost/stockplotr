## code to prepare `ss3_simple` dataset goes here
install.packages(c("remotes"))
remotes::install_github("r4ss/r4ss")
# Path to SS3 output file
r4ss_files <- system.file("extdata", "simple_small", package = "r4ss")
EX_REPORT_PATH <- r4ss::SS_output(r4ss_files)
# Install stockplotr
pak::local_install(".")
# Convert output
ss3_simple <- stockplotr::convert_output(EX_REPORT_PATH)
# Save object as rda into raw_data
# replace with example_data once report file is deprecated
usethis::use_data(ss3_simple, overwrite = TRUE)
