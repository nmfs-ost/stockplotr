## code to prepare `RCEATTLE DATASET` dataset goes here
# Run Rceattle
install.packages(c("remotes")) # , "readxl"
remotes::install_github("afsc-assessments/Rceattle")
# library(Rceattle)
# library(readxl)
GOApollock <- Rceattle::GOApollock
GOApollock$styr = 1977 # The SAFE model starts at 1970, so change styr to 1970 to run the full time series model (data is in there). I start them all at 1977 because thats the years with overlap.
pollock_model <- Rceattle::fit_mod(
  data_list = GOApollock,
  inits = NULL, # Initial parameters = 0
  file = NULL, # Don't save
  estimateMode = 0, # 0 = Estimate, 1 = Dont run estimation
  random_rec = FALSE, # No random recruitment
  msmMode = 0, # 0 = Single species mode, 1 = MSVPA multi-species mode
  verbose = 1, # Silence optimization output
  phase = TRUE) # Use default phasing
# Install stockplotr
pak::local_install(".")
# Convert output
# OK not to subset bc small
pollock <- stockplotr::convert_output(pollock_model)
# Save object as rda into raw_data -- and document
usethis::use_data(pollock, overwrite = TRUE)
# Overwrite compressed version
save(pollock, file = "data/pollock.rda", compress = "xz", compression_level = 9)
