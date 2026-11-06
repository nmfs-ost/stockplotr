## Code to prepare `bigskate_lencomp` and `bigskate_caal`
# Composition data and fits from the 2019 Big Skate stock assessment
# (BigSkate_2019 in the Stock Synthesis test models), the model used for the
# r4ss example plots at https://r4ss.github.io/r4ss/BigSkate/.
#
# The test model includes Report.sso but not CompReport.sso, so the model is
# rerun at its maximum likelihood estimates (from ss3.par, without
# estimation) to write the composition database, then read with
# r4ss::SS_output() and converted with convert_output().

model_dir <- file.path(tempdir(), "bigskate")
dir.create(model_dir, showWarnings = FALSE)

# Get the model and the Stock Synthesis executable
r4ss::download_models(dir = model_dir)
bigskate_dir <- file.path(model_dir, "models", "BigSkate_2019")
r4ss::get_ss3_exe(dir = bigskate_dir)

# Start from the estimated parameters in ss3.par and do not estimate
starter <- r4ss::SS_readstarter(file.path(bigskate_dir, "starter.ss"), verbose = FALSE)
starter[["init_values_src"]] <- 1
r4ss::SS_writestarter(starter, dir = bigskate_dir, overwrite = TRUE, verbose = FALSE)
r4ss::run(dir = bigskate_dir, extras = "-maxfn 0 -phase 50 -nohess", exe = "ss3", skipfinished = FALSE)

replist <- r4ss::SS_output(
  bigskate_dir,
  verbose = FALSE, printstats = FALSE, covar = FALSE, forecast = FALSE
)

# Convert to stockplotr's standard output and keep the composition data:
# length compositions (lendbase) and conditional age-at-length data (condbase)
std_output <- stockplotr::convert_output(replist, model = "ss3")
bigskate_lencomp <- dplyr::filter(std_output, module_name == "lendbase")
bigskate_caal <- dplyr::filter(std_output, module_name == "condbase")

usethis::use_data(bigskate_lencomp, bigskate_caal, overwrite = TRUE)
