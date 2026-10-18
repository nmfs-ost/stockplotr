## Code to prepare `bigskate_lencomp` and `bigskate_caal`
# Composition data and fits from the 2019 Big Skate stock assessment
# (BigSkate_2019 in the Stock Synthesis test models), the model used for the
# r4ss example plots at https://r4ss.github.io/r4ss/BigSkate/.
#
# The test model includes Report.sso but not CompReport.sso, so the model is
# rerun at its maximum likelihood estimates (from ss3.par, without
# estimation) to write the composition database, then read with
# r4ss::SS_output().

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

# Reshape r4ss composition databases to stockplotr's snake_case columns
tidy_dbase <- function(dbase, comp_type, fleet_names) {
  out <- tibble::tibble(
    comp_type = comp_type,
    fleet = fleet_names[dbase[["Fleet"]]],
    year = dbase[["Yr"]],
    month = dbase[["Month"]],
    # r4ss's lowercase "sex" (0 = unknown, 1 = female, 2 = male), not the
    # Stock Synthesis "Sex" code, which is 1 for male-only compositions
    sex = c("1" = "female", "2" = "male", "0" = "combined")[as.character(dbase[["sex"]])],
    part = dbase[["Part"]],
    length_bin = dbase[["Lbin_lo"]],
    bin = dbase[["Bin"]],
    observed = dbase[["Obs"]],
    expected = dbase[["Exp"]],
    pearson = dbase[["Pearson"]],
    n = dbase[["Nsamp_adj"]]
  )
  if (comp_type == "length") out[["length_bin"]] <- NULL
  # Single-sex male compositions are reported twice in the composition
  # database (once for each internal sex code) with identical values
  dplyr::distinct(out)
}

bigskate_lencomp <- tidy_dbase(replist[["lendbase"]], "length", replist[["FleetNames"]])
bigskate_caal <- tidy_dbase(replist[["condbase"]], "age", replist[["FleetNames"]])

usethis::use_data(bigskate_lencomp, bigskate_caal, overwrite = TRUE)
