# Build the Rceattle fixture used by test-convert_output.R.
#
# Source data is `GOAatf`, the Gulf of Alaska arrowtooth flounder dataset that
# ships with Rceattle, so the fixture contains nothing that is not already
# public. It is a single-species, two-sex assessment that converges with a
# positive-definite Hessian, which is what makes it exercise the `sdrep`
# branch of convert_output(). `GOAcod` is the one-sex alternative.
#
# Run from the package root, so test_path() resolves. The estimates depend on
# the Rceattle version, so a rebuild reproduces the fixture's structure and
# the test assertions, not its exact values.

utils::data("GOAatf", package = "Rceattle", envir = environment())

# estimateMode = 1 fits the hindcast only; getsd = TRUE is required because the
# converter reads its time series out of sdrep$value.
full <- Rceattle::fit_mod(
  data_list = GOAatf,
  estimateMode = 1,
  msmMode = 0,
  fit_control = Rceattle::fit_control(
    getsd = TRUE,
    verbose = 0,
    phase = FALSE
  )
)

stopifnot(
  !is.null(full$sdrep),
  isTRUE(full$sdrep$pdHess),
  full$data_list$nspp == 1
)

# A saved Rceattle fit is ~50 MB, nearly all of it the TMB objective-function
# object and the Hessian. Keep only the elements convert_output() reads, which
# brings the fixture to ~38 KB.
data_list_keep <- c(
  "styr", "endyr", "projyr", "nspp", "nsex", "nages", "spnames",
  "index_data", "catch_data", "comp_data", "fleet_control"
)
quantities_keep <- c("index_hat", "catch_hat", "age_hat", "log_index_hat")

fixture <- structure(
  list(
    data_list = full$data_list[intersect(data_list_keep, names(full$data_list))],
    sdrep = list(
      value = full$sdrep$value,
      sd = full$sdrep$sd,
      par.fixed = full$sdrep$par.fixed
    ),
    quantities = full$quantities[intersect(quantities_keep, names(full$quantities))],
    estimated_params = full$estimated_params[
      intersect("index_log_q", names(full$estimated_params))
    ]
  ),
  class = "Rceattle"
)

saveRDS(
  fixture,
  testthat::test_path("fixtures", "rceattle_goa_atf.rds"),
  compress = "xz"
)
