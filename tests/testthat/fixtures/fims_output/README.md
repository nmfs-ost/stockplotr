# FIMS fixture

`fims_estimates.rds` is a subset of `FIMS::estimates_big_optimized` (the output of
`FIMS::get_estimates()` for the `data_big` example model shipped with FIMS,
NOAA-FIMS/FIMS commit 506b290). Rows kept: every data row (non-missing
`observed` or `likelihood`), all fixed and random effects, and the spawning
biomass, recruitment, biomass, index, and catch derived quantities.
