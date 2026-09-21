table_reference_points <- function(
    dat,
    unit_label = "mt",
    module = NULL,
    digits = 2,
    scale_amount = 1,
    make_rda = FALSE,
    tables_dir = getwd()
) {
  # Not taking a standard approach to this table
  # Approach
  # - named vector of row for RP with the "value" extracted using extract function
  # - extract fxn pulls the estimate and units
  # - str_replace units with units from extraction
  # - append values in a vector
  # - add to df
  # - df into table + theme_noaa()
  
  # Below are ones specifically for NW -- need R&D for other regions
  labels <- c(
    "Unfished Spawning Output (units)" = "spawning_biomass_unfished",
    "Unfished Age 4+ Biomass (units)" = "biomass_unfished", # is this value for the fully selected ages + (aka varies by stock) ?
    "{year} Spawning Ouput (units)" = "spawning_biomass", # @assessment year
    "Unfished Recruitment (R0)" = "r0",
    # "{year} Spawning Output (units)" = "spawning_biomass",
    "{year} Fraction Unfished" = "", # ? not sure of name in output
    "Reference Points Based SO40%", # ?
    "Proxy Spawning Output (units) SO40%",
    "SPR Resulting in SO40%",
    "Exploitation Rate Resulting in SO40%",
    "Yield with SPR Based On SO40% (units)",
    "Reference Points Based on SPR Proxy for MSY",
    "Proxy Spawning output (units) (SPR50)",
    "SPR50",
    "Exploitation Rate Corresponding to SPR50",
    "Yield with SPR50 at SO SPR (units)",
    "Reference Points Based on Estimated MSY Values",
    "Spawning Output (units) at MSY (SO MSY)",
    "SPR MSY",
    "Exploitation Rate Corresponding to SPR MSY",
    "MSY (units)"
  )
  values <- c()
  upper <- c()
  lower <- c()
  
  extract_value <- function(dat, value) {
    search <- dat |>
      dplyr::filter(grepl(glue::glue("^{value}"), label))
    search_no_yr <- search |> dplyr::filter(is.na(year)) |> dplyr::pull(estimate)
    if (length(search_no_yr) > 1) {
      # check if values are equivalent
      if (length(unique(search_no_yr)) == 1) {
        return(search_no_yr[1])
      } else {
        # check if it can be summarized ?
        search_sum <- search |>
          dplyr::group_by(factors) |>
          dplyr::summarise(est = mean(estimate)) 
      }
    } else if (length(search_no_yr) == 0) {
      return("-")
    } else {
      # return value
      search_no_yr
    }
  }
}