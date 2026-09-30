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
  
  # TODO: add option to add comparison to previous assessment -- input of 2 models
  # OR just make it so they can add any number of models to make comparisons -- intention is year prev and curr but could be sensitivity?
  
  # reference point described as "common conceptual summary metrics" from 2023 pacific hake assessment doc
  
  # First draft labels
  labels <- c(
    "M",
    "Unfished Recruitment (R0)",
    "Unfished spawning biomass (units)",
    "F_{MSY}", #indicate what the proxy is through user input?
    "MSY (units)",
    "SB_{MSY} (units)",
    "SPR_{MSY}",
    "F_{target}",
    "Terminal F",
    "Terminal Biomass", # spawning biomass better?
    "OFL (units)",
    "ABC (units)",
    "Overfished", #Y/N or calc'd?
    "Overfishing" #Y/N or calc'd?
  )
  
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
  
  # NEFSC ref pts table
  labels <- c(
    "F_{msy proxy}",
    "SSB_{msy} (units)",
    "MSY (units)",
    "Median recruits (age-0) (units)",
    "overfishing", # calc'd quantity
    "Overfished" # calc'd quantity
  )
  
  # SEFSC similar table
  labels <- c(
    "MSST",
    "MFMT",
    "F_{MSY}",
    "MSY",
    "B_{MSY}",
    "R_{MSY}",
    "OY",
    "F_{OY}",
    "F_{target}",
    "Yield at F_{target} (equlibrium)",
    "M",
    "Terminal F",
    "Terminal Biomass",
    "Exploitation Status (F/MFMT)",
    "Biomass Status (F/MSST)"
  )
  
  # AFSC
  labels <- c(
    "M",
    # "Tier",
    # "Projected total (3+) biomass (units)",
    "Female spawning biomass (units)",
    "F_{OFL}",
    "maxF_{ABC}",
    "F_{ABC}",
    "OFL (units)",
    "maxABC (units)",
    "ABC (units)",
    "Overfished",
    "Overfishing"
  )
  
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