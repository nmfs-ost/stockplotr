table_reference_points <- function(
    dat,
    unit_label = "mt",
    module = NULL,
    digits = 2,
    reference = "msy",
    additional_values,
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
  
  # - default to "msy" but allow change ref_line like in plot reference lines
  
  # TODO: add option to add comparison to previous assessment -- input of 2 models
  # OR just make it so they can add any number of models to make comparisons -- intention is year prev and curr but could be sensitivity?
  
  # reference point described as "common conceptual summary metrics" from 2023 pacific hake assessment doc
  
  # First draft labels
  # reference points used for projections and management positions
  labels <- c(
    "M" = "natural_mortality", # maybe not
    "Unfished Recruitment (R0)" = "R0",
    "Unfished spawning biomass (units)" = "spawning_biomass_unfished",
    "F_{MSY}" = "fishing_mortality_msy", #indicate what the proxy is through user input?
    "MSY (units)" = "msy",
    "SB_{MSY} (units)" = "", # in kq
    "SPR_{MSY}",
    # AK does not report MSY -- uses proxy same with NW
    # or a proxy for MSY
    "F_{target}", # in kq
    "Terminal F", # in kq
    "Terminal Biomass", # spawning biomass better?; in kq
    "OFL (units)",
    "ABC (units)",
    "Average Recruitment", # mean or medium over timeseries or over last x # of years
    "Exploitation rate", # potentially in addition to F -- sometimes completely different consider
    "Depletion", # fraction unfished terminal SB/unfished SB, potentially SBratio in SS3 
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
    "Overfishing", # calc'd quantity
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
  for (i in seq_along(labels)) {
    kq <- labels[i]
    calc_kqs()
  }
 
}