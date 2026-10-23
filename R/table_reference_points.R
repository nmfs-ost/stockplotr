table_reference_points <- function(
    dat,
    unit_label = NULL,
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
  labels <- list(
    # Table name = df name, terminal, average type
    "M" = c("natural_mortality", FALSE, "mean"), # maybe not
    "Unfished Recruitment (R0)" = c("R0|recruitment_unfished", FALSE, "median"),
    "Unfished spawning biomass (units)" = c("spawning_biomass_unfished", FALSE, "mean"),
    "F_{MSY}" = c("F.MSY.terminal", FALSE, "mean"), #indicate what the proxy is through user input?
    "MSY (units)" = c("msy", FALSE, "mean"),
    "SB_{MSY} (units)" = c("sb_msy", FALSE, "mean"), # in kq
    "SPR_{MSY}" = c("spr_msy", FALSE, "mean"),
    # AK does not report MSY -- uses proxy same with NW
    # or a proxy for MSY
    "F_{target}" = c("F.target", FALSE, "mean"), # in kq
    "Terminal F" = c("F.terminal.est", TRUE, "mean"), # in kq
    "Terminal Biomass" = c("B.terminal.est", TRUE, "mean"), # spawning biomass better?; in kq
    "OFL (units)" = c("ofl", FALSE, "mean"),
    "ABC (units)" = c("abc", FALSE, "mean"),
    "Average Recruitment" = c("recruitment", FALSE, "mean"), # mean or medium over timeseries or over last x # of years
    "Exploitation rate" = c("exploitation_rate", TRUE, "mean"), # potentially in addition to F -- sometimes completely different consider
    # "Depletion" = "spawning_biomass", # fraction unfished terminal SB/unfished SB, potentially SBratio in SS3 
    "Overfished" = c("B.terminal.est|B.msy", FALSE, NULL), #Y/N or calc'd?
    "Overfishing" = c("F.terminal.est|F.MSY.terminal", FALSE, NULL) #Y/N or calc'd?
  )
  df_values <- c()
  for (i in seq_along(labels)) {
    kq <- labels[i]
    kq_val <- calc_kqs(
      returned_kq = kq[[1]],
      prepared_data = dat,
      dat = dat,
      final = NULL,
      relative = ifelse(grepl("relative|rel", kq), TRUE, FALSE),
      module = NULL, # is there a way I can set this up?
      terminal = kq[[2]],
      average = kq[[3]]
    )
    kq_val <- setNames(kq_val, kq)
    df_values <- c(df_values, kq_val)
  }
  
}

# Below are ones specifically for NW -- need R&D for other regions
# labels <- c(
#   "Unfished Spawning Output (units)" = "spawning_biomass_unfished",
#   "Unfished Age 4+ Biomass (units)" = "biomass_unfished", # is this value for the fully selected ages + (aka varies by stock) ?
#   "{year} Spawning Ouput (units)" = "spawning_biomass", # @assessment year
#   "Unfished Recruitment (R0)" = "r0",
#   # "{year} Spawning Output (units)" = "spawning_biomass",
#   "{year} Fraction Unfished" = "", # ? not sure of name in output
#   "Reference Points Based SO40%", # ?
#   "Proxy Spawning Output (units) SO40%",
#   "SPR Resulting in SO40%",
#   "Exploitation Rate Resulting in SO40%",
#   "Yield with SPR Based On SO40% (units)",
#   "Reference Points Based on SPR Proxy for MSY",
#   "Proxy Spawning output (units) (SPR50)",
#   "SPR50",
#   "Exploitation Rate Corresponding to SPR50",
#   "Yield with SPR50 at SO SPR (units)",
#   "Reference Points Based on Estimated MSY Values",
#   "Spawning Output (units) at MSY (SO MSY)",
#   "SPR MSY",
#   "Exploitation Rate Corresponding to SPR MSY",
#   "MSY (units)"
# )
# values <- c()
# upper <- c()
# lower <- c()
# 
# # NEFSC ref pts table
# labels <- c(
#   "F_{msy proxy}",
#   "SSB_{msy} (units)",
#   "MSY (units)",
#   "Median recruits (age-0) (units)",
#   "Overfishing", # calc'd quantity
#   "Overfished" # calc'd quantity
# )
# 
# # SEFSC similar table
# labels <- c(
#   "MSST",
#   "MFMT",
#   "F_{MSY}",
#   "MSY",
#   "B_{MSY}",
#   "R_{MSY}",
#   "OY",
#   "F_{OY}",
#   "F_{target}",
#   "Yield at F_{target} (equlibrium)",
#   "M",
#   "Terminal F",
#   "Terminal Biomass",
#   "Exploitation Status (F/MFMT)",
#   "Biomass Status (F/MSST)"
# )
# 
# # AFSC
# labels <- c(
#   "M",
#   # "Tier",
#   # "Projected total (3+) biomass (units)",
#   "Female spawning biomass (units)",
#   "F_{OFL}",
#   "maxF_{ABC}",
#   "F_{ABC}",
#   "OFL (units)",
#   "maxABC (units)",
#   "ABC (units)",
#   "Overfished",
#   "Overfishing"
# )