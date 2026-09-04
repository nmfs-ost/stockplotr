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
  # Below are ones specifically for NW -- need R&D for other regions
  labels <- c(
    "Unfished Spawning Output ({units})",
    "Unfished Age 4+ Biomass ({units})",
    "{Year} Spawning Ouput ({units})",
    "Unfished Recruitment (R0)",
    "{Year} Spawning Output ({units})",
    "{year} Fraction Unfished",
    "Reference Points Based SO40%", # ?
    "Proxy Spawning Output ({units}) SO40%",
    "SPR Resulting in SO40%",
    "Exploitation Rate Resulting in SO40%",
    "Yield with SPR Based On SO40% ({units})",
    "Reference Points Based on SPR Proxy for MSY",
    "Proxy Spawning output ({units}) (SPR50)",
    "SPR50",
    "Exploitation Rate Corresponding to SPR50",
    "Yield with SPR50 at SO SPR ({units})",
    "Reference Points Based on Estimated MSY Values",
    "Spawning Output ({units}) at MSY (SO MSY)",
    "SPR MSY",
    "Exploitation Rate Corresponding to SPR MSY",
    "MSY ({units})"
  )
  values <- c()
  upper <- c()
  lower <- c()
}