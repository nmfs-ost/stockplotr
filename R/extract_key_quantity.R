#' Extract key quantities
#'
#' @inheritParams write_captions
#' @param kq Key quantity that will be extracted. Possibilities include:
#'   \itemize{
#'     \item{`B.start.year`: start year of biomass plot}
#'     \item{`F.start.year`: start year of F plot}
#'     \item{`F.min`: minimum F}
#'     \item{`F.max`: maximum F}
#'     \item{`landings.start.year`: start year of landings plot}
#'     \item{`landings.end.year`: end year of landings plot}
#'     \item{`landings.min`: minimum landings}
#'     \item{`landings.max`: maximum landings}
#'     \item{`M.age.min`: minimum age of M}
#'     \item{`M.age.max`: maximum age of M}
#'     \item{`caa.start.year`: start year of CAA plot}
#'     \item{`caa.end.year`: end year of CAA plot}
#'     \item{`caa.age.min`: minimum CAA age}
#'     \item{`caa.age.max`: maximum CAA age}
#'     \item{`tot.catch.min`: catch composition minimum catch}
#'     \item{`tot.catch.max`: catch composition maximum catch}
#'     \item{`pop.naa.start.year`: start year of NAA plot}
#'     \item{`pop.naa.end.year`: end year of NAA plot}
#'     \item{`pop.naa.age.min`: minimum age of NAA plot}
#'     \item{`pop.naa.age.max`: maximum age of NAA plot}
#'     \item{`pop.naa.fish.min`: minimum abundance (number) of fish of NAA plot}
#'     \item{`pop.naa.fish.max`: maximum abundance (number) of fish of NAA plot}
#'     \item{`sr.age.min`: youngest-age recruited fish of estimated stock recruitment plot}
#'     \item{`recruitment.start.year`: start year of recruitment plot}
#'     \item{`recruit.dev.start.year`: end year of recruitment plot}
#'     \item{`recruit.dev.min`: minimum recruitment deviation}
#'     \item{`recruit.dev.max`: maximum recruitment deviation}
#'     \item{`ssb.start.year`: start year of spawning biomass plot}
#'     \item{`spr.min`: minimum spawning potential ratio}
#'     \item{`spr.max`: maximum spawning potential ratio}
#'     \item{`pop.baa.start.year`: start year of biomass at age plot}
#'     \item{`pop.baa.end.year`: end year of biomass at age plot}
#'     \item{`pop.baa.fish.min`: minimum biomass of biomass at age plot}
#'     \item{`pop.baa.fish.max`: maximum biomass of biomass at age plot}
#'     \item{`pop.baa.age.min`: minimum age of biomass at age plot}
#'     \item{`pop.baa.age.max`: maximum age of biomass at age plot}
#'     \item{`proj.catch.end.year`: end year of projected catch plot}
#'     \item{`proj.catch.min`: minimum projected catch}
#'     \item{`proj.catch.max`: maximum projected catch}
#'     }
#' @return A string containing a value for a chosen key quantity.
#' @export
#'
#' @examples
#' extract_key_quantity(
#'   dat = stockplotr:::example_data,
#'   kq = "B.start.year"
#' )
extract_key_quantity <- function(
    dat,
    kq 
    ) {
  
  # start year of biomass plot
  if (kq == "B.start.year"){
    value <- dat |>
      dplyr::filter(
        label == "biomass",
        module_name == "TIME_SERIES" | module_name == "t.series", # SS3 and BAM target module names
        is.na(fleet),
        is.na(age)
      ) |>
      dplyr::slice(which.min(year)) |>
      dplyr::select(year) |>
      as.numeric()    
  }
  
  # start year of F plot
  if (kq == "F.start.year"){
    F.start.year <- dat |>
      dplyr::filter(label == "fishing_mortality") |>
      dplyr::slice(which.min(year)) |>
      dplyr::select(year) |>
      as.numeric()
  }
  
  # minimum F
  if (kq == "F.min"){
    F.min <- dat |>
      dplyr::filter(label == "fishing_mortality")
    
    if (length(unique(F.min$age)) == 1) {
      if (is.na(unique(F.min$age))) {
        F.min <- F.min |>
          dplyr::filter(module_name %in% c("TIME_SERIES", "t.series")) |>
          dplyr::slice(which.min(estimate)) |>
          as.numeric() |>
          round(digits = 2)
      }
    } else {
      F.min <- F.min |>
        dplyr::group_by(age) |>
        dplyr::summarize(val = max(estimate)) |>
        dplyr::slice(which.min(val)) |>
        dplyr::select(val) |>
        as.numeric() |>
        round(digits = 2)
    }
  }
  
  # maximum F
  if (kq == "F.max"){
    F.max <- dat |>
      dplyr::filter(label == "fishing_mortality")
    
    if (length(unique(F.max$age)) == 1) {
      if (is.na(unique(F.max$age))) {
        F.min <- F.min |>
          dplyr::filter(module_name %in% c("TIME_SERIES", "t.series")) |>
          dplyr::slice(which.max(estimate)) |>
          as.numeric() |>
          round(digits = 2)
      }
    } else {
      F.max <- F.max |>
        dplyr::group_by(age) |>
        dplyr::summarize(val = max(estimate)) |>
        dplyr::slice(which.max(val)) |>
        dplyr::select(val) |>
        as.numeric() |>
        round(digits = 2)
    }
  }
  
  # start year of landings plot
  if (kq == "landings.start.year"){
    landings.start.year <- dat |>
    dplyr::filter(
      c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
      # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
      !is.na(fleet)
    ) |>
    dplyr::slice(which.min(year)) |>
    dplyr::select(year) |>
    as.numeric()
  }
  
  # end year of landings plot
  if (kq == "landings.end.year"){
    landings.end.year <- dat |>
      dplyr::filter(
        c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
        # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
        !is.na(fleet)
      ) |>
      dplyr::slice(which.max(year)) |>
      dplyr::select(year) |>
      as.numeric()
  }
  
  # minimum landings
  if (kq == "landings.min"){
    landings.min <- dat |>
      dplyr::filter(
        c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
        # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
        !is.na(fleet)
      ) |>
      dplyr::slice(which.min(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric() |>
      round(digits = 2)
  }
  
  # maximum landings
  if (kq == "landings.max"){
    landings.max <- dat |>
      dplyr::filter(
        c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
        # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
        !is.na(fleet)
      ) |>
      dplyr::group_by(fleet, year) |>
      dplyr::summarise(max_est = max(estimate)) |>
      dplyr::filter(!is.na(max_est)) |>
      dplyr::group_by(year) |>
      dplyr::summarise(max_est_yr = sum(max_est)) |>
      dplyr::slice(which.max(max_est_yr)) |>
      dplyr::select(max_est_yr) |>
      as.numeric() |>
      round(digits = 2)
  }
  
  ## natural mortality (M)- bam examples have label as natural_mortality
  ## but other formats don't (in input)
  # minimum age of M
  if (kq == "M.age.min"){
    if ("natural_mortality" %in% dat$label) {
      M.age.min <- dat |>
        dplyr::filter(label == "natural_mortality") |>
        dplyr::select(age) |>
        dplyr::filter(!is.na(age)) |>
        dplyr::slice(which.min(age)) |>
        as.numeric()
    } else {
      M.age.min <- dat |>
        #  dplyr::filter(label == "natural_mortality") |>
        dplyr::select(age) |>
        dplyr::filter(!is.na(age)) |>
        dplyr::slice(which.min(age)) |>
        as.numeric()
    }
  }
  
  if (kq == "M.age.max"){
    if ("natural_mortality" %in% dat$label) {
      M.age.max <- dat |>
        dplyr::filter(label == "natural_mortality") |>
        dplyr::select(age) |>
        dplyr::filter(!is.na(age)) |>
        dplyr::slice(which.max(age)) |>
        as.numeric()
    } else {
      M.age.max <- dat |>
        #  dplyr::filter(label == "natural_mortality") |>
        dplyr::select(age) |>
        dplyr::filter(!is.na(age)) |>
        dplyr::slice(which.max(age)) |>
        as.numeric()
    }
  }
  
  # start year of CAA plot
  if (kq == "caa.start.year"){
    caa.start.year <- dat |>
      dplyr::filter(label == "abundance" & !is.na(year)) |>
      dplyr::slice(which.min(year)) |>
      dplyr::select(year) |>
      as.numeric()
  }
  
  # end year of CAA plot
  if (kq == "caa.end.year"){
    caa.end.year <- dat |>
      dplyr::filter(label == "abundance" & !is.na(year)) |>
      dplyr::slice(which.max(year)) |>
      dplyr::select(year) |>
      as.numeric()
  }
  
  # minimum CAA age
  if (kq == "caa.age.min"){
    caa.age.min <- dat |>
      dplyr::filter(label == "abundance" & !is.na(year)) |>
      dplyr::slice(which.min(age)) |>
      dplyr::select(age) |>
      as.numeric()
  }
  
  # maximum CAA age
  if (kq == "caa.age.max"){
    caa.age.max <- dat |>
      dplyr::filter(label == "abundance" & !is.na(year)) |>
      dplyr::slice(which.max(age)) |>
      dplyr::select(age) |>
      as.numeric()
  }
  
  ## catch composition
  # minimum catch  
  if (kq == "tot.catch.min"){
    if (dim(dat |>
            dplyr::filter(label == "catch"))[1] > 1) {
      catch <- dat |>
        dplyr::filter(
          label == "catch",
          !is.na(fleet)
        ) |>
        dplyr::mutate(
          estimate = as.numeric(estimate),
          year = as.numeric(year),
          fleet = as.factor(fleet)
        ) |>
        dplyr::group_by(label, year, fleet) |>
        dplyr::summarise(estimate = sum(estimate)) |>
        dplyr::ungroup()
      
      tot.catch.min <- catch |>
        dplyr::slice(which.min(estimate)) |>
        dplyr::select(estimate) |>
        as.numeric() |>
        round(digits = 2)
    } else {
      tot.catch.min <- "NA"
    }
  }
  
  # maximum catch
  if (kq == "tot.catch.max"){
    if (dim(dat |>
            dplyr::filter(label == "catch"))[1] > 1) {
      catch <- dat |>
        dplyr::filter(
          label == "catch",
          !is.na(fleet)
        ) |>
        dplyr::mutate(
          estimate = as.numeric(estimate),
          year = as.numeric(year),
          fleet = as.factor(fleet)
        ) |>
        dplyr::group_by(label, year, fleet) |>
        dplyr::summarise(estimate = sum(estimate)) |>
        dplyr::ungroup()
      
      tot.catch.max <- catch |>
        dplyr::slice(which.max(estimate)) |>
        dplyr::select(estimate) |>
        as.numeric() |>
        round(digits = 2)
    } else {
      tot.catch.max <- "NA"
    }
  }
  
  ## NAA (numbers at age)
  # start year of NAA plot
  if (kq == "pop.naa.start.year"){
    pop.naa.start.year <- dat |>
      dplyr::filter(label == "abundance" & !is.na(year)) |>
      dplyr::slice(which.min(year)) |>
      dplyr::select(year) |>
      as.numeric()
  }
  
  # end year of NAA plot
  if (kq == "pop.naa.end.year"){
    pop.naa.end.year <- dat |>
      dplyr::filter(label == "abundance" & !is.na(year),
                    era == "time") |>
      dplyr::slice(which.max(year)) |>
      dplyr::select(year) |>
      as.numeric()
  }
  
  # minimum age
  if (kq == "pop.naa.age.min"){
    pop.naa.age.min <- dat |>
      dplyr::filter(label == "abundance" & !is.na(year)) |>
      dplyr::slice(which.min(age)) |>
      dplyr::select(age) |>
      as.numeric()
  }
  
  # maximum age
  if (kq == "pop.naa.age.max"){
    pop.naa.age.max <- dat |>
      dplyr::filter(label == "abundance" & !is.na(year)) |>
      dplyr::slice(which.max(age)) |>
      dplyr::select(age) |>
      as.numeric()
  }
  
  # minimum abundance (number) of fish
  if (kq == "pop.naa.fish.min"){
    pop.naa.fish.min <- dat |>
      dplyr::filter(grepl("abundance", label) & !is.na(year),
                    era == "time") |>
      dplyr::slice(which.min(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric() |>
      round(digits = 2)
  }
  
  # maximum abundance (number) of fish
  if (kq == "pop.naa.fish.max"){
    pop.naa.fish.max <- dat |>
      dplyr::filter(grepl("abundance", label) & !is.na(year),
                    era == "time") |>
      dplyr::slice(which.max(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric() |>
      round(digits = 2)
  }
  
  ## estimated stock recruitment
  # youngest-age recruited fish (instead of age-0)
  if (kq == "sr.age.min"){
    sr.age.min <- dat |>
      dplyr::filter(!is.na(year) & !is.na(age)) |>
      dplyr::slice(which.min(age)) |>
      dplyr::select(age) |>
      as.numeric()
  }
  
  # start year of recruitment ts plot
  if (kq == "recruitment.start.year"){
    recruitment.start.year <- dat |>
      dplyr::filter(
        label == "recruitment",
        module_name == "TIME_SERIES" | module_name == "t.series",
        !is.na(year),
        is.na(fleet) | length(unique(fleet)) <= 1,
        is.na(sex) | length(unique(sex)) <= 1,
        is.na(area) | length(unique(area)) <= 1,
        is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
        !year %in% year_exclusions
      ) |> # SS3 and BAM target module names
      dplyr::slice(which.min(year)) |>
      dplyr::select(year) |>
      as.numeric()
  }
  
  ## recruitment deviations
  # start year of recruitment deviations plot
  if (kq == "recruit.dev.start.year"){
    recruit.dev.start.year <- dat |>
      dplyr::filter(
        label == "recruitment_deviations" | label == "log_recruitment_deviations",
        module_name == "SPAWN_RECRUIT" | module_name == "t.series",
        !is.na(year),
        is.na(fleet) | length(unique(fleet)) <= 1,
        is.na(sex) | length(unique(sex)) <= 1,
        is.na(area) | length(unique(area)) <= 1,
        is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
        !year %in% year_exclusions
      ) |> # SS3 and BAM target module names
      dplyr::slice(which.min(year)) |>
      dplyr::select(year) |>
      as.numeric()
  }
  
  # minimum recruitment deviation
  if (kq == "recruit.dev.min"){
    recruit.dev.min <- dat |>
      dplyr::filter(
        label == "recruitment_deviations" | label == "log_recruitment_deviations",
        module_name == "SPAWN_RECRUIT" | module_name == "t.series",
        !is.na(year),
        is.na(fleet) | length(unique(fleet)) <= 1,
        is.na(sex) | length(unique(sex)) <= 1,
        is.na(area) | length(unique(area)) <= 1,
        is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
        !year %in% year_exclusions
      ) |> # SS3 and BAM target module names
      dplyr::slice(which.min(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric() |>
      round(digits = 2)
  }
  
  # maximum recruitment deviation
  if (kq == "recruit.dev.max"){
    recruit.dev.max <- dat |>
      dplyr::filter(
        label == "recruitment_deviations" | label == "log_recruitment_deviations",
        module_name == "SPAWN_RECRUIT" | module_name == "t.series",
        !is.na(year),
        is.na(fleet) | length(unique(fleet)) <= 1,
        is.na(sex) | length(unique(sex)) <= 1,
        is.na(area) | length(unique(area)) <= 1,
        is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
        !year %in% year_exclusions
      ) |> # SS3 and BAM target module names
      dplyr::slice(which.max(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric() |>
      round(digits = 2)
  }
  
  ## spawning_biomass (ssb)
  # start year of ssb plot
  if (kq == "ssb.start.year"){
    ssb.start.year <- dat |>
      dplyr::filter(
        label == "spawning_biomass",
        module_name %in% c("DERIVED_QUANTITIES", "t.series")
      ) |>
      dplyr::slice(which.min(year)) |>
      dplyr::select(year) |>
      as.numeric() |>
      round(digits = 2)
  }
  
  ## spr (spawning potential ratio)
  # minimum spr
  if (kq == "spr.min"){
    spr.min <- dat |>
      dplyr::filter(c(grepl("spr", label) |
                        label == "spr") &
                      !is.na(year) &
                      !is.na(estimate)) |>
      dplyr::slice(which.min(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric() |>
      round(digits = 2)
  }
  
  # maximum spr
  if (kq == "spr.max"){
    spr.max <- dat |>
      dplyr::filter(c(grepl("spr", label) |
                        label == "spr") & !is.na(year) & !is.na(estimate)) |>
      dplyr::slice(which.max(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric() |>
      round(digits = 2)
  }
  
  ## pop.baa (population biomass at age)
  # start year of pop.baa plot
  if (kq == "pop.baa.start.year"){
    pop.baa.start.year <- dat |>
      dplyr::filter(grepl("^biomass", label)) |>
      dplyr::slice(which.min(year)) |>
      dplyr::select(year) |>
      as.numeric()
  }
  
  # end year of pop.baa plot
  if (kq == "pop.baa.end.year"){
    pop.baa.end.year <- dat |>
      dplyr::filter(grepl("^biomass", label),
                    era == "time") |>
      dplyr::slice(which.max(year)) |>
      dplyr::select(year) |>
      as.numeric()
  }
  
  # minimum biomass of fish
  if (kq == "pop.baa.fish.min"){
    pop.baa.fish.min <- dat |>
      dplyr::filter(grepl("^biomass", label) & !is.na(year),
                    era == "time") |>
      dplyr::slice(which.min(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric() |>
      round(digits = 2)
  }
  
  # maximum biomass of fish
  if (kq == "pop.baa.fish.max"){
    pop.baa.fish.max <- dat |>
      dplyr::filter(grepl("^biomass", label) & !is.na(year),
                    era == "time") |>
      dplyr::slice(which.max(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric() |>
      round(digits = 2)
  }
  
  # minimum age
  if (kq == "pop.baa.age.min"){
    pop.baa.age.min <- dat |>
      dplyr::filter(
        label == "biomass",
        module_name == "BIOMASS_AT_AGE" | module_name == "B.age", # SS3 and BAM target module names
        !is.na(age)
      ) |>
      dplyr::slice(which.min(age)) |>
      dplyr::select(age) |>
      as.numeric()
  }
  
  # maximum age
  if (kq == "pop.baa.age.max"){
    pop.baa.age.max <- dat |>
      dplyr::filter(
        label == "biomass",
        module_name == "BIOMASS_AT_AGE" | module_name == "B.age", # SS3 and BAM target module names
        !is.na(age)
      ) |>
      dplyr::slice(which.max(age)) |>
      dplyr::select(age) |>
      as.numeric()
  }
  
  # end year of projected catch plot
  if (kq == "proj.catch.end.year"){
    proj.catch.end.year <- dat |>
      dplyr::filter(label == "catch" & module_name == "DERIVED_QUANTITIES") |>
      dplyr::slice(which.max(year)) |>
      dplyr::select(year) |>
      as.numeric()
  }
  
  # minimum projected catch
  if (kq == "proj.catch.min"){
    proj.catch.min <- dat |>
      # no BAM file has catch; will be NA
      dplyr::filter(label == "catch" & module_name == "DERIVED_QUANTITIES") |>
      dplyr::slice(which.min(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric()
  }

  # maximum projected catch
  if (kq == "proj.catch.max"){
    proj.catch.max <- dat |>
      dplyr::filter(label == "catch" & module_name == "DERIVED_QUANTITIES") |>
      dplyr::slice(which.max(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric()
  }
  
  value
}
