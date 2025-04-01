#' Write captions and alternative text
#'
#' Function to create captions and alternative text that contain
#' key quantities from the model results file.
#'
#' @inheritParams plot_spawning_biomass
#' @param dir Directory where the output captions and alt text file should be
#' saved. Defaults to working directory.
#' @param year the last year of the data or the current year this function is
#' being performed. Defaults to the current year.
#'
#' @return Exports .csv with captions and alt text for figures and tables
#' that contain key quantities (e.g., an assessment's start year) that
#' are automatically extracted from the converted model results file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' write_captions(dat,
#'   dir = getwd(),
#'   year = 2025
#' )
#' }
write_captions <- function(dat, # converted model output object
                           dir = getwd(),
                           year = format(Sys.Date(), "%Y")) {
  # only extract key quantities/export new csv if not present
  if (file.exists(fs::path(dir, "captions_alt_text.csv"))) {
    cli::cli_alert_danger("Captions and alternative text file (captions_alt_text.csv) already exists; write_captions() will not run.", wrap = TRUE)
    cli::cli_alert_info("To extract new key quantities and make a new captions_alt_text.csv file, delete existing captions_alt_text.csv and rerun write_captions().", wrap = TRUE)
  } else {
    # import pre-written captions and alt text that include placeholders
    # for key quantities (e.g., 'start_year' is the placeholder for the
    # assessment's start year)
    caps_alttext <- utils::read.csv(
      system.file("resources", "captions_alt_text_template.csv", package = "stockplotr")
    )

    dat <- dat |>
      dplyr::mutate(
        estimate = as.numeric(estimate),
        year = as.numeric(year),
        age = as.numeric(age)
      )

    # extract key quantities
    # REMINDERS:
    # -the variable names must exactly match those in the captions/alt text csv.

    # suppress warnings
    options(warn = -1)

    # FIGURES-----

    ## kobe plot- don't code quantities yet
    # kobe.end.year <-
    # value of B/B(MSY) at the end year
    # B.BMSY.end.yr <- B/BMSY
    # --B = time series of biomass, last year
    # --BMSY = dplyr::filter(grepl('b_target', label) | grepl('b_msy', label) | c(grepl('fishing_mortality_msy', label) & is.na(year)))
    #   CHECK: if length > 1, then select b_target

    # value of F/F(MSY) at the end year
    # F.FMSY.end.yr <-

    # object that should be "is" or "is not" and answers the question,
    # "the stock overfishing status ... overfished"
    # overfished.status.is.isnot <-

    # object that should be "is" or "is not" and answers the question,
    # "the stock ... experiencing overfishing"
    # overfishing.status.is.isnot <-


    ## Biomass plot
    # biomass reference point
    # B.ref.pt : added with add_more_key_quants

    # start year of biomass plot
    B.start.year <- dat |>
      dplyr::filter(
        label == "biomass",
        module_name == "TIME_SERIES" | module_name == "t.series", # SS3 and BAM target module names
        is.na(fleet),
        is.na(age)
      ) |>
      dplyr::slice(which.min(year)) |>
      dplyr::select(year) |>
      as.numeric()

    # end year of biomass plot
    # B.end.year : added with add_more_key_quants

    # units of B (plural)
    # B.units : added with add_more_key_quants

    # minimum B : added with add_more_key_quants

    # maximum B : added with add_more_key_quants


    # TODO: uncomment and recode once we get clarity about how to extract this value properly
    # R0
    # R0 <- dat |>
    #   dplyr::filter(
    #     # pull from BAM
    #     grepl('^recruitment$', label) & module_name == "parms" |
    #        grepl('^R0', label) |
    #     # pull from SS3
    #       grepl('recruitment_virgin', label)
    #                  ) |>
    #   dplyr::pull(estimate) |>
    #   unique() |>
    #   as.numeric() |>
    #   round(digits = 2)

    # Bend <-

    # TODO: uncomment and recode once we get clarity about how to extract this value properly
    # Target biomass
    # Btarg <- dat |>
    #   dplyr::filter(c(grepl('biomass', label) & grepl('target', label) & estimate >1) | label == 'biomass_msy') |>
    #   dplyr::pull(estimate) |>
    #   as.numeric() |>
    #   round(digits = 2)

    # Bmsy <-


    # TODO: uncomment and recode once we get clarity about how to extract Btarg properly
    ## relative B
    # relative B min
    # rel.B.min <- (B.min / Btarg) |>
    #   round(digits = 2)
    #
    # # relative B max
    # rel.B.max <- (B.max / Btarg) |>
    #   round(digits = 2)

    # TODO: uncomment and recode once we get clarity about how to extract this value properly
    ## mortality (F) plot
    # F reference point
    # F.ref.pt <- dat |>
    #   dplyr::filter(
    #     label == stringr::str_to_lower("F_targ") |
    #       label == stringr::str_to_lower("F_proxy") |
    #       label == stringr::str_to_lower("F_msy") |
    #       label == "F_target"
    #       # label == "F40"
    #       # label == "F30"
    #       # label == "F50"
    #       # label == "F_initial"
    #       # label == "Fmsy"
    #   ) |>
    #   dplyr::filter(module_name == "DERIVED_QUANTITIES" | module_name == "parms") |>
    #   dplyr::pull(estimate) |>
    #   as.numeric() |>
    #   round(digits = 2)

    # start year of F plot
    F.start.year <- dat |>
      dplyr::filter(label == "fishing_mortality") |>
      dplyr::slice(which.min(year)) |>
      dplyr::select(year) |>
      as.numeric()

    # terminal fishing mortality
    # F.end.year : added with add_more_key_quants

    # minimum F
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


    # maximum F
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

    # TODO: uncomment and recode once we get clarity about how to extract this value properly
    # fishing mortality at msy
    # Ftarg <- dat |>
    #   dplyr::filter(grepl('f_target', label) |
    #                   grepl('f_msy', label) |
    #                   c(grepl('fishing_mortality_msy', label) & is.na(year))) |>
    #   dplyr::pull(estimate) |>
    #   as.numeric() |>
    #   round(digits = 2)

    # Terminal year F respective to F target
    # F.Ftarg : added with add_more_key_quants


    ## landings plot

    # start year of landings plot
    landings.start.year <- dat |>
      dplyr::filter(
        c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
        # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
        !is.na(fleet)
      ) |>
      dplyr::slice(which.min(year)) |>
      dplyr::select(year) |>
      as.numeric()

    # end year of landings plot
    landings.end.year <- dat |>
      dplyr::filter(
        c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
        # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
        !is.na(fleet)
      ) |>
      dplyr::slice(which.max(year)) |>
      dplyr::select(year) |>
      as.numeric()

    # units of landings (plural)
    # landings.units : added with add_more_key_quants

    # minimum landings
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

    # maximum landings
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

    ## natural mortality (M)- bam examples have label as natural_mortality
    ## but other formats don't (in input)
    # minimum age of M
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

    # maximum age of M
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

    # minimum M rate- don't code quantities yet (see how it's coded in future fig)
    # M.rate.min <- dat |>
    #   dplyr::filter(
    #     grepl("natural_mortality", label)) |>
    # -label = natural_mortality (min); est in est col

    # maximum M rate- don't code quantities yet (see how it's coded in future fig)
    # M.rate.max <-
    # -label = natural_mortality (min); est in est col


    ## vonB LAA (von Bertalanffy growth function + length at age)- don't code quantities yet
    # vonb.age.min <- # minimum vonB age
    # vonb.age.max <- # maximum vonB age

    # vonB length units (plural)
    # vonb.length.units : added with add_more_key_quants

    # vonb.length.min <- # minimum vonB length
    # vonb.length.max <- # minimum vonB length


    ## length-type conversion plot- don't code quantities yet
    # total length units (plural)
    # total.length.units : added with add_more_key_quants

    # total.length.min <- # minimum total length
    # total.length.max <- # maximum total length
    # fork length units (plural)
    # fork.length.units : added with add_more_key_quants

    # fork.length.min <- # minimum fork length
    # fork.length.max <- # maximum fork length


    ## weight-length conversion plot- don't code quantities yet
    # length units (plural)
    # wl.length.units : added with add_more_key_quants

    # wl.length.min <- # minimum length
    # wl.length.max <- # maximum length

    # weight units (plural)
    # wl.weight.units : added with add_more_key_quants

    # wl.weight.min <- # minimum weight
    # wl.weight.max <- # maximum weight


    ## maturity schedule (proportion mature)- don't code quantities yet
    # length units (plural)
    # prop.mat.length.units : added with add_more_key_quants

    # prop.mat.length.min <- # minimum length
    # prop.mat.length.max <- # maximum length


    ## fecundity at length- don't code quantities yet
    # length units (plural)
    # fecundity.length.units : added with add_more_key_quants

    # fecundity.length.min <- # minimum length
    # fecundity.length.max <- # maximum length

    # fecundity units (plural)
    # fecundity.units : added with add_more_key_quants

    # fecundity.min <- # minimum fecundity
    # fecundity.max <- # maximum fecundity

    ## catch composition
    # minimum & maximum catch
    if (dim(dat |>
            dplyr::filter(label == "catch"))[1] > 1) {

      catch <- dat |>
        dplyr::filter(label == "catch",
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
        as.numeric()

      tot.catch.max <- catch |>
        dplyr::slice(which.max(estimate)) |>
        dplyr::select(estimate) |>
        as.numeric()
      } else {
        tot.catch.min <- "NA"
        tot.catch.max <- "NA"
    }

    ## CAL (catch at length)- don't code quantities yet
    # cal.length.min <- # minimum length group
    # cal.length.max <- # maximum length group
    # fleet.or.survey.name <- # fleet or survey name


    ## CPUE indices plot- don't code quantities yet
    # cpue.start.year <- # start year of CPUE indices plot
    # cpue.end.year <- # end year of CPUE indices plot

    # CPUE units (plural) (SHARED with mod.fit.abun, below)
    # cpue.units : added with add_more_key_quants

    # cpue.min <- # minimum CPUE (SHARED with mod_fit_abun, below)
    # cpue.max <- # maximum CPUE (SHARED with mod_fit_abun, below)


    ## NAA (numbers at age)
    # start year of NAA plot
    pop.naa.start.year.min <- dat |>
      dplyr::filter(label == "abundance" & !is.na(year)) |>
      dplyr::slice(which.min(year)) |>
      dplyr::select(year) |>
      as.numeric()

    # end year of NAA plot
    pop.naa.end.year.max <- dat |>
      dplyr::filter(label == "abundance" & !is.na(year)) |>
      dplyr::slice(which.max(year)) |>
      dplyr::select(year) |>
      as.numeric()

    # minimum age
    pop.naa.age.min <- dat |>
      dplyr::filter(label == "abundance" & !is.na(year)) |>
      dplyr::slice(which.min(age)) |>
      dplyr::select(age) |>
      as.numeric()

    # maximum age
    pop.naa.age.max <- dat |>
      dplyr::filter(label == "abundance" & !is.na(year)) |>
      dplyr::slice(which.max(age)) |>
      dplyr::select(age) |>
      as.numeric()


    ## mod_fit_catch (model fit to catch ts)- don't code quantities yet
    # mod.fit.catch.start.year <- # start year of model fit to catch ts plot
    # mod.fit.catch.end.year <- # end year of model fit to catch ts plot

    # catch units (plural)
    # mod.fit.catch.units : added with add_more_key_quants

    # mod.fit.catch.min <- # minimum catch
    # mod.fit.catch.max <- # maximum catch


    ## mod_fit_abun (model fit to abundance indices plot)- don't code quantities yet
    # start year of model fit to abundance indices plot
    # mod.fit.abun.start.year <-

    # end year of model fit to abundance indices plot
    # mod.fit.abun.end.year <-

    ## mod_fit_discards- will be by fleet
    ## for ss3, obs discards not in output file
    ## -filter labels as discard_observed | discard_predicted | discard
    ## -then group_by(year, fleet, label), then summarize(estimate_y = sum(estimate))
    ## ---then, get the following:
    ## -change alt text so that we add the line's min/max, but analyst has to describe further
    # mod.fit.discards.start.year <- # start year of model fit to discards plot
    # mod.fit.discards.end.year <- # end year of F
    # mod.fit.discards.units <- # discards units (plural)
    # mod.fit.discards.min <- # minimum discards
    # mod.fit.discards.max <- # maximum discards

    ## selectivity- don't code quantities yet
    # selectivity.start.year <- # start year of selectivity plot
    # selectivity.end.year <- # end year of selectivity plot
    # selectivity.length.units <- # length units (plural)
    # selectivity.length.min <- # minimum length
    # selectivity.length.max <- # maximum length


    ## estimated stock recruitment
    # youngest-age recruited fish (instead of age-0)
    sr.age.min <- dat |>
      dplyr::filter(!is.na(year) & !is.na(age)) |>
      dplyr::slice(which.min(age)) |>
      dplyr::select(age) |>
      as.numeric()

    # ssb units (plural)
    # sr.ssb.units : added with add_more_key_quants

    # minimum ssb : added with add_more_key_quants

    # maximum ssb : added with add_more_key_quants

    # recruitment units (plural)
    # sr.units : added with add_more_key_quants

    # minimum recruitment : added with add_more_key_quants

    # maximum recruitment: added with add_more_key_quants

    ## recruitment ts
    # recruitment units (plural) - numbers of fish, in thousands
    # recruitment.units : added with add_more_key_quants

    # start year of recruitment ts plot
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

    # end year of recruitment ts plot
    # recruitment.end.year : added with add_more_key_quants

    # TODO: uncomment and recode once we get clarity about how to extract R0 properly
    ## relative recruitment
    # # minimum relative recruitment
    # rel.recruitment.min <- (sr.min / R0) |>
    #   round(digits = 2)
    #
    # # maximum relative recruitment
    # rel.recruitment.max <- (sr.max / R0) |>
    #   round(digits = 2)


    ## recruitment deviations
    # start year of recruitment deviations plot
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

    # end year of recruitment deviations plot
    # recruit.dev.end.year : added with add_more_key_quants

    # minimum recruitment deviation
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

    # maximum recruitment deviation
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

    ## tot_b (total biomass): same as B plot above

    ## spawning_biomass (ssb)
    # start year of ssb plot
    ssb.start.year <- dat |>
      dplyr::filter(
        label == "spawning_biomass",
        module_name %in% c("DERIVED_QUANTITIES", "t.series")
      ) |>
      dplyr::slice(which.min(year)) |>
      dplyr::select(year) |>
      as.numeric() |>
      round(digits = 2)

    # end year of ssb plot
    # ssb.end.year : added with add_more_key_quants

    # ssb units (plural)
    # ssb.units : added with add_more_key_quants

    # minimum ssb
    # ssb.min : added with add_more_key_quants

    # maximum ssb
    # ssb.max : added with add_more_key_quants

    # ssb reference point
    # ssb.ref.pt : added with add_more_key_quants

    # ssbtarg : added with add_more_key_quants

    ## relative ssb
    # relative ssb min
    # rel.ssb.min : added with add_more_key_quants

    # relative ssb max
    # rel.ssb.max : added with add_more_key_quants


    ## spr (spawning potential ratio)
    # minimum spr
    spr.min <- dat |>
      dplyr::filter(c(grepl("spr", label) |
        label == "spr") &
        !is.na(year) &
        !is.na(estimate)) |>
      dplyr::slice(which.min(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric() |>
      round(digits = 2)

    # maximum spr
    spr.max <- dat |>
      dplyr::filter(c(grepl("spr", label) |
        label == "spr") & !is.na(year) & !is.na(estimate)) |>
      dplyr::slice(which.max(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric() |>
      round(digits = 2)

    # TODO: uncomment and recode once we get clarity about how to extract this value properly
    # spr reference point
    # spr.ref.pt <- dat |>
    # dplyr::filter(label == "spr_msy") |>
    #   dplyr::select(estimate) |>
    #   as.numeric()# |>
    #   round(digits = 2)


    ## pop.baa (population biomass at age)
    # start year of pop.baa plot
    pop.baa.start.year <- dat |>
      dplyr::filter(label == "abundance") |>
      dplyr::slice(which.min(year)) |>
      dplyr::select(year) |>
      as.numeric()

    # end year of pop.baa plot
    pop.baa.end.year <- landings.end.year

    # minimum number of fish
    pop.baa.fish.min <- dat |>
      dplyr::filter(label == "abundance" & !is.na(year)) |>
      dplyr::slice(which.min(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric() |>
      round(digits = 2)


    # maximum number of fish
    pop.baa.fish.max <- dat |>
      dplyr::filter(label == "abundance" & !is.na(year)) |>
      dplyr::slice(which.max(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric() |>
      round(digits = 2)

    # minimum age
    pop.baa.age.min <- dat |>
      dplyr::filter(
        label == "biomass",
        module_name == "BIOMASS_AT_AGE" | module_name == "B.age", # SS3 and BAM target module names
        !is.na(age)
      ) |>
      dplyr::slice(which.min(age)) |>
      dplyr::select(age) |>
      as.numeric()

    # maximum age
    pop.baa.age.max <- dat |>
      dplyr::filter(
        label == "biomass",
        module_name == "BIOMASS_AT_AGE" | module_name == "B.age", # SS3 and BAM target module names
        !is.na(age)
      ) |>
      dplyr::slice(which.max(age)) |>
      dplyr::select(age) |>
      as.numeric()


    ## proj_catch (projected catch)
    # projected catch units (plural)
    # proj.catch.units <- # probably mt, but wait until figure coded
    # --then add into add_more_key_quants()

    # start year of projected catch plot
    proj.catch.start.year <- landings.end.year + 1

    # end year of projected catch plot
    proj.catch.end.year <- dat |>
      dplyr::filter(label == "catch" & module_name == "DERIVED_QUANTITIES") |>
      dplyr::slice(which.max(year)) |>
      dplyr::select(year) |>
      as.numeric()

    # minimum projected catch
    proj.catch.min <- dat |>
      # no BAM file has catch; will be NA
      dplyr::filter(label == "catch" & module_name == "DERIVED_QUANTITIES") |>
      dplyr::slice(which.min(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric()

    # maximum projected catch
    proj.catch.max <- dat |>
      dplyr::filter(label == "catch" & module_name == "DERIVED_QUANTITIES") |>
      dplyr::slice(which.max(estimate)) |>
      dplyr::select(estimate) |>
      as.numeric()

    # TABLES-----

    ## catch
    # catch.fleet <- # fleet

    ## landings
    # landings.tbl.units <- # landings units; remove if units already in table

    ## discards
    # discards.tbl.units <- # discards units

    ## catchability
    # catchability.fleet <- # fleet


    # add in more quantities here, and update the quantities above

    # substitute quantity placeholders in the captions/alt text with
    # the real values, extracted above




    # make list with all placeholders
    # uncomment placeholders once uncommented, above
    patterns_replacements <- c(
      # FIGURES-----

      ## kobe plot
      # 'kobe.end.year' = as.character(kobe.end.year),
      # 'B.BMSY.end.yr' = as.character(B.BMSY.end.yr),
      # 'F.FMSY.end.yr' = as.character(F.FMSY.end.yr),
      # 'overfished.status.is.isnot' = as.character(overfished.status.is.isnot),
      # 'overfishing.status.is.isnot' = as.character(overfishing.status.is.isnot),

      ## Relative biomass plot
      # NOTE: moving this above biomass so rel.B.min isn't changed to "rel." + B.min (etc.)
      # 'rel.B.min' = as.character(rel.B.min),
      # 'rel.B.max' = as.character(rel.B.max),

      ## Biomass plot
      "B.start.year" = as.character(B.start.year),
      # 'R0' = as.character(R0),
      # 'Bend' = as.character(Bend),
      # 'Btarg' = as.character(Btarg),
      # 'Bmsy' = as.character(Bmsy),

      ## mortality (F) plot
      # 'F.ref.pt' = as.character(F.ref.pt),
      "F.start.year" = as.character(F.start.year),
      "F.min" = as.character(F.min),
      "F.max" = as.character(F.max),
      # 'Ftarg' = as.character(Ftarg),

      ## landings plot
      "landings.start.year" = as.character(landings.start.year),
      "landings.end.year" = as.character(landings.end.year),
      "landings.min" = as.character(landings.min),
      "landings.max" = as.character(landings.max),

      ## natural mortality (M)
      "M.age.min" = as.character(M.age.min),
      "M.age.max" = as.character(M.age.max),
      # 'M.rate.min' = as.character(M.rate.min),
      # 'M.rate.max' = as.character(M.rate.max),

      ## vonB LAA (von Bertalanffy growth function + length at age)
      # 'vonb.age.min' = as.character(vonb.age.min),
      # 'vonb.age.max' = as.character(vonb.age.max),
      # 'vonb.length.units' = as.character(vonb.length.units),
      # 'vonb.length.min' = as.character(vonb.length.min),
      # 'vonb.length.max' = as.character(vonb.length.max),

      ## length-type conversion plot
      # 'total.length.units' = as.character(total.length.units),
      # 'total.length.min' = as.character(total.length.min),
      # 'total.length.max' = as.character(total.length.max),
      # 'fork.length.units' = as.character(fork.length.units),
      # 'fork.length.min' = as.character(fork.length.min),
      # 'fork.length.max' = as.character(fork.length.max),

      ## weight-length conversion plot
      # 'wl.length.units' = as.character(wl.length.units),
      # 'wl.length.min' = as.character(wl.length.min),
      # 'wl.length.max' = as.character(wl.length.max),
      # 'wl.weight.units' = as.character(wl.weight.units),
      # 'wl.weight.min' = as.character(wl.weight.min),
      # 'wl.weight.max' = as.character(wl.weight.max),

      ## maturity schedule (proportion mature)
      # 'prop.mat.length.units' = as.character(prop.mat.length.units),
      # 'prop.mat.length.min' = as.character(prop.mat.length.min),
      # 'prop.mat.length.max' = as.character(prop.mat.length.max),

      ## fecundity at length
      # 'fecundity.length.units' = as.character(fecundity.length.units),
      # 'fecundity.length.min' = as.character(fecundity.length.min),
      # 'fecundity.length.max' = as.character(fecundity.length.max),
      # 'fecundity.units' = as.character(fecundity.units),
      # 'fecundity.min' = as.character(fecundity.min),
      # 'fecundity.max' = as.character(fecundity.max),

      ## catch composition
      # 'tot.catch.min' = as.character(tot.catch.min),
      # 'tot.catch.max' = as.character(tot.catch.max),

      ## CAL (catch at length)
      # 'cal.length.min' = as.character(cal.length.min),
      # 'cal.length.max' = as.character(cal.length.max),
      # 'fleet.or.survey.name' = as.character(fleet.or.survey.name),

      ## CPUE indices plot
      # 'cpue.start.year' = as.character(cpue.start.year),
      # 'cpue.end.year' = as.character(cpue.end.year),
      # 'cpue.min' = as.character(cpue.min),
      # 'cpue.max' = as.character(cpue.max),

      ## NAA (numbers at age)
      "pop.naa.start.year.min" = as.character(pop.naa.start.year.min),
      "pop.naa.end.year.max" = as.character(pop.naa.end.year.max),
      "pop.naa.age.min" = as.character(pop.naa.age.min),
      "pop.naa.age.max" = as.character(pop.naa.age.max),

      ## mod_fit_catch (model fit to catch ts)
      # 'mod.fit.catch.start.year' = as.character(mod.fit.catch.start.year),
      # 'mod.fit.catch.end.year' = as.character(mod.fit.catch.end.year),
      # 'mod.fit.catch.units' = as.character(mod.fit.catch.units),
      # 'mod.fit.catch.min' = as.character(mod.fit.catch.min),
      # 'mod.fit.catch.max' = as.character(mod.fit.catch.max),

      ## mod_fit_abun (model fit to abundance indices plot)
      # 'mod.fit.abun.start.year' = as.character(mod.fit.abun.start.year),
      # 'mod.fit.abun.end.year' = as.character(mod.fit.abun.end.year),

      ## mod_fit_discards
      # 'mod.fit.discards.start.year' = as.character(mod.fit.discards.start.year),
      # 'mod.fit.discards.end.year' = as.character(mod.fit.discards.end.year),
      # 'mod.fit.discards.units' = as.character(mod.fit.discards.units),
      # 'mod.fit.discards.min' = as.character(mod.fit.discards.min),
      # 'mod.fit.discards.max' = as.character(mod.fit.discards.max),

      ## selectivity
      # 'selectivity.start.year' = as.character(selectivity.start.year),
      # 'selectivity.end.year' = as.character(selectivity.end.year),
      # 'selectivity.length.units' = as.character(selectivity.length.units),
      # 'selectivity.length.min' = as.character(selectivity.length.min),
      # 'selectivity.length.max' = as.character(selectivity.length.max),

      ## estimated stock recruitment (aka spawning stock biomass)
      # 'sr.age.min' = as.character(sr.age.min),

      # relative recruitment ts
      # NOTE: moving this above recruitment so rel.recruitment.min isn't changed
      # to "rel." + recruitment.min (etc.)
      # 'rel.recruitment.min' = as.character(rel.recruitment.min),
      # 'rel.recruitment.max' = as.character(rel.recruitment.max),

      ## recruitment ts
      "recruitment.start.year" = as.character(recruitment.start.year),

      ## recruitment deviations
      "recruit.dev.start.year" = as.character(recruit.dev.start.year),
      "recruit.dev.min" = as.character(recruit.dev.min),
      "recruit.dev.max" = as.character(recruit.dev.max),

      ## spawning.biomass (ssb)
      "ssb.start.year" = as.character(ssb.start.year),

      ## spr (spawning potential ratio)
      "spr.min" = as.character(spr.min),
      "spr.max" = as.character(spr.max),
      #  'spr.ref.pt' = as.character(spr.ref.pt),

      # ## pop.baa (population biomass at age)
      "pop.baa.start.year" = as.character(pop.baa.start.year),
      "pop.baa.end.year" = as.character(pop.baa.end.year),
      "pop.baa.fish.min" = as.character(pop.baa.fish.min),
      "pop.baa.fish.max" = as.character(pop.baa.fish.max),
      "pop.baa.age.min" = as.character(pop.baa.age.min),
      "pop.baa.age.max" = as.character(pop.baa.age.max),


      ## proj_catch (projected catch)
      # 'proj.catch.units' = as.character(proj.catch.units),
      "proj.catch.start.year" = as.character(proj.catch.start.year),
      "proj.catch.end.year" = as.character(proj.catch.end.year),
      "proj.catch.min" = as.character(proj.catch.min),
      "proj.catch.max" = as.character(proj.catch.max) # ,

      # # TABLES-----
      #
      # ## catch
      # 'catch.fleet' = as.character(catch.fleet),
      #
      # ## landings
      # 'landings.tbl.units' = as.character(landings.tbl.units),
      #
      # ## discards
      # 'discards.tbl.units' = as.character(discards.tbl.units),
      #
      # ## catchability
      # 'catchability.fleet' = as.character(catchability.fleet)
    )

    # If a value in patterns_replacements = NA, then make it "NA"
    # to avoid errors
    patterns_replacements <- tidyr::replace_na(patterns_replacements, "NA")

    # take the values associated with the quantities and replace the df's
    # placeholders with them. For example, if ssb_min = 10, this will replace
    # "the minimum ssb = ssb_min" with "the minimum ssb = 10".

    # replace values in caption column
    caps_alttext$caption <- stringr::str_replace_all(
      caps_alttext$caption,
      patterns_replacements
    )

    # replace values in alt text column
    caps_alttext$alt_text <- stringr::str_replace_all(
      caps_alttext$alt_text,
      patterns_replacements
    )

    # export df with updated captions and alt text to csv
    utils::write.csv(
      x = caps_alttext,
      file = fs::path(
        dir,
        "captions_alt_text.csv"
      ),
      row.names = FALSE
    )


    # message explaining the extracted and inserted key quantities
    replaced_vals <- patterns_replacements |>
      as.data.frame() |>
      tibble::rownames_to_column() |>
      dplyr::rename(
        "name" = 1,
        "key_quantity" = 2
      )

    cli::cli_h3("Key quantities extracted and inserted from write_captions().")
    cli::cli_alert_info("NA values signify key quantities that were not extracted and inserted.", wrap = TRUE)
    for (i in 1:dim(replaced_vals)[1]) {
      cli::cli_li(paste0(
        replaced_vals[i, 1],
        ": ",
        replaced_vals[i, 2]
      ))
    }

    # enable warnings again
    options(warn = 0)
  }
}
