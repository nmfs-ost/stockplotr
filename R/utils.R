# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# General utility functions
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# substitute in more key quantities (units, end_years, reference points, and more)
# to captions/alt text
add_more_key_quants <- function(
    dat = NULL,
    topic = topic_label,
    fig_or_table = fig_or_table,
    dir = NULL,
    end_year = NULL,
    units = NULL,
    sr_ssb_units = NULL,
    sr_recruitment_units = NULL,
    ref_pt = NULL,
    scaling = NULL) {
  # import csv
  caps_alt_df <- utils::read.csv(fs::path(dir, "captions_alt_text.csv"))

  # make year character if not null
  if (!is.null(end_year)) {
    end_year <- as.character(end_year)
  }

  # select specific fig/table's caption/alt text
  topic_cap_alt <- caps_alt_df |>
    dplyr::filter(
      label == topic,
      type == fig_or_table
    )

  if (!is.null(dat)) {
    dat <- dat |>
      dplyr::mutate(
        estimate = as.numeric(estimate),
        year = as.numeric(year),
        age = as.numeric(age)
      )
  }

  message("The following key quantities were extracted and inserted from add_more_key_quants():")

  # calculate key quantities that rely on end_year for calculation
  ## terminal fishing mortality
  if (topic_cap_alt$label == "fishing.mortality") {
    if (is.null(dat)) {
      message("Some key quantities associated with fishing mortality were not extracted and added to captions_alt_text.csv due to missing data file (i.e., 'dat' argument).")
    } else {
      F.end.year <- dat |>
        dplyr::filter(
          c(label == "fishing_mortality" &
            year == end_year) |
            c(label == "terminal_fishing_mortality" & is.na(year))
        ) |>
        dplyr::pull(estimate) |>
        as.numeric() |>
        round(digits = 2)

      # COMMENTING OUT THESE LINES because the current alt text/captions csv
      # doesn't include Ftarg or F.Ftarg. If we alter them to include them,
      # then uncomment these lines and add code that would substitute the key
      # quantities into the df, like at the bottom of write_captions.
      #
      # # recalculate Ftarg for F.Ftarg, below
      # Ftarg <- dat |>
      #   dplyr::filter(grepl('f_target', label) |
      #                   grepl('f_msy', label) |
      #                   c(grepl('fishing_mortality_msy', label) &
      #                       is.na(year))) |>
      #   dplyr::pull(estimate) |>
      #   as.numeric() |>
      #   round(digits = 2)
      #
      # # Terminal year F respective to F target
      # F.Ftarg <- F.end.year / Ftarg

      if (!is.null(F.end.year)) {
        end_year <- as.character(F.end.year)
      }
    }
  }


  # calculate key quantities that rely on scaling for calculation
  # TODO: pull the relative forms of these three KQs (B, R, SSB) from write_captions,
  # write analogous code for each in this section, and remove placeholders from
  # the end of write_captions (once we get clarity about how to extract Btarg,
  # ssbtarg, and R0)
  ## biomass
  if (topic_cap_alt$label == "biomass") {
    if (is.null(dat)) {
      message("Some key quantities associated with biomass were not extracted and added to captions_alt_text.csv due to missing data file (i.e., 'dat' argument).")
    } else {
      # minimum biomass
      B.min <- dat |>
        dplyr::filter(
          label == "biomass",
          module_name == "TIME_SERIES" | module_name == "t.series", # SS3 and BAM target module names
          is.na(fleet),
          is.na(age)
        ) |>
        dplyr::slice(which.min(estimate)) |>
        dplyr::select(estimate) |>
        dplyr::mutate(estimate = estimate / scaling) |>
        as.numeric() |>
        round(digits = 2)

      # maximum biomass
      B.max <- dat |>
        dplyr::filter(
          label == "biomass",
          module_name == "TIME_SERIES" | module_name == "t.series", # SS3 and BAM target module names
          is.na(fleet),
          is.na(age)
        ) |>
        dplyr::slice(which.max(estimate)) |>
        dplyr::select(estimate) |>
        dplyr::mutate(estimate = estimate / scaling) |>
        as.numeric() |>
        round(digits = 2)

      # replace B.min and B.max placeholders within topic_cap_alt
      topic_cap_alt <- topic_cap_alt |>
        dplyr::mutate(alt_text = stringr::str_replace_all(
          alt_text,
          "B.min",
          as.character(B.min)
        )) |>
        dplyr::mutate(alt_text = stringr::str_replace_all(
          alt_text,
          "B.max",
          as.character(B.max)
        ))

      message(paste0("B.min: ", as.character(B.min)))
      message(paste0("B.max: ", as.character(B.max)))
    }
  }

  ## spawning biomass
  if (topic_cap_alt$label == "spawning.biomass") {
    if (is.null(dat)) {
      message("Some key quantities associated with spawning biomass were not extracted and added to captions_alt_text.csv due to missing data file (i.e., 'dat' argument).")
    } else {
      # minimum ssb
      sr.ssb.min <- dat |>
        dplyr::filter(
          label == "spawning_biomass",
          module_name == "TIME_SERIES" | module_name == "t.series",
          !is.na(year),
          is.na(fleet) | length(unique(fleet)) <= 1,
          is.na(sex) | length(unique(sex)) <= 1,
          is.na(area) | length(unique(area)) <= 1,
          is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
          !year %in% year_exclusions
        ) |> # SS3 and BAM target module names
        dplyr::slice(which.min(estimate)) |>
        dplyr::select(estimate) |>
        dplyr::mutate(estimate = estimate / scaling) |>
        as.numeric() |>
        round(digits = 2)

      # maximum ssb
      sr.ssb.max <- dat |>
        dplyr::filter(
          label == "spawning_biomass",
          module_name == "TIME_SERIES" | module_name == "t.series",
          !is.na(year),
          is.na(fleet) | length(unique(fleet)) <= 1,
          is.na(sex) | length(unique(sex)) <= 1,
          is.na(area) | length(unique(area)) <= 1,
          is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
          !year %in% year_exclusions
        ) |> # SS3 and BAM target module names
        dplyr::slice(which.max(estimate)) |>
        dplyr::select(estimate) |>
        dplyr::mutate(estimate = estimate / scaling) |>
        as.numeric() |>
        round(digits = 2)

      # replace sr.ssb.min and sr.ssb.max placeholders within topic_cap_alt
      topic_cap_alt <- topic_cap_alt |>
        dplyr::mutate(alt_text = stringr::str_replace_all(
          alt_text,
          "sr.ssb.min",
          as.character(sr.ssb.min)
        )) |>
        dplyr::mutate(alt_text = stringr::str_replace_all(
          alt_text,
          "sr.ssb.max",
          as.character(sr.ssb.max)
        ))

      message(paste0("sr.ssb.min: ", as.character(sr.ssb.min)))
      message(paste0("sr.ssb.max: ", as.character(sr.ssb.max)))
    }
  }

  ## recruitment
  if (topic_cap_alt$label == "recruitment") {
    if (is.null(dat)) {
      message("Some key quantities associated with recruitment were not extracted and added to captions_alt_text.csv due to missing data file (i.e., 'dat' argument).")
    } else {
      # minimum recruitment
      sr.min <- dat |>
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
        dplyr::slice(which.min(estimate)) |>
        dplyr::select(estimate) |>
        dplyr::mutate(estimate = estimate / scaling) |>
        as.numeric() |>
        round(digits = 2)

      # maximum recruitment
      sr.max <- dat |>
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
        dplyr::slice(which.max(estimate)) |>
        dplyr::select(estimate) |>
        dplyr::mutate(estimate = estimate / scaling) |>
        as.numeric() |>
        round(digits = 2)

      # replace sr.min and sr.max placeholders within topic_cap_alt
      topic_cap_alt <- topic_cap_alt |>
        dplyr::mutate(alt_text = stringr::str_replace_all(
          alt_text,
          "sr.min",
          as.character(sr.min)
        )) |>
        dplyr::mutate(alt_text = stringr::str_replace_all(
          alt_text,
          "sr.max",
          as.character(sr.max)
        ))

      message(paste0("sr.min: ", as.character(sr.min)))
      message(paste0("sr.max: ", as.character(sr.max)))
    }
  }

  # replace placeholders (e.g., if "end.year" is found in topic_alt, replace it with end_year)
  ## end_year-----
  if (!is.null(end_year)) {
    ### alt text
    ### this regex preserves the comma after the end year
    topic_cap_alt <- topic_cap_alt |>
      dplyr::mutate(alt_text = stringr::str_replace_all(
        alt_text,
        stringr::regex("(\\S*end\\.year\\S*)(?=\\s?,)"),
        end_year
      ))

    ### this regex removes a potential trailing space after the end year
    topic_cap_alt <- topic_cap_alt |>
      dplyr::mutate(alt_text = stringr::str_replace_all(
        alt_text,
        stringr::regex("\\S*end\\.year\\S*\\s*"),
        end_year
      ))

    message(paste0("end_year: ", as.character(end_year)))
  }
  ## units-----

  if (is.null(scaling)) {
    scale_label <- FALSE
  } else {
    scale_label <- TRUE
    magnitude <- floor(log10(scaling))
    if (magnitude == 0) {
      units <- units
      unit_mag <- ""
    } else if (magnitude > 0 & magnitude < 10) {
      scale_unit <- c(
        "tens of ",
        "hundreds of ",
        "thousands of",
        "tens of thousands of ",
        "hundreds of thousands of ",
        "millions of ",
        "tens of millions of ",
        "hundreds of millions of ",
        "billions of "
      )
      unit_mag <- paste(scale_unit[magnitude])
    } else {
      stop("Scaling out of bounds. Please choose a value ranging from 1-1000000000 (one billion) in orders of magnitude (e.g., 1, 10, 100, 1000, etc.)")
    }
  }


  if (!is.null(units)) {
    ### caption
    ### this regex preserves the closing ) after the units
    topic_cap_alt <- topic_cap_alt |>
      dplyr::mutate(caption = stringr::str_replace_all(
        caption,
        stringr::regex("(\\S*units\\S*)(?=\\s?\\))"),
        as.character(units)
      ))

    ### this regex preserves the period after the units
    topic_cap_alt <- topic_cap_alt |>
      dplyr::mutate(caption = stringr::str_replace_all(
        caption,
        stringr::regex("(\\S*units\\S*)(?=\\s?.)"),
        as.character(units)
      ))

    ### this regex replaces the units if it's not found with the previous two commands
    ### (i.e., there's no parenthesis or period adjacent to the units variable)
    topic_cap_alt <- topic_cap_alt |>
      dplyr::mutate(caption = stringr::str_replace_all(
        caption,
        stringr::regex("\\S*units\\S*"),
        as.character(units)
      ))

    message(paste0("units: ", as.character(units)))

  }

  if (!is.null(sr_ssb_units)) {
    ### this is for plot_spawn_recruitment, since there are two units
    #### replace sr.ssb.units with sr_ssb_units
    topic_cap_alt <- topic_cap_alt |>
      dplyr::mutate(alt_text = stringr::str_replace_all(
        alt_text,
        "sr.ssb.units",
        as.character(sr_ssb_units)
      ))

    message(paste0("sr.ssb.units: ", as.character(sr_ssb_units)))

  }

  if (!is.null(sr_recruitment_units)) {
    ### this is for plot_spawn_recruitment, since there are two units
    #### replace sr.units with sr_recruitment_units
    topic_cap_alt <- topic_cap_alt |>
      dplyr::mutate(alt_text = stringr::str_replace_all(
        alt_text,
        "sr.units",
        as.character(sr_recruitment_units)
      ))

    message(paste0("sr.units: ", as.character(sr_recruitment_units)))

  }

  if (!is.null(units)) {
    ### alt text
    ### this regex preserves the comma after the units
    topic_cap_alt <- topic_cap_alt |>
      dplyr::mutate(alt_text = stringr::str_replace_all(
        alt_text,
        stringr::regex("(\\S*units\\S*)(?=\\s?,)"),
        ifelse(scale_label,
          paste0(unit_mag, as.character(units)),
          as.character(units)
        )
      ))

    ### this regex replaces the units if it's not found with the previous command
    ### (i.e., there's no comma adjacent to the units variable)
    topic_cap_alt <- topic_cap_alt |>
      dplyr::mutate(alt_text = stringr::str_replace_all(
        alt_text,
        stringr::regex("\\S*units\\S*"),
        ifelse(scale_label,
          paste0(unit_mag, as.character(units)),
          as.character(units)
        )
      ))
  }
  ## reference points-----
  if (!is.null(ref_pt)) {
    ### caption
    ### this regex preserves the opening ( before the ref pt
    topic_cap_alt <- topic_cap_alt |>
      dplyr::mutate(caption = stringr::str_replace_all(
        caption,
        stringr::regex("\\(\\S*ref\\.pt*\\S*"),
        paste0("(", as.character(ref_pt))
      ))

    message(paste0("plot-specific reference point: ", as.character(ref_pt)))

  }

  # remove row with old caption/alt text, then add new row
  replaced_df <- dplyr::anti_join(caps_alt_df,
    topic_cap_alt,
    by = c("label", "type")
  ) |>
    dplyr::full_join(topic_cap_alt)

  # export df with updated captions and alt text to csv
  utils::write.csv(
    x = replaced_df,
    file = fs::path(
      dir,
      "captions_alt_text.csv"
    ),
    row.names = FALSE
  )
}
