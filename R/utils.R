# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# General utility functions
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Check end_year isn't past current year for non-projections plots
# make year character if not null
check_year <- function(end_year = NULL,
                       topic = NULL,
                       fig_or_table = NULL) {
  if (!is.null(end_year)) {
    # TODO: Update this to work for projections plots when developed
    # stop if end year is past current year for non-projections plots
    projections_plots <- c("proj.catch", "proj.biomass", "projection.ts", "sensitivity.runs")

    if (!is.null(topic)) {
      if (topic %in% projections_plots == FALSE) {
        if (!is.null(fig_or_table)) {
          if (as.numeric(end_year) > format(Sys.Date(), "%Y")) {
            cli::cli_abort("end_year is past the current year for a non-projections plot ({topic} {fig_or_table})", wrap = TRUE)
          }
        } else {
          cli::cli_abort("fig_or_table is NULL")
        }
      }
    } else {
      cli::cli_abort("topic is NULL")
    }
  } else {
    cli::cli_abort("end_year is NULL")
  }
}

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
    ref_line = NULL,
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

  cli::cli_h3("Key quantities extracted and inserted from add_more_key_quants():")

  # calculate key quantities that rely on end_year for calculation
  ## terminal fishing mortality
  if (topic_cap_alt$label == "fishing.mortality") {
    if (is.null(dat)) {
      cli::cli_alert_warning("Some key quantities associated with fishing mortality were not extracted and added to captions_alt_text.csv due to missing data file (i.e., 'dat' argument).", wrap = TRUE)
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
}
