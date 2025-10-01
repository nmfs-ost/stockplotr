# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# General utility functions
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# General utility functions
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Check end_year isn't past current year for non-projections plots
# make year character if not null
check_year <- function(
    end_year = NULL,
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

#------------------------------------------------------------------------------

# create notin operator
`%notin%` <- Negate(`%in%`)

#------------------------------------------------------------------------------
