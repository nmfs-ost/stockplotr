#' Plot Recruitment Deviations
#'
#' @inheritParams plot_recruitment
#' @param group
#' @param facet description
#' @param ... description
#'
#' @return Plot recruitment deviations relative to one over time from an
#' assessment model output file translated to a standardized output. There are
#' options to return a `ggplot2` object or export an .rda object containing
#' associated caption and alternative text for the figure.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' plot_recruitment_deviations(dat)
#'
#' plot_recruitment_deviations(
#'   dat,
#'   end_year = 2024,
#'   make_rda = TRUE,
#'   figures_dir = getwd()
#' )
#' }
plot_recruitment_deviations <- function(
    dat,
    group = NULL,
    facet = NULL,
    make_rda = FALSE,
    figures_dir = getwd(),
    ...) {
  
  time <- dat |>
    dplyr::filter(era == "time")
  end_year <- max(as.numeric(time$year), na.rm = TRUE)
  start_year <- min(as.numeric(time$year), na.rm = TRUE)

  # create plot-specific variables to use throughout fxn for naming and IDing
  topic_label <- "recruitment.deviations"

  # identify output
  fig_or_table <- "figure"

  # check year isn't past end_year if not projections plot
  check_year(
    end_year = end_year,
    fig_or_table = fig_or_table,
    topic = topic_label
  )

  rec_devs <- dat |>
    dplyr::filter(
      label == "recruitment_deviations" | label == "log_recruitment_deviations",
      module_name == "SPAWN_RECRUIT" | module_name == "t.series",
      !is.na(year),
      is.na(fleet) | length(unique(fleet)) <= 1,
      is.na(sex) | length(unique(sex)) <= 1,
      is.na(area) | length(unique(area)) <= 1,
      is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
      !year %in% year_exclusions,
      year <= end_year
    ) |> # SS3 and BAM target module names
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year)
    )
  if (nrow(rec_devs) == 0) {
    cli::cli_abort("No recruitment deviations found in data.")
  }

  # Plot
  plt <- plot_timeseries(
    rec_devs,
    x = year,
    y = estimate,
    geom = "point",
    ylab = "Recruitment Deviations"
    ) +
    reference_line(0, linetype = "dashed")
  plt <- add_theme(plt) # suppressWarnings()

  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    # run write_captions.R if its output doesn't exist
    if (!file.exists(
      fs::path(getwd(), "captions_alt_text.csv")
    )
    ) {
      stockplotr::write_captions(
        dat = dat,
        dir = figures_dir,
        year = end_year
      )
    }

    # add more key quantities included as arguments in this fxn
    add_more_key_quants(
      topic = topic_label,
      fig_or_table = fig_or_table,
      dir = figures_dir,
      end_year = end_year
    )

    # extract this plot's caption and alt text
    caps_alttext <- extract_caps_alttext(
      topic_label = topic_label,
      fig_or_table = fig_or_table,
      dir = figures_dir
    )

    export_rda(
      final = final,
      caps_alttext = caps_alttext,
      figures_tables_dir = figures_dir,
      topic_label = topic_label,
      fig_or_table = fig_or_table
    )
  }
  final
}
