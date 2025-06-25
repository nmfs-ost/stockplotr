#' Plot Spawn-Recruit Curve
#'
#' @inheritParams plot_recruitment
#' @param spawning_biomass_label Units for spawning biomass
#' @param recruitment_label units for recruitment
#'
#' @return Plot spawning recruitment relationship from a standardized output file originating from \link[asar]{convert_output}
#' @export
#'
#' @examples
#' \dontrun{
#' plot_spawn_recruitment(dat)
#'
#' plot_spawn_recruitment(
#'   dat,
#'   spawning_biomass_label = "sb label",
#'   recruitment_label = "rec label",
#'   end_year = 2024,
#'   make_rda = TRUE,
#'   rda_dir = getwd()
#' )
#' }
plot_spawn_recruitment <- function(
    dat,
    spawning_biomass_label = "metric tons",
    recruitment_label = "metric tons",
    end_year = NULL,
    make_rda = FALSE,
    rda_dir = getwd()) {
  # Extract recruitment
  rec <- dat |>
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
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year)
    ) |>
    dplyr::rename(recruitment = estimate) |>
    dplyr::select(-c(module_name, label))

  if (is.null(end_year)) {
    end_year <- format(Sys.Date(), "%Y")
  }
  stryr <- min(rec$year)

  # create plot-specific variables to use throughout fxn for naming and IDing
  topic_label <- "sr"

  # identify output
  fig_or_table <- "figure"

  # check year isn't past end_year if not projections plot
  check_year(
    end_year = end_year,
    fig_or_table = fig_or_table,
    topic = topic_label
  )

  # Extract spawning biomass
  sb <- dat |>
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
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year)
    ) |>
    dplyr::rename(spawning_biomass = estimate) |>
    dplyr::select(-c(module_name, label))

  # merge DF
  sr <- dplyr::full_join(sb, rec) |>
    dplyr::filter(year <= end_year)

  # Plot
  plt <- ggplot2::ggplot(data = sr) +
    ggplot2::geom_point(ggplot2::aes(x = spawning_biomass, y = recruitment / 1000)) +
    ggplot2::labs(
      x = glue::glue("Spawning Biomass ({spawning_biomass_label})"),
      y = glue::glue("Recruitment ({recruitment_label})")
    ) +
    ggplot2::theme(legend.position = "none")

  final <- suppressWarnings(add_theme(plt))

  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    # run write_captions.R if its output doesn't exist
    if (!file.exists(
      fs::path(getwd(), "captions_alt_text.csv")
    )
    ) {
      stockplotr::write_captions(
        dat = dat,
        dir = rda_dir,
        year = end_year
      )
    }

    # add more key quantities included as arguments in this fxn
    add_more_key_quants(
      topic = topic_label,
      fig_or_table = fig_or_table,
      dir = rda_dir,
      end_year = end_year,
      sr_ssb_units = spawning_biomass_label,
      sr_recruitment_units = recruitment_label,
      units = NULL
    )

    # extract this plot's caption and alt text
    caps_alttext <- extract_caps_alttext(
      topic_label = topic_label,
      fig_or_table = fig_or_table,
      dir = rda_dir
    )

    export_rda(
      final = final,
      caps_alttext = caps_alttext,
      rda_dir = rda_dir,
      topic_label = topic_label,
      fig_or_table = fig_or_table
    )
  }
  return(final)
}
