#' Plot Total Biomass
#'
#' @inheritParams plot_spawning_biomass
#' @param unit_label units for biomass
#' @param ref_line A string specifying the type of reference you want to
#'   compare biomass to. The default is `"msy"`, which looks for
#'   `"biomass_msy"` in the `"label"` column of `dat`. The actual
#'   searching in `dat` is case agnostic and will work with either upper- or
#'   lower-case letters but you must use one of the options specified in the
#'   default list to ensure that the label on the figure looks correct
#'   regardless of how it is specified in `dat`. Other possibilities may include
#'   "target", "MSY", and "unfished".
#' @return Plot total biomass from a stock assessment model as found in a NOAA
#' stock assessment report. Units of total biomass can either be manually added
#' or will be extracted from the provided file if possible. There are options #' to return a [ggplot2::ggplot()] object or export an rda object containing 
#' associated caption and alternative text for the figure.
#' @export
#'
#' @examples
#' plot_biomass(
#'   dat = stockplotr:::example_data,
#'   unit_label = "mt",
#'   ref_line = "target",
#'   scale_amount = 100,
#'   module = "TIME_SERIES",
#'   figures_dir = getwd()
#' )
#' plot_biomass(
#'   dat = stockplotr:::example_data,
#'   facet = "fleet",
#'   ref_line = "target",
#'   module = "TIME_SERIES",
#'   figures_dir = getwd()
#' )
plot_biomass <- function(
    dat,
    geom = "line",
    group = NULL,
    facet = NULL,
    ref_line = "msy",
    unit_label = "metric tons",
    module = NULL,
    scale_amount = 1,
    relative = FALSE,
    make_rda = FALSE,
    figures_dir = getwd(),
    interactive = TRUE,
    ...) {
  
  # TODO: Fix the unit label if scaling. Maybe this is up to the user to do if
  #       they want something scaled then they have to supply a better unit name
  #       or we create a helper function to do this.
  biomass_label <- ifelse(
    relative,
    yes = "Relative biomass",
    no = {
      label_magnitude(
        label = "Biomass",
        unit_label = unit_label,
        scale_amount = scale_amount,
        legend = FALSE
      )
    }
  )
  
  # Pull first df if in a list to find reference point
  if (!is.data.frame(dat)) {
    rp_dat <- dat[[1]]
  } else {
    rp_dat <- dat
  }
  
  if (relative & scale_amount > 1) {
    cli::cli_alert_warning("Scale amount is not applicable when relative = TRUE. Resetting scale_amount to 1.")
    scale_amount <- 1
  }
  
  # Filter data for spawning biomass
  prepared_data <- filter_data(
    dat = dat,
    label_name = "^biomass$|^biomass_retained$|^biomass_dead$",
    geom = geom,
    group = group,
    facet = facet,
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  )
  
  # check if all 3 are present and subset for one or two
  if (length(unique(prepared_data$label)) > 1 & any(grepl("biomass$", unique(prepared_data$label)))) {
    # cli::cli_alert_info("> 1 label name. Selecting total biomass only.")
    prepared_data <- prepared_data |>
      dplyr::filter(
        grepl("biomass$", label)
      )
  }

  # Process data for indexing/grouping
  # TODO: check and add into process_data step to summarize when theres >1 label
  processing <- process_data(
    prepared_data,
    group,
    facet
  )
  
  # variable <- processing[[1]]
  prepared_data <- processing[[1]]
  group <- processing[[2]]
  if (!is.null(processing[[3]])) facet <- processing[[3]]
  
  # Calculate estimate if relative
  if (relative) {
    if (!is.null(names(ref_line))) {
      ref_line_val <- ref_line[[1]]
      # ref_line <- names(ref_line)
    } else {
      ref_line_val <- calculate_reference_point(
        dat = rp_dat,
        reference_name = glue::glue("^biomass_", ref_line)
      ) / scale_amount
    }
    if (is.na(ref_line_val)) cli::cli_abort("Reference value not found. Cannot plot relative values.")
    prepared_data <- prepared_data |>
      dplyr::mutate(estimate = estimate / ref_line_val)
  }
  
  plt <- plot_timeseries(
    dat = prepared_data,
    y = "estimate",
    geom = geom,
    ylab = biomass_label,
    group = group,
    facet = facet
  )
  # Add reference line
  # getting data set - an ifelse statement in the fxn wasn't working
  
  final <- reference_line(
    plot = plt,
    dat = rp_dat,
    label_name = "biomass",
    reference = ref_line,
    relative = relative,
    scale_amount = scale_amount
  ) +
    theme_noaa()
  
  ### Make RDA ----
  if (make_rda) {
    create_rda(
      object = final,
      topic_label = ifelse(relative, "relative.biomass", "biomass"),
      fig_or_table = "figure",
      dat = rp_dat,
      dir = figures_dir,
      ref_line = ifelse(!is.null(names(ref_line)), names(ref_line), ref_line),
      scale_amount = scale_amount,
      unit_label = unit_label
    )
  }
  # Output final plot
  final
}