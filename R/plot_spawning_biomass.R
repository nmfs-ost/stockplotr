#' Plot spawning biomass (SB)
#'
#' Plot spawning biomass with a reference line as a dashed line. The figure can
#' also be made relative to this reference line rather than in absolute units.
#'
#' @param dat A data frame or names list of data frames returned from
#' \link[asar]{convert_output}. The first data frame in the list is used in
#' calculation of a reference line if one is present
#' @param geom A string stating the geom used for the plot. Default is "line".
#' Options include "line", "point", or "area"
#' @param group a string of a single column that groups the data (e.g. "fleet",
#' "sex", "area", etc.). Currently can only have one level of grouping.
#' @param facet a string or vector of strings of a column that facets the data
#' (e.g. "year", "area", etc.)
#' @param era a string naming the era of data such as historical ("early"), current ("time"), or 
#' projected ("fore") data if filtering should occur. Default is set to "time" which is 
#' the current time. To plot all data, set era to NULL. 
#' @param ref_line A string specifying the type of reference you want to
#'   compare spawning biomass to. The default is `"target"`, which looks for
#'   `"spawning_biomass_target"` in the `"label"` column of `dat`. The actual
#'   searching in `dat` is case agnostic and will work with either upper- or
#'   lower-case letters but you must use one of the options specified in the
#'   default list to ensure that the label on the figure looks correct
#'   regardless of how it is specified in `dat`.
#' @param unit_label units for spawning_biomass
#' @param module (Optional) A string indicating the linked module_name associated
#' with the label for the plot if known. Default is NULL. By default, the function
#' will select the most relevant module if more than 1 exists.
#' @param scale_amount A number describing how much to scale down the quantities
#' shown on the y axis. For example, scale_amount = 100 would scale down a value
#' from 500,000 --> 5,000. This scale will be reflected in the y axis label.
#' @param relative A logical value specifying if the resulting figures should
#'   be relative spawning biomass. The default is `FALSE`. `ref_line` indicates
#'   which reference point to use.
#' @param make_rda TRUE/FALSE; indicate whether to produce an .rda file containing
#' a list with the figure/table, caption, and alternative text (if figure). If TRUE,
#' the rda will be exported to the folder indicated in the argument "figures_dir".
#' Default is FALSE.
#' @param figures_dir The location of the folder containing the generated figure
#' rda files ("figures") that will be created if the argument `make_rda` = TRUE.
#' Default is the working directory.
#' @param interactive TRUE/FALSE; indicate whether the environment in which the
#' function is operating  is interactive. This bypasses some options for
#' filtering when preparing data for the plot. Default is FALSE.
#' @param ... Arguments called from ggplot2::geom_line or ggplot2::geom_point 
#' @return
#' Plot spawning biomass over time from the results of an assessment model translated to
#' the a standardized output (\link[asar]{convert_output}). There are options to return a
#' [ggplot2::ggplot()] object or export an rda object containing associated
#' caption and alternative text for the figure.
#' @export
#'
#' @examples
#' plot_spawning_biomass(
#'   dat = stockplotr:::example_data,
#'   geom = "line",
#'   ref_line = "sy",
#'   unit_label = "mt",
#'   scale_amount = 1000,
#'   interactive = FALSE,
#'   module = "TIME_SERIES" 
#' )
plot_spawning_biomass <- function(
    dat,
    geom = "line",
    group = NULL,
    facet = NULL,
    ref_line = "msy",
    unit_label = "metric tons",
    era = "time",
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
  spawning_biomass_label <- ifelse(
    relative,
    yes = "Relative spawning biomass",
    no = {
      label_magnitude(
        label = "Spawning Biomass",
        unit_label = unit_label,
        scale_amount = scale_amount,
        legend = TRUE
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
  filter_data <- prepare_data(
    dat = dat,
    label_name = "spawning_biomass$",
    geom = geom,
    era = era,
    group = group,
    facet = facet,
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  )

  # Override grouping variable when there is only NA's
  if (!is.null(group)) {
    if (group %notin% colnames(filter_data)) group = NULL
  }

  # Calculate estimate if relative
  if (relative) {
    if (!is.null(names(ref_line))) {
      ref_line_val <- ref_line[[1]]
      # ref_line <- names(ref_line)
    } else {
      ref_line_val <- calculate_reference_point(
        dat = rp_dat,
        reference_name = glue::glue("spawning_biomass_", ref_line)
      ) / scale_amount
    }
    if (is.na(ref_line_val)) cli::cli_abort("Reference value not found. Cannot plot relative values.")
    filter_data <- filter_data |>
      dplyr::mutate(estimate = estimate / ref_line_val)
  }

  plt <- plot_timeseries(
    dat = filter_data,
    y = "estimate",
    geom = geom,
    ylab = spawning_biomass_label,
    group = group,
    facet = facet,
    ...
  )
  # Add reference line
  # getting data set - an ifelse statement in the fxn wasn't working
  

  final <- reference_line(
    plot = plt,
    dat = rp_dat,
    era = era,
    label_name = "spawning_biomass",
    reference = ref_line,
    relative = relative,
    scale_amount = scale_amount
  ) + theme_noaa()

  # Plot vertical lines if era is not filtering
  if (is.null(era)) {
    # Find unique era
    eras <- unique(filter_data$era)
    if (length(eras) > 1) {
      year_vlines <- c()
      for (i in 2:length(eras)){
        erax <- filter_data |>
        dplyr::filter(era == eras[i]) |>
        dplyr::pull(year) |>
        min(na.rm = TRUE)
        year_vlines <- c(year_vlines, erax)
      }
    }
    final <- final +
      ggplot2::geom_vline(
        xintercept = year_vlines,
        color = "#999999"
      )
  }

  ### Make RDA ----
  if (make_rda) {
    create_rda(
      object = final,
      topic_label = ifelse(relative, "relative.spawning.biomass", "spawning.biomass"),
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
