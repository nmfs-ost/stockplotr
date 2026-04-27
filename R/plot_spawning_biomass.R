#' Plot spawning biomass (SB)
#'
#' Plot spawning biomass with a reference line as a dashed line. The figure can
#' also be made relative to this reference line rather than in absolute units.
#'
#' @param dat A tibble or named list of tibbles (input as `list()`)
#' returned from \link[stockplotr]{convert_output}.
#'
#' If inputting a list of tibbles, the first tibble's reference point defined
#' in `ref_line` is used to plot a reference line or calculate relative spawning biomass.
#' @param geom A string stating the geom used for the plot.
#'
#' Default: "line".
#'
#' Options: "line", "point", or "area"
#' @param group A string of a single column that groups the data (e.g. "fleet",
#' "sex", "area", etc.).
#'
#' Set group = "none" to summarize data over all indexing values.
#'
#' Default: NULL
#' @param facet A string or vector of strings of a column name.
#'
#' Default: NULL
#' @param era A string naming the era of data.
#'
#' Default: "time"
#'
#' Options: "early", "time", "fore" (forecast), or NULL (all data)
#' @param ref_line A string specifying a reference point name.
#'
#' Default: "target"
#'
#' Options: (including, but not limited to) "target", "msy", and "unfished"
#' If the reference point is not found in the data, set ref_line = c("{name}" = value).
#' @param unit_label A string specifying spawning biomass unit.
#'
#' Default: "metric tons"
#' @param lbs A logical value indicating whether to convert the y-axis values to pounds.
#'
#' Default: `FALSE`
#' @param module (Optional) A string indicating the module_name found in `dat`.
#'
#' Default: NULL
#'
#' If the interactive and >1 module_name is found, user will select the
#' module_name in the console. @seealso [filter_data()]
#' @param scale_amount A number to scale the y-axis values.
#'
#' Default: 1
#' @param relative A logical value specifying to set y-axis values relative to
#' the ref_line value.
#'
#' Default: `FALSE`
#' @param make_rda A logical value indicating whether to save the object and
#' make an automated caption and alternative text in the form of an `rda` object. If TRUE,
#' the rda will be exported to the folder indicated in the argument "figures_dir".
#'
#' Default: `FALSE`.
#' @param figures_dir A string indicating a path to the "figures" folder.
#'
#' Default: `getwd()`
#'
#' The folder is created within the path if it does not exist.
#' @param interactive A logical value indicating if the environment is interactive.
#'
#' Default: `FALSE`
#' @param ... Arguments called from \link[ggplot2]{geom_line} or \link[ggplot2]{geom_point}
#' @return
#' Plot spawning biomass over time from the results of an assessment model translated to
#' the a standardized output (\link[stockplotr]{convert_output}). There are options to return a
#' \link[ggplot2]{ggplot} object or export an rda object containing associated
#' caption and alternative text for the figure.
#'
#' @note
#' All plotting functions automatically recognize indexing variables and will
#' use them in groupings and/or facetting. @seealso [process_data()].
#'
#' @seealso [convert_output()], [plot_timeseries()],
#' [calculate_reference_point()], [reference_line()], [filter_data()],
#' [process_data()]
#'
#' @export
#'
#' @examples
#' plot_spawning_biomass(
#'   dat = stockplotr:::example_data,
#'   geom = "line",
#'   ref_line = "msy",
#'   unit_label = "mt",
#'   scale_amount = 1000,
#'   interactive = FALSE,
#'   module = "TIME_SERIES",
#'   linewidth = 1.5
#' )
#' plot_spawning_biomass(
#'   dat = stockplotr:::example_data,
#'   relative = TRUE,
#'   ref_line = "msy",
#'   module = "TIME_SERIES"
#' )
#' plot_spawning_biomass(
#'   dat = stockplotr:::example_data,
#'   ref_line = c("target" = 10),
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
  lbs = FALSE,
  era = "time",
  module = NULL,
  scale_amount = 1,
  relative = FALSE,
  make_rda = FALSE,
  figures_dir = getwd(),
  interactive = TRUE,
  ...
) {
  # this assumes that the previous units were metric tons
  if (lbs && unit_label %notin% c("lbs", "pounds", "lb")) {
    cli::cli_alert_info("Unit label was not changed. Setting unit_label to 'lbs'.")
    unit_label <- "1000 lbs"
  }
  
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
        scale_amount = dplyr::if_else(lbs, 1000 * scale_amount, scale_amount), # need to check if this is accurate bc from metric tons starting scale is 1000
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
  prepared_data <- filter_data(
    dat = dat,
    label_name = "^spawning_biomass$",
    geom = geom,
    era = era,
    group = group,
    facet = facet,
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  )

  # process the data for grouping
  processing <- process_data(
    dat = prepared_data,
    group = group,
    facet = facet,
    method = "sum",
    lbs = lbs
  )
  # variable <- processing[[1]]
  plot_data <- processing[[1]]
  group <- processing[[2]]
  if (!is.null(processing[[3]])) facet <- processing[[3]]

  # Override grouping variable when there is only NA's
  if (!is.null(group)) {
    if (group %notin% colnames(plot_data)) group <- NULL
  }

  # Calculate estimate if relative
  if (relative) {
    if (!is.null(names(ref_line))) {
      ref_line_val <- ref_line[[1]]
      # ref_line <- names(ref_line)
    } else {
      ref_line_val <- calculate_reference_point(
        dat = rp_dat,
        reference_name = glue::glue("spawning_biomass_", ref_line),
        lbs = lbs
      ) / scale_amount
    }
    if (is.na(ref_line_val)) cli::cli_abort("Reference value not found. Cannot plot relative values.")
    plot_data <- plot_data |>
      dplyr::mutate(estimate = estimate / ref_line_val)
  }

  plt <- plot_timeseries(
    dat = plot_data,
    y = "estimate",
    geom = geom,
    ylab = spawning_biomass_label,
    group = group,
    # add check in case facet is returned as character(0)
    facet = if (length(facet) > 0) facet else NULL,
    ...
  )
  # Add reference line
  # getting data set - an ifelse statement in the fxn wasn't working
  final <- reference_line(
    plot = plt,
    dat = rp_dat,
    # era = era,
    lbs = lbs,
    label_name = "spawning_biomass",
    reference = ref_line,
    relative = relative,
    scale_amount = scale_amount
  ) + theme_noaa()

  ### Make RDA ----
  if (make_rda) {
    if (relative) {
      # pulling out the 2nd df in 'data' works for several datasets
      rel.ssb.min <- ggplot2::ggplot_build(final)@data[[2]] |>
        as.data.frame() |>
        dplyr::pull(y) |>
        min() |>
        round(digits = 2)
      rel.ssb.max <- ggplot2::ggplot_build(final)@data[[2]] |>
        as.data.frame() |>
        dplyr::pull(y) |>
        max() |>
        round(digits = 2)

      # calculate & export key quantities
      export_kqs(rel.ssb.min, rel.ssb.max)

      # Add key quantities to captions/alt text
      insert_kqs(rel.ssb.min, rel.ssb.max)
    } else {
      ssb.min <- min(plot_data$estimate) |> round(digits = 3)
      ssb.max <- max(plot_data$estimate) |> round(digits = 3)

      export_kqs(ssb.min, ssb.max)
      insert_kqs(ssb.min, ssb.max)
    }

    # Obtain relevant key quantities for captions/alt text
    ssb.ref.pt <- as.character(ref_line)
    ssb.units <- as.character(unit_label)
    ssb.start.year <- min(plot_data$year)
    ssb.end.year <- max(plot_data$year)

    # calculate & export key quantities
    export_kqs(
      ssb.ref.pt,
      ssb.units,
      ssb.start.year,
      ssb.end.year
    )

    # Add key quantities to captions/alt text
    insert_kqs(
      ssb.ref.pt,
      ssb.units,
      ssb.start.year,
      ssb.end.year
    )

    create_rda(
      object = final,
      topic_label = ifelse(relative, "relative_spawning_biomass", "spawning_biomass"),
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
