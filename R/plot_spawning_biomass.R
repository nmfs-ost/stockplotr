#' Plot spawning biomass (SB)
#'
#' Plot spawning biomass with a reference line as a dashed line. The figure can
#' also be made relative to this reference line rather than in absolute units.
#'
#' @inheritParams plot_recruitment
#' @param ref_line A string specifying the type of reference you want to
#'   compare spawning biomass to. The default is `"msy"`, which looks for
#'   `"spawning_biomass_target"` in the `"label"` column of `dat`. The actual
#'   searching in `dat` is case agnostic and will work with either upper- or
#'   lower-case letters but you must use one of the options specified in the
#'   default list to ensure that the label on the figure looks correct
#'   regardless of how it is specified in `dat`. Other possibilities may include
#'   "target", "MSY", and "unfished".
#' @param ref_point A known value of the reference point along with the label
#'   for the reference point as specified in the output file. Please use this
#'   option if the ref_line cannot find your desired point. Indicate the
#'   reference point in the form c("label" = value).
#' @param unit_label units for spawning_biomass
#' @param module The selected module or name where the data was grouped in  the 
#' original output file.
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
#' @param ... Arguments called from ggplot2::geom_line or ggplotr2::geom_point 
#' @return
#' Plot spawning biomass from the results of an assessment model translated to
#' the standard output. The [ggplot2::ggplot()] object is returned for further
#' modifications if needed.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_spawning_biomass(dat)
#'
#' plot_spawning_biomass(
#'   dat,
#'   unit_label = "my_unit",
#'   ref_line = "msy",
#'   end_year = 2024,
#'   figures_dir = getwd()
#' )
#'
#' plot_spawning_biomass(
#'   dat,
#'   unit_label = "my_unit",
#'   scale_amount = 100,
#'   ref_point = 1000,
#'   end_year = 2024,
#'   relative = TRUE,
#'   make_rda = TRUE,
#'   figures_dir = getwd()
#' )
#' }
plot_spawning_biomass <- function(
    dat,
    geom = "line",
    group = NULL,
    facet = NULL,
    ref_line = "msy",
    unit_label = "metric tons",
    module = NULL,
    scale_amount = 1,
    ref_line = "msy",
    ref_point = NULL,
    end_year = format(Sys.Date(), "%Y"),
    relative = FALSE,
    make_rda = FALSE,
    figures_dir = getwd()) {
  if (!is.null(ref_point)) {
    ref_line <- names(ref_point)
  } else if (length(ref_line) > 1) {
    ref_line <- "target"
  }
  # TODO: Fix the unit label if scaling. Maybe this is up to the user to do if
  #       they want something scaled then they have to supply a better unit name
  #       or we create a helper function to do this.
  spawning_biomass_label <- ifelse(
    relative,
    yes = "Relative spawning biomass",
    no = glue::glue("Spawning biomass ({unit_label})")
  )

  # check year isn't past end_year if not projections plot
  # check_year(
  #   end_year = end_year,
  #   fig_or_table = fig_or_table,
  #   topic = topic_label
  # )

  # Filter data for spawning biomass
  filter_data <- prepare_data(
    dat = dat,
    label_name = "spawning biomass",
    geom = geom,
    group = group,
    module = module,
    scale_amount = scale_amount
  )

  plt <- plot_timeseries(
    dat = filter_data,
    y = "estimate",
    geom = geom,
    ylab = spawning_biomass_label,
    group = group,
    facet = facet
  )
  # Add reference line
  # getting data set - an ifelse statement in the fxn wasn't working
  if (!is.data.frame(dat)) {
    rp_dat <- dat[[1]]
  } else {
    topic_label <- "spawning.biomass"
  }

  final <- reference_line(
    plot = plt,
    dat = rp_dat,
    label_name = "spawning_biomass",
    reference  = ref_line,
    relative = relative,
    scale_amount = scale_amount
  ) + theme_noaa()

  # final <- 

  ### Make RDA ----
  if (make_rda) {
    create_rda(
      object = final,
      topic_label = ifelse(relative, "relative.spawning.biomass", "spawning.biomass"),
      fig_or_table = "figure",
      dat = rp_dat,
      dir = figures_dir,
      ref_line = ref_line,
      ref_point = ref_line, # need to remove this I think
      scale_amount = scale_amount,
      unit_label = unit_label
    )
  }
  # Output final plot
  final
}
