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
#' @param ref_line A string specifying the type of reference you want to
#'   compare spawning biomass to. The default is `"target"`, which looks for
#'   `"spawning_biomass_target"` in the `"label"` column of `dat`. The actual
#'   searching in `dat` is case agnostic and will work with either upper- or
#'   lower-case letters but you must use one of the options specified in the
#'   default list to ensure that the label on the figure looks correct
#'   regardless of how it is specified in `dat`.
#' @param unit_label units for spawning_biomass
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
#' plot_spawning_biomass(
#'   dat = list('base_model'=dat1,'sensitivity1'=dat2,'sensitivity2'=dat3),
#'   geom = "line",
#'   group= "sex",
#'   facet = "fleet",
#'   ref_line = "target",
#'   unit_label = "mt",
#'   scale_amount = 1000
#' )
#' }
plot_spawning_biomass <- function(
    dat,
    geom = "line",
    group = NULL,
    facet = NULL,
    ref_line = "msy",
    unit_label = "metric tons",
    scale_amount = 1,
    relative = FALSE,
    make_rda = FALSE,
    figures_dir = getwd(),
    ...
    ) {
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
    group = group
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
    rp_dat <- dat
  }
  plt2 <- reference_line(
    plot = plt,
    dat = rp_dat,
    label_name = "spawning_biomass",
    reference  = ref_line,
    relative = relative,
    scale_amount = scale_amount
  )
  
  final <- suppressWarnings(add_theme(plt2))
  
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
  return(final)
}
