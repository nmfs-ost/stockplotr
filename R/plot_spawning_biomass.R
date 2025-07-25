#' Plot spawning biomass (SB)
#'
#' Plot spawning biomass with a reference line as a dashed line. The figure can
#' also be made relative to this reference line rather than in absolute units.
#'
#' @param dat A data frame returned from \link[stockplotr]{prepate_data}
#' @param unit_label units for recruitment
#' @param scale_amount A number describing how much to scale down the quantities
#' shown on the y axis. For example, scale_amount = 100 would scale down a value
#' from 500,000 --> 5,000. This scale will be reflected in the y axis label.
#' @param end_year last year of assessment
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
#' @return
#' Plot spawning biomass from the results of an assessment model translated to
#' the standard output. The [ggplot2::ggplot()] object is returned for further
#' modifications if needed.
#' @export
#'
#' @examples
#' \dontrun{
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
  # if (!is.null(ref_point)) {
  #   ref_line <- names(ref_point)
  # } else if (length(ref_line) > 1) {
  #   ref_line <- "msy"
  # } else {
  #   ref_line <- match.arg(ref_line, several.ok = FALSE)
  # }
  # TODO: Fix the unit label if scaling. Maybe this is up to the user to do if
  #       they want something scaled then they have to supply a better unit name
  #       or we create a helper function to do this.
  spawning_biomass_label <- ifelse(
    relative,
    yes = "Relative spawning biomass",
    no = glue::glue("Spawning biomass ({unit_label})")
  )

  # Indicate if spawning biomass is relative or not
  # if (relative) {
  #   topic_label <- "relative.spawning.biomass"
  # } else {
  #   topic_label <- "spawning.biomass"
  # }
  
  # identify output
  # fig_or_table <- "figure"
  
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
  
  # Only run rda export if dat is one otherwise the alt text and caption is not accurate
  # if(length(dat) == 1) {
  #   # export figure to rda if argument = T
  #   if (make_rda == TRUE) {
  #     # run write_captions.R if its output doesn't exist
  #     if (!file.exists(
  #       fs::path(getwd(), "captions_alt_text.csv")
  #     )
  #     ) {
  #       stockplotr::write_captions(
  #         dat = dat,
  #         dir = figures_dir,
  #         year = end_year
  #       )
  #     }
  #     
  #     # add more key quantities included as arguments in this fxn
  #     add_more_key_quants(
  #       dat,
  #       topic = topic_label,
  #       fig_or_table = fig_or_table,
  #       dir = figures_dir,
  #       end_year = end_year,
  #       units = unit_label,
  #       ref_pt = ref_point,
  #       ref_line = ref_line,
  #       scaling = scale_amount
  #     )
  #     
  #     # extract this plot's caption and alt text
  #     caps_alttext <- extract_caps_alttext(
  #       topic_label = topic_label,
  #       fig_or_table = fig_or_table,
  #       dir = figures_dir
  #     )
  #     
  #     export_rda(
  #       final = final,
  #       caps_alttext = caps_alttext,
  #       figures_tables_dir = figures_dir,
  #       topic_label = topic_label,
  #       fig_or_table = fig_or_table
  #     )
  #   }
  # }
  return(final)
}
