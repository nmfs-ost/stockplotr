#' Plot natural mortality (M) at age
#'
#' @inheritParams plot_spawning_biomass
#'
#' @return Plot natural mortality at age from a stock assessment model as 
#' found in a NOAA stock assessment report.
#' @export
#' @examples
#' \dontrun{
#' plot_natural_mortality(
#'   dat = petrale,
#'   module = "Natural_Mortality"
#' )
#' }
#' 
#'
plot_natural_mortality <- function(
    dat,
    group = NULL,
    facet = NULL,
    era = NULL,
    interactive = TRUE,
    module = NULL,
    make_rda = FALSE,
    figures_dir = getwd(),
    ...
  ) {
  
  # TODO:
  # -update M.rate.min, max in write_captions once prev point done
  # -Make test
  # -add to exp_all_figs_tables
  
  # Extract natural mortality
  prepared_data <- filter_data(
    dat,
    label_name = "natural_mortality",
    era = era,
    group = "age",
    facet = facet,
    geom = "line",
    interactive = interactive,
    module = module
  )
  
  # STOP if there are no ages -- indicating this is a single M and would not be plotted
  if (all(is.na(prepared_data$age))) cli::cli_abort("Natural mortality by age not found.")
  
  # This process overrides grouping if it is inaccurate
  processing <- process_data(prepared_data, group, facet)
  # TODO: remove variable from process_data fxn output
  # variable <- processing[[1]]
  processed_data <- processing[[2]]
  group <- processing[[3]]
  facet <- processing[[4]]
  
  plt <- plot_timeseries(
    dat = processed_data |> dplyr::mutate(age = as.numeric(age)),
    x = "age",
    y = "estimate",
    geom = "line",
    xlab = "Age",
    ylab = "Natural Mortality",
    group = group,
    facet = facet
  )
  
  final <- plt + theme_noaa(discrete = TRUE)
  
  ### Make RDA ----
  if (make_rda) {
    create_rda(
      object = final,
      topic_label = "natural.mortality",
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir
    )
  }
  # Output final plot
  final
}