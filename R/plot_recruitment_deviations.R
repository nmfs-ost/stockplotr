#' Plot Recruitment Deviations
#'
<<<<<<< HEAD
#' @inheritParams plot_spawning_biomass
=======
#' @inheritParams plot_recruitment
#' @param group
#' @param facet description
#' @param ... description
>>>>>>> 057e873 (add baseline timeseries plot and adjustments to plot_rec_devs)
#'
#' @return Plot recruitment deviations relative to one over time from an
#' assessment model output(s) file translated to a standardized output. There are
#' options to return a `ggplot2` object or export an .rda object containing
#' associated caption and alternative text for the figure.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' plot_recruitment_deviations(
#'   dat = list("model1"=dat1,"model2"=dat2),
#'   geom = "point",
#'   make_rda = TRUE,
#'   figures_dir = getwd(),
#'   size = 3
#' )
#' }
plot_recruitment_deviations <- function(
    dat,
    # geom = "point",
    module = NULL,
    make_rda = FALSE,
    figures_dir = getwd(),
    ...
) {
  # Make plot
  filter_data <- prepare_data(
    dat = dat,
    label_name = "recruitment_deviations",
    geom = "point",
    group = NULL,
    module = module
  )

  if (nrow(filter_data) == 0) {
    cli::cli_abort("No recruitment deviations found in data.")
  }

  # Create final plot
  final <- plot_error(
    filter_data,
    geom = "point",
    facet = if(length(unique(filter_data$model)) > 1) "model" else NULL,
    xlab = "Year",
    ylab = "Recruitment Deviations",
    size = 2
  ) +
  # ggplot2::facet_wrap(~ model, scales = "free_y") +
  theme_noaa()

  # Make RDA
  if (make_rda) {
    if(!is.data.frame(dat)) {
      selected_dat <- dat[[1]]
    } else {
      selected_dat <- dat
    }
    create_rda(
      object = final,
      topic_label = "recruitment.deviations",
      fig_or_table = "figure",
      dat = selected_dat,
      dir = figures_dir,
      unit_label = ""
    )
  }
  final
}
