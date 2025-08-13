#' Plot Recruitment Deviations
#'
#' @inheritParams plot_spawning_biomass
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
    geom = "point",
    relative = FALSE,
    make_rda = FALSE,
    figures_dir = getwd(),
    ...
) {
  # Make plot
  filter_data <- prepare_data(
    dat = dat,
    label_name = "recruitment_deviations",
    geom = geom,
    group = NULL
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
    create_rda(
      object = final,
      topic_label = "recruitment.deviations",
      fig_or_table = "figure",
      dat = filter_data,
      dir = figures_dir,
      unit_label = ""
    )
  }

  }
  final
}
