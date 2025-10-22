#' Plot Abundance (or Numbers) at Age (AAA or NAA)
#'
#' @param dat A data frame returned from \link[asar]{convert_output}
#' @param facet a string or vector of strings of column(s) that
#' groups the data (e.g. "fleet", "sex", "area", etc.).
#' @param unit_label units for abundance
#' @param scale_amount A number describing how much to scale down the abundance at
#' age. Please choose a value ranging from 1-1,000,000,000 (one billion) in orders
#' of magnitude (e.g., 1, 10, 100, 1000, etc.). For example, scale_amount = 100
#' would scale down a value from 500,000 --> 5,000 and would report abundance in
#' hundreds of fish (if "fish" was the unit_label). This scale will be reflected
#' in the legend label if proportional is set to FALSE. The default is 1,000.
#' @param proportional Set size of points relative to z when TRUE, point
#' size are relative to one another while when set to FALSE, point size
#' is relative to z
#' @param make_rda TRUE/FALSE; indicate whether to produce an .rda file containing
#' a list with the figure/table, caption, and alternative text (if figure). If TRUE,
#' the .rda will be exported to the folder indicated in the argument "rda_dir".
#' Default is FALSE.
#' @param figures_dir The location of the folder containing the generated .rda files
#' ("rda_files") that will be created if the argument `make_rda` = TRUE.
#' Default is the working directory.
#' @return Plot total abundance (or numbers) at age from a stock assessment model
#' using a standardized output made from \link[asar]{convert_output}). Units of
#' abundance can either be manually added or will be extracted from the provided
#' file if possible.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_abundance_at_age(dat)
#'
#' plot_abundance_at_age(
#'   dat,
#'   facet = "area",
#'   unit_label = "fish",
#'   scale_amount = 1000,
#'   proportional = TRUE,
#'   make_rda = FALSE,
#'   figures_dir = getwd()
#' )
#' }
plot_abundance_at_age <- function(
  dat,
  facet = NULL,
  unit_label = "fish",
  scale_amount = 1000,
  proportional = TRUE,
  make_rda = FALSE,
  figures_dir = getwd()
) {
  # Create label for abundance units in legend
  abundance_label <- label_magnitude(
    label = "Abudance",
    unit_label = unit_label,
    scale_amount = scale_amount
  )
  # Filter data
  b <- prepare_data(
    dat = dat,
    label_name = "abundance",
    geom = "point",
    group = "age",
    scale_amount = scale_amount,
    interactive = FALSE
  )
  # Check for extracted data, if not return warning and empty plot
  if (nrow(b) == 0) {
    cli::cli_alert_warning("No data found for abundance at age. Please check the input data.")
    return(
      ggplot2::ggplot()
    )
  }

  # Plot data
  plot <- plot_aa(
    dat = b,
    facet = facet,
    label = abundance_label,
    proportional = proportional
  ) +
    average_age_line(
      dat = b,
      facet = facet
    )
  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    create_rda(
      object = plot,
      topic_label = "pop.naa",
      fig_or_table = "figure",
      dat,
      unit_label = "mt"
    )
  }
  plot
}
