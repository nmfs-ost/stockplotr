#' Plot Abundance (or Numbers) at Age (AAA or NAA)
#'
#' @param dat A data frame returned from \link[asar]{convert_output}
#' @param unit_label units for recruitment
#' @param end_year last year of assessment
#' @param relative A logical value specifying if the resulting figures should
#'   be relative spawning biomass. The default is `FALSE`. `ref_line` indicates
#'   which reference point to use.
#' @param make_rda TRUE/FALSE; indicate whether to produce an .rda file containing
#' a list with the figure/table, caption, and alternative text (if figure). If TRUE,
#' the .rda will be exported to the folder indicated in the argument "rda_dir".
#' Default is FALSE.
#' @param rda_dir The location of the folder containing the generated .rda files
#' ("rda_files") that will be created if the argument `make_rda` = TRUE.
#' Default is the working directory.
#' @param scale_amount A number describing how much to scale down the abundance at
#' age. Please choose a value ranging from 1-1,000,000,000 (one billion) in orders
#' of magnitude (e.g., 1, 10, 100, 1000, etc.). For example, scale_amount = 100
#' would scale down a value from 500,000 --> 5,000 and would report abundance in
#' hundreds of fish (if "fish" was the unit_label). This scale will be reflected
#' in the legend label. The default is 1,000.
#' @return Plot total abundance (or numbers) at age from a stock assessment model as found in a NOAA
#' stock assessment report. Units of abundance can either be manually added
#' or will be extracted from the provided file if possible.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_abundance_at_age(dat)
#'
#' plot_abundance_at_age(
#'   dat,
#'   group = "area",
#'   unit_label = "fish",
#'   scale_amount = 1000,
#'   make_rda = FALSE,
#'   figures_dir = getwd()
#' )
#' }
plot_abundance_at_age <- function(
    dat,
    group = NULL,
    unit_label = "fish",
    scale_amount = 1000,
    make_rda = FALSE,
    figures_dir = getwd()) {
  magnitude <- floor(log10(scale_amount))
  if (magnitude == 0) {
    scale_unit <- ""
    unit_mag <- ""
  } else if (magnitude > 0 & magnitude < 10) {
    scale_unit <- c(
      "tens of ",
      "hundreds of ",
      "thousands of ",
      "tens of thousands of ",
      "hundreds of thousands of ",
      "millions of ",
      "tens of millions of ",
      "hundreds of millions of ",
      "billions of "
    )
    unit_mag <- paste(scale_unit[magnitude])
  } else {
    cli::cli_abort("Scale_amount is out of bounds. Please choose a value ranging from 1-1000000000 (one billion) in orders of magnitude (e.g., 1, 10, 100, 1000, etc.)", wrap = TRUE)
  }
  
  abundance_label <- ifelse(
    scale_amount == 1000,
    yes = glue::glue("Abundance \n({unit_mag}{unit_label})"),
    no = glue::glue("Relative abundance")
  )

  b <- prepare_data(
    dat = dat,
    label_name = "abundance",
    geom = "point",
    group = "age"
  )
  # Plot data
  plot <- plot_aa(
    dat = b,
    facet = group,
    label = abundance_label
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
