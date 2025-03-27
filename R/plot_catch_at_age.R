#' Plot catch at age by fleet
#'
#' @inheritParams plot_recruitment
#' @param unit_label indicate the name of the units of catch as to label the axis
#'
#' @return Create a plot ready for a stock assessment report of catch at age
#' by fleet.
#' @export
#'
plot_catch_at_age <- function(dat,
                          unit_label = "metric tons",
                          make_rda = FALSE,
                          rda_dir = getwd()) {
  # Units
  catch_label <- glue::glue("Catch ({unit_label})")

  # description of plot, for my reference
  "Histograms for each year and fleet (one per year grouped by fleet or survey),
  which show the proportion of the catch in each age group.
  The x axis shows age groups, span from min to max.
  The y axis shows the proportion of the catch, spans from 0-1."

  # TODO: update alt text/caption to remove fleet name/survey name var?
  # -update the following dat code to work for BAM (log_catchability can't be right,
  # but there aren't other labels that have catchability in the name AND age != NA)
  # -update the following dat code to work for SS3
  # update caa vars in write_captions once prev points done
  # -Make unit test
  # -add to exp_all_figs_tables

  # read standard data file and extract target quantity
  caa <- dat |>
    dplyr::filter(
      c(grepl("catchability", label)),
      !is.na(fleet),
      !is.na(age)
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      age = as.numeric(age),
      fleet = as.character(fleet)#,
    #  estimate_log = estimate,
    #  estimate = exp(estimate_log)
    ) |>
    suppressWarnings()

  # Choose number of breaks for x-axis
  x_n_breaks <- round(length(unique(caa[["age"]])))
  if (x_n_breaks > 30) {
    x_n_breaks <- round(length(unique(caa[["age"]])) / 3)
  } else if (x_n_breaks > 15) {
    x_n_breaks <- round(length(unique(caa[["age"]])) / 2)
  }

  # Make generic plot
  plt <- ggplot2::ggplot(data = caa,
                         ggplot2::aes(
                           x = age,
                           y = estimate,
                           fill = fleet
                           )
                         ) +
    ggplot2::geom_bar(stat = "identity") +
  #   ggplot2::facet_wrap(~fleet) +
    ggplot2::labs(
      x = "Age (years)",
      y = catch_label
    ) +
    ggplot2::scale_x_continuous(
      n.breaks = x_n_breaks
    )

  final <- suppressWarnings(add_theme(plt))
final
  # create plot-specific variables to use throughout fxn for naming and IDing
  topic_label <- "CAA"

  # identify output
  fig_or_table <- "figure"

  # run write_captions.R if its output doesn't exist
  if (!file.exists(
    fs::path(getwd(), "captions_alt_text.csv")
  )
  ) {
    stockplotr::write_captions(
      dat = dat,
      dir = rda_dir,
      year = NULL
    )
  }

  # add more key quantities included as arguments in this fxn
  add_more_key_quants(
    topic = topic_label,
    fig_or_table = fig_or_table,
    dir = rda_dir,
    units = unit_label
  )

  # extract this plot's caption and alt text
  caps_alttext <- extract_caps_alttext(
    topic_label = topic_label,
    fig_or_table = fig_or_table,
    dir = rda_dir
  )

  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    export_rda(
      final = final,
      caps_alttext = caps_alttext,
      rda_dir = rda_dir,
      topic_label = topic_label,
      fig_or_table = fig_or_table
    )
  }
  return(final)
}
