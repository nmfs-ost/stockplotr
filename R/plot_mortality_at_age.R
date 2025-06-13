#' Plot natural mortality (M) at age
#'
#' @inheritParams plot_recruitment
#'
#' @return Plot natural mortality at age from a stock assessment model as found in a NOAA
#' stock assessment report.
#' @export
#'
plot_mortality_at_age <- function(dat,
                          make_rda = FALSE,
                          rda_dir = getwd()) {

  # TODO:
  # -Sam check converter- BAM should use a.series, probably, but bsb has
  # only age 0 for that module
  # -Find out how to find M for SS3 models
  # -update M.rate.min, max in write_captions once prev point done
  # -Make test
  # -add to exp_all_figs_tables

  # bam examples have label as natural_mortality but other formats don't (in input)
  # minimum age of M
  if ("natural_mortality" %in% dat$label) {
    m <- dat |>
      dplyr::filter(label == "natural_mortality",
                    !is.na(age)) |>
      dplyr::mutate(
        estimate = as.numeric(estimate),
        age = as.numeric(age)
      )
  } else {
    m <- dat
  }

  # Choose number of breaks for x-axis
  x_n_breaks <- round(length(unique(m[["age"]])))
  if (x_n_breaks > 20) {
    x_n_breaks <- round(length(unique(m[["age"]])) / 5)
  } else if (x_n_breaks > 10) {
    x_n_breaks <- round(length(unique(m[["age"]])) / 2)
  }

  # Make generic plot
  plt <- ggplot2::ggplot(data = m) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = age,
        y = estimate
      ),
      size = 1
    ) +
    ggplot2::labs(
      x = "Age (years)",
      y = "Natural mortality"
    ) +
    ggplot2::scale_x_continuous(
      n.breaks = x_n_breaks
    )

  final <- suppressWarnings(add_theme(plt))

  # create plot-specific variables to use throughout fxn for naming and IDing
  topic_label <- "natural.mortality"

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
    dir = rda_dir
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
