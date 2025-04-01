#' Plot catch composition by fleet
#'
#' @inheritParams plot_recruitment
#' @param unit_label indicate the name of the units of catch as to label the axis
#'
#' @return Create a plot ready for a stock assessment report of catch composition
#' by fleet.
#' @export
#'
plot_catch_comp <- function(dat,
                          unit_label = "metric tons",
                          make_rda = FALSE,
                          rda_dir = getwd()) {
  # Units
  catch_label <- glue::glue("Catch ({unit_label})")

  # TODO: update alt text/caption to remove fleet name/survey name var?
  # -update the following dat code to work for BAM
  # -Make unit test
  # -add to exp_all_figs_tables

  # read standard data file and extract target quantity
  if (dim(dat |>
      dplyr::filter(label == "catch"))[1] > 1) {
    caa <- dat |>
      dplyr::filter(label == "catch",
                    !is.na(fleet)
      ) |>
      dplyr::mutate(
        estimate = as.numeric(estimate),
        year = as.numeric(year),
        fleet = as.factor(fleet)
      ) |>
      dplyr::group_by(label, year, fleet) |>
      dplyr::summarise(estimate = sum(estimate)) |>
      suppressWarnings()

    # Choose number of breaks for x-axis
    x_n_breaks <- round(length(unique(caa[["year"]])) / 10)
    if (x_n_breaks <= 5) {
      x_n_breaks <- round(length(unique(caa[["year"]])) / 5)
    } else if (x_n_breaks > 10) {
      x_n_breaks <- round(length(unique(caa[["year"]])) / 15)
    }

    # Make generic plot
    plt <- ggplot2::ggplot(data = caa,
                           ggplot2::aes(
                             x = year,
                             y = estimate,
                             fill = fleet
                           )
    ) +
      ggplot2::geom_bar(stat = "identity",
                        position = "stack") +
      #   ggplot2::facet_wrap(~fleet) +
      ggplot2::labs(
        x = "Year",
        y = catch_label,
        fill = "Fleet",
        color = "Fleet"
      ) +
      ggplot2::scale_x_continuous(
        n.breaks = x_n_breaks
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_comma()
      )

    final <- suppressWarnings(add_theme(plt))

    # create plot-specific variables to use throughout fxn for naming and IDing
    topic_label <- "catch"

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
  } else {
    message("This plot has not yet been developed for models without catch output.")
  }

}
