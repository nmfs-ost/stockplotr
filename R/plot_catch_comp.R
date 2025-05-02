#' Plot catch composition by fleet
#'
#' @inheritParams plot_recruitment
#' @param unit_label indicate the name of the units of catch as to label the axis
#'
#' @return A plot ready for a stock assessment report of catch composition by fleet.
#' This plot is made only when landings are explicitly named in the output file.
#' The current plot function does not combine all sources of catch.
#' @export
#'
plot_catch_comp <- function(dat,
                            unit_label = "metric tons",
                            end_year = NULL,
                            make_rda = FALSE,
                            rda_dir = getwd()) {
  # Units
  catch_label <- glue::glue("Catch ({unit_label})")

  # get end year if not defined (current year)
  if (is.null(end_year)) {
    end_year <- as.numeric(format(Sys.Date(), "%Y"))
  }

  # create plot-specific variables to use throughout fxn for naming and IDing
  topic_label <- "catch"

  # identify output
  fig_or_table <- "figure"

  # check year isn't past end_year if not projections plot
  check_year(
    end_year = end_year,
    fig_or_table = fig_or_table,
    topic = topic_label
  )

  # TODO:
  # -update the following dat code to work for BAM

  # read standard data file and extract target quantity
  if (dim(dat |>
      dplyr::filter(label == "catch"))[1] > 1) {
    catch <- dat |>
      dplyr::filter(label == "catch",
                    !is.na(fleet),
                    year <= end_year
      ) |>
      dplyr::mutate(
        estimate = as.numeric(estimate),
        year = as.numeric(year),
        fleet = as.factor(fleet)
      ) |>
      dplyr::group_by(label, year, fleet) |>
      dplyr::summarise(estimate = sum(estimate)) |>
      suppressWarnings() |>
      dplyr::filter(
        !is.na(year)
      )

    # Choose number of breaks for x-axis
    x_n_breaks <- round(length(unique(catch[["year"]])) / 10)
    if (x_n_breaks <= 5) {
      x_n_breaks <- round(length(unique(catch[["year"]])) / 5)
      if (length(unique(catch[["year"]])) <= 10) {
        x_n_breaks <- NULL
      }
    } else if (x_n_breaks > 10) {
      x_n_breaks <- round(length(unique(catch[["year"]])) / 15)
    }

    # Make generic plot
    plt <- ggplot2::ggplot(data = catch) +
      ggplot2::geom_area(
        ggplot2::aes(
          x = year,
          y = estimate,
          fill = fleet
        )
      ) +
      ggplot2::labs(
        x = "Year",
        y = catch_label,
        fill = "Fleet"
      ) +
      ggplot2::scale_x_continuous(
        n.breaks = x_n_breaks
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_comma()
      )

    final <- suppressWarnings(add_theme(plt))

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
