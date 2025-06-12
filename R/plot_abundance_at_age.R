#' Plot Abundance (or Numbers) at Age (AAA or NAA)
#'
#' @inheritParams plot_recruitment
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
#'   unit_label = "my_unit",
#'   scale_amount = 10,
#'   end_year = 2024,
#'   make_rda = TRUE,
#'   figures_tables_dir = getwd()
#' )
#' }
plot_abundance_at_age <- function(
    dat,
    unit_label = "fish",
    scale_amount = 1000,
    end_year = NULL,
    make_rda = FALSE,
    figures_tables_dir = getwd()) {
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
    yes = glue::glue("Abundance ({unit_mag}{unit_label})"),
    no = glue::glue("Relative abundance ({unit_mag}{unit_label})")
  )

  b <- dat |>
    dplyr::filter(
      label == "abundance",
      module_name != "proj.N.age", # SS3 and BAM target module names
      !is.na(age)
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year),
      estimate_orig = estimate,
      estimate = estimate_orig / scale_amount
    )

  if (dim(b |>
    dplyr::select(time) |>
    dplyr::filter(!is.na(time)))[1] > 1) {
    b <- b |>
      dplyr::filter(time %% 1 != 0.5) |>
      dplyr::group_by(age, year) |>
      dplyr::summarise(estimate = sum(estimate), .groups = "drop") |>
      dplyr::ungroup()
  }

  # get end year if not defined
  if (is.null(end_year)) {
    end_year <- format(Sys.Date(), "%Y")
  }

  # create plot-specific variables to use throughout fxn for naming and IDing
  topic_label <- "pop.naa"

  # identify output
  fig_or_table <- "figure"

  # check year isn't past end_year if not projections plot
  check_year(
    end_year = end_year,
    fig_or_table = fig_or_table,
    topic = topic_label
  )

  b <- b |>
    dplyr::filter(year <= end_year)

  total_fish_per_year <- b |>
    dplyr::group_by(year) |>
    dplyr::summarise(total_fish = sum(estimate))

  annual_means <- b |>
    dplyr::group_by(age, year) |>
    dplyr::summarise(years_per_year = sum(estimate)) |>
    dplyr::filter(age != 0) |>
    dplyr::group_by(year) |>
    dplyr::summarise(interm = sum(age * years_per_year)) |>
    dplyr::full_join(total_fish_per_year) |>
    dplyr::mutate(avg = interm / total_fish)

  # Choose number of breaks for x-axis
  x_n_breaks <- round(length(unique(b[["year"]])) / 10)
  if (x_n_breaks <= 5) {
    x_n_breaks <- round(length(unique(b[["year"]])) / 5)
    if (x_n_breaks <= 2) {
      x_n_breaks <- round(length(unique(b[["year"]])))
    }
  } else if (x_n_breaks > 10) {
    x_n_breaks <- round(length(unique(b[["year"]])) / 15)
  }

  # Choose number of major breaks for y-axis
  y_n_breaks <- round(length(unique(b[["age"]])))
  if (y_n_breaks > 80) {
    y_n_breaks <- round(length(unique(b[["age"]])) / 6)
  } else if (y_n_breaks > 40) {
    y_n_breaks <- round(length(unique(b[["age"]])) / 3)
  }

  # Choose number of minor breaks for y-axis
  y_n_breaks_minor <- as.vector(unique(b$age))
  if (length(y_n_breaks_minor) > 40) {
    y_n_breaks_minor <- NULL
  } else if (length(y_n_breaks_minor) > 20) {
    y_n_breaks_minor <- y_n_breaks_minor[c(TRUE, FALSE)]
  }



  # plot
  plt <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = b,
      ggplot2::aes(
        x = year,
        y = age,
        size = estimate
      ),
      shape = 21,
      alpha = 0.3,
      color = "black",
      fill = "gray40"
    ) +
    ggplot2::scale_size(
      range = c(0.2, 10),
      name = abundance_label,
      labels = scales::label_comma()
    ) +
    # add line
    ggplot2::geom_line(
      data = annual_means,
      ggplot2::aes(
        x = year,
        y = avg
      ),
      linewidth = 1,
      color = "red"
    ) +
    ggplot2::labs(
      x = "Year",
      y = "Age"
    ) +
    ggplot2::scale_x_continuous(
      n.breaks = x_n_breaks,
      guide = ggplot2::guide_axis(minor.ticks = TRUE)
    ) +
    ggplot2::scale_y_continuous(
      minor_breaks = y_n_breaks_minor,
      n.breaks = y_n_breaks,
      guide = ggplot2::guide_axis(minor.ticks = TRUE),
      limits = c(min(b$age), max(b$age))
    )

  final <- stockplotr::add_theme(plt)

  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    # run write_captions.R if its output doesn't exist
    if (!file.exists(
      fs::path(getwd(), "captions_alt_text.csv")
    )
    ) {
      stockplotr::write_captions(
        dat = dat,
        dir = figures_tables_dir,
        year = end_year
      )
    }

    # add more key quantities included as arguments in this fxn
    add_more_key_quants(
      dat,
      topic = topic_label,
      fig_or_table = fig_or_table,
      dir = figures_tables_dir,
      end_year = end_year,
      units = unit_label,
      # ref_pt = ref_point,
      scaling = scale_amount
    )

    # extract this plot's caption and alt text
    caps_alttext <- extract_caps_alttext(
      topic_label = topic_label,
      fig_or_table = fig_or_table,
      dir = figures_tables_dir
    )

    export_rda(
      final = final,
      caps_alttext = caps_alttext,
      figures_tables_dir = figures_tables_dir,
      topic_label = topic_label,
      fig_or_table = fig_or_table
    )
  }
  return(final)
}
