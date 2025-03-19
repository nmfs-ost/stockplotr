#' Plot Total Biomass at Age (BAA)
#'
#' @inheritParams plot_recruitment
#' @return Plot total biomass at age from a stock assessment model as found in a NOAA
#' stock assessment report. Units of total biomass can either be manually added
#' or will be extracted from the provided file if possible. In later releases, model will not
#' @export
#'
plot_biomass_at_age <- function(
    dat,
    unit_label = "metric tons",
    end_year = NULL,
    make_rda = FALSE,
    rda_dir = getwd()) {

  biomass_label <- glue::glue("Biomass ({unit_label})")

   b <- dat |>
    dplyr::filter(
      label == "biomass",
      module_name == "BIOMASS_AT_AGE" | module_name == "B.age", # SS3 and BAM target module names
      !is.na(age)
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year)
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
    end_year <- max(b$year)
  }

   total_fish_per_year <- b |>
     dplyr::group_by(year) |>
     dplyr::summarise(total_fish = sum(estimate))

   annual_means <- b |>
     dplyr::group_by(age, year) |>
     dplyr::summarise(years_per_year = sum(estimate)) |>
     dplyr::filter(age != 0) |>
     dplyr::group_by(year) |>
     dplyr::summarise(interm = sum(age*years_per_year)) |>
     dplyr::full_join(total_fish_per_year) |>
     dplyr::mutate(avg = interm/total_fish)

  # plot
  plt <- ggplot2::ggplot() +
    ggplot2::geom_point(data = subset(b, year <= end_year),
                        ggplot2::aes(x = year,
                                     y = age,
                                     size = estimate),
                        shape=21,
                        alpha = 0.2,
                        color = "black",
                        fill = "gray40") +
    ggplot2::scale_size(range = c(.1, 3),
                        name = biomass_label) +
    # add line
    ggplot2::geom_line(
      data = annual_means,
      ggplot2::aes(x = year,
          y = avg),
      linewidth = 1,
      color = "red"
    ) +
    ggplot2::labs(
      x = "Year",
      y = "Age"
    )

  final <- stockplotr::add_theme(plt)

  final

  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    # create plot-specific variables to use throughout fxn for naming and IDing
    # Indicate if biomass is relative or not
      topic_label <- "pop.baa"

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
        year = end_year
      )
    }

    # add more key quantities included as arguments in this fxn
    add_more_key_quants(
      dat,
      topic = topic_label,
      fig_or_table = fig_or_table,
      dir = rda_dir,
      end_year = end_year,
      units = unit_label#,
     # ref_pt = ref_point,
     # scaling = scale_amount
    )

    # extract this plot's caption and alt text
    caps_alttext <- extract_caps_alttext(
      topic_label = topic_label,
      fig_or_table = fig_or_table,
      dir = rda_dir
    )

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
