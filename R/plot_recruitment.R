#' Plot Recruitment
#'
#' @inheritParams plot_spawning_biomass
#' @param unit_label units for recruitment
#' 
#' @return Plot recruitment over time from an assessment model output file
#' translated to a standardized output. There are options to return a
#' [ggplot2::ggplot()] object or export an rda object containing associated
#' caption and alternative text for the figure.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_recruitment(dat)
#'
#' plot_recruitment(
#'   dat,
#'   unit_label = "my_unit_label",
#'   scale_amount = 100,
#'   relative = TRUE,
#'   interactive = TRUE,
#'   module = "TIME_SERIES",
#'   make_rda = TRUE,
#'   figures_dir = getwd()
#' )
#' }
plot_recruitment <- function(
    dat,
    unit_label = "mt",
    scale_amount = 1,
    relative = FALSE,
    interactive = TRUE,
    module = NULL,
    make_rda = FALSE,
    figures_dir = getwd()
    ) {
  # TODO: Fix the unit label if scaling
  recruitment_label <- ifelse(
    relative,
    yes = "Relative recruitment",
    no = {
      label_magnitude(
        label = "Recruitment",
        unit_label = unit_label,
        scale_amount = scale_amount,
        legend = FALSE
      )
    }
  )
  
  # Extract recruitment
  recruitment <- prepare_data(
    dat = dat,
    label_name = "recruitment",
    geom = "point",
    interactive = interactive,
    module = module
  ) 
  if (length(unique(recruitment$label)) > 1) {
    recruitment <- recruitment |>
      tidyr::pivot_wider(
        id_cols = c(year, model, group_var, estimate_lower, estimate_upper),
        names_from = label,
        values_from = estimate
      )
  } else {
    recruitment <- recruitment |>
      dplyr::rename(predicted_recruitment = estimate) |>
      dplyr::select(-c(label))
  }
  
  # Plot
  final <- plot_error(
    dat = recruitment,
    x = "year",
    y = "predicted_recruitment",
    color = "black",
    xlab = "Year",
    ylab = recruitment_label,
    hline = FALSE,
    facet = {
      if (length(unique(recruitment$model)) > 1) {
        "model"
      } else {
        NULL
      }
    }
  ) +
    theme_noaa()
  
  if ("expected_recruitment" %in% names(recruitment)) {
    final <- final +
      ggplot2::geom_line(
        data = recruitment,
        ggplot2::aes(x = year, y = expected_recruitment),
        color = "red",
        size = 1
      )
  }
  
  # Make RDA
  if (make_rda) {
    create_rda(
      object = final,
      topic_label = "recruitment",
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir# ,
      # unit_label = unit_label
    )
  }
  final
  
  
  
  
  
  
  
  # # Find R0
  # R0 <- dat |>
  #   dplyr::filter(c(grepl("recruitment", label) & age == 0) |
  #     c(grepl("recruitment_virgin", label))) |>
  #   dplyr::summarise(estimate = max(as.numeric(estimate))) |>
  #   dplyr::pull(estimate)
  # # Units
  # # TODO: fix unit label is scaling
  # recruitment_label <- ifelse(
  #   relative,
  #   yes = "Relative Recruitment (R/R0)",
  #   no = glue::glue("Recruitment ({unit_label})")
  # )
  # # Extract recruitment
  # # TODO: summarize recruitment by season or facet by season
  # rec <- dat |>
  #   dplyr::filter(
  #     label == "recruitment",
  #     module_name == "TIME_SERIES" | module_name == "t.series",
  #     !is.na(year),
  #     is.na(fleet) | length(unique(fleet)) <= 1,
  #     is.na(sex) | length(unique(sex)) <= 1,
  #     is.na(area) | length(unique(area)) <= 1,
  #     is.na(growth_pattern) | length(unique(growth_pattern)) <= 1,
  #     !year %in% year_exclusions
  #   ) |> # SS3 and BAM target module names
  #   dplyr::mutate(
  #     estimate = as.numeric(estimate),
  #     year = as.numeric(year),
  #     estimate_y = estimate / ifelse(relative, R0, scale_amount)
  #   ) # |>
  # # dplyr::rename(recruitment = estimate) |>
  # # dplyr::select(-c(module_name, label))
  # 
  # stryr <- min(rec$year)
  # 
  # rec <- rec |>
  #   dplyr::filter(year <= end_year)
  # 
  # # create plot-specific variables to use throughout fxn for naming and IDing
  # # Indicate if recruitment is relative or not
  # if (relative) {
  #   topic_label <- "relative.recruitment"
  # } else {
  #   topic_label <- "recruitment"
  # }
  # 
  # # identify output
  # fig_or_table <- "figure"
  # 
  # # check year isn't past end_year if not projections plot
  # check_year(
  #   end_year = end_year,
  #   fig_or_table = fig_or_table,
  #   topic = topic_label
  # )
  # 
  # # Choose number of breaks for x-axis
  # x_n_breaks <- round(length(rec[["year"]]) / 10)
  # if (x_n_breaks <= 5) {
  #   x_n_breaks <- round(length(rec[["year"]]) / 5)
  #   if (x_n_breaks <= 2) {
  #     x_n_breaks <- round(length(rec[["year"]]))
  #   }
  # }
  # 
  # plt <- ggplot2::ggplot(data = rec) +
  #   ggplot2::geom_point(
  #     ggplot2::aes(
  #       x = year,
  #       y = estimate_y
  #     )
  #   ) +
  #   ggplot2::geom_line(
  #     ggplot2::aes(
  #       x = year,
  #       y = estimate_y
  #     ),
  #     linewidth = 1
  #   ) +
  #   ggplot2::labs(
  #     x = "Year",
  #     y = recruitment_label
  #   ) +
  #   ggplot2::theme(
  #     legend.position = "none"
  #   ) +
  #   ggplot2::scale_x_continuous(
  #     n.breaks = x_n_breaks,
  #     guide = ggplot2::guide_axis(
  #       minor.ticks = TRUE
  #     )
  #   )
  # 
  # final <- suppressWarnings(add_theme(plt))
  # 
  # # export figure to rda if argument = T
  # if (make_rda == TRUE) {
  #   # run write_captions.R if its output doesn't exist
  #   if (!file.exists(
  #     fs::path(getwd(), "captions_alt_text.csv")
  #   )
  #   ) {
  #     stockplotr::write_captions(
  #       dat = dat,
  #       dir = figures_dir,
  #       year = end_year
  #     )
  #   }
  # 
  #   # add more key quantities included as arguments in this fxn
  #   add_more_key_quants(
  #     dat,
  #     topic = topic_label,
  #     fig_or_table = fig_or_table,
  #     dir = figures_dir,
  #     end_year = end_year,
  #     units = unit_label,
  #     scaling = scale_amount
  #   )
  # 
  #   # extract this plot's caption and alt text
  #   caps_alttext <- extract_caps_alttext(
  #     topic_label = topic_label,
  #     fig_or_table = fig_or_table,
  #     dir = figures_dir
  #   )
  # 
  #   export_rda(
  #     object = final,
  #     caps_alttext = caps_alttext,
  #     figures_tables_dir = figures_dir,
  #     topic_label = topic_label,
  #     fig_or_table = fig_or_table
  #   )
  # }
  # final
}
