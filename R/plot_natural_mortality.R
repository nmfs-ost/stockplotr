#' Plot natural mortality (M) at age
#'
#' @inheritParams plot_recruitment
#'
#' @return Plot natural mortality at age from a stock assessment model as 
#' found in a NOAA stock assessment report.
#' @export
#' @examples
#' \dontrun{
#' plot_natural_mortality(
#'   dat = petrale,
#'   module = "Natural_Mortality"
#' )
#' }
#' 
#'
plot_natural_mortality <- function(
    dat,
    group = NULL,
    facet = NULL,
    era = "time",
    interactive = TRUE,
    make_rda = FALSE,
    rda_dir = getwd()
  ) {
  
  # TODO:
  # -update M.rate.min, max in write_captions once prev point done
  # -Make test
  # -add to exp_all_figs_tables
  
  # Extract natural mortality
  filter_data <- prepare_data(
    dat,
    label_name = "natural_mortality",
    era = era,
    group = "age",
    facet = facet,
    geom = geom
  )
  if (!is.null(group)) {
    filter_data <- dplyr::mutate(
      filter_data,
      group_var = .data[[group]]
    ) 
  }
  
  # Check if there is age or year or both
  # Then either plot over ages with lines for year or single year
  if (any(!is.na(filter_data$age))) {
    filter_data <- dplyr::filter(filter_data, !is.na(age))
    if (any(!is.na(filter_data$year))) {
      filter_data <- dplyr::filter(filter_data, !is.na(year))
      # check if all values are the same
      variable <- ifelse(
        length(unique(filter_data$estimate)) != length(unique(filter_data$age)),
        TRUE, # more M values than ages
        FALSE # same # or less M values than ages
        )
    } else {
      variable <- FALSE
    }
  }
  
  if (variable) {
    filter_data <- filter_data |>
      dplyr::group_by(year, age, model, group_var, .data[[group]]) |>
      dplyr::reframe(estimate = unique(estimate))
    # plt <- 
    plot_timeseries(
      dat = filter_data,
      x = "age",
      y = "estimate",
      geom = geom,
      xlab = "Age",
      ylab = "Natural Mortality",
      group = "year",
      facet = c(facet, group)
    )
  } else {
    filter_data <- filter_data |>
      dplyr::group_by(model, group_var, age) |>
      dplyr::summarise(estimate = unique(estimate)) |>
      dplyr::mutate(
        age = as.numeric(age),
        estimate_lower =  NA_real_,
        estimate_upper = NA_real_
        )
    
    plt <- plot_timeseries(
      dat = filter_data,
      x = "age",
      y = "estimate",
      geom = geom,
      group = group,
      facet = facet,
      xlab = "Age",
      ylab = "Natural Mortality"
    )
  }
  
  final <- plt + theme_noaa()
  
  ### Make RDA ----
  if (make_rda) {
    create_rda(
      object = final,
      topic_label = "natural.mortality",
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir
    )
  }
  # Output final plot
  final
  
  # bam examples have label as natural_mortality but other formats don't (in input)
  # minimum age of M
  # if ("natural_mortality" %in% dat$label) {
  #   m <- dat |>
  #     dplyr::filter(label == "natural_mortality",
  #                   !is.na(age)) |>
  #     dplyr::mutate(
  #       estimate = as.numeric(estimate),
  #       age = as.numeric(age)
  #     )
  # } else {
  #   m <- dat
  # }
  # 
  # # Choose number of breaks for x-axis
  # x_n_breaks <- round(length(unique(m[["age"]])))
  # if (x_n_breaks > 20) {
  #   x_n_breaks <- round(length(unique(m[["age"]])) / 5)
  # } else if (x_n_breaks > 10) {
  #   x_n_breaks <- round(length(unique(m[["age"]])) / 2)
  # }
  # 
  # # Make generic plot
  # plt <- ggplot2::ggplot(data = m) +
  #   ggplot2::geom_line(
  #     ggplot2::aes(
  #       x = age,
  #       y = estimate
  #     ),
  #     size = 1
  #   ) +
  #   ggplot2::labs(
  #     x = "Age (years)",
  #     y = "Natural mortality"
  #   ) +
  #   ggplot2::scale_x_continuous(
  #     n.breaks = x_n_breaks
  #   )
  # 
  # final <- suppressWarnings(add_theme(plt))
  # 
  # # create plot-specific variables to use throughout fxn for naming and IDing
  # topic_label <- "natural.mortality"
  # 
  # # identify output
  # fig_or_table <- "figure"
  # 
  # # run write_captions.R if its output doesn't exist
  # if (!file.exists(
  #   fs::path(getwd(), "captions_alt_text.csv")
  # )
  # ) {
  #   stockplotr::write_captions(
  #     dat = dat,
  #     dir = rda_dir,
  #     year = NULL
  #   )
  # }
  # 
  # # add more key quantities included as arguments in this fxn
  # add_more_key_quants(
  #   topic = topic_label,
  #   fig_or_table = fig_or_table,
  #   dir = rda_dir
  # )
  # 
  # # extract this plot's caption and alt text
  # caps_alttext <- extract_caps_alttext(
  #   topic_label = topic_label,
  #   fig_or_table = fig_or_table,
  #   dir = rda_dir
  # )
  # 
  # # export figure to rda if argument = T
  # if (make_rda == TRUE) {
  #   export_rda(
  #     final = final,
  #     caps_alttext = caps_alttext,
  #     rda_dir = rda_dir,
  #     topic_label = topic_label,
  #     fig_or_table = fig_or_table
  #   )
  # }
  # return(final)
}