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
    geom = "line",
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
    geom = geom,
    interactive = TRUE
  )
  # if (!is.null(group)) {
  #   filter_data <- dplyr::mutate(
  #     filter_data,
  #     group_var = .data[[group]]
  #   ) 
  # } else {
  #   index_variables <- check_grouping(filter_data)
  #   if (length(index_variables) > 0) {
  #     filter_data <- filter_data |>
  #       dplyr::select(dplyr::all_of(c(
  #         "estimate", # "uncertainty", "uncertainty_label",
  #         "estimate_upper", "estimate_lower", "group_var",
  #         # need to maintain columns made from prep data
  #         "model", "group_var",
  #         index_variables
  #         )))
  #   }
  # }
  # 
  # # Check if there is age or year or both
  # # Then either plot over ages with lines for year or single year
  # if (any(!is.na(filter_data$age))) {
  #   filter_data <- dplyr::filter(filter_data, !is.na(age))
  #   index_variables <- index_variables[-grep("age", index_variables)]
  #   if (any(!is.na(filter_data$year))) {
  #     filter_data <- dplyr::filter(filter_data, !is.na(year))
  #     # check if all values are the same
  #     variable <- ifelse(
  #       length(unique(filter_data$estimate)) != length(unique(filter_data$age)),
  #       TRUE, # more M values than ages
  #       FALSE # same # or less M values than ages
  #       )
  #     if (!is.null(group)) {
  #       facet <- c(facet, group)
  #     }
  #     group <- "year"
  #     index_variables <- index_variables[-grep("year", index_variables)]
  #   } else {
  #     variable <- FALSE
  #   }
  # }
  # 
  # # Check if this is still the case if a group not NULL
  # if (!is.null(group) & group != "year") {
  #   check_group_data <- filter_data |>
  #     tidyr::pivot_wider(
  #       id_cols = c(year, age, model),
  #       names_from = dplyr::any_of(group),
  #       values_from = estimate
  #     )
  #   # overwrite variable if grouping is what makes it variable in above conditions
  #   variable <- ifelse(
  #     any(unique(
  #       dplyr::select(
  #         check_group_data, 
  #         dplyr::any_of(unique(filter_data[[group]]))
  #         )
  #       ) > 1),
  #     TRUE,
  #     FALSE
  #   )
  # } else if (length(index_variables) > 0) {
  #   # overwrite variable if a grouping/indexing column was identified
  #   # based on only the first group if >1 identified
  #   variable <- ifelse(
  #     length(unique(filter_data[[index_variables[1]]])) > 1,
  #     TRUE,
  #     FALSE
  #   )
  #   if (group == "year") {
  #     facet <- c(facet, index_variables)
  #   } else {
  #     group <- index_variables[1]
  #     facet <- ifelse(
  #       !is.null(facet),
  #       c(facet, index_variables[-1]),
  #       index_variables[-1]
  #     )
  #   }
  #   
  # }
  
  processing <- process_data(filter_data, group, facet)
  variable <- processing[[1]]
  processed_data <- processing[[2]]
  group <- processing[[3]]
  facet <- processing[[4]]
  
  if (variable) {
    # plt <- 
    plot_timeseries(
      dat = processed_data |> dplyr::mutate(age = as.numeric(age)),
      x = "age",
      y = "estimate",
      geom = geom,
      xlab = "Age",
      ylab = "Natural Mortality",
      group = group,
      facet = facet
    )
  } else {
    plt <- plot_timeseries(
      dat = filter_data |> dplyr::mutate(age = as.numeric(age)),
      x = "age",
      y = "estimate",
      geom = geom,
      group = group,
      facet = facet,
      xlab = "Age",
      ylab = "Natural Mortality"
    )
  }
  
  final <- plt + theme_noaa(discrete = TRUE)
  
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
}