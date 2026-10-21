#' Plot Age-to-Length Conversion
#'
#' Creates a ridgeline plot of either fixed age-to-length conversion input or
#' realized Growth-derived age-to-length conversion output.
#'
#' @param data A [FIMSFrame()] containing `age_to_length_conversion` rows, or
#'   long output from [get_estimates()] containing
#'   `age_to_length_conversion_derived` rows.
#' @param fleet Optional fleet name for realized Growth-derived output. It is
#'   required when the output contains multiple fleets.
#'
#' @return A ggplot object.
#'
#' @export
plot_age_to_length_conversion <- function(
  dat,
  group = NULL,
  facet = NULL,
  module = NULL,
  make_rda = FALSE,
  figures_dir = getwd(),
  interactive = TRUE,
  ...
) {
  # if (!requireNamespace("ggridges", quietly = TRUE)) {
  #   cli::cli_abort(
  #     "Package {.pkg ggridges} is required to plot age-to-length conversion."
  #   )
  # }

  # if (inherits(data, "FIMSFrame")) {
  #   if (!is.null(fleet)) {
  #     cli::cli_abort(
  #       "{.arg fleet} only applies to realized Growth-derived model output."
  #     )
  #   }

  #   fixed_conversion_data <- get_data(data) |>
  #     dplyr::filter(
  #       .data[["type"]] == "age_to_length_conversion"
  #     )

  #   if (nrow(fixed_conversion_data) == 0) {
  #     cli::cli_abort(
  #       "The supplied {.cls FIMSFrame} has no {.val age_to_length_conversion} rows."
  #     )
  #   }

  #   plot_data <- fixed_conversion_data |>
  #     dplyr::group_by(
  #       .data[["age"]],
  #       .data[["length"]]
  #     ) |>
  #     dplyr::summarize(
  #       value = mean(.data[["observed"]], na.rm = TRUE),
  #       .groups = "drop"
  #     ) |>
  #     dplyr::mutate(
  #       timing = "Input"
  #     )

  #   plot_title <- "Observed fixed age-to-length conversion input"
  #   plot_subtitle <- "Length distributions by age"
  #   x_label <- "Length"
  #   y_label <- "Age"
  # } else if (inherits(data, "data.frame")) {
  #   required_columns <- c(
  #     "fleet",
  #     "label",
  #     "year_i",
  #     "age_i",
  #     "length_i",
  #     "estimated"
  #   )
  #   missing_columns <- setdiff(required_columns, names(data))

  #   if (length(missing_columns) > 0) {
  #     cli::cli_abort(
  #       "Model output is missing required columns: {.val {missing_columns}}."
  #     )
  #   }

  #   derived_conversion_data <- data |>
  #     dplyr::filter(
  #       .data[["label"]] == "age_to_length_conversion_derived"
  #     )

  #   if (nrow(derived_conversion_data) == 0) {
  #     cli::cli_abort(
  #       "Model output has no {.val age_to_length_conversion_derived} rows."
  #     )
  #   }

  #   available_fleets <- derived_conversion_data |>
  #     dplyr::pull(.data[["fleet"]]) |>
  #     unique()

  #   if (anyNA(available_fleets)) {
  #     cli::cli_abort(
  #       "Model output must include fleet names to plot a derived conversion."
  #     )
  #   }

  #   selected_fleet <- if (is.null(fleet)) {
  #     if (length(available_fleets) != 1) {
  #       cli::cli_abort(
  #         "Model output contains multiple fleets. Supply {.arg fleet} from: {.val {available_fleets}}."
  #       )
  #     }

  #     available_fleets[[1]]
  #   } else {
  #     if (!is.character(fleet) || length(fleet) != 1 || is.na(fleet)) {
  #       cli::cli_abort(
  #         "{.arg fleet} must be a single non-missing character string."
  #       )
  #     }

  #     if (!fleet %in% available_fleets) {
  #       cli::cli_abort(
  #         "{.arg fleet} is not present in model output. Available fleets are: {.val {available_fleets}}."
  #       )
  #     }

  #     fleet
  #   }

  #   plot_data <- derived_conversion_data |>
  #     dplyr::filter(
  #       .data[["fleet"]] == selected_fleet
  #     ) |>
  #     dplyr::transmute(
  #       timing = .data[["year_i"]],
  #       age = .data[["age_i"]],
  #       length = .data[["length_i"]],
  #       value = .data[["estimated"]]
  #     )

  #   plot_title <- "Realized Growth-derived age-to-length conversion"
  #   plot_subtitle <- "Length-bin distributions by age and model-year index"
  #   x_label <- "Fleet length-bin index"
  #   y_label <- "Age index"
  # } else {
  #   cli::cli_abort(
  #     "{.var data} must be a {.cls FIMSFrame} or a long model-output data frame."
  #   )
  # }

  # Filter data for spawning biomass
  prepared_data <- filter_data(
    dat = dat,
    label_name = ifelse(relative, glue::glue("age_to_length_conversion"), "^age_to_length_conversion$"), #Not sure if this is correct
    group = group,
    facet = facet,
    module = module,
    interactive = interactive
  )

    if (nrow(prepared_data) == 0) {
      cli::cli_abort("No data found for relative age-to-length conversion. Please check that your data contains a label for 'age_to_length_conversion'.")
      stop()
    }

  # process the data for grouping
  processing <- process_data(
    dat = prepared_data,
    group = group,
    facet = facet
  )
  # variable <- processing[[1]]
  plot_data <- processing[[1]]
  group <- processing[[2]]
  if (!is.null(processing[[3]])) facet <- processing[[3]]

  # Override grouping variable when there is only NA's
  if (!is.null(group)) {
    if (group %notin% colnames(plot_data)) group <- NULL
  }

#Add plot_density()

  length_bins <- unique(plot_data[["length"]])
  n_length_bins <- dplyr::n_distinct(plot_data[["length"]])

  plt <- plot_density(
    dat = plot_data,
    y = "len_bin", #length bin, not sure what its called yet
    ylab = "Length bin",
    group = group,
    # add check in case facet is returned as character(0)
    facet = if (length(facet) > 0) facet else NULL,
    ...
  )

 ### Make RDA ----
  if (make_rda) {

    #add any additional key quantities or processing needed before creating the RDA

    create_rda(
      object = final,
      topic_label = "age_to_length_conversion",
      fig_or_table = "figure",
      dat = rp_dat,
      dir = figures_dir
    )
  } #end make_rda
  # Output final plot
  final
}


plot_density <- function(){

    # Move this to plot_density function
  plot_data |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["length"]],
        y = as.factor(.data[["age"]]),
        weight = .data[["value"]],
        group = .data[["age"]]
      )
    ) +
    ggridges::geom_density_ridges(
      stat = "binline",
      bins = n_length_bins,
      scale = 1.5,
      alpha = 0.8
    ) +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = x_label,
      y = y_label
    ) +
    ggplot2::scale_x_continuous(
      breaks = length_bins
    ) +
    ggplot2::facet_grid(timing ~ .) +
    ggridges::theme_ridges() +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        color = "grey80",
        linewidth = 0.3
      )
    )
}