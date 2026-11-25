#' Plot Age-to-Length Conversion
#'
#' Creates a density plot of age-to-length conversion, either fixed input or
#' realized growth-derived output.
#' 
#' @inheritParams plot_spawning_biomass
#' @param ... Arguments called from \link[ggridges]{geom_density_ridges}
#' @examples
#' \dontrun{
#' plot_age_to_length_conversion(
#'   dat = your_data,
#'   unit_label = "cm"
#' )
#' }
#'
#' @return A ggplot object.
#'
#' @export
plot_age_to_length_conversion <- function(
  dat,
  group = NULL,
  facet = NULL,
  module = NULL,
  unit_label = "cm",
  make_rda = FALSE,
  figures_dir = getwd(),
  interactive = TRUE,
  ...
) {
  # Filter data for age-to-length conversion
  prepared_data <- filter_data(
    dat = dat,
    label_name = glue::glue("age_to_length_conversion"),
    facet = facet,
    module = module,
    interactive = interactive,
    geom = "line"
  )
  check_data(dat)

  if (nrow(prepared_data) == 0) cli::cli_abort("No data found for relative age-to-length conversion. Please check that your data contains a label for 'age_to_length_conversion'.")

  # process the data for grouping
  processing <- process_data(
    dat = prepared_data,
    facet = facet
  )
 
  plot_data <- processing[[1]]
  group <- processing[[2]]
  if (!is.null(processing[[3]])) facet <- processing[[3]]
  if(group %in% c("age", "length_bins")){
    group <- NULL
  } else {
    facet <- c(facet, group)
    group <- NULL
  }
  if(any(facet %in% c("age", "length_bins"))) {
    facet <- facet[-grep("age|length_bins", facet)]
  }
  # Override grouping variable when there is only NA's
  if (!is.null(group)) {
    if (group %notin% colnames(plot_data)) group <- NULL
  }

  length_bins <- sort(as.numeric(unique(plot_data[["length_bins"]])))
  n_length_bins <- dplyr::n_distinct(plot_data[["length_bins"]])

  plt <- plot_density(
    dat = plot_data,
    xlab = ifelse(is.null(unit_label), "Length bins", glue::glue("Length bins ({unit_label})")),
    n_length_bins = n_length_bins,
    # add check in case facet is returned as character(0)
    facet = if (length(facet) > 0) facet else NULL,
    ...
  ) + 
  theme_noaa()
 

 ### Make RDA ----
  if (make_rda) {

    plot_data <- plot_data |>
      dplyr::mutate(
        age = as.numeric(age),
        length_bins = as.numeric(length_bins)
      )

    #add any additional key quantities or processing needed before creating the RDA
    age.length.group <- glue::glue_collapse(facet, sep = ", ", last = " and ")
    length.units <- unit_label
    age.length.age.min <- min(plot_data[["age"]], na.rm = TRUE)
    age.length.age.max <- max(plot_data[["age"]], na.rm = TRUE)
    age.length.length.min <- min(plot_data[["length_bins"]], na.rm = TRUE)
    age.length.length.max <- max(plot_data[["length_bins"]], na.rm = TRUE)

    get_mean.length.description <- function(which.age) {
      age_data <- plot_data |>
        dplyr::filter(.data[["age"]] == which.age)
      if (length(facet) > 0) {
        age_data <- age_data |>
          dplyr::group_by(dplyr::across(dplyr::all_of(facet)))
      }

      age_data |>
        dplyr::summarize(
          length.at.age = sum(
            .data[["length_bins"]] * .data[["estimate"]],
            na.rm = TRUE
          ) / sum(.data[["estimate"]], na.rm = TRUE),
          .groups = "drop"
        )
    }

    length.at.min.age.kq <- get_mean.length.description(
      age.length.age.min
    )

    length.at.max.age.kq <- get_mean.length.description(
      age.length.age.max
    )

    if (length(facet) > 0) {
      # get the first two facet groups seen in the ggplot
      panel.groups <- ggplot2::ggplot_build(plt)$layout$layout |>
        dplyr::arrange(.data[["PANEL"]]) |>
        dplyr::select(dplyr::all_of(facet)) |>
        dplyr::distinct() |>
        dplyr::slice_head(n = 2)

      # create tibble of key quantities for the first two facet groups
      panel.kqs <- panel.groups |>
        dplyr::left_join(
          dplyr::rename(
            length.at.min.age.kq,
            length.at.min.age = length.at.age
          ),
          by = facet
        ) |>
        dplyr::left_join(
          dplyr::rename(
            length.at.max.age.kq,
            length.at.max.age = length.at.age
          ),
          by = facet
        )
    } else {
      panel.kqs <- tibble::tibble(
        length.at.min.age = length.at.min.age.kq[["length.at.age"]][[1]],
        length.at.max.age = length.at.max.age.kq[["length.at.age"]][[1]]
      )
    }

    # create an empty vector to store the mean length descriptions for each panel
    mean.length.description <- character(nrow(panel.kqs))
    for (i in seq_len(nrow(panel.kqs))) {
      facet.description <- if (length(facet) > 0) {
        # create the facet label used in the ggplot
        paste(
          vapply(
            facet,
            function(f) {
              paste(f, as.character(panel.kqs[[f]][[i]]))
            },
            character(1)
          ),
          collapse = ", "
        )
      } else {
        NULL
      }
      # set variable for which facet is getting described
      description.prefix <- if (is.null(facet.description)) {
        ""
      } else {
        glue::glue("For {facet.description},")
      }

      length.at.min.age <- signif(panel.kqs[["length.at.min.age"]][[i]], digits = 2)
      length.at.max.age <- signif(panel.kqs[["length.at.max.age"]][[i]], digits = 2)
      mean.length.description[[i]] <- glue::glue(
        "{description.prefix} mean length increases from about {length.at.min.age} {length.units} at age {age.length.age.min} to about {length.at.max.age} {length.units} at age {age.length.age.max}. "
      )
    }

    # create full mean length description for all panels
    mean.length.description <- glue::glue_collapse(
      mean.length.description,
      sep = " "
    )

    # calculate & export key quantities
    export_kqs(
      length.units,
      age.length.group,
      age.length.age.min,
      age.length.age.max,
      age.length.length.min,
      age.length.length.max,
      mean.length.description,
      dir = figures_dir
    )
    # Add key quantities to captions/alt text
    insert_kqs(
      length.units,
      age.length.group,
      age.length.age.min,
      age.length.age.max,
      age.length.length.min,
      age.length.length.max,
      mean.length.description,
      dir = figures_dir
    )

    create_rda(
      object = plt,
      topic_label = "age_to_length_conversion",
      fig_or_table = "figure",
      dat = plot_data,
      dir = figures_dir
    )
  } #end make_rda
  
  if (exists("selected_module")) {
    rm(selected_module, envir = .GlobalEnv)
  }
  
  # Output final plot
  plt
}
