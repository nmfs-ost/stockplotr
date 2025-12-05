#' Landed catch by fleet and year table
#'
#' @inheritParams plot_recruitment
#' @param unit_label Abbreviated units of landings
#' @param tables_dir The location of the folder containing the generated table
#' rda files ("tables") that will be created if the argument `make_rda` = TRUE.
#' Default is the working directory.
#'
#' @return Create a table ready for a stock assessment report of landed catch by
#' fleet and year.
#' @export
#'
#' @examples
#' \dontrun{
#' table_landings(dat)
#'
#' table_landings(
#'   dat,
#'   unit_label = "landings label",
#'   end_year = 2024,
#'   make_rda = TRUE,
#'   tables_dir = getwd()
#' )
#' }
table_landings <- function(dat,
                           unit_label = "mt",
                           end_year = format(Sys.Date(), "%Y"),
                           make_rda = FALSE,
                           tables_dir = getwd()) {
  
  # create plot-specific variables to use throughout fxn for naming and IDing
  topic_label <- "landings"
  
  # identify output
  fig_or_table <- "table"
  
  # check year isn't past end_year if not projections plot
  check_year(
    end_year = end_year,
    fig_or_table = fig_or_table,
    topic = topic_label
  )
  
  # filter_data <- function(
  #   dat,
  #   label_name,
  #   module = NULL,
  #   era = "time",
  #   geom,
  #   group = NULL,
  #   facet = NULL,
  #   scale_amount = 1,
  #   interactive = TRUE) {

  # read standard data file and extract target quantity
  prepared_data <- dat |>
    dplyr::filter(
      c(module_name == "TIME_SERIES" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
      # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
      !is.na(fleet)
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year)
    ) |>
    suppressWarnings() |>
    dplyr::filter(
      !is.na(year)
    ) |>
    dplyr::filter(year <= end_year) 
  
  
  
  
  
  
  
  
  
  
  
  #   # TODO: add option to scale data
  #   # Replace all spaces with underscore if not in proper format
  #   label_name <- gsub(" ", "_", tolower(label_name))
  #   list_of_data <- list()
  #   length_dat <- ifelse(
  #     is.data.frame(dat),
  #     1,
  #     length(dat)
  #   )
  #   for (i in 1:length_dat) {
  #     # start for loop to bring together each data as their own geom
  #     # Add columns to data if grouping is selected
  #     # format geoms the way we want
  #     # ggplot easier and more consistent to use
  #     # defaults are focused for stock assessment
  #     # vignette to show how you can filter the data instead of the devs
  #     # vignette is the effort to show what to do and has example
  #     # would have to use the plus operator
  #     
  #     if (is.data.frame(dat)) {
  #       data <- dat
  #       model_label = FALSE
  #     } else {
  #       data <- dat[[i]]
  #       model_label = TRUE
  #     }
  #     data <- data |>
  #       # make sure all labels are lowercase and spaces are replaced with underscores
  #       dplyr::mutate(
  #         label = tolower(gsub(" ", "_", label))
  #       ) |>
  #       dplyr::filter(
  #         grepl(glue::glue("{label_name}"), label)
  #         # era == era
  #       ) |>
  #       dplyr::mutate(
  #         year = as.numeric(year),
  #         model = ifelse(model_label, get_id(dat)[i], NA),
  #         estimate = as.numeric(estimate) / scale_amount,
  #         # calc uncertainty when se
  #         # TODO: calculate other sources of error to upper and lower (cv,)
  #         estimate_lower = dplyr::case_when(
  #           grepl("se", uncertainty_label) ~ (estimate - (1.96 * uncertainty)) / scale_amount,
  #           grepl("sd", tolower(uncertainty_label)) | grepl("std", tolower(uncertainty_label)) ~ (estimate - uncertainty) / scale_amount,
  #           grepl("cv", tolower(uncertainty_label)) ~ (estimate - (1.96 * (uncertainty * estimate))) / scale_amount,
  #           TRUE ~ NA
  #         ),
  #         estimate_upper = dplyr::case_when(
  #           grepl("se", uncertainty_label) ~ (estimate + (1.96 * uncertainty)) / scale_amount,
  #           grepl("sd", tolower(uncertainty_label)) | grepl("std", tolower(uncertainty_label)) ~ (estimate + uncertainty) / scale_amount,
  #           grepl("cv", tolower(uncertainty_label)) ~ (estimate + (1.96 * (uncertainty * estimate))) / scale_amount,
  #           TRUE ~ NA
  #         )
  #       )
  #     # must rename era arg bc dplyr gets confused
  #     era_selection <- era
  #     if (!is.null(era)) {
  #       data <- dplyr::filter(
  #         data,
  #         grepl(era_selection, era)
  #       )
  #     }
  #     if (nrow(data) < 1) cli::cli_abort("{label_name} not found.")
  #     if (is.null(group)) {
  #       if (!is.data.frame(dat)) {
  #         data <- data |>
  #           dplyr::mutate(
  #             group_var = as.character(.data[["model"]])
  #           )
  #       } else {
  #         data <- data |>
  #           dplyr::mutate(
  #             group_var = switch(geom,
  #                                "line" = "solid",
  #                                "point" = "black",
  #                                1
  #             )
  #           )
  #       }
  #     } else if (all(is.na(data[[group]]))) {
  #       data <- data |>
  #         dplyr::mutate(
  #           group_var = switch(geom,
  #                              "line" = "solid",
  #                              "point" = "black",
  #                              1
  #           )
  #         )
  #       # Set group to NULL if second condition is met
  #       group = NULL
  #     } else {
  #       data <- data |>
  #         dplyr::mutate(
  #           group_var = .data[[group]]
  #         )
  #     }
  #     list_of_data[[get_id(dat)[i]]] <- data
  #   }
  #   # Put in
  #   plot_data <- dplyr::bind_rows(list_of_data, .id = "model")
  #   # do.call(rbind, list_of_data)
  #   
  #   # Check if there are multiple module_names present
  #   if (length(unique(plot_data$module_name)) > 1) {
  #     if (!is.null(module)) {
  #       plot_data <- plot_data |>
  #         dplyr::filter(
  #           module_name %in% module
  #         )
  #     } else {
  #       cli::cli_alert_warning("Multiple module names found in data. \n")
  #       options <- c()
  #       for (i in seq_along(unique(plot_data$module_name))) {
  #         # options <- paste0(options, " ", i, ") ", unique(plot_data$module_name)[i], "\n")
  #         options[i] <- paste0(unique(plot_data$module_name)[i])
  #       }
  #       if (interactive()) {
  #         if(interactive) {
  #           # question1 <- utils::menu(
  #           #         options,
  #           #         title = "Please select one of the following:"
  #           #       )
  #           question1 <- utils::select.list(
  #             options,
  #             multiple = TRUE,
  #             title = "Select one or more of the following module names"
  #           )
  #           # selected_module <- unique(plot_data$module_name)[as.numeric(question1)]
  #           selected_module <- intersect(
  #             unique(plot_data$module_name),
  #             question1
  #           )
  #         } else {
  #           selected_module <- unique(plot_data$module_name)[1]
  #           cli::cli_alert_info("Selection bypassed. Filtering by {selected_module}.")
  #         }
  #       } else {
  #         selected_module <- unique(plot_data$module_name)[1]
  #         cli::cli_alert_info(glue::glue("Environment not interactive. Selecting {selected_module}."))
  #       }
  #       if (length(selected_module) > 0) {
  #         plot_data <- plot_data |>
  #           dplyr::filter(
  #             module_name %in% selected_module
  #           )
  #       }
  #     }
  #   }
  # }
  # 
  
  
  
  
  
  
  
  
  
  
  # TODO: add an option to stratify by gear type

  # Units
  land_label <- glue::glue("Landings ({unit_label})")

  # read standard data file and extract target quantity
  land_dat <- dat |>
    dplyr::filter(
      c(module_name == "t.series" & grepl("landings_observed", label)) | c(module_name == "CATCH" & grepl("ret_bio", label)),
      # t.series is associated with a conversion from BAM output and CATCH with SS3 converted output
      !is.na(fleet)
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year)
    ) |>
    suppressWarnings() |>
    dplyr::filter(
      !is.na(year)
    ) |>
    dplyr::filter(year <= end_year)


  # if (is.numeric(land_dat$fleet)){
  #   land_dat$fleet <- paste0("00", land_dat$fleet)
  # }

  if ("uncertainty" %in% names(land_dat)) {
    if ("uncertainty_label" %in% names(land_dat)) {
      uncert_label <- land_dat |>
        dplyr::select(uncertainty_label) |>
        unique() |>
        as.character() |>
        toupper()

      land_dat <- land_dat |>
        dplyr::mutate(uncertainty = round(uncertainty, 2))

      if (uncert_label != "NA") {
        land_dat <- land_dat |>
          dplyr::rename(!!(uncert_label) := "uncertainty")

        piv_vals <- c(
          "Landings",
          uncert_label
        )
      } else {
        uncert_label <- NULL
        piv_vals <- "Landings"
      }
    }
  } else {
    uncert_label <- NULL
    piv_vals <- "Landings"
       }

  # TODO: Reorder column names so that numeric fleets show up in chronological
  # order (currently, lists 1, 10, 11, 12, etc.)

  # Check number of areas and season - if any are >1 then need to use alternative plot (or summarize)
  narea <- length(unique(land_dat$area))
  nseas <- length(unique(land_dat$season))

  if (narea > 1) {
    # factors <- TRUE
    idcols <- c("year", "Area")
    # will need facet if TRUE
  } else {
    idcols <- c("year")
    # factors <- FALSE
  }

  # Check for nseas > 1 - mean of landings through the year
  if (nseas > 1) {
    land_dat <- land_dat |>
      dplyr::group_by(year, fleet, sex, area, growth_pattern) |>
      dplyr::summarize(estimate = mean(estimate)) |>
      dplyr::mutate(fleet = as.factor(fleet)) |>
      dplyr::rename("Area" = area)
  }

  # Extract fleet names
  fleet_names <- unique(as.character(land_dat$fleet))

  land <- land_dat |>
    # dplyr::mutate(
    #   fleet = as.factor(fleet),
    #   #  fleet = paste0("Fleet_", fleet),
    #   year = as.factor(year),
    #   estimate = round(estimate, digits = 0)
    # ) |>
    dplyr::rename("Landings" = estimate) |>
    dplyr::relocate(fleet, .after = season) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(idcols),
      names_from = fleet,
      #  names_prefix = "Fleet_",
      values_from = piv_vals,
      names_glue = "Fleet {fleet}_{.value}"
    ) |>
    dplyr::rename("Year" = year)

  land <- land |>
    dplyr::select(order(colnames(land),
      method = "auto"
    )) |>
    dplyr::relocate(Year, .before = 1) |>
    dplyr::rename_with(~ stringr::str_replace(
      .,
      "Landings",
      land_label
    ))
  
  # add theming to final table
  final <- land |>
    flextable::flextable() |>
    flextable::separate_header() |>
    flextable::merge_h(part = "header") |>
    flextable::align(part = "header") |>
    add_theme() |>
    suppressWarnings()
  final

  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    # run write_captions.R if its output doesn't exist
    if (!file.exists(
      fs::path(getwd(), "captions_alt_text.csv")
    )
    ) {
      stockplotr::write_captions(
        dat = dat,
        dir = tables_dir,
        year = end_year
      )
    }

    # add more key quantities included as arguments in this fxn
    add_more_key_quants(
      dat,
      topic = topic_label,
      fig_or_table = fig_or_table,
      dir = tables_dir,
      end_year = end_year,
      units = unit_label
    )

    # extract this plot's caption and alt text
    caps_alttext <- extract_caps_alttext(
      topic_label = topic_label,
      fig_or_table = fig_or_table,
      dir = tables_dir
    )
    
    # create LaTeX-based table
    latex_table <- create_latex_table(data = land,
                       caption = caps_alttext[1],
                       label = "landings_latex")

    export_rda(
      object = final,
      caps_alttext = caps_alttext,
      figures_tables_dir = tables_dir,
      topic_label = topic_label,
      fig_or_table = fig_or_table,
      latex_table = latex_table
    )
  }
  # Return finished table
  final
}
