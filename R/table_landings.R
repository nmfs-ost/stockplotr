#' Landed catch by fleet and year table
#'
#' @inheritParams plot_recruitment
#' @param unit_label Abbreviated units of landings
#' @param group A string identifying the indexing variable of the data. If you
#' want to just summarize the data across all factors, set group = "none".
#' @param method A string describing the method of summarizing data when group
#' is set to "none". Options are "sum" or "mean". Default is "sum".
#' @param tables_dir The location of the folder containing the generated table
#' rda files ("tables") that will be created if the argument `make_rda` = TRUE.
#' @param label The label that will be chosen from the input file. If unspecified,
#' the function will search the "label" column and use the first matching label
#' in this ordered list: "landings_weight",  "landings_numbers", "landings_expected",
#' "landings_predicted", "landings".
#'
#' @return Create a table ready for a stock assessment report of landed catch by
#' fleet and year.
#' @export
#'
#' @examples
#' table_landings(stockplotr::example_data)
#'
#' table_landings(
#'   stockplotr::example_data,
#'   unit_label = "landings label",
#'   group =
#'   )
table_landings <- function(
  dat,
  unit_label = "mt",
  era = NULL,
  interactive = TRUE,
  group = NULL,
  method = "sum",
  module = NULL,
  label = NULL,
  make_rda = FALSE,
  tables_dir = getwd()
) {
  # TODO: do group and facet need to be uncommented and updated?
  # Filter data for landings
  prepared_data <- filter_data(
    dat = dat,
    label_name = "landings",
    geom = "line",
    era = era,
    module = module,
    scale_amount = 1,
    interactive = interactive
  ) |>
    dplyr::mutate(estimate = round(as.numeric(estimate), digits = 0)) |>
    dplyr::mutate(uncertainty = round(as.numeric(uncertainty), digits = 2))

  # Add check if there is any data
  if (nrow(prepared_data) == 0) {
    cli::cli_abort("No landings data found.")
  }

  # get uncertainty label by model
  uncert_lab <- prepared_data |>
    dplyr::filter(!is.na(uncertainty_label)) |>
    dplyr::group_by(model) |>
    dplyr::reframe(unique_uncert = unique(uncertainty_label)) # changed to reframe -- may cause errors
  uncert_lab <- stats::setNames(uncert_lab$unique_uncert, uncert_lab$model)
  # if (length(unique(uncert_lab)) == 1) uncert_lab <- unique(uncert_lab) # might need this line

  # This needs to be adjusted when comparing different models and diff error
  if (length(uncert_lab) > 1 & length(unique(uncert_lab)) == 1 | length(names(uncert_lab)) == 1) { # prepared_data$model
    # cli::cli_alert_warning("More than one value for uncertainty exists: {uncert_lab}")
    uncert_lab <- uncert_lab[[1]]
    # cli::cli_alert_warning("The first value ({uncert_lab}) will be chosen.")
  }

  if (is.na(uncert_lab)) uncert_lab <- "uncertainty"

  # get fleet names
  # TODO: change from fleets to id_group AFTER the process data step and adjust throughout the table based on indexing
  fleets <- unique(prepared_data$fleet) |>
    # sort numerically even if fleets are 100% characters
    stringr::str_sort(numeric = TRUE)

  # TODO: fix this so that fleet names aren't removed if, e.g., group = "fleet"
  table_data_info <- process_table(
    dat = prepared_data,
    # group = group,
    method = method,
    label = label
  )
  table_data <- table_data_info[[1]]
  indexed_vars <- table_data_info[[2]]
  id_col_vals <- table_data_info[[3]]

  # id_group_vals <- sapply(id_cols, function(x) unique(prepared_data[[x]]), simplify = FALSE)
  # TODO: add check if there is a landings column for every error column -- if not remove the error (can keep landings)

  # merge error and landings columns and rename
  df_list <- merge_error(
    table_data,
    uncert_lab,
    fleets,
    label = "landings",
    unit_label
  )
  
  # Add check for length label >1
  # below method will only work when unqiue(label) == 2
  if (length(unique(prepared_data$label)) > 1){
    cli::cli_alert_info("Multiple labels found in prepared data: {unique(prepared_data$label)}")
    # check if the multiple labels are equal for all indexing
    # all non-indexing variables
    indexing_vars <- colnames(prepared_data)[-grep(
      paste(
        c("year", "estimate", 
          "uncertainty", "uncertainty_label", 
          "label", "module_name", 
          "likelihood", "initial"),
        collapse = "|"), colnames(prepared_data))]

    # compare estimate across all indexing vars and see if they are different over years
    label_differences <- prepared_data |>
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(indexing_vars),
        names_from = label,
        values_from = estimate
      ) |>
      dplyr::mutate(
        diff = .data[[unique(prepared_data$label)[1]]] - .data[[unique(prepared_data$label)[2]]]
      )
    
    if (all(label_differences$diff == 0)){
      cli::cli_alert_info("Labels have identical values. Using only the first label: {unique(prepared_data$label)[1]}")
      prepared_data <- prepared_data |>
        dplyr::filter(label == unique(prepared_data$label)[1])
      multi_label <- FALSE
    } else  {
      multi_label <- TRUE
    }
  }
  
  # order potential labels by applicability
  ordered_labels <- c(
    # "landings_weight",
    # "landings_numbers",
    # "landings_expected",
    # "landings_predicted",
    "landings_observed_weight",
    "landings_predicted_weight",
    "landings_observed_number",
    "landings_predicted_number",
    "landings")
  
  if (is.null(label) & multi_label){
    cli::cli_alert_info("`label` not specified.")
    # Choose label to filter by, based on presence in prepared_data
    for (lab in ordered_labels) {
      if (lab %in% prepared_data$label) {
        target_label <- lab
        break
      }
    }
    cli::cli_alert_info("`label` selected as {target_label}.")
  } else if (length(label) > 1 & multi_label){
    cli::cli_alert_info("More than one `label` exists.")
    target_label <- label[1]  
    cli::cli_alert_info("The first `label` value will be selected {target_label}.")
  } else if (!is.null(label)){
    target_label <- label
  } else {
    target_label <- unique(prepared_data$label)
  }

  prepared_data2 <- prepared_data |>
    dplyr::filter(label == target_label) |>
    dplyr::mutate(estimate = round(as.numeric(estimate), digits = 0)) |>
    dplyr::mutate(uncertainty = round(as.numeric(uncertainty), digits = 2))
  
  # get uncertainty label
  uncert_lab <- prepared_data2$uncertainty_label |> 
    unique()
  
  if (length(uncert_lab) > 1){
    cli::cli_alert_warning("More than one value for uncertainty exists: {uncert_lab}")
    uncert_lab <- uncert_lab[[1]]
    cli::cli_alert_warning("The first value ({uncert_lab}) will be chosen.")
  }
  
  # get fleet names
  fleets <- prepared_data2$fleet |>
    unique() |>
    # sort numerically even if fleets are 100% characters
    stringr::str_sort(numeric = TRUE)

  #TODO: fix this so that fleet names aren't removed if, e.g., group = "fleet"
  table_data <- process_table(
    dat = prepared_data2,
   # group = group,
    method = method
    )
  
  # put table_data into a nice table
  capitalized_names <- c("Year" = "year",
                         "Sex" = "sex",
                         "Fleet" = "fleet",
                         "Model" = "model")
  
  
  
  
  
  
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

  # transform dfs into tables
  final <- lapply(df_list, function(df) {
    df |>
      gt::gt() |>
      add_theme()
  })



  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    create_rda(
      object = final,
      topic_label = "landings",
      fig_or_table = "table",
      dat = dat,
      dir = tables_dir,
      scale_amount = 1,
      unit_label = unit_label,
      table_df = final_df
  )
  }
  # Send table(s) to viewer
  if (!is.data.frame(table_data)) {
    for (t in final) {
      print(t)
    }
    # Return table list invisibly
    return(invisible(final))
  } else {
    # Return finished table (when only one table)
    return(final)
  }
}
