# Post processing filtered data for groupings
# ready data for ggplot
process_data <- function(
    dat,
    group,
    facet
) {
  # Set group_var to identified grouping
  if (!is.null(group)) {
    data <- dplyr::mutate(
      dat,
      group_var = .data[[group]]
    ) 
  } else {
    data <- dat
  }
  # check for additional indexed variables
  index_variables <- check_grouping(data)
  if (length(index_variables) > 0) {
    data <- data |>
      dplyr::select(dplyr::all_of(c(
        "estimate", # "uncertainty", "uncertainty_label",
        "estimate_upper", "estimate_lower",
        # need to maintain columns made from prep data
        "model", "group_var",
        index_variables
      )))
    # TODO: figure out more efficient way to do these checks
    # cannot have test for null and group == something in same line when group is NULL
    if (!is.null(group) && group %in% index_variables) index_variables <- index_variables[-grep(group, index_variables)]
  }
  
  # Check if there is age or year or both
  # Then either plot over ages with lines for year or single year
  if (any(!is.na(data$age))) {
    data <- dplyr::filter(data, !is.na(age))
    index_variables <- index_variables[-grep("age", index_variables)]
    if ("year" %in% colnames(data) && any(!is.na(data$year))) {
      index_variables <- index_variables[-grep("year", index_variables)]
      data <- dplyr::filter(data, !is.na(year))
      # check if M varies in ANY year
      # pivot data for 1st indexed data and check if all the same
      if (length(index_variables) > 0) {
        pivot_data <- data |>
          dplyr::select(year, age, estimate, .data[[index_variables[1]]]) |>
          tidyr::pivot_wider(id_cols = c(year),
                             names_from = dplyr::all_of(index_variables[1]),
                             values_from = estimate) |> suppressWarnings()
        column_data <- pivot_data[[
          unique(
            data[[index_variables[1]]]
          )[1]]]
      } else {
        # Look at group_var and identify if there is a needed grouping or can just plot one year 
        pivot_data <- data |>
          dplyr::select(year, age, estimate, group_var) |>
          tidyr::pivot_wider(id_cols = c(year),
                             names_from = group_var,
                             values_from = estimate) |> suppressWarnings()
        column_data <- pivot_data[[
          unique(
            data[["group_var"]]
          )[1]]]
      }
      first_year_data <- column_data[[1]]
      if (all(vapply(
        column_data,
        identical,
        logical(1),
        first_year_data))
        ) {
        # if TRUE filter out to only one year
        data <- data |> dplyr::filter(year == max(year))
      } else {
        # if FALSE filter data for only the years which it started then changed
        # first year
        year_initial <- min(data$year, na.rm = TRUE)
        data <- data |> dplyr::mutate(group_var = as.character(year))
        if (!is.null(group)) {
          facet <- c(facet, group)
        }
        group <- "year"
        # TODO: update this part in the cases that M is time-varying
        # Year that differ from the first to the next to the next ect
        cli::cli_alert_warning("Function not currently compatible with time-varying parameter.")
      } 
      # check if all values are the same
      variable <- ifelse(
        length(unique(data$estimate)) != length(unique(data$age)),
        TRUE, # more M values than ages
        FALSE # same # or less M values than ages
      )
    } else {
      variable <- FALSE
    }
  }
  
  # Check if this is still the case if a group not NULL
  if (!is.null(group) && group != "year") {
    # if () {
      check_group_data <- data |>
        tidyr::pivot_wider(
          id_cols = c(year, age, model),
          names_from = dplyr::any_of(group),
          values_from = estimate
        )
      # overwrite variable if grouping is what makes it variable in above conditions
      variable <- ifelse(
        any(unique(
          dplyr::select(
            check_group_data, 
            dplyr::any_of(unique(data[[group]]))
          )
        ) > 1),
        TRUE,
        FALSE
      )
    # }
    # add any remaining index_variables into facet
    if (length(index_variables) > 0) facet <- c(facet, index_variables)
  } else if (length(index_variables) > 0) {
    # overwrite variable if a grouping/indexing column was identified
    # based on only the first group if >1 identified
    variable <- ifelse(
      length(unique(data[[index_variables[1]]])) > 1,
      TRUE,
      FALSE
    )
    # Move remaining indexing variables to facet
    if (!is.null(group) && group == "year") {
      facet <- c(facet, index_variables)
    } else {
      # Set first indexing variable to group
      group <- index_variables[1]
      # Remaining id'd index variables moved to facet
      facet <- ifelse(
        !is.null(facet),
        c(facet, index_variables[-1]),
        index_variables[-1]
      )
    }
    # check that group_var matches the group col
    if (all(data[[group]]!=data[["group_var"]])) {
      # overwrite group_var to match group so plot is correct
      data <- data |>
        dplyr::mutate(group_var = .data[[group]])
    }
  }
  
  # Final check if group = NULL, then set group var to 1
  if (is.null(group)) data <- dplyr::mutate(data, group_var = "1")
  # Export list of objects
  list(
    variable,
    data,
    group,
    facet
  )
}