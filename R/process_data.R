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
    # subset out nas if ages exist for this 
    # not sure if  this works for all cases -- are there situations where we want the NA and not age?
    data <- dplyr::filter(data, !is.na(age))
    index_variables <- index_variables[-grep("age", index_variables)]
  }
  # move year to another check -- age always used?
  if ("year" %in% colnames(data) && any(!is.na(data$year))) {
    index_variables <- index_variables[-grep("year", index_variables)]
    data <- dplyr::filter(data, !is.na(year))
    # check if value varies in ANY year
    # pivot data for 1st indexed data and check if all the same
    if (length(index_variables) > 0) {
      pivot_data <- data |>
        dplyr::select(tidyselect::any_of(c("year", "age", "estimate", index_variables))) |>
        tidyr::pivot_wider(id_cols = c(year),
                           names_from = dplyr::any_of(index_variables),
                           values_from = estimate) |> suppressWarnings()
      column_data <- pivot_data[-1]
    } else {
      # Look at group_var and identify if there is a needed grouping or can just plot one year 
      pivot_data <- data |>
        dplyr::select(tidyselect::any_of(c("year", "age", "estimate", "group_var"))) |>
        tidyr::pivot_wider(id_cols = c(year),
                           names_from = group_var,
                           values_from = estimate) |> suppressWarnings()
      column_data <- pivot_data[-1]
      # column_data <- pivot_data[[
      #   unique(
      #     data[["group_var"]]
      #   )[1]]]
    }
    # compare grouping columns to see if all the same
    first_year_data <- column_data[[1]]
    if (all(vapply(
      column_data,
      identical,
      logical(1),
      first_year_data))
    ) {
      # if TRUE filter out to only one year bc everything else redundant
      # check if same through all years
      if (length(unique(first_year_data)) > 1) {
        data <- data |>
          dplyr::mutate(group_var = dplyr::case_when(
            # !is.null(group) & group == index_variables[1] ~ .data[[index_variables[1]]],
            is.null(group) & length(index_variables) > 0 ~ .data[[index_variables[1]]],
            TRUE ~ group_var
          ))
        if (is.na(unique(data[["group_var"]])[1])) {
          data <- data |> dplyr::filter(is.na(group_var))
        } else {
          data <- data |> dplyr::filter(group_var == unique(.data[["group_var"]])[1])
        }
      } else {
        data <- data |> dplyr::filter(year == max(year))
      }
    } # else {
    #   # if FALSE filter data for only the years which it started then changed
    # not applicable to everything and in this case, there would only be visible
    # lines for unique sequences
    #   # first year
    #   year_initial <- min(data$year, na.rm = TRUE)
    #   data <- data |> dplyr::mutate(group_var = as.character(year))
    #   if (!is.null(group)) {
    #     facet <- c(facet, group)
    #   }
    #   group <- "year"
    #   # TODO: update this part in the cases that M is time-varying
    #   # Year that differ from the first to the next to the next ect
    #   cli::cli_alert_warning("Function not currently compatible with time-varying parameter.")
    # } 
    # check if all values are the same
    variable <- ifelse(
      length(unique(data$estimate)) != length(unique(data$age)),
      TRUE, # more M values than ages
      FALSE # same # or less M values than ages
    )
  } else {
    variable <- FALSE
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
    } else if (!is.null(group)) {
      if (group %in% index_variables) {
        # remove group from index variables so no repeats
        index_variables <- index_variables[-grep(group, index_variables)]
        # Add remaining index_variables to facet
        facet <- c(facet, index_variables)
      }
    } else { # group is null
      # Set first indexing variable to group
      group <- index_variables[1]
      # Remaining id'd index variables moved to facet
      facet <- ifelse(
        !is.null(facet),
        c(facet, index_variables[-1]),
        index_variables[-1]
      )
    }
  }
  # check that group_var matches the group col
  # at this point, group should be properly identified
  # if (all(data[[group]]!=data[["group_var"]])) {
  #   # overwrite group_var to match group so plot is correct
  #   data <- data |>
  #     dplyr::mutate(group_var = .data[[group]])
  # }
  
  # Final check if group = NULL, then set group var to 1
  if (is.null(group)) {
    data <- dplyr::mutate(data, group_var = "1")
  } else if (length(unique(data[[group]])) == 1) {
    data <- dplyr::mutate(data, group_var = "1")
    group <- NULL
    # check that group_var matches the group col
    # at this point, group should be properly identified
    # below should always pass unless group is already correct
  } else { # if (all(data[[group]]!=data[["group_var"]])) {
    # overwrite group_var to match group so plot is correct
    data <- data |>
      dplyr::mutate(group_var = .data[[group]])
  }
  # Export list of objects
  list(
    variable,
    data,
    group,
    facet
  )
}