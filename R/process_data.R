#' Post processing of filtered data
#'
#' @param dat Pre-filtered data from \link[stockplotr]{filter_data} following a
#' long format data.
#' @param group A string identifying the indexing variable of the data. If you
#' want to just summarize the data across all factors, set group = "none".
#' @param facet A string or vector of strings identifying the faceting
#' variable(s) of the data.
#' @param method A string describing the method of summarizing data when group
#' is set to "none". Options are "sum" or "mean". Default is "sum".
#'
#' @returns Automatically detects potential grouping and faceting data from a 
#' dataframe output from \link[stockplotr]{filter_data}. It returns the following 
#' list of 3 objects:
#' \item{data}{A data frame of the processed data ready for plotting.}
#' \item{group}{A string identifying the grouping variable of the data.
#' If NULL, no grouping variable is identified. If not NULL, the function
#' will verify that the data is indexed by this variables, otherwise it will 
#' overwrite it to a different, valid indexed variable or NULL.}
#' \item{facet}{A string or vector of strings identifying the faceting
#' variable(s) of the data. If NULL, no faceting variable is identified. Any
#' identified indexed variables found in this function will be added to facet.}
#' @export
#'
#' @examples {
#' filtered <- stockplotr::filter_data(
#'  dat = stockplotr:::example_data,
#'  label_name = "fishing_mortality$",
#'  geom = "line",
#'  module = "TIME_SERIES"
#' )
#' process_data(dat = filtered, method = "sum")
#' }
process_data <- function(
    dat,
    group = NULL,
    facet = NULL,
    method = "sum"
) {
  # check if >1 model
  if (length(unique(dat$model)) > 1) {
    # Check for indexing variables equivalent
    index_list <- c()
    for (mod in unique(dat$model)) {
      mod_data <- dplyr::filter(dat, model == mod)
      mod_index <- check_grouping(mod_data)
      index_list <- c(index_list, list(mod_index))
    }
    if (!all(vapply(
      index_list,
      identical,
      logical(1),
      index_list[[1]]))
    ) {
      cli::cli_alert_warning("Multiple models with differing indexing variables detected.")
      if (!is.null(group) && group != "none") {
        cli::cli_alert_info("Setting group to \"none\".")
      } else if (is.null(group)) {
        cli::cli_alert_info("No grouping variable specified. Setting group to \"none\".")
      }
      group <- "none"
    }
  }
  # check for additional indexed variables
  index_variables <- check_grouping(dat)
  # If user input "none" to group this makes the plot remove any facetting or summarize?
  if (!is.null(group) && group == "none") {
    # group <- NULL
    id_group <- index_variables[-grepl("year|age", index_variables)]
    index_variables <- intersect(c("year", "age"), index_variables)
    if (length(id_group) > 0) {
      dat <- switch(
        method,
        "mean" = dat |>
          dplyr::group_by(dplyr::across(tidyselect::all_of(c("label", "model", "group_var", index_variables)))) |>
          dplyr::summarize(
            estimate = mean(estimate),
            estimate_lower = mean(estimate_lower),
            estimate_upper = mean(estimate_upper)
          ),
        "sum" = dat |>
          dplyr::group_by(dplyr::across(tidyselect::all_of(c("label", "model", "group_var", index_variables)))) |>
          dplyr::summarize(
            estimate = sum(estimate),
            estimate_lower = sum(estimate_lower),
            estimate_upper = sum(estimate_upper)
         )
        )
    }
  }
  # Warn  user when group not indexed in data
  if (!is.null(group) && group %notin% index_variables) {
    if (group != "none") {
      cli::cli_alert_warning("{group} not an index of data.")
      # reset group to NULL so it's not added to grouping incorrectly
      group <- NULL
    }
  }
  # Set group_var to identified grouping
  # if (!is.null(group) && group == "none") {
  #   # if group is none and there exists an index group, filter to nas of index group
  #   # commented out bc this issue is solved in the above 1st step
  #   if (length(id_group) > 0) {
  #     data <- dplyr::filter(
  #       dat,
  #       is.na(.data[[id_group[1]]])
  #     )
  #   } else {
  #     data <- dat
  #   }
  # } else 
  if (!is.null(group) && group != "none") {
    data <- dplyr::mutate(
      dat,
      group_var = .data[[group]]
    ) 
  } else {
    data <- dat
  }
  if (length(index_variables) > 0) {
    data <- data |>
      dplyr::select(tidyselect::all_of(c(
        "label", "estimate", # "uncertainty", "uncertainty_label",
        "estimate_upper", "estimate_lower",
        # need to maintain columns made from prep data
        "model", "group_var",
        index_variables
      )))
    # TODO: figure out more efficient way to do these checks
    # cannot have test for null and group == something in same line when group is NULL
    if (!is.null(group) && group %in% index_variables) index_variables <- index_variables[-grep(group, index_variables)]
  } else {
    cli::cli_abort("Please check df. This scenario hasn't been considered.")
  }
  
  # Check if there is age or year or both
  # Then either plot over ages with lines for year or single year
  if ("age" %in% colnames(data) && any(!is.na(data$age))) {
    # subset out nas if ages exist for this 
    # not sure if  this works for all cases -- are there situations where we want the NA and not age?
    data <- dplyr::filter(data, !is.na(age))
    if (!is.null(group) && group == "age") {
      if ("age" %in% index_variables) index_variables <- index_variables[-grep("age", index_variables)]
    }
    if (!is.null(facet) && facet == "age") {
      if ("age" %in% index_variables) index_variables <- index_variables[-grep("age", index_variables)]
    }
  }
  # move year to another check -- age always used?
  if ("year" %in% colnames(data) && any(!is.na(data$year))) {
    data <- dplyr::filter(data, !is.na(year))
    # if (!is.null(group) && group == "year") {
      if ("year" %in% index_variables) index_variables <- index_variables[-grep("year", index_variables)]
    # }
    # if (!is.null(facet) && facet == "year") {
    #   if ("year" %in% index_variables) index_variables <- index_variables[-grep("year", index_variables)]
    # }
  }
  
  # Set any remaining index variables to group (first) and facet
  # Check if this is still the case if a group not NULL
  if (!is.null(group) && group != "year") {
    # if () {
    # Remove NAs from grouping or keep NA if none
    if (group != "none") {
      data <- dplyr::filter(data, !is.na(.data[[group]]))
      
      check_group_data <- data |>
        tidyr::pivot_wider(
          id_cols = tidyselect::any_of(c("label","year", "age", "model")),
          names_from = tidyselect::any_of(group),
          values_from = estimate,
          values_fn = list
        )
      # overwrite variable if grouping is what makes it variable in above conditions
      # variable <- ifelse(
      #   any(length(unique(
      #     dplyr::select(
      #       check_group_data, 
      #       dplyr::any_of(unique(data[[group]]))
      #     )
      #   )) > 1),
      #   TRUE,
      #   FALSE
      # )
    }
    
    # add any remaining index_variables into facet
    if (length(index_variables) > 0) {
      if (!is.null(facet) && facet %in% index_variables) {
        index_variables <- index_variables[-grep(facet, index_variables)]
      }
      facet <- c(facet, index_variables)
    }
  } else if (length(index_variables) > 0) {
    # overwrite variable if a grouping/indexing column was identified
    # based on only the first group if >1 identified
    # variable <- ifelse(
    #   length(unique(data[[index_variables[1]]])) > 1,
    #   TRUE,
    #   FALSE
    # )
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
      data <- dplyr::filter(data, !is.na(.data[[group]]))
      # Remaining id'd index variables moved to facet
      if (length(index_variables) > 1) {
        if (!is.null(facet)) {
          if (facet %in% index_variables) index_variables <- index_variables[-grep(facet, index_variables)]
          facet <- c(facet, index_variables[-1])
        } else {
          facet <- index_variables[-1]
        }
        # add message for what vaues are in facet
        cli::cli_alert_info("Faceting by {paste(facet, collapse = ', ')}.")
        # filter out NA for each value in facet
        for (f in facet) {
          if (any(is.na(unique(data[[f]]))) & length(unique(data[[f]])) == 2) {
            data <- dplyr::filter(data, is.na(.data[[f]]))
            facet <- facet[-grepl(f, facet)]
          } else {
            data <- dplyr::filter(data, !is.na(.data[[f]]))
          }
        }
      }
    }
  }
  
  if(!is.null(group) && group != "none") {
    # check if value varies in ANY year
    # pivot data for 1st indexed data and check if all the same
    if (length(index_variables) > 0) {
      pivot_data <- data |>
        dplyr::select(tidyselect::any_of(c("year", "age", "estimate", index_variables))) |>
        tidyr::pivot_wider(id_cols = c(year),
                           names_from = tidyselect::any_of(index_variables),
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
        # this step might be redundant
        # data <- data |>
        #   dplyr::mutate(group_var = dplyr::case_when(
        #     # !is.null(group) & group == index_variables[1] ~ .data[[index_variables[1]]],
        #     is.null(group) & length(index_variables) > 0 ~ .data[[index_variables[1]]],
        #     TRUE ~ group_var
        #   ))
        if (is.na(unique(data[["group_var"]])[1])) {
          data <- data |> dplyr::filter(is.na(group_var))
        } else {
          data <- data |> dplyr::filter(group_var == unique(data[["group_var"]])[1])
        }
      } else {
        data <- data |> dplyr::filter(year == max(year))
      }
    }
    # check if all values are the same
    # variable <- ifelse(
    #   length(unique(data$estimate)) != length(unique(data$age)),
    #   TRUE, # more M values than ages
    #   FALSE # same # or less M values than ages
    # )
  }
  
  # Final check if group = NULL, then set group var to 1
  if (is.null(group)) {
    data <- dplyr::mutate(data, group_var = "1")
  } else if (length(unique(data[[group]])) == 1) {
    data <- dplyr::mutate(data, group_var = "1")
    group <- NULL
  } else {
    if (group != "none") {
      data <- dplyr::mutate(data, group_var = .data[[group]]) |>
        dplyr::filter(!is.na(group_var))
    } else {
      # change group back to NULL bc no longer needs data processing
      group <- NULL
    }
  }
  # Export list of objects
  list(
    # variable,
    data,
    group,
    facet
  )
}