#############################
# Utility functions 
#############################

#' Plot time series trends
#'
#' @param dat 
#' @param x 
#' @param y 
#' @param geom type of geom to use for plotting found in ggplot2 (e.g. "point", "line", etc.)
#' @param xlab a string of the x-axis label (default is "Year")
#' @param ylab a string of the y-axis label. If NULL, it will be set to the name of `y`.
#' @param group a string of a single column that groups the data (e.g. "fleet", "sex", "area", etc.)
#' @param facet a string of a single column that facets the data (e.g. "year", "area", etc.)
#' @param ... 
#'
#' @returns
#' @export
#'
#' @examples
plot_timeseries <- function(
    dat,
    x,
    y,
    geom,
    xlab = "Year",
    ylab = NULL,
    group = NULL,
    facet = NULL,
    ...
) {
  # Start plot
  plot <- ggplot2::ggplot()
  # Break down data
  if (is.list(dat)) {
    list_of_data <- list()
    for (i in 1:length(dat)) {
      # start for loop to bring together each data as their own geom
      # Add columns to data if grouping is selected
      data <- data.frame(dat[[i]]) |>
        dplyr::mutate(
          year = as.numeric(year),
          model = names(dat)[i]
        )
      if (!is.null(group)) {
        data <- data
          dplyr::mutate(
            group_var = .data[[group]]
          )
      } else {
        data <- data |>
          dplyr::mutate(
            group_var = switch(geom,
                               "line" = "solid",
                               "point" = "black",
                               1
            )
          )
      }
      list_of_data[[names(dat)[i]]] <- data
    }
    # Put in
    plot_data <- do.call(rbind, list_of_data)
    # Add geom aka type
    plot <- switch(
      geom,
      "point" = {
        plot + 
          ggplot2::geom_point(
            data = data,
            ggplot2::aes(
              .data[[x]],
              .data[[y]],
              color = model,
              shape = group_var
            ),
            size = 2.0)
      },
      "line" = {
        plot + 
          ggplot2::geom_line(
            data = data,
            ggplot2::aes(
              .data[[x]],
              .data[[y]],
              linetype = group_var,
              color = model
            ),
            linewidth = 1.0
          )
      },
      "area" = {
        plot + 
          ggplot2::geom_area(
            data = data,
            ggplot2::aes(
              .data[[x]],
              .data[[y]],
              fill = group_var
            )
          )
      }
    )
  } else {
    # Add columns to data if grouping is selected
    if (!is.null(group)) {
      data <- data |>
        dplyr::mutate(
          group_var = .data[[group]],
          year = as.numeric(year)
        )
    } else {
      data <- data |>
        dplyr::mutate(
          group_var = switch(geom,
                             "line" = "solid",
                             "point" = "black",
                             1
          ),
          year = as.numeric(year)
        )
    }
    
    # Start plot
    plot <- ggplot2::ggplot(
      data = data, 
      ggplot2::aes(
        .data[[x]],
        .data[[y]])
    )
    
    # choose right number of colors and assign fleet names to those colors
    # select_color <- function(x, data){
    #   # select group that is color
    #   col_group <- group[[grep("color|colour", names(group))]]
    #   nmfs_colors <- nmfspalette::nmfs_palette(palette = "regional")
    #   colors <- nmfs_colors(length(unique(data[[col_group]])))
    #   names(colors) <- unique(data[[col_group]])
    #   return(colors)
    # }
    # if (any(grepl("color", names(group)))) {
    #   selected_colors <- select_color(group, data)
    # } else {
    #   selected_colors <- "black"
    # }
    
    # Add geom aka type
    plot <- switch(
      geom,
      "point" = {
        plot + 
          ggplot2::geom_point(
            ggplot2::aes(
              # TODO: add more groupings
              # shape = ifelse(any(grepl("shape", names(group))), .data[[group[[grep("shape", names(group))]]]], 1),
              # color = ifelse(any(grepl("color", names(group))), .data[[group[[grep("color", names(group))]]]], "black")
              shape = group_var
            ),
            size = 2.0)
      },
      "line" = {
        plot + 
          ggplot2::geom_line(
            ggplot2::aes(
              linetype = group_var
              # linetype = ifelse(!is.null(group), .data[[group]], "solid")
            ),
            linewidth = 1.0
          )
      },
      "area" = {
        plot + 
          ggplot2::geom_area(
            ggplot2::aes(
              fill = group_var
            )
          )
      }
      # Below are all one variable mapping plots 
      # Do not belong in this fxn
      # "bar" = ggplot2::geom_bar(),
      # "histogram" = ggplot2::geom_histogram(),
      # "density" = ggplot2::geom_density()
    )
  }
  
  # Add labels to axis and legend
  labs <- plot + ggplot2::labs(
    x = xlab,
    y = ylab,
    color = "Model",
    linetype = cap_first_letter(group),
    fill = cap_first_letter(group),
    shape = cap_first_letter(group)
  )
  
  # Calc axis breaks
  x_n_breaks <- axis_breaks(data)
  breaks <- ggplot2::scale_x_continuous(
    n.breaks = x_n_breaks,
    guide = ggplot2::guide_axis(
      minor.ticks = TRUE
    )
  )
  
  # Put together final plot
  final <- labs + breaks
  
  # Remove legend if no group is selected
  if (is.null(group)) {
    final <- final + ggplot2::theme(legend.position = "none")
  }
  
  # Check if facet(s) are desired
  if (!is.null(facet)){
    facet <- paste("~", paste(facet, collapse = " + "))
    facet_formula <- stats::reformulate(facet)

    final <- final + ggplot2::facet_wrap(facet_formula)
  }
  final
}

#------------------------------------------------------------------------------

#' Create plot with error
#' 
#' @param dat filtered data frame from standard output file(s) pre-formatted for
#'  the target label from \link[stockplotr]{prepare_data}
#' @param x a string of the column name of data used to plot on the x-axis (default 
#' is "year")
#' @param y a string of the column name of data used to plot on the y-axis (default 
#' is "estimate")
#' @param geom type of geom to use for plotting found in ggplot2 (e.g. "point", 
#' "line", etc.). Default is "line". Other options are "point" and "area".
#' @param xlab a string of the x-axis label (default is "Year")
#' @param ylab a string of the y-axis label. If NULL, it will be set to the name
#'  of `y`.
#' @param group a string of a single column that groups the data (e.g. "fleet", 
#' "sex", "area", etc.). Currently can only have one level of grouping.
#' @param facet a string or vector of strings of a column that facets the data 
#' (e.g. "year", "area", etc.)
#' @param ... inherited arguments from internal functions from ggplot2::geom_xx
#' 
#' 
plot_error <- function(
    dat,
    x = "year",
    y = "estimate",
    geom = "point",
    group = NULL,
    facet = NULL,
    xlab = "Year",
    ylab = NULL,
    ...
) {
  plot_timeseries(
    dat = dat,
    x = x,
    y = y,
    geom = geom,
    xlab = xlab,
    ylab = ylab,
    group = group,
    facet = facet,
    colour = "black"
    # ...
  ) +
    ggplot2::geom_segment(
      data = dat,
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]],
        yend = estimate_upper
      ),
      color = "#5798fa",
      alpha = 0.5
    ) +
    ggplot2::geom_segment(
      data = dat,
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]],
        yend = estimate_lower
      ),
      color = "#5798fa",
      alpha = 0.5
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 1,
      linetype = "solid", # "dashed",
      colour = "#6e6e6e"
    )
}

#------------------------------------------------------------------------------

#' Pre-formatted reference line
#'
#' @param dat standard data frame where reference point should be extracted
#' @param label_name string of the name of the quantity that users want to 
#' extract the reference point from
#' @param reference string. name of the reference point such as "msy", 
#' "unfished", or "target"
#'
#' @returns a ggplot2 geom_hline object for a reference point that can be added 
#' to a plot
#' @export
#'
#' @examples
#' \dontrun{
#' reference_line(dat, "biomass", "msy")
#' }
reference_line <- function(
    plot,
    dat, 
    label_name,
    reference, 
    relative = FALSE, 
    scale_amount = 1
    ) {
  # calculate reference point value
  ref_line_val <- calculate_reference_point(
    dat = dat,
    reference_name = glue::glue("{label_name}_{reference}")
  )
}

axis_breaks <- function(data){
  # change plot breaks
  x_n_breaks <- round(length(data[["year"]]) / 10)
  if (x_n_breaks <= 5) {
    x_n_breaks <- round(length(data[["year"]]) / 5)
    if (x_n_breaks <= 2) {
      x_n_breaks <- round(length(data[["year"]]))
    }
  } else if (x_n_breaks > 10) {
    x_n_breaks <- round(length(data[["year"]]) / 15)
  }
  x_n_breaks
}

cap_first_letter <- function(s) {
  if (length(s) == 0 || is.na(s) || nchar(s) == 0) {
    return(s) # Handle empty, NA, or zero-length strings
  }
  first_letter <- toupper(substring(s, 1, 1))
  rest_of_string <- substring(s, 2)
  paste0(first_letter, rest_of_string)
}

#------------------------------------------------------------------------------

calculate_uncertainty <- function() {
  
}

#------------------------------------------------------------------------------

#' Prep data for input into aesthetics for ggplot2
#'
#' @param dat a data frame or list of data frames that contains the data to be
#' plotted.
#' @param label_name a string of the name of the label that is used to filter
#' the data.
#' @param geom Type of plot user wants to create. Options are "line", "point",
#' and "area".
#' @param group Selected grouping for the data. If you want to just summarize
#' the data across all factors, set group = "none".
#' @param interactive logical. If TRUE, the user will be prompted to select
#' a module_name when there was more than one found in the filtered dataset.
#'
#' @returns a data frame that is pre-formatted for plotting with ggplot2.
#' @export
#'
#' @examples
#' \dontrun{
#' prepare_data(dat, "biomass", "line", group = "fleet")
#' }
prepare_data <- function(
    dat,
    label_name,
    module = NULL,
    geom,
    group = NULL,
    interactive = TRUE){
  # Replace all spaces with underscore if not in proper format
  label_name <- sub(" ", "_", label_name)
  list_of_data <- list()
  length_dat <- ifelse(
    is.data.frame(dat),
    1,
    length(dat)
  )
  for (i in 1:length_dat) {
    # start for loop to bring together each data as their own geom
    # Add columns to data if grouping is selected
    # format geoms the way we want
    # ggplot easier and more consistent to use
    # defaults are focused for stock assessment
    # vignette to show how you can filter the data instead of the devs
    # vignette is the effort to show what to do and has example
    # would have to use the plus operator
    
    if (is.data.frame(dat)) {
      data <- dat
      model_label = FALSE
    } else {
      data <- dat[[i]]
      model_label = TRUE
    }
    data <- data |>
      dplyr::filter(
        grepl(glue::glue("{label_name}$"), label),
        era == "time"
      ) |>
      dplyr::mutate(
        year = as.numeric(year),
        model = ifelse(model_label, get_id(dat)[i], NA),
        estimate = as.numeric(estimate),
        # calc uncertainty when se
        # TODO: calculate other sources of error to upper and lower (cv,)
        estimate_lower = dplyr::case_when(
          grepl("se", uncertainty_label) ~ estimate - 1.96 * uncertainty,
          TRUE ~ NA
        ),
        estimate_upper = dplyr::case_when(
          grepl("se", uncertainty_label) ~ estimate + 1.96 * uncertainty,
          TRUE ~ NA
        )
      )
    if (nrow(data) < 1) cli::cli_abort("{label_name} not found.")
    if (is.null(group)) {
      data <- data |>
        dplyr::mutate(
          group_var = switch(geom,
                             "line" = "solid",
                             "point" = "black",
                             1
          )
        )
    } else if (group == "none"){
      data <- data |>
        dplyr::mutate(
          group_var = switch(geom,
                             "line" = "solid",
                             "point" = "black",
                             1
          )
        )
    } else {
      data <- data |>
        dplyr::mutate(
          group_var = .data[[group]]
        )
    }
    list_of_data[[get_id(dat)[i]]] <- data
  }
  # Put in
  plot_data <- dplyr::bind_rows(list_of_data, .id = "model")
  # do.call(rbind, list_of_data)
  
  # Check if there are multiple module_names present
  if (length(unique(plot_data$module_name)) > 1) {
    if (!is.null(module)) {
      plot_data <- plot_data |>
        dplyr::filter(
          module_name == module
        )
    } else {
      cli::cli_alert_warning("Multiple module names found in data. \n")
      options <- c()
      for (i in seq_along(unique(plot_data$module_name))) {
        # options <- paste0(options, " ", i, ") ", unique(plot_data$module_name)[i], "\n")
        options[i] <- paste0(" ", i, ") ", unique(plot_data$module_name)[i])
      }
      if (interactive()) {
        if(interactive) {
          question1 <- utils::menu(
                  options,
                  title = "Please select one of the following:"
                )
          selected_module <- unique(plot_data$module_name)[as.numeric(question1)]
        } else {
          selected_module <- unique(plot_data$module_name)[1]
          cli::cli_alert_info("Selection bypassed. Filtering by {selected_module}.")
        }
      } else {
        selected_module <- unique(plot_data$module_name)[1]
        cli::cli_alert_info(glue::glue("Environment not interactive. Selecting {selected_module}."))
      }
      if (length(selected_module) > 0) {
        plot_data <- plot_data |>
          dplyr::filter(
            module_name == selected_module
          )
      }
    }
  }
  
  # If group is false then filter out/summarize data for plotting
  # unsure if want to keep this
  # this is filtering for time series
  # TODO: change or remove in the future when moving to other plot types
  if (is.null(group)){
    plot_data <- plot_data |>
      dplyr::filter(
        !is.na(year),
        is.na(fleet) | length(unique(fleet)) <= 1,
        is.na(sex) | length(unique(sex)) <= 1,
        is.na(area) | length(unique(area)) <= 1,
        is.na(growth_pattern) | length(unique(growth_pattern)) <= 1
      ) |>
      dplyr::group_by(
        year,
        model,
        group_var,
        module_name,
        label
      ) |>
      dplyr::summarise(
        estimate = mean(estimate, na.rm = TRUE),
        estimate_lower = mean(estimate_lower, na.rm = TRUE),
        estimate_upper = mean(estimate_upper, na.rm = TRUE)
      ) |>
      dplyr::ungroup()
  } else if (group == "none") {
    plot_data <- plot_data |>
      dplyr::filter(
        !is.na(year),
        is.na(fleet) | length(unique(fleet)) <= 1,
        is.na(sex) | length(unique(sex)) <= 1,
        is.na(area) | length(unique(area)) <= 1,
        is.na(growth_pattern) | length(unique(growth_pattern)) <= 1
      ) |>
      dplyr::group_by(
        year,
        model,
        group_var,
        module_name,
        label
      ) |>
      dplyr::summarise(
        estimate = mean(estimate, na.rm = TRUE),
        estimate_lower = mean(estimate_lower, na.rm = TRUE),
        estimate_upper = mean(estimate_upper, na.rm = TRUE)
      ) |>
      dplyr::ungroup()
  }
  
  if (geom == "area") {
    plot_data2 <- dplyr::mutate(
      plot_data,
      model = reorder(.data[["model"]], .data[["estimate"]], function(x) -max(x) )
    )
  }
  plot_data
}

#------------------------------------------------------------------------------

# helper function to get the names of a list or name the elements of the list in number
get_id <- function(dat) {
  if (is.null(names(dat))) {
    # If the list is unnamed, return the sequence of its elements
    return(seq_along(dat))
  } else {
    # If the list is named, return its names
    return(names(dat))
  }
}

#------------------------------------------------------------------------------

# Calculate reference value point

calculate_reference_point <- function(
    dat,
    reference_name
) {
  # Check if the reference point exists in the data
  if (inherits(try(solve(as.numeric(dat[
    grep(
      pattern = glue::glue("^{reference_name}$"),
      x = dat[["label"]]
    ),
    "estimate"
  ])), silent = TRUE), "try-error")) {
    ref_line_val <- NULL
  } else {
    ref_line_val <- as.numeric(dat[
      grep(
        pattern = glue::glue("^{reference_name}$"),
        x = dat[["label"]]
      ),
      "estimate"
    ])
  }
  
  # Check if the reference value was found
  if (length(ref_line_val) == 0) {
    cli::cli_alert_warning(
      "The resulting reference value of `{reference_name}` was not found.",
      wrap = TRUE
    )
    ref_line_val <- NULL
  } else if (length(ref_line_val) > 1) {
    cli::cli_alert_warning("More than one of the resulting reference value of `{reference_name}` was found. \n")
    options <- c()
    for (i in seq_along(unique(plot_data$module_name))) {
      # options <- paste0(options, " ", i, ") ", unique(plot_data$module_name)[i], "\n")
      options[i] <- paste0(" ", i, ") ", unique(plot_data$module_name)[i])
    }
    ref_line_val <- utils::menu(
      options,
      title = "Please select one:"
    )
    ref_line_val <- as.numeric(ref_line_val)
  }
  ref_line_val
}
