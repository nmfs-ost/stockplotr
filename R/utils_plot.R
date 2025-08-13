#############################
# Utility functions 
#############################

#' Plot time series trends
#'
#' @param dat filtered data frame from a standard output file or a list of 
#' filtered data from standard output file which provide names for each model 
#' used for labelling (i.e. list("model 1" = output_1, "model 2" = output_2))
#' @param x a string of the column of data used to plot on the x-axis (default 
#' is "year")
#' @param y a string of the column of data used to plot on the y-axis (default 
#' is "estimate")
#' @param geom type of geom to use for plotting found in ggplot2 (e.g. "point", 
#' "line", etc.)
#' @param xlab a string of the x-axis label (default is "Year")
#' @param ylab a string of the y-axis label. If NULL, it will be set to the name
#'  of `y`.
#' @param group a string of a single column that groups the data (e.g. "fleet", 
#' "sex", "area", etc.). Currently can only have one level of grouping.
#' @param facet a string or vector of strings of a column that facets the data 
#' (e.g. "year", "area", etc.)
#' @param ... inherited arguments from internal functions from ggplot2
#'
#' @returns
#' @export
#'
#' @examples
plot_timeseries <- function(
    dat,
    x = "year",
    y = "estimate",
    geom = "line",
    xlab = "Year",
    ylab = NULL,
    group = NULL,
    facet = NULL,
    ...
) {
  # Start plot
  plot <- ggplot2::ggplot(
    data = data, 
    ggplot2::aes(
      .data[[x]],
      .data[[y]])
  )
  # make into new geom?
  # more defaults and fxnality for ggplot
  # Break down data
  
  # Add geom aka type
  plot <- switch(
    geom,
    "point" = {
      plot + 
        ggplot2::geom_point(
          data = plot_data,
          ggplot2::aes(
            .data[[x]],
            .data[[y]],
            # TODO: add more groupings
            # shape = ifelse(any(grepl("shape", names(group))), .data[[group[[grep("shape", names(group))]]]], 1),
            # color = ifelse(any(grepl("color", names(group))), .data[[group[[grep("color", names(group))]]]], "black")
            color = model,
            shape = group_var
          ),
          size = 2.0)
    },
    "line" = {
      plot + 
        ggplot2::geom_line(
          data = plot_data,
          ggplot2::aes(
            .data[[x]],
            .data[[y]],
            linetype = group_var,
            # linetype = ifelse(!is.null(group), .data[[group]], "solid")
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
  # Add labels to axis and legend
  labs <- plot + ggplot2::labs(
    x = xlab,
    y = ylab,
    color = "Model",
    linetype = cap_first_letter(group),
    fill = cap_first_letter(group),
    shape = cap_first_letter(group)
  )
  
  # Remove linetype or point when there is no grouping
  if (is.null(group)) {
    labs <- switch(
      geom,
      "line" = labs + ggplot2::guides(linetype = "none"),
      "point" = labs + ggplot2::guides(point = "none"),
      # return plot if option beyong line and point for now
      labs
    )
  }
  
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
  if (is.null(group) & is.data.frame(dat)) {
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

reference_line <- function(x, ...) {
  ggplot2::geom_hline(
    yintercept = x,
    color = "black"
  )
}

#------------------------------------------------------------------------------

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


calculate_uncertainty <- function() {
  
}

#------------------------------------------------------------------------------

# Prep data for input into aesthetics for ggplot2
prep_dat <- function(dat,
                     label_name,
                     geom,
                     group = TRUE){
  list_of_data <- list()
  for (i in 1:length(dat)) {
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
        label == label_name
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
    list_of_data[[get_id(dat)[i]]] <- data
  }
  # Put in
  plot_data <- dplyr::bind_rows(list_of_data, .id = "model")
  # do.call(rbind, list_of_data)
  
  # If group is false then filter out/summarize data for plotting
  # unsure if want to keep this
  if (!group) {
    plot_data <- plot_data |>
      dplyr::filter(
        !is.na(year),
        is.na(fleet) | length(unique(fleet)) <= 1,
        is.na(sex) | length(unique(sex)) <= 1,
        is.na(area) | length(unique(area)) <= 1,
        is.na(growth_pattern) | length(unique(growth_pattern)) <= 1
      )
      dplyr::group_by(
        year,
        model,
        group_var
      ) |>
      dplyr::summarise(
        estimate = mean(estimate, na.rm = TRUE),
        estimate_lower = mean(estimate_lower, na.rm = TRUE),
        estimate_upper = mean(estimate_upper, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # Check if there are multiple module_names present
  if (length(unique(plot_data$module_name)) > 1) {
    cli::cli_alert_warning("Multiple module names found in data. \n")
    options <- c()
    for (i in seq_along(unique(plot_data$module_name))) {
      # options <- paste0(options, " ", i, ") ", unique(plot_data$module_name)[i], "\n")
      options[i] <- paste0(" ", i, ") ", unique(plot_data$module_name)[i])
    }
    question1 <- utils::menu(
      options,
      title = "Please select one of the following:"
    )
    selected_module <- unique(plot_data$module_name)[as.numeric(question1)]
    if (length(selected_module) > 0) {
      plot_data <- plot_data |>
        dplyr::filter(
          module_name == selected_module
      )
    }
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
