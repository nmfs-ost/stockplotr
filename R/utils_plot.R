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
