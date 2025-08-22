#############################
# Utility functions for plotting
#############################

#' Plot time series trends
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
#' @returns Create a time series plot for a stock assessment report. The user 
#' has options to create a line, point, or area plot where the x-axis is year 
#' and Y can vary for any time series quantity. Currently, grouping is
#' restricted to one group where faceting can be any number of facets.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_timeseries(dat,
#'                x = "year",
#'                y = "estimate",
#'                geom = "line",
#'                xlab = "Year",
#'                ylab = "Biomass",
#'                group = "fleet",
#'                facet = "area")
#' }
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
  plot <- ggplot2::ggplot()
  # make into new geom?
  # more defaults and fxnality for ggplot
  
  # Add geom
  plot <- switch(
    geom,
    "point" = {
      point_size = ifelse(
        is.null(list(...)$size),
        2.0,
        list(...)$size
      )
      plot + 
        ggplot2::geom_point(
          data = dat,
          ggplot2::aes(
            .data[[x]],
            .data[[y]],
            # TODO: add more groupings
            # shape = ifelse(any(grepl("shape", names(group))), .data[[group[[grep("shape", names(group))]]]], 1),
            # color = ifelse(any(grepl("color", names(group))), .data[[group[[grep("color", names(group))]]]], "black")
            color = model,
            shape = group_var
          ),
          # size = point_size,
          ...
        )
    },
    "line" = {
      plot + 
        ggplot2::geom_line(
          data = dat,
          ggplot2::aes(
            .data[[x]],
            .data[[y]],
            linetype = group_var,
            # linetype = ifelse(!is.null(group), .data[[group]], "solid")
            color = model
          ),
          linewidth = 1.0,
          ...
        ) +
        ggplot2::geom_ribbon(
          dat = dat|> dplyr::filter(!is.na(estimate_lower)),
          ggplot2::aes(
            x = .data[[x]],
            ymin = estimate_lower,
            ymax = estimate_upper
          ),
          colour = "grey",
          alpha = 0.3
        )
    },
    "area" = {
      plot + 
        ggplot2::geom_area(
          data = dat,
          ggplot2::aes(
            .data[[x]],
            .data[[y]],
            fill = model
          ),
          ...
        )
    }
  )
  
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
      "point" = labs + ggplot2::guides(shape = "none"),
      # return plot if option beyond line and point for now
      labs
    )
  }
  if (length(unique(dat$model)) == 1){
    labs <- switch(
      geom,
      "line" = labs + ggplot2::guides(color = "none"),
      "point" = labs + ggplot2::guides(color = "none"),
      "area" = labs + ggplot2::guides(fill = "none"),
      # return plot if option beyond line and point for now
      labs
    )
  }
  
  # Calc axis breaks
  x_n_breaks <- axis_breaks(dat)
  breaks <- ggplot2::scale_x_continuous(
    n.breaks = x_n_breaks,
    guide = ggplot2::guide_axis(
      minor.ticks = TRUE
    )
  )
  
  # Put together final plot
  final <- labs + breaks + ggplot2::expand_limits(y = 0)
  
  # Remove legend if no group is selected
  if (is.null(group) & is.data.frame(dat) & any(is.na(unique(dat$model)))) {
    final <- final + ggplot2::theme(legend.position = "none")
  }
  
  # Check if facet(s) are desired
  if (!is.null(facet)) {
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

#' Create "at-age" plot
#'
#' @param dat filtered data frame from standard output file(s) pre-formatted for
#'  the target label from \link[stockplotr]{prepare_data}
#' @param x a string of the column name of data used to plot on the x-axis
#' (default is "year")
#' @param y a string of the column name of data used to plot on the y-axis
#' (default is "age")
#' @param z a string of the column name of data used to control the size of the
#' bubbles (default is "estimate")
#' @param label a string of the label for the size of the bubbles
#' (default is "Abundance")
#' @param xlab a string of the x-axis label (default is "Year")
#' @param ylab a string of the y-axis label (default is "Age")
#' @param facet a string or vector of strings of a column that facets the data
#' (e.g. "sex", "area", etc.). It is not recommended to include more than one
#' facet due to the complexity of the plot.
#' @param proportional Set size of points relative to z when TRUE, point
#' size are relative to one another while when set to FALSE, point size
#' is relative to z
#' @param ... inherited arguments from internal functions from
#' \link[ggplot2]{geom_point}
#' 
#' @return Create a plot of abundance at age for a stock assessment report.
#' @export
#' @examples \dontrun{
#'  plot_aa(dat)
#' }
plot_aa <- function(
  dat,
  x = "year",
  y = "age",
  z = "estimate",
  label = "Abundance",
  xlab = "Year",
  ylab = "Age",
  facet = NULL,
  proportional = TRUE,
  ...
) {
  # Make sure age is numeric
  dat <- dat |>
    dplyr::mutate(
      age = as.numeric(age),
      # zvar = .data[[z]],
      zvar = dplyr::case_when(
        proportional ~ sqrt(.data[[z]]),
        TRUE ~ .data[[z]]
      )
    )
  # Caclaulate x-axis breaks
  x_n_breaks <- axis_breaks(dat)
  # Calculate y-axis breaks
  y_n_breaks <- y_axis_breaks(dat)

  # Initialize gg plot
  plot <- ggplot2::ggplot() +
  # Add geom
    ggplot2::geom_point(
      data = dat,
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]],
        size = zvar
      ),
      shape = 21,
      alpha = 0.3,
      color = "black",
      fill = "gray40"
      # ...
    ) +
    ggplot2::labs(
      x = xlab,
      y = ylab,
      size = label
    ) +
    ggplot2::scale_size(
      # range = c(0.2, 10),
      # name = label,
      labels = scales::label_comma()
    ) +
    # Add axis breaks
    ggplot2::scale_x_continuous(
      n.breaks = x_n_breaks,
      guide = ggplot2::guide_axis(minor.ticks = TRUE)
    ) +
    ggplot2::scale_y_continuous(
      minor_breaks = y_n_breaks[[1]],
      n.breaks = y_n_breaks[[2]],
      guide = ggplot2::guide_axis(minor.ticks = TRUE)
      # limits = c(min(.data[[y]]), max(.data[[y]])),
      # expand = c(0, NA)
    ) +
    # ggplot2::coord_cartesian(expand = FALSE)
    # add noaa theme
    theme_noaa()
  
  if (proportional) {
    plot <- plot +
      # Remove legend since circles are calculated 
      # proportionally to catch and not exactly catch
      ggplot2::theme(legend.position = "none")
  }

  # Facet plot if groups are present
  if (!is.null(facet)) {
    if (length(unique(dat$model)) > 1) facet <- c(facet, "model")
    # Replace spaces with underscores
    facet <- gsub(" ", "_", facet)
    # If facet is a vector, paste together with +
    facet <- paste("~ ", paste(facet, collapse = " + "))
    facet_formula <- stats::reformulate(facet)
    # facet_formula <- stats::reformulate(facet)
    plot <- plot + ggplot2::facet_wrap(facet_formula)
  }
  plot
}

#------------------------------------------------------------------------------

# Average age line
average_age_line <- function(
  dat,
  facet) {
   # Calculate annual mean age
  grouping <- intersect(colnames(dat), facet)
  total_fish_per_year <- dat |>
    dplyr::mutate(age = as.numeric(as.character(age))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("year", grouping)))) |>
    dplyr::summarise(total_fish = sum(estimate))
  annual_means <- dat |>
    dplyr::mutate(age = as.numeric(as.character(age))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("age", "year", grouping)))) |>
    dplyr::summarise(years_per_year = sum(estimate)) |>
    # dplyr::filter(age != 0) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("year", grouping)))) |>
    dplyr::summarise(interm = sum(as.numeric(age) * as.numeric(years_per_year))) |>
    dplyr::full_join(total_fish_per_year) |>
    dplyr::mutate(avg = interm / total_fish)

# Add average age line to plot
  list(
    ggplot2::geom_line(
      data = annual_means,
      ggplot2::aes(
        x = year,
        y = avg
      ),
      linewidth = 1,
      color = "red"
    )
  )
}

#------------------------------------------------------------------------------

# Cohort line
cohort_line <- function(
  dat,
  x = "year",
  y = "age",
  z = "estimate"
){
  # Make sure all dimensions are numeric
  dat <- dat |>
    dplyr::mutate(
      age = as.numeric(age),
      year = as.numeric(year),
      estimate = as.numeric(estimate)
    )
# Calculate the total estimate for each cohort
cohort_estimates <- dat |>
  dplyr::mutate(cohort = as.numeric(.data[[x]]) - as.numeric(.data[[y]])) |>
  dplyr::group_by(cohort) |>
  dplyr::summarize(total_estimate = sum(.data[[z]], na.rm = TRUE))
# Filter for top 5% of cohorts
# Find the 95th percentile of total_estimate
threshold <- quantile(cohort_estimates$total_estimate, 0.95)

# Filter the original data to keep only the top 5% cohorts
top_cohorts_data <- dat |>
  dplyr::mutate(cohort = as.numeric(.data[[x]]) - as.numeric(.data[[y]])) |>
  dplyr::filter(cohort %in% (cohort_estimates |>
                       dplyr::filter(total_estimate >= threshold) |>
                       dplyr::pull(cohort)))

  list(
    # Create line for only the top 5% of cohorts
    ggplot2::geom_line(
      data = top_cohorts_data,
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]],
        group = cohort
      ),
      linewidth = 1,
      linetype = "solid",
      alpha = 0.8,
      color = "black"
      # color = "#747474"
    )
  )
}

#------------------------------------------------------------------------------

#' Pre-formatted reference line
#'
#' @param plot a ggplot2 object where the reference line will be added
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
  if(!is.null(names(reference))) {
    ref_line_val <- reference[[1]]
    reference <- names(reference)
  } else {
    # calculate reference point value
    ref_line_val <- calculate_reference_point(
      dat = dat,
      reference_name = glue::glue("{label_name}_{reference}")
    )
  }
  
  # Add geom for ref line
  plot +
    ggplot2::geom_hline(
      ggplot2::aes(
        yintercept = ref_line_val / ifelse(relative, ref_line_val, scale_amount)
      ),
      color = "black",
      linetype = "dashed"
    ) +
    ggplot2::annotate(
      geom = "text",
      # TODO: need to change this for general process
      x = as.numeric(max(dat$year[dat$era == "time"], na.rm = TRUE)), # - as.numeric(max(dat$year[dat$era == "time"], na.rm = TRUE))/200,
      y = ref_line_val / ifelse(relative, ref_line_val, scale_amount),
      label = glue::glue("{label_name}_{reference}"), # list(bquote(label_name[.(reference)])),
      parse = TRUE,
      hjust = 1,
      vjust = 0
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
    if (x_n_breaks < 20) {
      x_n_breaks <- round(length(data[["year"]]) / 15)
    } else if (x_n_breaks >=20 & x_n_breaks < 50) {
      x_n_breaks <- round(length(data[["year"]]) / 20)
    } else {
      x_n_breaks <- round(length(data[["year"]]) / 35)
    }
    
  }
  x_n_breaks
}

y_axis_breaks <- function(data){
  # TODO: generalize this so we can input any column for the y-axis values
  y_n_breaks <- round(length(unique(data[["age"]])))
  if (y_n_breaks > 80) {
    y_n_breaks <- round(length(unique(data[["age"]])) / 6)
  } else if (y_n_breaks > 40) {
    y_n_breaks <- round(length(unique(data[["age"]])) / 3)
  }
  y_n_breaks_minor <- as.vector(unique(data[["age"]]))
  if (length(y_n_breaks_minor) > 40) {
    y_n_breaks_minor <- NULL
  } else if (length(y_n_breaks_minor) > 20) {
    y_n_breaks_minor <- y_n_breaks_minor[c(TRUE, FALSE)]
  }
  list(y_n_breaks_minor, y_n_breaks)
}

#------------------------------------------------------------------------------

cap_first_letter <- function(s) {
  if (length(s) == 0 || is.na(s) || nchar(s) == 0) {
    return(s) # Handle empty, NA, or zero-length strings
  }
  first_letter <- toupper(substring(s, 1, 1))
  rest_of_string <- substring(s, 2)
  paste0(first_letter, rest_of_string)
}

#------------------------------------------------------------------------------

# calculate_uncertainty <- function() {
  
# }

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
#' @param scale_amount A number describing how much to scale down the quantities
#' shown on the y axis.
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
    scale_amount = 1,
    interactive = TRUE) {
  # TODO: add option to scale data
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
        estimate = as.numeric(estimate) / scale_amount,
        # calc uncertainty when se
        # TODO: calculate other sources of error to upper and lower (cv,)
        estimate_lower = dplyr::case_when(
          grepl("se", uncertainty_label) ~ (estimate - 1.96 * uncertainty) / scale_amount,
          TRUE ~ NA
        ),
        estimate_upper = dplyr::case_when(
          grepl("se", uncertainty_label) ~ (estimate + 1.96 * uncertainty) / scale_amount,
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

#------------------------------------------------------------------------------

# Set magnitude of label
label_magnitude <- function(
  label,
  unit_label = "mt",
  scale_amount = 1,
  legend = FALSE
) {
  magnitude <- floor(log10(scale_amount))
  if (magnitude == 0) {
    scale_unit <- ""
    unit_mag <- ""
  } else if (magnitude > 0 & magnitude < 10) {
    scale_unit <- c(
      "tens of ",
      "hundreds of ",
      "thousands of ",
      "tens of thousands of ",
      "hundreds of thousands of ",
      "millions of ",
      "tens of millions of ",
      "hundreds of millions of ",
      "billions of "
    )
    unit_mag <- paste(scale_unit[magnitude])
  } else {
    cli::cli_abort("Scale_amount is out of bounds. Please choose a value ranging from 1-1000000000 (one billion) in orders of magnitude (e.g., 1, 10, 100, 1000, etc.)", wrap = TRUE)
  }
  # Create label for abundance units in legend
  glue::glue("{label} {ifelse(legend, \"\n\", \"\")}({unit_mag}{unit_label})")
}
