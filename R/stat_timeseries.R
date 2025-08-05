library(ggplot2)

# Stat ----
# Make stat object - used for filtering the data before creating the geom which uses this then creates the layers
StatTimeseries <- ggproto("StatTimeseries", Stat,
  required_aes = c("x", "y"),
  non_required_aes = c("label", "era", "module_name", "uncertainty_label", "uncertainty", "ref_pt"),

  setup_params = function(data, params){
    # message("--- Entering setup_params ---")
    # message(glue::glue("params before modification: {paste(names(params), collapse = ', ')}"))

    params$label_name <- ifelse(is.null(params$label_name), "spawning_biomass", params$label_name)

    # message(glue::glue("params after modification: {paste(names(params), collapse = ', ')}"))
    # message("--- Exiting setup_params ---")
    params
  },
  
  setup_data = function(data, params, label_name) {
    
    # message("--- Entering setup_data ---")
    # message(glue::glue("Initial data dimensions: {paste(dim(data), collapse = 'x')}"))
    # message(glue::glue("Initial data columns: {paste(names(data), collapse = ', ')}"))
    
    # Ensure x and y are numeric and create 'year' and 'estimate' columns
    if (!("x" %in% names(data)) || !("y" %in% names(data))) {
      stop("Data must contain 'x' and 'y' aesthetics mapped to columns.")
    }
    
    data <- data |>
      dplyr::mutate(
        x = as.numeric(.data$x),
        y = as.numeric(.data$y)
    )
    # message(glue::glue("After x/y mutate, data dimensions: {paste(dim(data), collapse = 'x')}"))
    
    
    # Initialize estimate_lower and estimate_upper
    data$ymin <- NA_real_
    data$ymax <- NA_real_
    
    # Initial filtering of the data
    # message(glue::glue("Checking for 'label' and 'era' columns... label present: {'label' %in% names(data)}, era present: {'era' %in% names(data)}"))
    if ("label" %in% names(data) && "era" %in% names(data)) {
      # message(glue::glue("Filtering by label ('{params$label_name}') and era ('time')..."))
      # message(glue::glue("Sample of data$label before filter: {paste(head(data$label), collapse = ', ')}"))
      # message(glue::glue("Sample of data$era before filter: {paste(head(data$era), collapse = ', ')}"))
      data <- data |>
        dplyr::filter(
          grepl(glue::glue("{params$label_name}$"), .data$label),
          .data$era == "time"
        )
      # message(glue::glue("After label/era filter, data dimensions: {paste(dim(data), collapse = 'x')}"))
      # message(glue::glue("Current column names: {paste(names(data), collapse = ', ')}"))
      # message(glue::glue("Sample of data$label after filter: {paste(head(data$label), collapse = ', ')}"))
      # message(glue::glue("Sample of data$era after filter: {paste(head(data$era), collapse = ', ')}"))
    } else {
      # If 'label' or 'era' are not provided, you might want to issue a warning
      # or handle this case based on your desired behavior.
      message("Warning: 'label' or 'era' aesthetic not provided. Filtering skipped.")
    }
    
    # Conditional calculation of uncertainty bounds
    # Only calculate if 'uncertainty' and 'uncertainty_label' columns are present
    # message(glue::glue("Checking for 'uncertainty' and 'uncertainty_label' columns... uncertainty present: {'uncertainty' %in% names(data)}, uncertainty_label present: {'uncertainty_label' %in% names(data)}"))
    if ("uncertainty" %in% names(data) && "uncertainty_label" %in% names(data)) {
      # message("Calculating uncertainty bounds...")
      data <- data |>
        dplyr::mutate(
          ymin = dplyr::case_when(
            grepl("se", .data$uncertainty_label) ~ .data$x - 1.96 * .data$uncertainty,
            TRUE ~ NA_real_
          ),
          ymax = dplyr::case_when(
            grepl("se", .data$uncertainty_label) ~ .data$x + 1.96 * .data$uncertainty,
            TRUE ~ NA_real_
          )
        )
      # message(glue::glue("After uncertainty calculation, data dimensions: {paste(dim(data), collapse = 'x')}"))
    }
    # Filter for module_name if exists
    # message(glue::glue("Checking for 'module_name' column... module_name present: {'module_name' %in% names(data)}"))
    if ("module_name" %in% names(data) && length(unique(data$module_name)) > 1) {
      data <- data |>
        dplyr::filter(.data$module_name %in% c("TIME_SERIES", "t.series"))
      # message(glue::glue("After module_name filter, data dimensions: {paste(dim(data), collapse = 'x')}"))
    }
    # message(glue::glue("Unique values of data$module_name after filter: {unique(data$module_name)}"))

    # message(glue::glue("Final data:"))
    # message(glue::glue("{paste(names(data), collapse = ', ')}"))
    # message(glue::glue("{head(data)}\n"))
    
    
    # message("--- Exiting setup_data ---")
    return(data)
  },
  
  handle_na = function(self, data, params) {
    remove_missing(data, params$na.rm,
                   c(self$required_aes, self$non_missing_aes),
                   snake_class(self)
    )
  },
  
  compute_group = function(data,
                           scales, 
                           params,
                           label_name){
    # message("--- Entering compute_group ---")
    
    # message(glue::glue("Column names after computing groups: {paste(names(data), collapse = ', ')}"))
    # message(glue::glue("Unique values of data$label of final df: {unique(data$label)}"))
    # message("--- Exiting compute_group ---")
    # ensure the processed data returns
    return(data)
  }
)


stat_timeseries <- function(mapping = NULL, data = NULL, 
                            geom = "line", position = "identity", 
                            na.rm = FALSE, show.legend = NA, 
                            inherit.aes = TRUE, label_name = NULL,
                            ...) {
  layer(
    stat = StatTimeseries, 
    data = data, 
    mapping = mapping, 
    geom = geom, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(
      label_name = label_name,
      na.rm = na.rm,
      ...
    )
  )
}



# Utilities -----------------------------------------------------------
# From ggplot2
empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is_waiver(df)
}

is_waiver <- function(x) {
  inherits(x, "waiver")
}

# My utils
calculate_reference_point <- function(
    dat,
    reference_name,
    label_name
) {
  # Check if the reference point exists in the data
  if (inherits(try(solve(as.numeric(dat[
    grep(
      pattern = glue::glue("^{glue::glue(\"{label_name}_{reference_name}\")}$"),
      x = dat[["label"]]
    ),
    "y"
  ])), silent = TRUE), "try-error")) {
    ref_line_val <- NULL
  } else {
    ref_line_val <- as.numeric(dat[
      grep(
        pattern = glue::glue("^{glue::glue(\"{label_name}_{reference_name}\")}$"),
        x = dat[["label"]]
      ),
      "y"
    ])
  }
  
  # Check if the reference value was found
  if (length(ref_line_val) == 0) {
    cli::cli_alert_warning(
      "The resulting reference value of `{label_name}_{reference_name}` was not found.",
      wrap = TRUE
    )
    ref_line_val <- 0
  } else if (length(ref_line_val) > 1) {
    cli::cli_alert_warning("More than one of the resulting reference value of `{label_name}_{reference_name}` was found. \n")
    options <- c()
    for (i in seq_along(unique(dat$module_name))) {
      # options <- paste0(options, " ", i, ") ", unique(plot_data$module_name)[i], "\n")
      options[i] <- paste0(" ", i, ") ", unique(dat$module_name)[i])
    }
    ref_line_val <- utils::menu(
      options,
      title = "Please select one:"
    )
    ref_line_val <- as.numeric(ref_line_val)
  }
  ref_line_val
}

# test stat ------------------
test <- ggplot(data = sample_data, 
       aes(x = year, 
           y = estimate,
           label = label,
           era = era,
           module_name = module_name
       )) +
  stat_timeseries(
    aes(
      # shape = fleet
      # fill = fleet
      linetype = fleet,
      color = fleet
      ),
    label_name = 'spawning_biomass', 
    # geom = "point", 
    geom = "line",
    # geom = "area",
    na.rm = TRUE) + 
  # geom_ribbon(aes(ymin = estimate_lower, ymax = estimate_upper), alpha = 0.2) +
  facet_wrap(~season)
