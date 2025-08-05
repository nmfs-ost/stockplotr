library(ggplot2)

# Stat ----
# Make stat object - used for filtering the data before creating the geom which uses this then creates the layers
StatTimeseries <- ggproto("StatTimeseries", Stat,
  required_aes = c("x", "y"),
  non_required_aes = c("label", "era", "module_name", "uncertainty_label", "uncertainty", "ref_pt"),

  setup_params = function(data, params){
    message("--- Entering setup_params ---")
    message(glue::glue("params before modification: {paste(names(params), collapse = ', ')}"))

    params$label_name <- ifelse(is.null(params$label_name), "spawning_biomass", params$label_name)

    message(glue::glue("params after modification: {paste(names(params), collapse = ', ')}"))
    message("--- Exiting setup_params ---")
    params
  },
  
  setup_data = function(data, params, label_name) {
    
    message("--- Entering setup_data ---")
    message(glue::glue("Initial data dimensions: {paste(dim(data), collapse = 'x')}"))
    message(glue::glue("Initial data columns: {paste(names(data), collapse = ', ')}"))
    
    # Ensure x and y are numeric and create 'year' and 'estimate' columns
    if (!("x" %in% names(data)) || !("y" %in% names(data))) {
      stop("Data must contain 'x' and 'y' aesthetics mapped to columns.")
    }
    
    data <- data |>
      dplyr::mutate(
        x = as.numeric(.data$x),
        y = as.numeric(.data$y)
    )
    message(glue::glue("After x/y mutate, data dimensions: {paste(dim(data), collapse = 'x')}"))
    
    
    # Initialize estimate_lower and estimate_upper
    data$ymin <- NA_real_
    data$ymax <- NA_real_
    
    # Initial filtering of the data
    message(glue::glue("Checking for 'label' and 'era' columns... label present: {'label' %in% names(data)}, era present: {'era' %in% names(data)}"))
    if ("label" %in% names(data) && "era" %in% names(data)) {
      message(glue::glue("Filtering by label ('{params$label_name}') and era ('time')..."))
      message(glue::glue("Sample of data$label before filter: {paste(head(data$label), collapse = ', ')}"))
      message(glue::glue("Sample of data$era before filter: {paste(head(data$era), collapse = ', ')}"))
      data <- data |>
        dplyr::filter(
          grepl(glue::glue("{params$label_name}$"), .data$label),
          .data$era == "time"
        )
      message(glue::glue("After label/era filter, data dimensions: {paste(dim(data), collapse = 'x')}"))
      message(glue::glue("Current column names: {paste(names(data), collapse = ', ')}"))
      message(glue::glue("Sample of data$label after filter: {paste(head(data$label), collapse = ', ')}"))
      message(glue::glue("Sample of data$era after filter: {paste(head(data$era), collapse = ', ')}"))
    } else {
      # If 'label' or 'era' are not provided, you might want to issue a warning
      # or handle this case based on your desired behavior.
      message("Warning: 'label' or 'era' aesthetic not provided. Filtering skipped.")
    }
    
    # Conditional calculation of uncertainty bounds
    # Only calculate if 'uncertainty' and 'uncertainty_label' columns are present
    message(glue::glue("Checking for 'uncertainty' and 'uncertainty_label' columns... uncertainty present: {'uncertainty' %in% names(data)}, uncertainty_label present: {'uncertainty_label' %in% names(data)}"))
    if ("uncertainty" %in% names(data) && "uncertainty_label" %in% names(data)) {
      message("Calculating uncertainty bounds...")
      data <- data |>
        dplyr::mutate(
          ymin = dplyr::case_when(
            grepl("se", .data$uncertainty_label) ~ .data$estimate - 1.96 * .data$uncertainty,
            TRUE ~ NA_real_
          ),
          ymax = dplyr::case_when(
            grepl("se", .data$uncertainty_label) ~ .data$estimate + 1.96 * .data$uncertainty,
            TRUE ~ NA_real_
          )
        )
      message(glue::glue("After uncertainty calculation, data dimensions: {paste(dim(data), collapse = 'x')}"))
    }
    # Filter for module_name if exists
    message(glue::glue("Checking for 'module_name' column... module_name present: {'module_name' %in% names(data)}"))
    if ("module_name" %in% names(data) && length(unique(data$module_name)) > 1) {
      data <- data |>
        dplyr::filter(.data$module_name %in% c("TIME_SERIES", "t.series"))
      message(glue::glue("After module_name filter, data dimensions: {paste(dim(data), collapse = 'x')}"))
    }
    message(glue::glue("Unique values of data$module_name after filter: {unique(data$module_name)}"))

    message(glue::glue("Final data:"))
    message(glue::glue("{paste(names(data), collapse = ', ')}"))
    message(glue::glue("{head(data)}\n"))
    
    
    message("--- Exiting setup_data ---")
    return(data)
  },
  
  compute_group = function(data,
                           scales, 
                           params,
                           label_name){
    message("--- Entering compute_group ---")
    
    message(glue::glue("Column names after computing groups: {paste(names(data), collapse = ', ')}"))
    
    message("--- Exiting compute_group ---")
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

# Geom ----
GeomTimeseries <- ggproto("GeomTimeseries", Geom,
    # Fields ----------------------------------------
    # character vector naming aesthetics that are necessary to render geom
    # required_aes = character(),
    
    # a character vector naming aesthetics that will cause removal if they have missing values
    # non_missing_aes = character(),
    
    # character vector naming aesthetics that will be accepted by layer(), but are not required or described in default_aes field
    optional_aes = c("uncertainty", "label", "era", "module_name", "uncertainty_label"),
    
    # a [mapping][aes()] of default values for aethetics
    # default_aes = aes(),
    
    # T/F whether to rename size aesthetics to linewidth
    # rename_size = FALSE,
    
    # a character vector of param names in addition to those imputed from the draw_panel() or draw_groups() methods.
    # This field can be set to include params for setup_data() or handle_na() methods
    extra_params = c("na.rm", "label_name", "reference_name"), # na.rm default
    
    # A function generating a single legend glyph for the geom
    # draw_key = draw_key_point,
    
    # Methods -----------------------------------------------------------------
    
    setup_params = function(data, params) {
      message("---Entering setup_params---")
      # Add new param needed for filtering
      message(glue::glue("Original params: {paste0(names(params), collapse = ', ')}"))
      params$label_name <- ifelse(is.null(params$label_name), "spawning_biomass", params$label_name)
      params$reference_name <- ifelse(is.null(params$reference_name), "msy", params$reference_name)
      message(glue::glue("Params after modification: {paste0(names(params), collapse = ', ')}"))
      print(params)
      message(glue::glue("label_name value: {params$label_name}"))
      message(glue::glue("parameter_name value: {params$reference_name}"))
      message("---Exiting setup_params---")
      params
    },
    
    # function to modify or check the data prior to adding defaults
    setup_data = function(data, params){
      message("---Entering setup_data---")
      message(glue::glue("Dimensions of initial data: {paste(dim(data), collapse = 'x')}"))
      message(glue::glue("Initial data column names: {paste(names(data), collapse = ', ')}"))
      # Calculate reference point
      ref_pt <- calculate_reference_point(
        dat = data,
        reference_name = params$reference_name,
        label_name = params$label_name
      )
      
      # Ensure x and y are numeric and create 'year' and 'estimate' columns
      if (!("x" %in% names(data)) || !("y" %in% names(data))) {
        stop("Data must contain 'x' and 'y' aesthetics mapped to columns.")
      }
      
      data <- data |>
        dplyr::mutate(
          x = as.numeric(.data$x),
          y = as.numeric(.data$y),
          ref_pt = ref_pt
        )
     
      # Initialize estimate_lower and estimate_upper
      data$ymin <- NA_real_ # previously estimate_lower
      data$ymax <- NA_real_ # previously estimate_upper
      message(glue::glue("Data column names after ref_pt and estimate intervals added: \n{paste(names(data), collapse = ', ')}"))
      
      # Initial filtering of the data
      if ("label" %in% names(data) && "era" %in% names(data)) {
        message(glue::glue("Data contains label and era -- filtering by {params$label_name}"))
        data <- data |>
          dplyr::filter(
            grepl(glue::glue("{params$label_name}$"), .data$label),
            .data$era == "time"
          )
        message(glue::glue("Dimensions of data after filter 1: {paste(dim(data), collapse = 'x')}"))
        message(glue::glue("Data column names after filter 1: {paste(names(data), collapse = ', ')}"))
      } else {
        # If 'label' or 'era' are not provided, you might want to issue a warning
        # or handle this case based on your desired behavior.
        message("Warning: 'label' or 'era' aesthetic not provided. Filtering skipped.")
      }
      
      # Conditional calculation of uncertainty bounds
      # Only calculate if 'uncertainty' and 'uncertainty_label' columns are present
      if ("uncertainty" %in% names(data) && "uncertainty_label" %in% names(data)) {
        message("Uncertainty column(s) found -- calculating ymin and ymax")
        data <- data |>
          dplyr::mutate(
            ymin = dplyr::case_when(
              grepl("se", .data$uncertainty_label) ~ .data$estimate - 1.96 * .data$uncertainty,
              TRUE ~ NA_real_
            ),
            ymax = dplyr::case_when(
              grepl("se", .data$uncertainty_label) ~ .data$estimate + 1.96 * .data$uncertainty,
              TRUE ~ NA_real_
            )
          )
        message(glue::glue("Current data dimensions: {paste(dim(data), collapse = 'x')}"))
        message("Top 5 rows of data")
        print(head(data, 5))
      }
      # Filter for module_name if exists
      if ("module_name" %in% names(data) && length(unique(data$module_name)) > 1) {
        message("Filtering data by module_name...")
        message(glue::glue("Data dimensions before filtering module_name:  {paste(dim(data), collapse = 'x')}"))
        if ("TIME_SERIES" %in% unique(data$module_name) | "t.series" %in% unique(data$module_name)) {
          module_name1 <- intersect(c("t.series","TIME_SERIES"), unique(data$module_name))
        } else {
          # Select first module_name in list
          module_name1 <- unique(data$module_name)[1]
        }
        data <- data |>
          dplyr::filter(.data$module_name == module_name1)
        message(glue::glue("Data dimensions after filtering module_name:  {paste(dim(data), collapse = 'x')}"))
        message(glue::glue("Data column names after filtering module_name:  {paste(names(data), collapse = ', ')}"))
      }
      message(glue::glue("Final data column names: {paste(names(data), collapse = ', ')}"))
      message("Top 5 row of final data:")
      print(head(data, 5))
      message("---Exiting setup_data---")
      data
    },
    
    # Function that takes on the tasks of evaluating the default aes, handle the scaling, and fill in fixed, unmapped aes passed as params
    # DO NOT MODIFY - for now
    # use_defaults = function(){ },
    
    # draw_geom ---------------------------------------------------------------
    
    # function to handle missing values - below is default from ggplot2
    handle_na = function(self, data, params) {
      remove_missing(data, params$na.rm,
                     c(self$required_aes, self$non_missing_aes),
                     snake_class(self)
      )
    },
    
    # Function for drawing the entire later
    # default method splits the data and passes on drawing tasks to draw_panel.
    # Not recommended to use this as an extension point
    draw_layer = function(self, data, params, layout, coord) {
      message("---Entering draw_layer---")
      if (empty(data)) {
        n <- if (is.factor(data$PANEL)) nlevels(data$PANEL) else 1L
        return(rep(list(zeroGrob()), n))
      }
      
      # Trim off extra parameters
      params <- params[intersect(names(params), self$parameters())]
      
      if (nlevels(as.factor(data$PANEL)) > 1L) {
        data_panels <- split(data, data$PANEL)
      } else {
        data_panels <- list(data)
      }
      lapply(data_panels, function(data) {
        if (empty(data)) return(zeroGrob())
        
        panel_params <- layout$panel_params[[data$PANEL[1]]]
        rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
      })
      message("---Exiting draw_layer---")
    },
    
    # Function to draw the later for a single panel or group
    # drawl_panel splits the data into groups and passes it to
    # draw_group which assembles into a single grob
    # ... argument is reserved for ententions. By default passed onto the draw_group method
    draw_panel = function(self, data, panel_params, 
                          coord, ...) { # not sure what `self` is
      message("---Entering draw_panel---")
      # Default code from ggplot2
      # groups <- split(data, factor(data$group))
      # grobs <- lapply(groups, function(group) {
      #   self$draw_group(group, panel_params, coord, ...)
      # })
      # 
      # ggname(snake_class(self), grid::gTree(
      #   children = inject(grid::gList(!!!grobs))
      # ))
      
      # Custom code
      # Transformed data for the ribbon
      # error1 <- data |>
      #   dplyr::mutate(
      #     estimate_lower = dplyr::case_when(
      #       grepl("se", .data$uncertainty_label) ~ .data$estimate - 1.96 * .data$uncertainty,
      #       TRUE ~ NA_real_
      #     ),
      #     estimate_upper = dplyr::case_when(
      #       grepl("se", .data$uncertainty_label) ~ .data$estimate + 1.96 * .data$uncertainty,
      #       TRUE ~ NA_real_
      #     )
      #   )
      
      # data_hline <- modifyList(data, ref_pt)
      
      groups <- split(data, factor(data$group))
      message("Look of the first 10 rows of data when split by groups: ")
      print(head(groups, 10))
      grid::gTree(children = grid::gList(
      # grobs <- lapply(groups, function(group) {
        # GeomLine$draw_panel(data, panel_params, coord, ...)
        # GeomLine$draw_panel(
        #   data = data, 
        #   panel_params = panel_params, 
        #   coord = coord,
        #   na.rm = na.rm,
        #   ...
        # ),
        # GeomRibbon$draw_panel(data, panel_params, coord, ...),
        GeomHline$draw_panel(data, panel_params, coord, ...)
      # })
      ))

      # ggname(snake_class(self), grid::gTree(
      #   children = rlang::inject(grid::gList(!!!grobs))
      # ))
      
      # grid::grid::gList(
      #   GeomLine$draw_panel(data, panel_params, coord, ...),
      #   GeomRibbon$draw_panel(error1, panel_params, coord, ...)
      # )
      message("---Entering draw_panel---")
    },
    
    draw_group = function(self, data, panel_params, coord, label_name) {
      message("---Entering draw_group---")
      groups <- split(data, factor(data$group))
      # grid::gTree(children = grid::gList(
        grobs <- lapply(groups, function(group) {
        # GeomLine$draw_panel(data, panel_params, coord, ...)
        GeomLine$draw_panel(
          data = data, 
          panel_params = panel_params, 
          coord = coord,
          na.rm = na.rm,
          ...,
          label_name = label_name
        )
        GeomRibbon$draw_panel(data, panel_params, coord, ...)
        })
      # ))
      
      ggname(snake_class(self), grid::gTree(
        children = rlang::inject(grid::gList(!!!grobs))
      ))
      message("---Exiting draw_group---")
      # ggplot2 default below
      # cli::cli_abort("{.fn {snake_class(self)}}, has not implemented a {.fn draw_group} method")
    },
    
    # Utilities ---------------------------------------------------------------
    
    # Function for listing out all acceptable parameters for this geom
    # ggplot2 default is FALSE, but setting to TRUE since we know we are adding at least one
    # parameters = function(self, extra = TRUE) {
    #   # Look first in draw_panel. If it contains ... then look in draw groups
    #   panel_args <- names(ggproto_formals(self$draw_panel))
    #   group_args <- names(ggproto_formals(self$draw_group))
    #   args <- if ("..." %in% panel_args) group_args else panel_args
    #   
    #   # Remove arguments of defaults
    #   args <- setdiff(args, names(ggproto_formals(Geom$draw_group)))
    #   
    #   if (extra) {
    #     args <- union(args, self$extra_params)
    #   }
    #   args
    # },
    
    # Function for listing out all acceptable aesthetics for geom
    # aesthetics = function(self) {
    #   # gplot2 default below
    #   if (is.null(self$required_aes)) {
    #     required_aes <- NULL
    #   } else {
    #     required_aes <- unlist(strsplit(self$required_aes, '|', fixed = TRUE))
    #   }
    #   c(union(required_aes, names(self$default_aes)), self$optional_aes, "group")
    # }
)

geom_timeseries <- function(
    mapping = NULL, 
    data = NULL, 
    stat = "timeseries", 
    position = "identity", 
    ...,
    label_name = "spawning_biomass",
    reference_name = "msy",
    na.rm = FALSE,
    show.legend = NA, 
    inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeseries,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      label_name = "spawning_biomass",
      reference_name = "msy",
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
ggplot(data = sample_data, 
       aes(x = year, 
           y = estimate,
           label = label,
           era = era,
           module_name = module_name
       )) +
  stat_timeseries(
    aes(
      # shape = fleet
      color = fleet
      ),
    label_name = 'spawning_biomass', 
    # geom = "point", 
    geom = "line", 
    na.rm = TRUE) + 
  # geom_ribbon(aes(ymin = estimate_lower, ymax = estimate_upper), alpha = 0.2) +
  facet_wrap(~season)

# test geom -------
ggplot(data = sample_data,
       aes(x = year,
           y = estimate,
           label = label,
           era = era,
           module_name = module_name
           )
       ) +
  geom_timeseries(
    label_name = "spawning_biomass", 
    reference_name = "msy",
    na.rm = TRUE
    )

  # theme_minimal()
  
  

