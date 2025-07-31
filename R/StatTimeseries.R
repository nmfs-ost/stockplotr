library(ggplot2)

# Stat ----
# Make stat object - used for filtering the data before creating the geom which uses this then creates the layers
StatTimeseries <- ggproto("StatTimeseries", Stat,
  required_aes = c("x", "y"),
  non_required_aes = c("label", "era", "module_name", "uncertainty_label", "uncertainty"),

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
    data$estimate_lower <- NA_real_
    data$estimate_upper <- NA_real_
    
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
          estimate_lower = dplyr::case_when(
            grepl("se", .data$uncertainty_label) ~ .data$estimate - 1.96 * .data$uncertainty,
            TRUE ~ NA_real_
          ),
          estimate_upper = dplyr::case_when(
            grepl("se", .data$uncertainty_label) ~ .data$estimate + 1.96 * .data$uncertainty,
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
    # 
    # message(glue::glue("Final data:"))
    # message(glue::glue("{paste(names(data), collapse = ', ')}"))
    # message(glue::glue("{head(data)}\n"))
    
    
    # message("--- Exiting setup_data ---")
    return(data)
  },
  
  compute_group = function(data,
                           scales, 
                           params,
                           label_name){
    # message("--- Entering compute_group ---")
    
    # message(glue::glue("Column names after computing groups: {paste(names(data), collapse = ', ')}"))
    
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

# Geom ----
GeomTimeseries <- ggproto("GeomTimeseries", Geom,
    # Fields ----------------------------------------
    # character vector naming aesthetics that are necessary to render geom
    required_aes = character(),
    
    # a character vector naming aesthetics that will cause removal if they have missing values
    non_missing_aes = character(),
    
    # character vector naming aesthetics that will be accepted by layer(), but are not required or described in default_aes field
    optional_aes = character(),
    
    # a [mapping][aes()] of default values for aethetics
    default_aes = aes(),
    
    # T/F whether to rename size aesthetics to linewidth
    rename_size = FALSE,
    
    # a character vector of param names in addition to those imputed from the draw_panel() or draw_groups() methods.
    # This field can be set to include params for setup_data() or handle_na() methods
    extra_params = c("na.rm"), # na.rm default
    
    # A function generating a single legend glyph for the geom
    draw_key = draw_key_point,
    
    # Methods ----------------------------------------
    
    setup_params = function(data, params) {
      params
    },
    
    # function to modify or check the data prior to adding defaults
    setup_data = function(data, params){
      data
    },
    
    # Function that takes on the tasks of evaluating the default aes, handle the scaling, and fill in fixed, unmapped aes passed as params
    # DO NOT MODIFY - for now
    # use_defaults = function(){ },
    
    # draw_geom ----------------------------------------------
    
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
        inject(self$draw_panel(data, panel_params, coord, !!!params))
      })
    },
    
    # Function to draw the later for a single panel or group
    # drawl_panel splits the data into groups and passes it to
    # draw_group which assembles into a single grob
    # ... argument is reserved for ententions. By default passed onto the draw_group method
    draw_panel = function(self, data, panel_params, coord, ...) { # not sure what `self` is
      # Default code from ggplot2
      groups <- split(data, factor(data$group))
      grobs <- lapply(groups, function(group) {
        self$draw_group(group, panel_params, coord, ...)
      })
      
      ggname(snake_class(self), gTree(
        children = inject(gList(!!!grobs))
      ))
      
      # Custom code
      # Transformed data for the ribbon
      error1 <- data |>
        dplyr::mutate(
          estimate_lower = dplyr::case_when(
            grepl("se", .data$uncertainty_label) ~ .data$estimate - 1.96 * .data$uncertainty,
            TRUE ~ NA_real_
          ),
          estimate_upper = dplyr::case_when(
            grepl("se", .data$uncertainty_label) ~ .data$estimate + 1.96 * .data$uncertainty,
            TRUE ~ NA_real_
          )
        )
      
      grid::gList(
        GeomLine$draw_panel(data, panel_params, coord, ...),
        GeomRibbon(error1, panel_params, coord, ...)
      )
    },
    
    draw_group = function(self, data, panel_params, coord) {
      cli::cli_abort("{.fn {snake_class(self)}}, has not implemented a {.fn draw_group} method")
    },
    
    # Utilities ---------------------------------------------
    
    # Function for listing out all acceptable parameters for this geom
    # ggplot2 default is FALSE, but setting to TRUE since we know we are adding at least one
    parameters = function(self, extra = TRUE) {
      # Look first in draw_panel. If it contains ... then look in draw groups
      panel_args <- names(ggproto_formals(self$draw_panel))
      group_args <- names(ggproto_formals(self$draw_group))
      args <- if ("..." %in% panel_args) group_args else panel_args
      
      # Remove arguments of defaults
      args <- setdiff(args, names(ggproto_formals(Geom$draw_group)))
      
      if (extra) {
        args <- union(args, self$extra_params)
      }
      args
    },
    
    # Function for listing out all acceptable aesthetics for geom
    aesthetics = function(self) {
      # gplot2 default below
      if (is.null(self$required_aes)) {
        required_aes <- NULL
      } else {
        required_aes <- unlist(strsplit(self$required_aes, '|', fixed = TRUE))
      }
      c(union(required_aes, names(self$default_aes)), self$optional_aes, "group")
    }
)


testplt <- ggplot(data = sample_data, 
       aes(x = year, 
           y = estimate,
           label = label,
           era = era,
           module_name = module_name
       )) +
  stat_timeseries(
    aes(color = fleet),
    label_name = 'spawning_biomass', 
    geom = "line", 
    na.rm = TRUE) + 
  # geom_ribbon(aes(ymin = estimate_lower, ymax = estimate_upper), alpha = 0.2) +
  facet_wrap(~season)

  # theme_minimal()
  
  
# GeomTimeseries <- ggproto("GeomTimeseries", Geom,
#                           required_aes = c("x", "y", "label"),
#                           
#                           setup_data = function(data, params, label) {
#                             data
#                           },
#                           
#                           draw_panel = function(data, panel_scales, coord) {
#                             ggname("geom_timeseries", ggplot2::GeomPath$draw_panel(data, panel_scales, coord))
#                           },
#                           
#                           default_aes = aes(colour = "black", size = 0.5, linetype = "solid", alpha = NA)
# )
# 
# geom_timeseries <- function(mapping = NULL, data = NULL, stat = "identity",
#                             position = "identity", na.rm = FALSE, show.legend = NA,
#                             inherit.aes = TRUE, ...) {
#   layer(
#     data = data,
#     mapping = mapping,
#     stat = stat,
#     geom = GeomTimeseries,
#     position = position,
#     show.legend = show.legend,
#     inherit.aes = inherit.aes,
#     na.rm = na.rm,
#     ...
#   )
# }

