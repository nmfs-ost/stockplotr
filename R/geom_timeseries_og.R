# Stat ----
StatTimeseries <- ggproto("StatTimeseries", Stat,
                          required_aes = c("x", "y"),
                          # non_required_aes = c("label", "era", "module_name", "uncertainty_label", "uncertainty", "ref_pt"),
                          optional_aes = c("uncertainty", "label", "era", "module_name", "uncertainty_label"),
                          extra_params = c("na.rm", "label_name", "reference_name"), 
                          
                          compute_group = function(data, params){
                            message("--- Entering setup_params ---")
                            message(glue::glue("params before modification: {paste(names(params), collapse = ', ')}"))
                            
                            params$label_name <- params$label_name %||% "spawning_biomass"
                            params$reference_name <- params$reference_name %||% "msy"
                            
                            message(glue::glue("params after modification: {paste(names(params), collapse = ', ')}"))
                            message("--- Exiting setup_params ---")
                            params
                          },
                          
                          setup_data = function(data, params) {
                            
                            message("--- Entering setup_data ---")
                            message(glue::glue("Initial data dimensions: {paste(dim(data), collapse = 'x')}"))
                            message(glue::glue("Initial data columns: {paste(names(data), collapse = ', ')}"))
                            
                            # Calculate reference point
                            yintercept <- calculate_reference_point(
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
                                yintercept = yintercept
                              )
                            message(glue::glue("After x/y mutate, data dimensions: {paste(dim(data), collapse = 'x')}"))
                            
                            
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
                                    grepl("se", .data$uncertainty_label) ~ .data$x - 1.96 * .data$uncertainty,
                                    TRUE ~ NA_real_
                                  ),
                                  ymax = dplyr::case_when(
                                    grepl("se", .data$uncertainty_label) ~ .data$x + 1.96 * .data$uncertainty,
                                    TRUE ~ NA_real_
                                  )
                                )
                              message(glue::glue("After uncertainty calculation, data dimensions: {paste(dim(data), collapse = 'x')}"))
                            } else {
                              # Initialize estimate_lower and estimate_upper
                              data$ymin <- NA_real_
                              data$ymax <- NA_real_
                            }
                            # Filter for module_name if exists
                            message(glue::glue("Checking for 'module_name' column... module_name present: {'module_name' %in% names(data)}"))
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
                            message(glue::glue("Unique values of data$module_name after filter: {unique(data$module_name)}"))
                            
                            message(glue::glue("Final data:"))
                            print(head(data, 10))
                            
                            message(glue::glue("Checking for NA in data: {any(is.na(data$x))}"))
                            message("--- Exiting setup_data ---")
                            return(data)
                          }
)

# Geom ----
GeomTimeseries <- ggproto("GeomTimeseries", Geom,
                          # Fields ----------------------------------------
                          # character vector naming aesthetics that are necessary to render geom
                          required_aes = c("x", "y", "yintercept"),
                          
                          # a character vector naming aesthetics that will cause removal if they have missing values
                          # non_missing_aes = character(),
                          
                          # character vector naming aesthetics that will be accepted by layer(), but are not required or described in default_aes field
                          optional_aes = c("ymin", "ymax", "uncertainty", "label", "era", "module_name", "uncertainty_label"),
                          
                          # a [mapping][aes()] of default values for aethetics
                          default_aes = aes(colour = "black", fill = "grey70", size = 1, alpha = 0.5,
                                            linetype = 1, shape = 19, group = 1),
                          
                          # T/F whether to rename size aesthetics to linewidth
                          # rename_size = FALSE,
                          
                          # a character vector of param names in addition to those imputed from the draw_panel() or draw_groups() methods.
                          # This field can be set to include params for setup_data() or handle_na() methods
                          # extra_params = c("na.rm", "label_name", "reference_name"), # na.rm default
                          
                          # A function generating a single legend glyph for the geom
                          # draw_key = draw_key_point,
                          
                          # Link geom to stat
                          default_stat = StatTimeseries,
                          
                          # Methods -----------------------------------------------------------------
                          
                          # No longer need setup params bc it's dont for us in the stat
                          # setup_params = function(data, params) {
                          #   message("---Entering setup_params---")
                          #   # Add new param needed for filtering
                          #   message(glue::glue("Original params: {paste0(names(params), collapse = ', ')}"))
                          #   params$label_name <- ifelse(is.null(params$label_name), "spawning_biomass", params$label_name)
                          #   params$reference_name <- ifelse(is.null(params$reference_name), "msy", params$reference_name)
                          #   message(glue::glue("Params after modification: {paste0(names(params), collapse = ', ')}"))
                          #   print(params)
                          #   message(glue::glue("label_name value: {params$label_name}"))
                          #   message(glue::glue("parameter_name value: {params$reference_name}"))
                          #   message("---Exiting setup_params---")
                          #   params
                          # },
                          
                          # function to modify or check the data prior to adding defaults
                          # Do not need since done for us in stat
                          # setup_data = function(data, params, label_name){
                          # message("---Entering setup_data---")
                          # message(glue::glue("Dimensions of initial data: {paste(dim(data), collapse = 'x')}"))
                          # message(glue::glue("Initial data column names: {paste(names(data), collapse = ', ')}"))
                          # # Calculate reference point
                          # ref_pt <- calculate_reference_point(
                          #   dat = data,
                          #   reference_name = params$reference_name,
                          #   label_name = params$label_name
                          # )
                          # 
                          # # Ensure x and y are numeric and create 'year' and 'estimate' columns
                          # if (!("x" %in% names(data)) || !("y" %in% names(data))) {
                          #   stop("Data must contain 'x' and 'y' aesthetics mapped to columns.")
                          # }
                          # 
                          # data <- data |>
                          #   dplyr::mutate(
                          #     x = as.numeric(.data$x),
                          #     y = as.numeric(.data$y),
                          #     ref_pt = ref_pt
                          #   )
                          # 
                          # # Initialize estimate_lower and estimate_upper
                          # data$ymin <- NA_real_ # previously estimate_lower
                          # data$ymax <- NA_real_ # previously estimate_upper
                          # message(glue::glue("Data column names after ref_pt and estimate intervals added: \n{paste(names(data), collapse = ', ')}"))
                          # 
                          # # Initial filtering of the data
                          # if ("label" %in% names(data) && "era" %in% names(data)) {
                          #   message(glue::glue("Data contains label and era -- filtering by {params$label_name}"))
                          #   data <- data |>
                          #     dplyr::filter(
                          #       grepl(glue::glue("{params$label_name}$"), .data$label),
                          #       .data$era == "time"
                          #     )
                          #   message(glue::glue("Dimensions of data after filter 1: {paste(dim(data), collapse = 'x')}"))
                          #   message(glue::glue("Data column names after filter 1: {paste(names(data), collapse = ', ')}"))
                          # } else {
                          #   # If 'label' or 'era' are not provided, you might want to issue a warning
                          #   # or handle this case based on your desired behavior.
                          #   message("Warning: 'label' or 'era' aesthetic not provided. Filtering skipped.")
                          # }
                          # 
                          # # Conditional calculation of uncertainty bounds
                          # # Only calculate if 'uncertainty' and 'uncertainty_label' columns are present
                          # if ("uncertainty" %in% names(data) && "uncertainty_label" %in% names(data)) {
                          #   message("Uncertainty column(s) found -- calculating ymin and ymax")
                          #   data <- data |>
                          #     dplyr::mutate(
                          #       ymin = dplyr::case_when(
                          #         grepl("se", .data$uncertainty_label) ~ .data$x - 1.96 * .data$uncertainty,
                          #         TRUE ~ NA_real_
                          #       ),
                          #       ymax = dplyr::case_when(
                          #         grepl("se", .data$uncertainty_label) ~ .data$x + 1.96 * .data$uncertainty,
                          #         TRUE ~ NA_real_
                          #       )
                          #     )
                          #   message(glue::glue("Current data dimensions: {paste(dim(data), collapse = 'x')}"))
                          #   message("Top 5 rows of data")
                          #   print(head(data, 5))
                          # }
                          # # Filter for module_name if exists
                          # if ("module_name" %in% names(data) && length(unique(data$module_name)) > 1) {
                          #   message("Filtering data by module_name...")
                          #   message(glue::glue("Data dimensions before filtering module_name:  {paste(dim(data), collapse = 'x')}"))
                          #   if ("TIME_SERIES" %in% unique(data$module_name) | "t.series" %in% unique(data$module_name)) {
                          #     module_name1 <- intersect(c("t.series","TIME_SERIES"), unique(data$module_name))
                          #   } else {
                          #     # Select first module_name in list
                          #     module_name1 <- unique(data$module_name)[1]
                          #   }
                          #   data <- data |>
                          #     dplyr::filter(.data$module_name == module_name1)
                          #   message(glue::glue("Data dimensions after filtering module_name:  {paste(dim(data), collapse = 'x')}"))
                          #   message(glue::glue("Data column names after filtering module_name:  {paste(names(data), collapse = ', ')}"))
                          # }
                          # message(glue::glue("Final data column names: {paste(names(data), collapse = ', ')}"))
                          # message("Top 5 row of final data:")
                          # print(head(data, 5))
                          # message("---Exiting setup_data---")
                          #   data
                          # },
                          
                          # Function that takes on the tasks of evaluating the default aes, handle the scaling, and fill in fixed, unmapped aes passed as params
                          # DO NOT MODIFY - for now
                          # use_defaults = function(){ },
                          
                          # draw_geom ---------------------------------------------------------------
                          
                          # function to handle missing values - below is default from ggplot2
                          # handle_na = function(self, data, params) {
                          #   remove_missing(data, params$na.rm,
                          #                  c(self$required_aes, self$non_missing_aes),
                          #                  snake_class(self)
                          #   )
                          # },
                          # 
                          # # Function for drawing the entire later
                          # # default method splits the data and passes on drawing tasks to draw_panel.
                          # # Not recommended to use this as an extension point
                          # draw_layer = function(self, data, params, layout, coord) {
                          #   message("---Entering draw_layer---")
                          #   if (empty(data)) {
                          #     n <- if (is.factor(data$PANEL)) nlevels(data$PANEL) else 1L
                          #     return(rep(list(zeroGrob()), n))
                          #   }
                          #   
                          #   # Trim off extra parameters
                          #   params <- params[intersect(names(params), self$parameters())]
                          #   
                          #   if (nlevels(as.factor(data$PANEL)) > 1L) {
                          #     data_panels <- split(data, data$PANEL)
                          #   } else {
                          #     data_panels <- list(data)
                          #   }
                          #   message("---Exiting draw_layer---")
                          #   lapply(data_panels, function(data) {
                          #     if (empty(data)) return(zeroGrob())
                          #     
                          #     panel_params <- layout$panel_params[[data$PANEL[1]]]
                          #     rlang::inject(self$draw_panel(data, panel_params, coord, !!!params))
                          #   })
                          # },
                          
                          # Function to draw the later for a single panel or group
                          # drawl_panel splits the data into groups and passes it to
                          # draw_group which assembles into a single grob
                          # ... argument is reserved for ententions. By default passed onto the draw_group method
                          # draw_panel = function(self, data, panel_params, 
                          # label_name, coord, ...) { # not sure what `self` is
                          # message("---Entering draw_panel---")
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
                          #       grepl("se", .data$uncertainty_label) ~ .data$x - 1.96 * .data$uncertainty,
                          #       TRUE ~ NA_real_
                          #     ),
                          #     estimate_upper = dplyr::case_when(
                          #       grepl("se", .data$uncertainty_label) ~ .data$x + 1.96 * .data$uncertainty,
                          #       TRUE ~ NA_real_
                          #     )
                          #   )
                          
                          # data_hline <- modifyList(data, ref_pt)
                          
                          # groups <- split(data, factor(data$group))
                          # message("Look of the first 5 rows of data when split by groups: ")
                          # print(head(groups, 5))
                          
                          # data$y <- data$yintercept
                          # message("---Exiting draw_panel---")
                          # grid::gTree(children = grid::gList(
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
                          # GeomHline$draw_panel(data, panel_params, coord, ...)
                          # })
                          # ))
                          
                          # ggname(snake_class(self), grid::gTree(
                          #   children = rlang::inject(grid::gList(!!!grobs))
                          # ))
                          
                          # grid::grid::gList(
                          #   GeomLine$draw_panel(data, panel_params, coord, ...),
                          #   GeomRibbon$draw_panel(error1, panel_params, coord, ...)
                          # )
                          # },
                          
                          # Panel from gemini
                          draw_panel = function(self, data, panel_params, coord, ...) {
                            # If the data frame is empty, we must return an empty grob
                            if (nrow(data) == 0) {
                              return(zeroGrob())
                            }
                            
                            # Transform coordinates
                            coords <- coord$transform(data, panel_params)
                            
                            # Draw the ribbon (if ymin and ymax are available)
                            ribbon_grob <- GeomRibbon$draw_panel(
                              data = coords,
                              panel_params = panel_params,
                              coord = coord
                            )
                            
                            # Draw the line
                            line_grob <- GeomLine$draw_panel(
                              data = coords,
                              panel_params = panel_params,
                              coord = coord
                            )
                            
                            # Draw the horizontal line
                            hline_data <- data.frame(
                              yintercept = coords$yintercept[1],
                              group = coords$group[1],
                              # Pass through aesthetics for the hline
                              linetype = coords$linetype[1],
                              colour = "red" # You can hardcode or use an aesthetic
                            )
                            hline_grob <- GeomHline$draw_panel(
                              data = hline_data,
                              panel_params = panel_params,
                              coord = coord
                            )
                            
                            # Combine all the grobs into a single gTree
                            gTree(children = gList(ribbon_grob, line_grob, hline_grob))
                          }
                          
                          # draw_group = function(self, data, panel_params, coord, label_name) {
                          #   message("---Entering draw_group---")
                          #   groups <- split(data, factor(data$group))
                          #   # grid::gTree(children = grid::gList(
                          #   grobs_line <- lapply(groups, function(group) {
                          #     # GeomLine$draw_panel(data, panel_params, coord, ...)
                          #     GeomLine$draw_panel(
                          #       data = data, 
                          #       panel_params = panel_params, 
                          #       coord = coord,
                          #       na.rm = na.rm,
                          #       ...,
                          #       label_name = label_name
                          #     )
                          #   })
                          #   grobs_ribbon <- lapply(groups, function(group) {
                          #     GeomRibbon$draw_panel(data, panel_params, coord, ...)
                          #   })
                          #   # ))
                          #   message("---Exiting draw_group---")
                          #   
                          #   ggname(snake_class(self), grid::gTree(
                          #     children = rlang::inject(grid::gList(grobs_line, grobs_ribbon))
                          #   ))
                          #   # ggplot2 default below
                          #   # cli::cli_abort("{.fn {snake_class(self)}}, has not implemented a {.fn draw_group} method")
                          # }
                          
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
      label_name = label_name,
      reference_name = reference_name,
      na.rm = na.rm,
      ...
    )
  )
}

# test geom -------
ggplot(data = sample_data,
       aes(x = year,
           y = estimate,
           label = label,
           era = era,
           module_name = module_name,
           uncertainty = uncertainty,
           uncertainty_label = uncertainty_label
       )
) +
  geom_timeseries(
    label_name = "biomass", 
    reference_name = "msy",
    na.rm = TRUE
  )
