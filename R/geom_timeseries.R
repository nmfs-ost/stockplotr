# Stat ----
StatTimeseries <- ggproto("StatTimeseries", Stat,
  required_aes = c("x", "y"),
  # non_required_aes = c("label", "era", "module_name", "uncertainty_label", "uncertainty"),
  optional_aes = c("uncertainty", "label", "era", "module_name", "uncertainty_label"),
  non_missing_aes = c("colour"), # used to include x,y but this variable determine which aesthetics cannot have NA values
  extra_params = c("na.rm", "label_name", "reference_name"),

  setup_params = function(
    data, params
    # label_name = "spawning_biomass",
    # reference_name = "msy"
  ) {
    params$label_name <- params$label_name %||% "spawning_biomass"
    params$reference_name <- params$reference_name %||% "msy"
    params
  },
  # changed from compute_group to setup_data
  # previously when using compute_group, both label_names were plotted
  # compute_group = function(data, scales, params,
  #                          label_name = "spawning_biomass",
  #                          reference_name = "msy",
  #                          na.rm = TRUE) {
  setup_data = function(data, params,
    label_name,
    reference_name,
    na.rm = TRUE
  ) {
    # If the data frame is empty, we must return an empty data frame.
    if (nrow(data) == 0) {
      data
    }

    # Ensure x and y are numeric and create 'year' and 'estimate' columns
    if (!("x" %in% names(data)) || !("y" %in% names(data))) {
      stop("Data must contain 'x' and 'y' aesthetics mapped to columns.")
    }

    # message("Check that sb_msy exists:")
    # ref_point <- dplyr::filter(data, grepl("spawning_biomass_msy", label))
    # print(ref_point)

    print(unique(data$label))

    # Calculate reference point
    # Needs to be before filtering
    data$yintercept <- calculate_reference_point(
      dat = data,
      reference_name = reference_name, # params$reference_name,
      label_name = label_name # params$label_name
    )

    data <- data |>
      dplyr::mutate(
        x = as.numeric(.data$x),
        y = as.numeric(.data$y)
      )

    # Initial filtering of the data
    if ("label" %in% names(data) && "era" %in% names(data)) {
    # label_pattern <- glue::glue("{label_name}$")
      data <- data |>
        dplyr::filter(
          grepl(label_name, .data$label),
          .data$era == "time",
          !is.na(.data$y)
        )
    }

    # Conditional calculation of uncertainty bounds
    # Only calculate if 'uncertainty' and 'uncertainty_label' columns are present
    if ("uncertainty" %in% names(data) && "uncertainty_label" %in% names(data)) {
      data <- data |>
        dplyr::mutate(
          ymin = dplyr::case_when(
            grepl("se", .data$uncertainty_label) ~ .data$y - 1.96 * .data$uncertainty,
            TRUE ~ NA_real_
          ),
          ymax = dplyr::case_when(
            grepl("se", .data$uncertainty_label) ~ .data$y + 1.96 * .data$uncertainty,
            TRUE ~ NA_real_
          )
        )
    } else {
      # Initialize estimate_lower and estimate_upper
      data$ymin <- NA_real_
      data$ymax <- NA_real_
    }

    # Filter for module_name if exists
    # if ("module_name" %in% names(data) && length(unique(data$module_name)) > 1) {
    if ("TIME_SERIES" %in% unique(data$module_name) | "t.series" %in% unique(data$module_name)) {
      module_name1 <- intersect(c("t.series","TIME_SERIES"), unique(data$module_name))
    } else {
      # Select first module_name in list
      module_name1 <- unique(data$module_name)[1]
    }
    data <- data |>
      dplyr::filter(.data$module_name == module_name1)
    # }

    # Remove rows with NA's that were created during the computations above.
    # We check the aesthetics that will be used for plotting.
    if (na.rm) {
      # Note: `yintercept` is an important aesthetic, but it's okay for it to be NA
      # if you want the horizontal line to not be drawn.
      # The crucial aesthetics for the line and ribbon are x, y, ymin, and ymax.
      data <- data[!is.na(data$x) & !is.na(data$y), ]
    }
    # message(glue::glue("Processed data dimensions: {paste(dim(data), collapse = 'x')}"))
    # message(glue::glue("Processed data columns: {paste(names(data), collapse = ', ')}"))
    # print(head(data, 10))

    data
  },

  handle_na = function(self, data, params) {
    # Combine required_aes and non_missing_aes to ensure all critical
    # aesthetics are checked for NA values.
    missing_aes <- unique(c(self$required_aes, self$non_missing_aes))
    remove_missing(data, params$na.rm, missing_aes, snake_class(self))
    # remove_missing(data, params$na.rm,
    #                c(self$required_aes, self$non_missing_aes),
    #                snake_class(self)
    # )
  }
)

stat_timeseries <- function(mapping = NULL, data = NULL,
                             geom = "line", position = "identity",
                             na.rm = TRUE, show.legend = NA,
                             inherit.aes = TRUE, label_name = NULL,
                             reference_name = NULL, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatTimeseries,
    geom = geom,
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

# Geom ----
GeomTimeseries <- ggproto("GeomTimeseries", Geom,
                          # Fields ----------------------------------------
                          # character vector naming aesthetics that are necessary to render geom
                          required_aes = c("x", "y"), # "yintercept"
                          
                          # a [mapping][aes()] of default values for aethetics
                          default_aes = aes(colour = "black", fill = "grey70", linewidth = 1, alpha = 0.5,
                                            linetype = 1, shape = 19, group = 1),
                          
                          # a character vector of param names in addition to those imputed from the draw_panel() or draw_groups() methods.
                          # This field can be set to include params for setup_data() or handle_na() methods
                          extra_params = c("na.rm", "label_name", "reference_name"), # na.rm default
                          
                          # Link geom to stat
                          default_stat = function(.) StatTimeseries,
                          
                          # Methods -----------------------------------------------------------------
                          
                          # setup_params = function(data, params){
                          #   params$label_name <- params$label_name %||% "spawning_biomass"
                          #   params$reference_name <- params$reference_name %||% "msy"
                          #   return(params)
                          # },
                          
                          # Panel from gemini
                          draw_panel = function(self, data, panel_params, coord, ymin, ymax, yintercept, ...) {
                            # If the data frame is empty, we must return an empty grob
                            if (nrow(data) == 0) {
                              return(zeroGrob())
                            }

                            # Transform coordinates
                            coords <- coord$transform(data, panel_params)
                            # message("Coordinate data for plots:")
                            # print(head(coords, 10))
                            
                            # Create a list to hold the grobs
                            grobs <- list()
                            
                            # Draw the ribbon (if ymin and ymax are available)
                            ribbon_data <- coords[!is.na(coords$ymin) & !is.na(coords$ymax), ]
                            if (nrow(ribbon_data) > 0) {
                              ribbon_grob <- GeomRibbon$draw_panel(
                                data = ribbon_data,
                                panel_params = panel_params,
                                coord = coord
                              )
                              grobs <- c(grobs, list(ribbon_grob))
                              message("Ribbon grob:")
                              print(ribbon_grob$data$colour)
                            }

                            # Draw the line
                            if (nrow(coords) > 0) {
                              line_grob <- GeomLine$draw_panel(
                                data = coords,
                                panel_params = panel_params,
                                coord = coord
                              )
                              grobs <- c(grobs, list(line_grob))
                              message("Line grob:")
                              print(line_grob)
                            }
                            
                            # Draw the horizontal line
                            if (!is.na(coords$yintercept[1])) {
                              hline_data <- data.frame(
                                yintercept = coords$yintercept[1],
                                group = coords$group[1],
                                linetype = coords$linetype[1],
                                linewidth = coords$linewidth[1],
                                colour = "red"
                              )

                              hline_grob <- GeomHline$draw_panel(
                                data = hline_data,
                                panel_params = panel_params,
                                coord = coord
                              )
                              grobs <- c(grobs, list(hline_grob))
                            }
                            
                            # Combine all the grobs into a single gTree
                            if (length(grobs) == 0) {
                              return(zeroGrob())
                            }
                            
                            grid::gTree(children = do.call(grid::gList, grobs))
                          }
)

geom_timeseries <- function(
    mapping = NULL, 
    data = NULL, 
    stat = "timeseries", 
    position = "identity", 
    label_name = "spawning_biomass",
    reference_name = "msy",
    na.rm = FALSE,
    show.legend = NA, 
    inherit.aes = TRUE,
    ...
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
test <- ggplot(data = sample_data,
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
    label_name = "spawning_biomass", 
    reference_name = "msy",
    na.rm = TRUE,
    aes(colour = fleet,
    linetype = season)
  ) +
  facet_grid(~ module_name)

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
  stat_timeseries(
    aes(
      # shape = fleet
      color = fleet
      ),
    label_name = 'spawning_biomass', 
    # geom = "point",
    geom = "line",
    na.rm = TRUE
  ) +
  facet_wrap(~season)
