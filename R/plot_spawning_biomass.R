#' Plot spawning biomass (SB)
#'
#' Plot spawning biomass with a reference line as a dashed line. The figure can
#' also be made relative to this reference line rather than in absolute units.
#'
#' @param dat Data frame or list. A tibble or named list of tibbles (input as `list()`)
#' returned from \link[stockplotr]{convert_output}.
#'
#' If inputting a list of tibbles, the first tibble's reference point defined
#' in `ref_line` is used to plot a reference line or calculate relative spawning biomass.
#' @param geom String. Geom used for the plot.
#'
#' Default: "line".
#'
#' Options: "line", "point", or "area"
#' @param group String. Single column that groups the data.
#'
#' Set group = "none" to summarize data over all indexing values.
#'
#' Default: NULL
#' Options: Including, but not limited to: "year", "area", "fleet", "sex", "none", NULL
#'
#' @param facet Character vector. Column name or names used for faceting.
#'
#' Default: NULL
#' @param era String. Era of data.
#'
#' Default: "time"
#'
#' Options: "early", "time", "fore" (forecast), or NULL (all data)
#' @param ref_line String. Reference point name.
#'
#' Default: "target"
#'
#' Options: (including, but not limited to) "target", "msy", and "unfished"
#' If the reference point is not found in the data, set ref_line = c("name" = value).
#' @param unit_label String. Spawning biomass unit.
#'
#' Default: "mt"
#' @param lbs Logical. TRUE/FALSE; indicate whether to convert the y-axis values from
#' kilograms to pounds. The default units match the default in the
#' unit_label argument - 'mt'.
#'
#' Default: `FALSE`
#' @param module Character vector. (Optional) Module name found in `dat`.
#' If selecting >1 module, place them in a vector like c("module1", "module2").
#'
#' Default: NULL
#'
#' If the interactive and >1 module_name is found, user will select the
#' module_name in the console. @seealso [filter_data()]
#' @param scale_amount Number. A number to scale the y-axis values.
#'
#' Default: 1
#' @param relative Logical. TRUE/FALSE; specify whether to set y-axis values relative to
#' the ref_line value.
#'
#' Default: `FALSE`
#' @param make_rda Logical. TRUE/FALSE; indicate whether to save the object and
#' make an automated caption and alternative text in the form of an `rda` object. If TRUE,
#' the rda will be exported to the folder indicated in the argument "figures_dir".
#'
#' Default: `FALSE`.
#' @param figures_dir Path. Path to the "figures" folder.
#'
#' Default: `getwd()`
#'
#' The folder is created within the path if it does not exist.
#' @param interactive Logical. TRUE/FALSE; indicate whether the environment is interactive.
#'
#' Default: `FALSE`
#' @param ... Arguments called from \link[ggplot2]{geom_line} or \link[ggplot2]{geom_point}
#'
#' @return A plot showing spawning biomass over time.
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a `ggplot2` object or export an .rda object
#' containing associated caption and alternative text for the figure.
#'
#' @note
#' All plotting functions automatically recognize indexing variables and will
#' use them in groupings and/or facetting. @seealso [process_data()].
#'
#' @seealso [convert_output()], [plot_timeseries()], [calculate_reference_point()], [reference_line()], [filter_data()], [process_data()], [export_kqs()], [insert_kqs()], [create_rda()]
#'
#' @export
#'
#' @examples
#' plot_spawning_biomass(
#'   dat = stockplotr:::example_data,
#'   geom = "line",
#'   ref_line = "msy",
#'   unit_label = "mt",
#'   scale_amount = 1000,
#'   interactive = FALSE,
#'   module = "TIME_SERIES",
#'   linewidth = 1.5
#' )
#' plot_spawning_biomass(
#'   dat = stockplotr:::example_data,
#'   ref_line = c("target" = 10),
#'   interactive = FALSE,
#'   module = "TIME_SERIES"
#' )
plot_spawning_biomass <- function(
  dat,
  geom = "line",
  group = NULL,
  facet = NULL,
  ref_line = "msy",
  unit_label = "mt",
  era = NULL,
  lbs = FALSE,
  module = NULL,
  scale_amount = 1,
  relative = FALSE,
  make_rda = FALSE,
  figures_dir = getwd(),
  interactive = TRUE,
  ...
) {
  # this assumes that the previous units were metric tons
  if (lbs && unit_label %notin% c("lbs", "pounds", "lb")) {
    cli::cli_alert_info("Unit label was not changed. Setting unit_label to 'lbs'.")
    unit_label <- "lbs"
  }

  # TODO: Fix the unit label if scaling. Maybe this is up to the user to do if
  #       they want something scaled then they have to supply a better unit name
  #       or we create a helper function to do this.
  spawning_biomass_label <- ifelse(
    relative,
    yes = "Relative spawning biomass",
    no = {
      label_magnitude(
        label = "Spawning Biomass",
        unit_label = unit_label,
        scale_amount = dplyr::if_else(
          lbs,
          ifelse(unit_label %in% c("mt", "mts", "metric tons", "metric ton"), 1000, 1) * scale_amount,
          scale_amount
        ),
        legend = TRUE
      )
    }
  )
  # Pull first df if in a list to find reference point
  # if (!is.data.frame(dat)) {
  #   rp_dat <- dat[[1]]
  # } else {
  #   rp_dat <- dat
  # }

  if (relative & scale_amount > 1) {
    cli::cli_alert_warning("Scale amount is not applicable when relative = TRUE. Resetting scale_amount to 1.")
    scale_amount <- 1
  }

  # Filter data for spawning biomass
  prepared_data <- filter_data(
    dat = dat,
    label_name = ifelse(relative, glue::glue("spawning_biomass_spawning_biomass_{ref_line}|spawning_biomass_ratio"), "^spawning_biomass$"),
    geom = geom,
    era = era,
    group = group,
    facet = facet,
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  )

  if (relative) {
    if (nrow(prepared_data) == 0) {
      cli::cli_abort("No data found for relative biomass. Please check that your data contains a label for 'biomass_biomass_unfished'.")
      stop()
    }
  }

  # process the data for grouping
  processing <- process_data(
    dat = prepared_data,
    group = group,
    facet = facet,
    method = "sum",
    lbs = lbs
  )
  # variable <- processing[[1]]
  plot_data <- processing[[1]]
  group <- processing[[2]]
  if (!is.null(processing[[3]])) facet <- processing[[3]]

  # Override grouping variable when there is only NA's
  if (!is.null(group)) {
    if (group %notin% colnames(plot_data)) group <- NULL
  }

  plt <- plot_timeseries(
    dat = plot_data,
    y = "estimate",
    geom = geom,
    ylab = spawning_biomass_label,
    group = group,
    # add check in case facet is returned as character(0)
    facet = if (length(facet) > 0) facet else NULL,
    ...
  )
  # Add reference line
  # Conditions for ref line
  # 1. all are comparing same value = msy, target, unfished...
  # 2. custom input vector = c("sabe23_msy"=30, "sable25_msy"=40) -- user must indicate which model in label otherwise it will assign in order of dat
  # 3. input vector of labels = c("msy", "target")
  # getting data set - an ifelse statement in the fxn wasn't working
  if (relative) {
    # don't add any reference line here and just add theme for final plot
    final <- plt + theme_noaa()
  } else {
    plt2 <- plt
    # Put into for loop and add lines sequentially to plt
    for (i in 1:length(ref_line)) {
      # find the reference point value
      if (is.null(names(ref_line[i]))) {
        ref_line_x <- calculate_reference_point(
          dat = dat[[i]],
          reference_name = i,
          lbs = lbs
        ) / scale_amount
        ref_line_x <- setNames(ref_line_x, i)
      } else {
        ref_line_x <- ref_line[i] / scale_amount
      }
      
        if ("unfished" %in% names(ref_line_x)) {
        # find the minimum x axis value from the plot
        min_year <- min <- ggplot2::ggplot_build(plt)[["data"]][[2]] |> # I think this was causing issues on linux
          as.data.frame() |>
          dplyr::pull(x) |>
          min() |>
          round(digits = 2)
        # add point to plot and add theme
        plt2 <- plt2 +
          ggplot2::geom_point(ggplot2::aes(x = min_year - 1, y = ref_point)) + # should I keep -1 or set as first year?
          theme_noaa()
      } else {
        # add apply/purrr/or for loop for reference lines -- not just the first anymore
        plt2 <- plt2 +
          reference_line(
            # conditionally add label name
            label_name = ifelse(length(names(ref_line)) == 1, "spawning_biomass", names(dat)[i]), #"spawning_biomass",
            ref_line = ref_line_x,
            scale_amount = 1
          )
      }
    }
    final <- plt2 + theme_noaa()
  }

  ### Make RDA ----
  if (make_rda) {
    if (relative) {
      rel.ssb.min <- calc_kqs(returned_kq = "rel.ssb.min",
                              final = final)
      
     
      rel.ssb.max <- calc_kqs(returned_kq = "rel.ssb.max",
                              final = final)
        
      # calculate & export key quantities
      export_kqs(rel.ssb.min, rel.ssb.max)

      # Add key quantities to captions/alt text
      insert_kqs(rel.ssb.min, rel.ssb.max)
    } else {
      ssb.min <- min(plot_data$estimate) |> round(digits = 3)
      ssb.max <- max(plot_data$estimate) |> round(digits = 3)

      export_kqs(ssb.min, ssb.max)
      insert_kqs(ssb.min, ssb.max)
    }

    # Obtain relevant key quantities for captions/alt text
    ssb.ref.pt <- as.character(ref_line)
    ssb.units <- as.character(unit_label)
    ssb.start.year <- min(plot_data$year)
    ssb.end.year <- max(plot_data$year)

    # calculate & export key quantities
    export_kqs(
      ssb.ref.pt,
      ssb.units,
      ssb.start.year,
      ssb.end.year
    )

    # Add key quantities to captions/alt text
    insert_kqs(
      ssb.ref.pt,
      ssb.units,
      ssb.start.year,
      ssb.end.year
    )

    create_rda(
      object = final,
      topic_label = ifelse(relative, "relative_spawning_biomass", "spawning_biomass"),
      fig_or_table = "figure",
      dat = rp_dat,
      dir = figures_dir,
      ref_line = ifelse(!is.null(names(ref_line)), names(ref_line), ref_line),
      scale_amount = scale_amount,
      unit_label = unit_label
    )
  }
  # Output final plot
  final
}
