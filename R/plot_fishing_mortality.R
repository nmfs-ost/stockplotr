#' Plot fishing mortality timeseries
#'
#' @inheritParams plot_spawning_biomass
#'
#' @returns A plot showing fishing mortality over time.
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a `ggplot2` object or export an .rda object
#' containing associated caption and alternative text for the figure.
#'
#' @seealso [convert_output()], [filter_data()], [process_data()], [plot_timeseries()], [reference_line()], [export_kqs()], [insert_kqs()], [create_rda()]
#'
#' @export
#'
#' @examples
#' plot_fishing_mortality(
#'   dat = stockplotr:::example_data,
#'   ref_line = c("target" = 0.2),
#'   group = "fleet",
#'   interactive = FALSE,
#'   module = "TIME_SERIES"
#' )
#' plot_fishing_mortality(
#'   dat = stockplotr:::example_data,
#'   ref_line = c("target" = 0.2),
#'   group = "none",
#'   interactive = FALSE,
#'   module = "TIME_SERIES"
#' )
plot_fishing_mortality <- function(
  dat,
  geom = "line",
  group = NULL,
  facet = NULL,
  ref_line = "msy",
  era = NULL,
  module = NULL,
  make_rda = FALSE,
  figures_dir = getwd(),
  interactive = TRUE,
  ...
) {
  orig_group <- group
  orig_facet <- facet
  # Filter out data for fishing mortality
  prepared_data <- filter_data(
    dat = dat,
    label_name = "^fishing_mortality$",
    geom = geom,
    era = era,
    group = group,
    facet = facet,
    module = module,
    scale_amount = 1,
    interactive = interactive
  )
  # Process data
  processed_data <- process_data(
    dat = prepared_data,
    group = group,
    facet = facet,
    method = "mean"
  )
  prepared_data <- processed_data[[1]]
  group <- processed_data[[2]]
  facet <- processed_data[[3]]

  # Create base plot
  plt <- plot_timeseries(
    dat = prepared_data,
    y = "estimate",
    geom = geom,
    ylab = "Fishing Mortality",
    group = group,
    facet = facet,
    ...
  )
  # Add reference line
  # Conditions for ref line
  # 1. all are comparing same value = msy, target, unfished...
  # 2. custom input vector = c("sable23_msy"=30, "sable25_msy"=40) -- user must indicate which model in label otherwise it will assign in order of dat
  # 3. input vector of labels = c("msy", "target")
  # getting data set - an ifelse statement in the fxn wasn't working
  if (relative) {
    # don't add any reference line here and just add theme for final plot
    final <- plt + theme_noaa()
  } else {
    plt2 <- plt
    # Check if length of ref_line = dat
    # replicate value if not
    if (length(ref_line) != length(dat)) ref_line <- rep(ref_line, length(dat))
    # Put into for loop and add lines sequentially to plt
    for (i in 1:length(ref_line)) {
      # find the reference point value
      if (is.null(names(ref_line[i]))) {
        ref_line_x <- calculate_reference_point(
          dat = dat[[i]],
          reference_name = glue::glue("fishing_mortality_{ref_line[i]}"),
          lbs = lbs
        ) / scale_amount
        ref_line_x <- setNames(ref_line_x, ref_line[i])
      } else {
        ref_line_x <- ref_line[i] / scale_amount
      }
      
      if ("unfished" %in% names(ref_line_x)) {
        # find the minimum x axis value from the plot
        min_year <- min <- ggplot2::ggplot_build(plt2)[["data"]][[2]] |> # I think this was causing issues on linux?
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
            label_name = ifelse(length(names(ref_line)) == 1, "fishing_mortality", names(dat)[i]), #"spawning_biomass",
            ref_line = ref_line_x,
            scale_amount = 1
          )
      }
    }
    final <- plt2 + theme_noaa()
  }

  ### Make RDA ----
  if (make_rda) {
    F.min <- calc_kqs(returned_kq = "F.min",
                      prepared_data = prepared_data)
    F.max <- calc_kqs(returned_kq = "F.max",
                      prepared_data = prepared_data)

    export_kqs(F.min, F.max)
    insert_kqs(F.min, F.max)

    F.ref.pt <- as.character(ref_line)
    F.start.year <- min(prepared_data$year)
    F.end.year <- max(prepared_data$year)
    F.terminal.year <- calc_kqs(returned_kq = "F.terminal.year",
      dat = dat
    )
    
    F.target <- calc_kqs(returned_kq = "F.target",
                         dat = dat)
    f.limit <- calc_kqs(returned_kq = "f.limit",
                        dat = dat)
    
    export_kqs(
      F.ref.pt,
      F.start.year,
      F.end.year,
      F.terminal.year,
      F.target,
      f.limit
    )

    insert_kqs(
      F.ref.pt,
      F.start.year,
      F.end.year,
      F.terminal.year,
      F.target,
      f.limit
    )

    create_rda(
      object = final,
      topic_label = "fishing_mortality", # ifelse(relative, "relative_fishing_mortality", "fishing_mortality"),
      fig_or_table = "figure",
      dat = dat,
      dir = figures_dir,
      ref_line = ifelse(!is.null(names(ref_line)), names(ref_line), ref_line),
      scale_amount = 1,
      unit_label = "" # no unit for F
    )
  }
  # Output final plot
  final
}
