#' Plot Total Biomass
#'
#' @inheritParams plot_spawning_biomass
#' @param unit_label String. Biomass units
#'
#' Default: "mt"
#' @param ref_line String. Type of reference point to
#'   compare biomass to. The default `"msy"` looks for
#'   `"biomass_msy"` in the `"label"` column of `dat`. The actual
#'   searching in `dat` is case-agnostic and will work with either upper- or
#'   lower-case letters. However, you must use one of the options specified in the
#'   default list to ensure that the label on the figure looks correct
#'   regardless of how it is specified in `dat`.
#'
#' Default: "msy"
#'
#' Options: Including, but not limited to: "target", "MSY", "unfished"
#' @returns A plot showing total biomass.
#'
#' @details The input is from an assessment model output file
#' translated to a standardized output (\link[stockplotr]{convert_output}).
#' There are options to return a `ggplot2` object or export an .rda object
#' containing associated caption and alternative text for the figure.
#'
#' @seealso [convert_output()], [plot_timeseries()], [calculate_reference_point()], [reference_line()], [filter_data()], [process_data()], [export_kqs()], [insert_kqs()], [create_rda()]
#'
#' @export
#'
#' @examples
#' plot_biomass(
#'   dat = stockplotr:::example_data,
#'   unit_label = "mt",
#'   ref_line = c("target" = 20000),
#'   scale_amount = 100,
#'   module = "TIME_SERIES",
#'   figures_dir = getwd()
#' )
#' plot_biomass(
#'   dat = stockplotr:::example_data,
#'   ref_line = NULL,
#'   module = "TIME_SERIES",
#'   figures_dir = getwd()
#' )
plot_biomass <- function(
  dat,
  geom = "line",
  group = NULL,
  facet = NULL,
  ref_line = "msy",
  era = NULL,
  unit_label = "mt",
  module = NULL,
  scale_amount = 1,
  relative = FALSE,
  make_rda = FALSE,
  figures_dir = getwd(),
  interactive = TRUE,
  ...
) {
  # TODO: Fix the unit label if scaling. Maybe this is up to the user to do if
  #       they want something scaled then they have to supply a better unit name
  #       or we create a helper function to do this.
  biomass_label <- ifelse(
    relative,
    yes = "Relative biomass",
    no = {
      label_magnitude(
        label = "Biomass",
        unit_label = unit_label,
        scale_amount = scale_amount,
        legend = FALSE
      )
    }
  )

  # Pull first df if in a list to find reference point
  if (!is.data.frame(dat)) {
    rp_dat <- dat[[1]]
  } else {
    rp_dat <- dat
  }

  if (relative & scale_amount > 1) {
    cli::cli_alert_warning("Scale amount is not applicable when relative = TRUE. Resetting scale_amount to 1.")
    scale_amount <- 1
  }
  
  orig_group <- group
  orig_facet <- facet
  
  # Filter data for biomass
  # TODO: determine method to ID that first point in the timeseries is actually Bunfished ref pt
  prepared_data <- filter_data(
    dat = dat,
    label_name = ifelse(relative, "biomass_biomass_unfished|biomass_ratio", "^biomass$"), # what exactly is biomass_ratio?
    geom = geom,
    group = group,
    facet = facet,
    era = era,
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  )
  if (relative) {
    if (nrow(prepared_data) == 0) {
      cli::cli_abort("No data found for relative biomass. Please check that your data contains a label for 'biomass_biomass_unfished'.")
      stop()
    }
  } else {
    # check if all 3 are present and subset for one or two
    if (length(unique(prepared_data$label)) > 1 & any(grepl("biomass$", unique(prepared_data$label)))) {
      # cli::cli_alert_info("> 1 label name. Selecting total biomass only.")
      prepared_data <- prepared_data |>
        dplyr::filter(
          grepl("biomass$", label)
        )
    }
  }

  # Process data for indexing/grouping
  # TODO: check and add into process_data step to summarize when theres >1 label
  processing <- process_data(
    prepared_data,
    group,
    facet
  )

  # variable <- processing[[1]]
  prepared_data <- processing[[1]]
  group <- processing[[2]]
  if (!is.null(processing[[3]])) facet <- processing[[3]]


  plt <- plot_timeseries(
    dat = prepared_data,
    y = "estimate",
    geom = geom,
    ylab = biomass_label,
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
          reference_name = glue::glue("biomass_{ref_line[i]}"),
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
            label_name = ifelse(length(names(ref_line)) == 1, "biomass", names(dat)[i]), #"spawning_biomass",
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
      rel.B.min <- calc_kqs(returned_kq = "rel.B.min",
                            final = final)
      rel.B.max <- calc_kqs(returned_kq = "rel.B.max",
                            final = final)

      # calculate & export key quantities
      export_kqs(rel.B.min, rel.B.max)

      # Add key quantities to captions/alt text
      insert_kqs(rel.B.min, rel.B.max)
    } else {
      B.min <- calc_kqs(returned_kq = "B.min",
                        prepared_data = prepared_data)
      B.max <- calc_kqs(returned_kq = "B.max",
                        prepared_data = prepared_data)

      export_kqs(B.min, B.max)
      insert_kqs(B.min, B.max)
    }

    B.ref.pt <- as.character(ref_line)
    B.units <- as.character(unit_label)
    B.start.year <- calc_kqs(returned_kq = "B.start.year",
                             prepared_data = prepared_data)
    B.end.year <- calc_kqs(returned_kq = "B.end.year", 
                           prepared_data = prepared_data)
    B.terminal.year <- calc_kqs(returned_kq = "B.terminal.year",
                                dat = dat,
                                relative = relative)

    # SS3, FIMS
    if ("spawning_biomass_msy" %in% dat$label) {
      sb_msy <- calc_kqs(returned_kq = "sb_msy",
                         dat = dat)
      b_sb_msy <- calc_kqs(returned_kq = "b_sb_msy",
                           dat = dat)
      B.msy <- calc_kqs(returned_kq = "B.msy",
                        dat = dat)
      B.msy.min <- NA
      B.msy.max <- NA
    # BAM
    } else if ("bmsy" %in% dat$label) {
      B.msy <- calc_kqs(returned_kq = "B.msy", 
                        dat = dat)
      B.msy.min_uncert <- calc_kqs(returned_kq = "B.msy.min_uncert",
                                   dat = dat)
      B.msy.min <- calc_kqs(returned_kq = "B.msy.min", 
                            dat = dat)
      B.msy.max <- calc_kqs(returned_kq = "B.msy.max",
                            dat = dat)
    # Rceattle
    } else {
      B.msy <- NA
      B.msy.min <- NA
      B.msy.max <- NA
    }
    
    export_kqs(
      B.ref.pt,
      B.units,
      B.start.year,
      B.end.year,
      B.terminal.year,
      B.msy.min,
      B.msy.max,
      B.msy
    )

    insert_kqs(
      B.ref.pt,
      B.units,
      B.start.year,
      B.end.year,
      B.terminal.year,
      B.msy.min,
      B.msy.max,
      B.msy
    )

    insert_kqs(
      B.ref.pt,
      B.units,
      B.start.year,
      B.end.year,
      B.terminal.year,
      B.msy.min,
      B.msy.max,
      B.msy
    )

    create_rda(
      object = final,
      topic_label = ifelse(relative, "relative_biomass", "biomass"),
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
