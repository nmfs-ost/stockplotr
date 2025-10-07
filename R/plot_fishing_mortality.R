plot_fishing_mortality <- function(
  dat,
  geom = "line",
  group = NULL,
  facet = NULL,
  ref_line = "msy",
  era = "time",
  module = NULL,
  relative = FALSE,
  make_rda = FALSE,
  figures_dir = getwd(),
  interactive = TRUE,
  ...
) {
  # Filter out data for fishing mortality
  filter_data <- prepare_data(
    dat = dat,
    label_name = "^fishing_mortality$",
    geom = geom,
    era = NULL,
    group = group,
    facet = facet,
    module = module,
    scale_amount = 1,
    interactive = interactive
  )

  # Extract reference point unless explicit
  if (!is.null(names(ref_line))) {
    ref_line_val <- ref_line[[1]]
  } else {
    reference_point <- calculate_reference_point(
      dat = dat,
      reference_name = glue::glue("fishing_mortality_", ref_line)
    )
  }

  # Create base plot
  plt <- plot_timeseries(
    dat = filter_data,
    y = "estimate",
    geom = geom,
    ylab = "Fishing Mortality",
    group = group,
    facet = facet#,
    # ...
  )
  # Add reference line and theme
  final <- reference_line(
    plot = plt,
    dat = dat,
    era = "time",
    label_name = "fishing_mortality",
    reference = ref_line,
    relative = relative,
    scale_amount = 1
  ) + theme_noaa()

  ### Make RDA ----
  if (make_rda) {
    create_rda(
      object = final,
      topic_label = ifelse(relative, "relative.fishing.mortality", "fishing.mortality"),
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