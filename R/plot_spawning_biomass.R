#' Plot spawning biomass (SB)
#'
#' Plot spawning biomass with a reference line as a dashed line. The figure can
#' also be made relative to this reference line rather than in absolute units.
#'
#' @inheritParams plot_recruitment
#' @param ref_line A string specifying the type of reference you want to
#'   compare spawning biomass to. The default is `"target"`, which looks for
#'   `"spawning_biomass_target"` in the `"label"` column of `dat`. The actual
#'   searching in `dat` is case agnostic and will work with either upper- or
#'   lower-case letters but you must use one of the options specified in the
#'   default list to ensure that the label on the figure looks correct
#'   regardless of how it is specified in `dat`.
#' @param ref_point A known value of the reference point along with the label
#'   for the reference point as specified in the output file. Please use this
#'   option if the ref_line cannot find your desired point. Indicate the
#'   reference point in the form c("label" = value).
#' @param unit_label units for spawning_biomass
#' @return
#' Plot spawning biomass from the results of an assessment model translated to
#' the standard output. The {ggplot2} object is returned for further
#' modifications if needed.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_spawning_biomass(dat)
#'
#' plot_spawning_biomass(
#'   dat,
#'   unit_label = "my_unit",
#'   ref_line = "msy",
#'   end_year = 2024,
#'   rda_dir = getwd()
#' )
#'
#' plot_spawning_biomass(
#'   dat,
#'   unit_label = "my_unit",
#'   scale_amount = 100,
#'   ref_point = 1000,
#'   end_year = 2024,
#'   relative = TRUE,
#'   make_rda = TRUE,
#'   rda_dir = getwd()
#' )
#' }
plot_spawning_biomass <- function(
    dat,
    unit_label = "metric tons",
    scale_amount = 1,
    ref_line = c("target", "unfished", "msy"),
    ref_point = NULL,
    end_year = NULL,
    relative = FALSE,
    make_rda = FALSE,
    rda_dir = getwd()) {
  if (!is.null(ref_point)) {
    ref_line <- names(ref_point)
  } else if (length(ref_line) > 1) {
    ref_line <- "target"
  } else {
    ref_line <- match.arg(ref_line, several.ok = FALSE)
  }
  # TODO: Fix the unit label if scaling. Maybe this is up to the user to do if
  #       they want something scaled then they have to supply a better unit name
  #       or we create a helper function to do this.
  spawning_biomass_label <- ifelse(
    relative,
    yes = "Relative spawning biomass",
    no = glue::glue("Spawning biomass ({unit_label})")
  )

  # output <- dat
  # Determine the end year
  all_years <- dat[["year"]][grepl("^[0-9\\.]+$", dat[["year"]])]
  if (is.null(end_year)) {
    end_year <- format(Sys.Date(), "%Y")
  }

  # create plot-specific variables to use throughout fxn for naming and IDing
  # Indicate if spawning biomass is relative or not
  if (relative) {
    topic_label <- "relative.spawning.biomass"
  } else {
    topic_label <- "spawning.biomass"
  }

  # identify output
  fig_or_table <- "figure"

  # check year isn't past end_year if not projections plot
  check_year(
    end_year = end_year,
    fig_or_table = fig_or_table,
    topic = topic_label
  )

  # Commenting out bc this might not be consistent now with new setup 23dec2024
  # stopifnot(any(end_year >= all_years))

  # Select value for reference line and label
  # TODO: add case if ref_line not indicated or hard to find - find one of the
  # options and set as ref_line
  if (!is.null(ref_point)) {
    ref_line_val <- as.numeric(ref_point)
  } else {
    if (inherits(try(solve(as.numeric(dat[
      grep(
        pattern = glue::glue("^spawning_biomass.*{tolower(ref_line)}$"),
        x = dat[["label"]]
      ),
      "estimate"
    ])), silent = TRUE), "try-error")) {
      ref_line_val <- NULL
    } else {
      ref_line_val <- as.numeric(dat[
        grep(
          pattern = glue::glue("^spawning_biomass.*{tolower(ref_line)}$"),
          x = dat[["label"]]
        ),
        "estimate"
      ])
    }
    # ref_line_val <- as.numeric(dat[
    #   grep(
    #     pattern = glue::glue("^biomass.*{tolower(ref_line)}$"),
    #     x = dat[["label"]]
    #   ),
    #   "estimate"
    # ])
  }
  if (length(ref_line_val) == 0) {
    cli::cli_alert_warning(
      "The resulting reference value of `spawning_biomass_{ref_line}` was not found in `dat[[\"label\"]]`.",
      wrap = TRUE
    )
    cli::cli_alert_warning("Reference line will not be plotted on the figure.")
  } else if (length(ref_line_val) > 1) {
    cli::cli_alert_warning(
      "More than one of the resulting reference value of 'spawing_biomass_{ref_line}` was not in `dat[[\"label\"]]`. \n Both reference points will be plotted on the figure.",
      wrap = TRUE
    )
  }
  sb <- dat |>
    dplyr::filter(
      label == "spawning_biomass",
      module_name %in% c("DERIVED_QUANTITIES", "t.series"),
      year <= end_year
    ) |>
    dplyr::mutate(
      estimate = as.numeric(estimate),
      year = as.numeric(year),
      estimate_y = estimate / ifelse(relative, ref_line_val, scale_amount),
      # TODO: Determine what unit uncertainty is in, following is for normal sd
      # TODO: This fails for Bayesian estimation
      estimate_lower = (estimate - 1.96 * uncertainty) /
        ifelse(relative, ref_line_val, scale_amount),
      estimate_upper = (estimate + 1.96 * uncertainty) /
        ifelse(relative, ref_line_val, scale_amount)
    )

  # Choose number of breaks for x-axis
  x_n_breaks <- round(length(sb[["year"]]) / 10)
  if (x_n_breaks <= 5) {
    x_n_breaks <- round(length(sb[["year"]]))
  }

  plt <- ggplot2::ggplot(data = sb) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = year,
        y = estimate_y
      ),
      linewidth = 1
    ) +
    {
      if (!is.null(ref_line_val)) ggplot2::geom_hline(yintercept = ref_line_val / ifelse(relative, ref_line_val, scale_amount), linetype = 2)
    } +
    # Only add confidence intervals for the non NA estimates
    # which allows for no warnings if uncertainty = NA
    ggplot2::geom_ribbon(
      data = sb |> dplyr::filter(!is.na(estimate_lower)),
      ggplot2::aes(
        x = year,
        ymin = estimate_lower,
        ymax = estimate_upper
      ),
      colour = "grey",
      alpha = 0.3,
    ) +
    ggplot2::labs(
      x = "Year",
      y = spawning_biomass_label
    ) +
    ggplot2::scale_x_continuous(
      n.breaks = x_n_breaks,
      guide = ggplot2::guide_axis(minor.ticks = TRUE)
    ) +
    ggplot2::annotate(
      geom = "text",
      x = as.numeric(end_year) - (0.2 * length(sb$year)),
      y = ref_line_val / ifelse(relative, ref_line_val, scale_amount),
      label = list(bquote(SB[.(ref_line)])),
      parse = TRUE
    ) +
    ggplot2::expand_limits(y = 0)

  final <- suppressWarnings(add_theme(plt))

  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    # run write_captions.R if its output doesn't exist
    if (!file.exists(
      fs::path(getwd(), "captions_alt_text.csv")
    )
    ) {
      stockplotr::write_captions(
        dat = dat,
        dir = rda_dir,
        year = end_year
      )
    }

    # add more key quantities included as arguments in this fxn
    add_more_key_quants(
      dat,
      topic = topic_label,
      fig_or_table = fig_or_table,
      dir = rda_dir,
      end_year = end_year,
      units = unit_label,
      ref_pt = ref_point,
      ref_line = ref_line,
      scaling = scale_amount
    )

    # extract this plot's caption and alt text
    caps_alttext <- extract_caps_alttext(
      topic_label = topic_label,
      fig_or_table = fig_or_table,
      dir = rda_dir
    )

    export_rda(
      final = final,
      caps_alttext = caps_alttext,
      rda_dir = rda_dir,
      topic_label = topic_label,
      fig_or_table = fig_or_table
    )
  }
  return(final)
}
