#' Plot Total Biomass
#'
#' @inheritParams plot_recruitment
#' @param ref_line A string specifying the type of reference you want to
#'   compare biomass to. The default is `"target"`, which looks for
#'   `"biomass_target"` in the `"label"` column of `dat`. The actual
#'   searching in `dat` is case agnostic and will work with either upper- or
#'   lower-case letters but you must use one of the options specified in the
#'   default list to ensure that the label on the figure looks correct
#'   regardless of how it is specified in `dat`.
#' @param ref_point A known value of the reference point along with the label
#'   for the reference point as specified in the output file. Please use this
#'   option if the ref_line cannot find your desired point. Indicate the
#'   reference point in the form c("label" = value).
#' @return Plot total biomass from a stock assessment model as found in a NOAA
#' stock assessment report. Units of total biomass can either be manually added
#' or will be extracted from the provided file if possible. In later releases, model will not
#' @export
#'
plot_biomass <- function(
    dat,
    unit_label = "metric tons",
    scale_amount = 1,
    ref_line = c("target", "MSY", "msy", "unfished"),
    ref_point = NULL,
    end_year = NULL,
    relative = FALSE,
    make_rda = FALSE,
    rda_dir = getwd()
){

  if (!is.null(ref_point)) {
    ref_line <- names(ref_point)
  } else if(length(ref_line)>1){
    ref_line <- "target"
  } else {
    ref_line <- match.arg(ref_line, several.ok = FALSE)
  }
  # TODO: fix unit label if scaled
  biomass_label <- ifelse(
    relative,
    yes = "Relative Biomass (B/Btarg)",
    no = glue::glue("Biomass ({unit_label})")
  )

  # Select value for reference line and label
  # update the target option later
  # TODO: add option to indicate the reference pt
  if (!is.null(ref_point)) {
    ref_line_val <- as.numeric(ref_point)
  } else {
    if ( inherits( try( solve(as.numeric(dat[
      grep(
        pattern = glue::glue("^biomass.*{tolower(ref_line)}$"),
        x = dat[["label"]]
      ),
      "estimate"
    ])), silent = TRUE), "try-error")) {
      ref_line_val <- NULL
    } else {
      ref_line_val <- as.numeric(dat[
        grep(
          pattern = glue::glue("^biomass.*{tolower(ref_line)}$"),
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
    warning(glue::glue(
      "The resulting reference value of `biomass_{ref_line}` was
      not found in `dat[[\"label\"]]`."
    ))
    warning("Reference line will not be plotted on the figure.")
  } else if (length(ref_line_val) > 1) {
    warning(glue::glue(
      "More than one of the resulting reference value of 'biomass_{ref_line}` was
      not in `dat[[\"label\"]]`. \n Both reference points will be plotted on the figure."
    ))
  }

  b <- dat |>
    dplyr::filter(label == "biomass",
                  module_name == "TIME_SERIES" | module_name == "t.series", # SS3 and BAM target module names
                  is.na(fleet),
                  is.na(age)) |>
    dplyr::mutate(estimate = as.numeric(estimate),
                  estimate_y = estimate / ifelse(relative, ref_line_val, scale_amount),
                  year = as.numeric(year),
                  estimate_lower = estimate - 1.96 * uncertainty /
                    ifelse(relative, ref_line_val, scale_amount),
                  estimate_upper = estimate + 1.96 * uncertainty /
                    ifelse(relative, ref_line_val, scale_amount))

  # get end year if not defined
  if (is.null(end_year)){
    end_year <- max(b$year)
  }

  # Choose number of breaks for x-axis
  x_n_breaks <- round(length(b[["year"]]) / 10)
  if (x_n_breaks <= 5) {
    x_n_breaks <- round(length(b[["year"]]) / 5)
  } else if (x_n_breaks > 10){
    x_n_breaks <- round(length(b[["year"]]) / 15)
  }

  # plot
  plt <- ggplot2::ggplot(data = subset(b, year<=end_year)) +
    ggplot2::geom_line(
      ggplot2::aes(
        x = year,
        y = estimate_y
      ),
      linewidth = 1) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        x = year,
        ymin = estimate_lower,
        ymax = estimate_upper),
      colour = "grey",
      alpha = 0.3) +
    {if(!is.null(ref_line_val)) ggplot2::geom_hline(yintercept = ref_line_val / ifelse(relative, ref_line_val, scale_amount),linetype = 2)} +
    ggplot2::labs(
      x = "Year",
      y = biomass_label) +
    ggplot2::scale_x_continuous(
      n.breaks = x_n_breaks,
      guide = ggplot2::guide_axis(minor.ticks = TRUE)) +
    ggplot2::annotate(
      geom = "text",
      x = end_year + 0.05,
      y = ref_line_val / ifelse(relative, ref_line_val, scale_amount),
      label = list(bquote(B[.(ref_line)])),
      parse = TRUE,
      fill = "white"
    )
    # ggtext::geom_richtext(
    #   ggplot2::aes(
    #     x = end_year,
    #     y = ref_line_val / ifelse(relative, ref_line_val, scale_amount)
    #     ),
    #   nudge_x = 0.05,
    #   nudge_y = 10,
    #   label.padding = 0.5,
    #   label.margin = 3
    # )

  final <- add_theme(plt)

  # export figure to rda if argument = T
  if (make_rda == TRUE){
    # create plot-specific variables to use throughout fxn for naming and IDing
    # Indicate if biomass is relative or not
    if (relative) {
      topic_label <- "relative.biomass"
    } else {
      topic_label <- "biomass"
    }

    # identify output
    fig_or_table <- "figure"

    # run write_captions.R if its output doesn't exist
    if (!file.exists(
      fs::path(getwd(), "captions_alt_text.csv"))
    ) {
      stockplotr::write_captions(dat = dat,
                           dir = rda_dir,
                           year = end_year)
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
      scaling = scale_amount
    )

    # extract this plot's caption and alt text
    caps_alttext <- extract_caps_alttext(topic_label = topic_label,
                                         fig_or_table = fig_or_table,
                                         dir = rda_dir)

    export_rda(final = final,
               caps_alttext = caps_alttext,
               rda_dir = rda_dir,
               topic_label = topic_label,
               fig_or_table = fig_or_table)

    }
  return(final)
}
