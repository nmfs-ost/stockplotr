#' Plot Index of Abundance
#'
#' @inheritParams plot_recruitment
#' @param unit_label units for index of abundance/CPUE
#'
#' @return Plot the estimated indices as indicated from a standard assessment
#' model output file.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_indices(dat)
#'
#' plot_indices(
#'   dat,
#'   unit_label = "my_unit",
#'   end_year = 2024,
#'   make_rda = TRUE,
#'   figures_dir = getwd()
#' )
#' }
plot_indices <- function(
    dat,
    unit_label = "kg/hr",
    end_year = NULL,
    make_rda = FALSE,
    figures_dir = NULL) {
  # Set cpue unit label for plot
  u_units <- glue::glue("Estimated CPUE ({unit_label})")

  # get end year if not defined
  if (is.null(end_year)) {
    end_year <- format(Sys.Date(), "%Y")
  }

  # Load data
  output <- dat |>
    dplyr::filter(module_name == "INDEX_2" | module_name == "t.series")
  # Check for U
  if (any(unique(output$module_name == "INDEX_2"))) {
    output <- output |>
      dplyr::filter(grepl("expected_indices", label)) # grepl("input_indices", label) |
  } else if (any(unique(output$module_name == "t.series"))) {
    output <- output |>
      dplyr::filter(grepl("cpue", label))
  }
  # Extract fleet names
  fleet_names <- unique(as.character(output$fleet))
  factors <- c("year", "fleet", "fleet_name", "age", "sex", "area", "seas", "season", "time", "era", "subseas", "subseason", "platoon", "platoo", "growth_pattern", "gp")
  # re-structure df for table
  indices <- output |>
    tidyr::pivot_wider(
      # id_cols = c(year, uncertainty, uncertainty_label),
      names_from = label,
      values_from = estimate
    ) |>
    dplyr::select(year, fleet, unique(output$label), uncertainty, uncertainty_label) # |>

  # na.omit()
  # check if uncertainty is a measure in the df
  if (all(is.na(indices$uncertainty))) {
    indices <- indices |>
      dplyr::select(-c(uncertainty_label, uncertainty))
  } else {
    uncertainty_col <- paste("uncertainty_", unique(indices$uncertainty_label), sep = "")
    colnames(indices) <- stringr::str_replace(colnames(indices), "^uncertainty$", uncertainty_col)
    indices <- dplyr::select(indices, -uncertainty_label)
  }

  # Check if observed/inital values are in the df
  if (any(grepl("observed", colnames(indices)))) {
    indices <- indices |>
      dplyr::select(-colnames(indices)[grep(c("observed"), colnames(indices))])
  }

  # rename columns to remove cpue/effort
  if (any(grep("_indices", colnames(indices)))) {
    colnames(indices) <- stringr::str_replace_all(colnames(indices), "_indices", "")
  } else {
    colnames(indices) <- stringr::str_replace_all(colnames(indices), "cpue_", "")
  }

  # Check for which column is U and filter out na values
  if (any(grep("predicted", colnames(indices)))) {
    indices <- indices |>
      dplyr::filter(!is.na(predicted)) # |>
    # dplyr::rename(estimated = predicted)
  }
  if (any(grep("expected", colnames(indices)))) {
    indices <- indices |>
      dplyr::filter(!is.na(expected)) # |>
    # dplyr::rename(estimated = predicted)
  }

  # Check for correct number of columns in dataframe
  if (4 < ncol(indices) | ncol(indices) > 4) cli::cli_abort("Incorrect number of columns. Additional factor present.", wrap = TRUE)
  # Check for error type to present to user
  # Double check this warning is correct
  # if (grep("cv", colnames(indices)[4])) cli::cli_alert_warning("Confidence Intervals were calculated using cv rather than se. Please use caution interpreting of the output plot.", wrap = TRUE)
  # Create condition for error type
  if (grepl("cv", colnames(indices)[4])) {
    err_val <- TRUE # indicates that the error value is cv
  } else {
    err_val <- FALSE # indicated that the error value is se
  }

  # Rename column names for easier plotting
  # This will break if columns are not in the correct order - add check?
  colnames(indices) <- c("year", "fleet", "estimate", "uncertainty")

  # Final data set for plotting
  indices2 <- indices |>
    dplyr::mutate(
      estimate_lower = dplyr::case_when(
        err_val ~ estimate - ((estimate * uncertainty) * 1.96),
        TRUE ~ (estimate - 1.96 * uncertainty)
      ),
      estimate_upper = dplyr::case_when(
        err_val ~ estimate + ((estimate * uncertainty) * 1.96),
        TRUE ~ (estimate + 1.96 * uncertainty)
      ),
      fleet = as.character(fleet),
      year = as.numeric(year)
    ) |>
    dplyr::filter(year <= as.numeric(end_year))


  topic_label <- "CPUE.indices"

  # identify output
  fig_or_table <- "figure"

  # check year isn't past end_year if not projections plot
  check_year(
    end_year = end_year,
    fig_or_table = fig_or_table,
    topic = topic_label
  )

  # create plot
  plt <- ggplot2::ggplot(data = indices2) +
    ggplot2::geom_line(ggplot2::aes(x = year, y = estimate), linewidth = 1) +
    ggplot2::geom_ribbon(
      data = indices2 |> dplyr::filter(!is.na(estimate_lower)),
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
      y = u_units
    ) +
    ggplot2::facet_wrap(~fleet) # +
  # ggplot2::scale_x_continuous(
  #   n.breaks = x_n_breaks,
  #   guide = ggplot2::guide_axis(minor.ticks = TRUE)
  # )

  final <- suppressWarnings(add_theme(plt))

  if (make_rda) {
    # create plot-specific variables to use throughout fxn for naming and IDing

    # run write_captions.R if its output doesn't exist
    if (!file.exists(
      fs::path(getwd(), "captions_alt_text.csv")
    )
    ) {
      stockplotr::write_captions(
        dat = dat,
        dir = figures_dir,
        year = end_year
      )
    }

    # add more key quantities included as arguments in this fxn
    add_more_key_quants(
      topic = topic_label,
      fig_or_table = fig_or_table,
      end_year = end_year,
      dir = figures_dir,
      units = unit_label
    )

    # extract this plot's caption and alt text
    caps_alttext <- extract_caps_alttext(
      topic_label = topic_label,
      fig_or_table = fig_or_table,
      dir = figures_dir
    )

    export_rda(
      final = final,
      caps_alttext = caps_alttext,
      figures_tables_dir = figures_dir,
      topic_label = topic_label,
      fig_or_table = fig_or_table
    )
  }
  return(final)
}
