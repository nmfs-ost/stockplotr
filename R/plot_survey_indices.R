#' Plot Survey Abundance Indices
#'
#' @param dat A standardized asar_stock object containing survey module data.
#' @param unit_label Character string for the Y-axis units (e.g., "kg", "mt").
#' @param geom Character string defining the primary geometry type for stockplotr filtering.
#' @param group Column name used to group/color the surveys (default is "fleet").
#' @param facet Column name used to panel the plot (default is "fleet").
#' @param era Filter for specific time periods (e.g., "time", "fore").
#' @param scale_amount Numeric multiplier for scaling Y-axis values (e.g., 1000).
#' @param module The data module to extract. Default is "survey".
#' @param interactive Logical; if TRUE, prepares data for plotly compatibility.
#' @param make_rda Logical; if TRUE, saves plot and data as an .rda file.
#' @param figures_dir Character string of the directory path for saving .rda files.
#' @param show_line Logical; toggles the interpolation line between survey points.
#' @param line_type Character string defining the style of the interpolation line.
#' @param ... Additional arguments passed to stockplotr::filter_data.
#'
#' @return A ggplot2 object with adaptive uncertainty layers (ribbons for annual data, 
#'         error bars for sparse data).
#' @export
plot_survey_indices <- function(dat, 
                                unit_label = "kg", 
                                geom = "line", 
                                group = "fleet", 
                                facet = "fleet", 
                                era = NULL, 
                                scale_amount = 1, 
                                module = "survey", 
                                interactive = FALSE, 
                                make_rda = FALSE, 
                                figures_dir = getwd(), 
                                show_line = TRUE,      
                                line_type = "solid",   
                                ...) {
  
  # Filter data for 'index' labels using stockplotr core
  prepared_data <- stockplotr::filter_data(
    dat = dat, 
    label_name = "index", 
    geom = geom, 
    era = era, 
    group = group, 
    facet = facet, 
    module = module, 
    scale_amount = scale_amount, 
    interactive = interactive
  )
  
  if (nrow(prepared_data) == 0) {
    stop("No data found for label 'index' in the survey module.")
  }
  
  # Prepare data and determine sampling continuity per fleet
  p_dat_final <- prepared_data |>
    dplyr::mutate(
      year = as.numeric(year),
      estimate = as.numeric(estimate),
      cv_val = as.numeric(uncertainty)
    ) |>
    dplyr::filter(!is.na(estimate)) |> 
    dplyr::group_by(!!ggplot2::sym(group)) |>
    dplyr::mutate(
      # Identify if survey is annual (gap <= 1) or sparse (gap > 1)
      max_gap = max(diff(sort(year)), na.rm = TRUE),
      is_consecutive = max_gap <= 1,
      has_cv = !is.na(cv_val) & cv_val > 0,
      sdlog = ifelse(has_cv, sqrt(log(cv_val^2 + 1)), NA),
      lci = ifelse(has_cv, estimate * exp(-1.645 * sdlog), NA),
      uci = ifelse(has_cv, estimate * exp(1.645 * sdlog), NA),
      group_var = !!ggplot2::sym(group)
    ) |>
    dplyr::ungroup()
  
  # Initialize plot
  plt <- ggplot2::ggplot(p_dat_final, ggplot2::aes(x = year, y = estimate))
  
  # Shaded ribbons for consecutive (annual) data
  plt <- plt + ggplot2::geom_ribbon(
    data = subset(p_dat_final, is_consecutive == TRUE),
    ggplot2::aes(ymin = lci, ymax = uci, fill = group_var), 
    alpha = 0.3, 
    na.rm = TRUE
  )
  
  # Discrete error bars for sparse (periodic) data
  plt <- plt + ggplot2::geom_errorbar(
    data = subset(p_dat_final, is_consecutive == FALSE),
    ggplot2::aes(ymin = lci, ymax = uci, color = group_var), 
    width = 0.5, 
    alpha = 0.8,
    na.rm = TRUE
  )
  
  # Conditional interpolation line
  if (show_line) {
    plt <- plt + 
      ggplot2::geom_line(
        ggplot2::aes(color = group_var, linetype = is_consecutive), 
        linewidth = 0.8,
        alpha = 0.6
      ) +
      # Use solid for annual, dashed for sparse to visually signal interpolation
      ggplot2::scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed"), guide = "none")
  }
  
  # Plot actual observed points
  plt <- plt + ggplot2::geom_point(ggplot2::aes(color = group_var), size = 2)
  
  # Faceting logic
  if (!is.null(facet)) {
    plt <- plt + ggplot2::facet_wrap(
      ggplot2::vars(!!ggplot2::sym(facet)), 
      scales = "free_y", 
      ncol = 2
    )
  }
  
  # Final styling and metadata attribution
  plt <- plt + 
    stockplotr::theme_noaa() + 
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.1)), 
      limits = c(0, NA)
    ) +
    ggplot2::labs(
      y = paste0("Index (", unit_label, ")"), 
      x = "Year",
      color = "Survey",
      fill = "Survey",
      #caption = attr(dat, "metadata")$cap_surv
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      #plot.caption = ggplot2::element_text(hjust = 0, size = 8)
    )
  
  return(plt)
}