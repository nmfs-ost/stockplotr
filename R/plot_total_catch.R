#' Plot Total Catch by Fleet
#'
#' @param dat A standardized asar_stock object containing catch module data.
#' @param unit_label Character string for the Y-axis units (e.g., "metric tons", "kg").
#' @param type Character string defining the plot type: "line" (default), "bar", or "histogram".
#' @param group Column name used to group/color the data (default is "fleet").
#' @param facet Column name used to panel the plot (default is NULL).
#' @param era Filter for specific time periods (e.g., "time", "fore").
#' @param scale_amount Numeric multiplier for scaling Y-axis values (e.g., 1000).
#' @param module The data module to extract. Default is "catch".
#' @param interactive Logical; if TRUE, prepares data for plotly compatibility.
#' @param make_rda Logical; if TRUE, saves plot and data as an .rda file.
#' @param figures_dir Character string of the directory path for saving .rda files.
#' @param ... Additional arguments passed to stockplotr::filter_data.
#'
#' @return A ggplot2 object showing total catch. If type is "bar", the function 
#'         automatically filters out 'Total' fleet rows to ensure proper stacking.
#' @export
plot_total_catch <- function(dat, 
                             unit_label = "metric tons", 
                             type = "line", 
                             group = "fleet", 
                             facet = NULL, 
                             era = NULL, 
                             scale_amount = 1, 
                             module = "catch", 
                             interactive = FALSE, 
                             make_rda = FALSE, 
                             figures_dir = getwd(), 
                             ...) {
  
  # Determine magnitude label based on scale_amount
  magnitude_text <- if (scale_amount == 1000) {
    " (000s "
  } else if (scale_amount == 1e+06) {
    " (millions "
  } else {
    " ("
  }
  
  catch_label <- paste0("Total Catch", magnitude_text, unit_label, ")")
  
  # Filter data using stockplotr core logic
  prepared_data <- stockplotr::filter_data(
    dat = dat, 
    label_name = "total_catch", 
    geom = "line", 
    era = era, 
    group = group, 
    facet = facet, 
    module = module, 
    scale_amount = scale_amount, 
    interactive = interactive
  )
  
  # Ensure numeric types for plotting
  prepared_data$year <- as.numeric(prepared_data$year)
  prepared_data$estimate <- as.numeric(prepared_data$estimate)
  
  # Logic to prevent double-counting in stacked bars
  if (type %in% c("bar", "histogram")) {
    prepared_data <- prepared_data |> 
      dplyr::filter(!tolower(fleet) %in% c("total", "all"))
  }
  
  # Process and aggregate data through stockplotr
  processed_data <- stockplotr::process_data(
    dat = prepared_data, 
    group = group, 
    facet = facet, 
    method = "sum"
  )
  
  p_dat <- processed_data[[1]]
  g_var <- processed_data[[2]]
  f_var <- processed_data[[3]]
  
  plt <- ggplot2::ggplot(p_dat, ggplot2::aes(x = year, y = estimate))
  
  # Render appropriate geometry based on type
  if (type %in% c("bar", "histogram")) {
    plt <- plt + 
      ggplot2::geom_col(ggplot2::aes(fill = !!ggplot2::sym(g_var)), position = "stack") +
      ggplot2::labs(fill = "Fleet")
  } else {
    plt <- plt + 
      ggplot2::geom_line(ggplot2::aes(color = !!ggplot2::sym(g_var)), linewidth = 1) +
      ggplot2::labs(color = "Fleet")
  }
  
  # Faceting if specified
  if (!is.null(f_var)) {
    plt <- plt + ggplot2::facet_wrap(ggplot2::vars(!!ggplot2::sym(f_var)))
  }
  
  # Final styling
  plt <- plt + 
    stockplotr::theme_noaa() + 
    ggplot2::labs(y = catch_label, x = "Year")
  
  # Remove legend if only one group exists
  if (length(unique(p_dat[[g_var]])) == 1) {
    plt <- plt + ggplot2::theme(legend.position = "none")
  }
  
  # Artifact generation
  if (make_rda) {
    stockplotr::create_rda(
      object = plt, topic_label = "total_catch", fig_or_table = "figure", 
      dat = dat, dir = figures_dir, scale_amount = scale_amount, unit_label = unit_label
    )
  }
  
  return(plt)
}