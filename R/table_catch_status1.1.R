#' Create Catch and Status Trend Table
#'
#' @param dat A standardized asar_stock object.
#' @param n_years Numeric; number of recent years to display (default is 7).
#' @param stock_name Character; display name for the stock.
#' @param caption Character; table subtitle/caption.
#' @param label Character; internal ID for the table.
#'
#' @return A formatted gt table object.
#' @export
table_catch_status <- function(dat, 
                               n_years       = 7, 
                               stock_name    = attr(dat, "metadata")$spp_name,
                               caption       = attr(dat, "metadata")$cap_status,
                               label         = "Catch_Status_Table") {
  
  meta <- attr(dat, "metadata")
  is_latex <- !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to")) && 
    knitr::opts_knit$get("rmarkdown.pandoc.to") == "latex"
  
  # Null-safe labels
  ssb_lab  <- meta$mod_ssb_name %||% "Spawning Stock Biomass"
  f_lab    <- meta$mod_f_name   %||% "Fishing Mortality"
  rec_lab  <- meta$mod_recr_name %||% "Recruitment"
  
  terminal_year <- max(dat$year[dat$module_name %in% c("model_results", "catch")], na.rm = TRUE)
  year_range    <- (terminal_year - n_years + 1):terminal_year
  
  model_metrics <- dat |>
    dplyr::filter(module_name == "model_results", year %in% year_range) |>
    dplyr::mutate(label = dplyr::case_when(
      label == "biomass" ~ ssb_lab,
      label == "fishing_mortality" ~ f_lab,
      label == "recruitment" ~ rec_lab,
      TRUE ~ label
    )) |>
    dplyr::select(year, label, estimate)
  
  catch_metrics <- dat |>
    dplyr::filter(module_name == "catch", year %in% year_range) |>
    dplyr::select(year, fleet, estimate) |>
    dplyr::rename(label = fleet)
  
  df_wide <- dplyr::bind_rows(catch_metrics, model_metrics) |>
    dplyr::mutate(label_chr = as.character(label)) |>
    dplyr::mutate(Metric = if (is_latex) to_latex_caption(label_chr) else clean_assessment_latex(label_chr)) |>
    dplyr::mutate(Metric = dplyr::if_else(is.na(Metric) | Metric == "", label_chr, Metric)) |>
    dplyr::select(Metric, year, estimate) |>
    dplyr::distinct(Metric, year, .keep_all = TRUE) |>
    tidyr::pivot_wider(names_from = year, values_from = estimate)
  
  final_table <- df_wide |>
    gt::gt() |>
    gt::fmt_number(
      columns = where(is.numeric),
      rows = grepl("ratio|&frasl;|~|/|R0", Metric, ignore.case = TRUE),
      decimals = 3,
      use_seps = FALSE
    ) |>
    gt::fmt_number(
      columns = where(is.numeric),
      rows = !grepl("ratio|&frasl;|~|/|R0", Metric, ignore.case = TRUE),
      decimals = 0,
      use_seps = TRUE
    ) |>
    gt::fmt_markdown(columns = Metric) |>
    gt::cols_label(Metric = "") |>
    
    gt::tab_header(
      title = if (is_latex) NULL else stock_name,
      subtitle = if (is_latex) NULL else gt::html(clean_assessment_latex(caption))
    ) |>
    gt::opt_row_striping() |>
    gt::tab_options(
      table.font.size = if (is_latex) gt::px(12) else gt::px(14),
      data_row.padding = gt::px(4),
      table.width = if (is_latex) gt::pct(100) else NULL
    )
  
  return(final_table)
}