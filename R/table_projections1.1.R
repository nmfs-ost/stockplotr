#' Create Projection Summary Table
#'
#' Generates a table showing assumed and projected years of Catch, SSB, and F.
#' This version dynamically maps the fishing mortality label and includes
#' NULL-safety for metadata attributes.
#'
#' @param dat A standardized asar_stock object.
#' @param stock_name Character; display name for the stock.
#' @param caption Character; table subtitle/caption.
#' @param label Character; internal ID for the table.
#'
#' @return A formatted gt table object.
#' @export
table_projections <- function(dat, 
                              stock_name    = attr(dat, "metadata")$spp_name,
                              caption       = attr(dat, "metadata")$cap_proj,
                              label         = "Projection_Table") {
  
  meta <- attr(dat, "metadata")
  is_latex <- !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to")) && 
    knitr::opts_knit$get("rmarkdown.pandoc.to") == "latex"
  
  # Dynamic F Label logic
  legacy_f_label <- meta$mod_f_name %||% "F"
  f_display_label <- if (is_latex) to_latex_caption(legacy_f_label) else clean_assessment_latex(legacy_f_label)
  
  # Data processing
  proj_raw <- dat |> dplyr::filter(module_name == "projections")
  proj_wide <- proj_raw |>
    dplyr::select(year, label, estimate_chr) |>
    dplyr::distinct(year, label, .keep_all = TRUE) |>
    tidyr::pivot_wider(names_from = label, values_from = estimate_chr)
  
  proj_data <- proj_wide |>
    dplyr::mutate(Period = dplyr::case_when(year == min(year, na.rm = TRUE) ~ "Assumed", TRUE ~ "Projected")) |>
    dplyr::select(Period, Year = year, `Catch (mt)` = total_catch, `SSB (mt)` = biomass, F_VAL = fishing_mortality)
  
  # Build table
  final_table <- proj_data |>
    gt::gt(groupname_col = "Period", id = label) |>
    gt::cols_align(align = "right", columns = dplyr::everything()) |>
    gt::cols_align(align = "left", columns = Year) |>
    gt::cols_label(F_VAL = if (is_latex) gt::md(f_display_label) else gt::html(f_display_label)) |>
    
    # Titling: Only used for HTML; suppressed for LaTeX to avoid double-titles
    gt::tab_header(
      title = if (is_latex) NULL else stock_name,
      subtitle = if (is_latex) NULL else gt::html(clean_assessment_latex(caption))
    ) |>
    
    gt::opt_row_striping() |>
    gt::tab_options(
      #table.width = if (is_latex) gt::pct(100) else gt::px(700),
      table.font.size = if (is_latex) gt::px(12) else gt::px(14),
      # Use this instead to keep the table compact
      table.width = if (is_latex) NULL else gt::pct(100),
      # Ensure data rows aren't overly tall
      data_row.padding = gt::px(4),
      row_group.font.weight = "bold",
      row_group.background.color = "#eeeeee"
    )
  
  return(final_table)
}