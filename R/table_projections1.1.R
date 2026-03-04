#' Create Projection Summary Table
#'
#' Generates a table showing assumed and projected years of Catch, SSB, and F.
#'
#' @inheritParams plot_spawning_biomass
#'
#' @return A formatted gt table object.
#' @export
#' @examples
#' \dontrun{
#' # add example here
#' }
table_projections <- function(dat) {
  
  #TODO: Identify how to extract future data from data
  # - use era? Allow user to set years? etc.
  #TODO: Add thanks to Dan somewhere
  
  # meta <- attr(dat, "metadata")
  # is_latex <- !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to")) && 
  #   knitr::opts_knit$get("rmarkdown.pandoc.to") == "latex"
  
  # Dynamic F Label logic
  # legacy_f_label <- meta$mod_f_name %||% "F"
  # f_display_label <- if (is_latex) to_latex_caption(legacy_f_label) else clean_assessment_latex(legacy_f_label)
  
  # TODO: Replace this with our existing infrastructure/pipeline with filter_data(), etc.
  # TODO: use purrr to iterate through 3 label_names: SB, fishing_mortality, and catch
  prepared_data1 <- filter_data(
    dat = dat,
    label_name = "spawning_biomass$",
    geom = "line",
    era = "fore",
    module = module,
    scale_amount = 1,
    interactive = interactive
  ) |>
    dplyr::mutate(estimate = round(as.numeric(estimate), digits = 0)) |>
    dplyr::mutate(uncertainty = round(as.numeric(uncertainty), digits = 2))

  
  # TODO: add purrr as a dependency (if not present already)
  # TODO: edit process_table() to avoid losing year when summarizing; right now, it's only examining index vars
  # TODO: add 'method' option for 'distinct'
  test <- process_table(prepared_data1,
                        group = "none",
                        method = "mean")
  
  # this is what our processing should achieve
  # proj_raw <- dat |> dplyr::filter(module_name == "projections")
  # proj_wide <- proj_raw |>
  #   dplyr::select(year, label, estimate_chr) |>
  #   dplyr::distinct(year, label, .keep_all = TRUE) |>
  # TODO: pivot names with a setup like the code below (pivot_wider): year is a col, then labels are individual cols
    # tidyr::pivot_wider(names_from = label,
    #                    values_from = estimate_chr)
  
  # proj_data <- proj_wide |>
  #   dplyr::mutate(Period = dplyr::case_when(
  #     year == min(year, na.rm = TRUE) ~ "Assumed", 
  #     TRUE ~ "Projected")) |>
  #   dplyr::select(Period, Year = year, `Catch (mt)` = total_catch, `SSB (mt)` = biomass, F_VAL = fishing_mortality)
  
  # Build table
  final_table <- proj_data |>
    gt::gt(groupname_col = "Period", id = label) |>
    gt::cols_align(align = "right", columns = dplyr::everything()) |>
    gt::cols_align(align = "left", columns = Year) |>
    gt::cols_label(F_VAL = if (is_latex) gt::md(f_display_label) else gt::html(f_display_label)) |>
    
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