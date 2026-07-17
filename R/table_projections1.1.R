#' Create Projection Summary Table
#'
#' Generates a table showing assumed and projected years of Catch, SB, and F.
#'
#' @inheritParams table_landings
#'
#' @return A formatted gt table object.
#' @export
#' @examples
#' \dontrun{
#' # add example here
#' }
table_projections <- function(
    dat,
    unit_label = c("catch" = "mt", "spawning_biomass" = "mt", "fishing_mortality" = ""),
    uncertainty = c("catch" = "stddev", "spawning_biomass" = "stddev", "fishing_mortality" = "stddev"),
    interactive = TRUE,
    module = NULL,
    make_rda = FALSE,
    tables_dir = getwd()
    ) {
  
  #TODO: Add thanks to Dan somewhere
  
  # meta <- attr(dat, "metadata")
  # is_latex <- !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to")) && 
  #   knitr::opts_knit$get("rmarkdown.pandoc.to") == "latex"
  
  # Dynamic F Label logic
  # legacy_f_label <- meta$mod_f_name %||% "F"
  # f_display_label <- if (is_latex) to_latex_caption(legacy_f_label) else clean_assessment_latex(legacy_f_label)
  
  # TODO: use purrr to iterate through 3 label_names: SB, fishing_mortality, and catch
  # have info message for each iteration to know which module you are selecting for
  lab_list <- purrr::map(
    c("catch", "spawning_biomass", "fishing_mortality"),
    function(x) {
      cli::cli_alert_info(paste0("Processing ", x))
      filtered_data <- filter_data(
        dat = dat,
        label_name = paste0(x, "$"),
        geom = "line",
        era = "fore",
        module = module,
        scale_amount = 1,
        interactive = interactive
      ) |>
        dplyr::mutate(estimate = round(as.numeric(estimate), digits = 0)) |>
        dplyr::mutate(uncertainty = round(as.numeric(uncertainty), digits = 2))
      # process to reduce colums
      # processed_data <- 
      process_table(filtered_data)
      # data <- processed_data[[1]][[1]]
      # group <- processed_data[[2]]
    }
  )
  
  # TODO: extract uncert lab from uncert col in processed_data
  # final df to merge
  final <- merge_error(
    table_data = lab_list,
    id_col_vals = group,
    unit_label = unit_label[grepl(x, names(unit_label))][[1]],
    uncert_lab = uncertainty[grepl(x, names(uncertainty))][[1]]
  )
  # TODO: cbind columns with year/matching groups
  # TODO: add to gt and add_theme
  # TODO: add step for exporting as rda
  
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
  final_table <- test |>
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