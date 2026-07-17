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
      processed_data <- process_table(filtered_data,
                                      digits = ifelse(x == "fishing_mortality", 5, 2))
      data <- processed_data[[1]]
      group <- processed_data[[2]]
      # TODO: extract uncert lab from uncert col in processed_data
      # final df to merge
      merge_error(
        table_data = data,
        id_col_vals = group,
        unit_label = unit_label[grepl(x, names(unit_label))][[1]],
        uncert_lab = uncertainty[grepl(x, names(uncertainty))][[1]]
      )[[1]]
    }
  )
  
  combine_data <- purrr::reduce(lab_list, dplyr::full_join, by = c("Year")) |>
    gt::gt()
  
  final_table <- add_theme(combine_data)
  
  # TODO: fix issue with missing parentheses around unit label in table
  # TODO: add step for exporting as rda
  
  
  return(final_table)
}