#' Create Projection Summary Table
#'
#' Generates a table showing assumed and projected years of Catch, SB, and F.
#'
#' @inheritParams table_landings
#'
#' @return A formatted gt table object.
#' @details We would like to thank Dan Hennen for sharing his projections table
#' function code, which served as the foundation for this function.
#' 
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
  
  # iterate through 3 label_names
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
      # process to reduce columns
      processed_data <- process_table(filtered_data,
                                      digits = ifelse(x == "fishing_mortality", 5, 2))
      data <- processed_data[[1]]
      group <- processed_data[[2]]
      # merge data with uncertainty and unit labels
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
  
  # export figure to rda if argument = T
  if (make_rda == TRUE) {
      # No key quantities for captions/alt text since only values
      # are units for catch, SB, and F, which are specified for
      # the main figures/tables for each
      # landings.units <- unit_label
     
      create_rda(
        object = final$label,
        # get name of function and remove "table_" from it
        topic_label = gsub("table_", "", tail(as.character(sys.call()[[1]]), n = 1)),
        fig_or_table = "table",
        dat = dat,
        dir = tables_dir,
        scale_amount = 1,
        unit_label = unit_label,
        table_df = final
      )
    }

    return(final)
}