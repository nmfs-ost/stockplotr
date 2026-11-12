#' Create Projection Summary Table
#'
#' Generates a table showing assumed and projected years of Catch, SB, and F.
#'
#' @inheritParams table_landings
#'
#' @param catch_label String. Units for catch
#'
#' Default: NULL
#'
#' @param spawning_biomass_label String. Units for spawning biomass
#'
#' Default: NULL
#'
#' @return A formatted gt table object.
#' @details We would like to thank Dan Hennen for sharing his projections table
#' function code, which served as the foundation for this function.
#'
#' @export
#' @examples
#' table_projections(
#'   dat = stockplotr::example_data,
#'   interactive = FALSE,
#'   module = "DERIVED_QUANTITIES"
#' )
#' 
#' table_projections(
#'  dat = stockplotr::example_data,
#'  interactive = FALSE,
#'  catch_label = "kg",
#'  spawning_biomass_label = "kg",
#'  module = c(NULL, "DERIVED_QUANTITIES", "DERIVED_QUANTITIES")
#'  )
table_projections <- function(
  dat,
  catch_label = NULL,
  spawning_biomass_label = NULL,
  interactive = TRUE,
  module = NULL,
  make_rda = FALSE,
  tables_dir = getwd()
) {
  check_data(dat)

  # check if catch in data else landings
  catch_lab <- ifelse(
    any(grepl("catch$", dat$label)),
    "catch",
    "landings_weight"
  )
  # # change default unit and uncert labs when catch is changed to landings
  # if ("landings" %notin% names(catch_lab)) {
  #   catch_label <- c(catch_label, "landings_weight" = catch_label[["catch"]])
  # }
  
  named_vec <- c(
    "catch" = catch_lab,
    "spawning_biomass" = "spawning_biomass",
    "fishing_mortality" = "fishing_mortality"
  )
  
  # iterate through 3 label names
  lab_list <- purrr::map(
    names(named_vec),
    function(x) {
      cli::cli_alert_info(paste0("Processing ", x))
      filtered_data <- filter_data(
        dat = dat,
        label_name = paste0("^", x, "$"),
        geom = "line",
        era = "fore",
        module = module,
        scale_amount = 1,
        interactive = interactive
      ) |>
        dplyr::mutate(estimate = ifelse(x != "fishing_mortality", round(as.numeric(estimate), digits = 0), round(as.numeric(estimate), digits = 4))) |>
        dplyr::mutate(uncertainty = round(as.numeric(uncertainty), digits = 2))
      uncertainty_label <- ifelse(
        is.na(unique(filtered_data$uncertainty_label)),
        "Uncertainty",
        unique(filtered_data$uncertainty_label)
      )
      extra_grouping <- check_grouping(filtered_data)[-grep("year|age", check_grouping(filtered_data))]
      if (length(extra_grouping) > 1) {
        cli::cli_abort(
          "Table not created due to {extra_grouping} variables in {x}."
        )
        # return() # uncomment if requested to return smaller table
      }
      # process to reduce columns
      processed_data <- process_table(
        filtered_data,
        digits = ifelse(x == "fishing_mortality", 5, 2)
      )
      data <- processed_data[[1]]
      group <- processed_data[[2]]

      # Labs
      # get unit labels if not NULL
      unit_name <- named_vec[grep(x, names(named_vec), fixed = TRUE)][[1]]
      
      if (x == "fishing_mortality") {
        fishing_mortality_label <- ""
        unit_label <- fishing_mortality_label
        unit_name <- "fishing_mortality"
      } else if (is.null(get(paste0(unit_name, "_label")))) {
        unit_label <- filtered_data |>
          dplyr::filter(grepl(x, unit_name),
                        !is.na(unit)) |>
          dplyr::pull(unit) |>
          unique()
        if (is.na(unit_label) || length(unit_label) == 0) {
          cli::cli_alert_warning("No unit label found in the data for {unit_name}. Please provide a unit label using the '{unit_name}_label' argument.")
          unit_label <- ""
        }
      } else {
        unit_label <- get(paste0(unit_name, "_label"))
      }
      
      # merge data with uncertainty and unit labels
      merge_error(
        table_data = data,
        id_col_vals = group,
        unit_label = unit_label,
        uncert_lab = uncertainty_label
      )[[1]]
    }
  )

  combine_data <- purrr::reduce(
    purrr::compact(lab_list),
    dplyr::full_join,
    by = c("Year")
    ) |>
    gt::gt()

  final <- theme_table(combine_data)

  # export table to rda if argument = T
  if (make_rda == TRUE) {
    # No key quantities for captions/alt text since only values
    # are units for catch, SB, and F, which are specified for
    # the main figures/tables for each
    # So, export captions/alt text csv if absent
    if (!file.exists(fs::path(tables_dir, "captions_alt_text.csv"))) {
      caps_alttext <- utils::read.csv(
        system.file("resources", "captions_alt_text_template.csv", package = "stockplotr")
      )
      # export df with captions and alt text to csv
      utils::write.csv(
        x = caps_alttext,
        file = fs::path(tables_dir, "captions_alt_text.csv"),
        row.names = FALSE
      )
    }

    create_rda(
      object = final,
      # get name of function and remove "table_" from it
      topic_label = "projections",
      fig_or_table = "table",
      dat = dat,
      dir = tables_dir,
      scale_amount = 1,
      table_df = final$`_data`
    )
  }
  
  if (exists("selected_module")) {
    rm(selected_module, envir = .GlobalEnv)
  }

  final
}
