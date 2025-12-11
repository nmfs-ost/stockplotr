#' Landed catch by fleet and year table
#'
#' @inheritParams plot_recruitment
#' @param unit_label Abbreviated units of landings
#' @param tables_dir The location of the folder containing the generated table
#' rda files ("tables") that will be created if the argument `make_rda` = TRUE. 
#' @param label The label that will be chosen from the input file. If unspecified, the function will search the "label" column and use the first matching label in this ordered list: "landings_weight",  "landings_numbers", "landings_expected", "landings_predicted", "landings".
#'
#' @return Create a table ready for a stock assessment report of landed catch by
#' fleet and year.
#' @export
#'
#' @examples
#' \dontrun{
#' table_landings(dat)
#'
#' table_landings(
#'   dat,
#'   unit_label = "landings label",
#'   end_year = 2024,
#'   make_rda = TRUE,
#'   tables_dir = getwd()
#' )
#' }
table_landings <- function(dat,
                           unit_label = "mt",
                           era = "time",
                           interactive = TRUE,
                           module = NULL,
                           scale_amount = 1,
                           label = "landings_weight",
                           make_rda = FALSE,
                           tables_dir = getwd()) {
  
  # create plot-specific variables to use throughout fxn for naming and IDing
  topic_label <- "landings"
  
  # identify output
  fig_or_table <- "table"
  
  #TODO: add these args to the table_landings() args
  # Filter data for landings
  prepared_data <- filter_data(
    dat = dat,
    label_name = "landings",
    geom = "line",
    era = era,
   # group = ifelse(length(group) > 1, group[1], group),
   # facet = ifelse(length(group) > 1, group[-1], NULL),
    module = module,
    scale_amount = scale_amount,
    interactive = interactive
  )
  
  # order potential labels by applicability
  ordered_labels <- c("landings_weight", 
                      "landings_numbers",
                      "landings_expected",
                      "landings_predicted",
                      "landings")
  
  # Choose label to filter by, based on presence in prepared_data
  for (lab in ordered_labels) {
    if (lab %in% prepared_data$label) {
      target_label <- lab
      break
    }
  }
  prepared_data2 <- prepared_data |>
    dplyr::filter(label == target_label)
  
  #TODO: add check for if length of label > 1 (if TRUE, then a specific value (e.g., observed?) will need to be selected)

  # add a check for which landings-related name to extract (e.g., expected, observed, cv...)
  
  table_data <- process_table(
    dat = prepared_data2,
    group = group,
    method = method)
  
  # put table_data into a nice table
  # ensure cols in order: estimate, error, est, error, etc.
  # try to keep it to one column
  capitalized_names <- c("Year" = "year",
                         "Sex" = "sex",
                         "Fleet" = "fleet",
                         "Model" = "model")
  
  landings_colname <- paste0("Landings (", unit_label, ")")
  
  #TODO: Update add_theme() for gt tables
  final_df <- table_data |>
      dplyr::rename(dplyr::any_of(capitalized_names)) |>
      dplyr::rename_with(~ gsub("_NA|_label|estimate_", "", .)) |>
      dplyr::rename(dplyr::any_of(stats::setNames(target_label, landings_colname))) |>
      dplyr::rename_with(~ gsub(target_label, "", .)) |>
      dplyr::rename_with(~ gsub("^uncertainty_$", "Uncertainty", .))
  
  final <- final_df |>
      gt::gt() 

  # Progress:
    # for bsb, hake, vsnap, and stockplotr::example_data, cols are:
    #    "Year", "Landings (<unit>)", "Uncertainty"

      
  # TODO: Reorder column names so that numeric fleets show up in chronological
  # order (currently, lists 1, 10, 11, 12, etc.)
  
  # export figure to rda if argument = T
  if (make_rda == TRUE) {
    # run write_captions.R if its output doesn't exist
    if (!file.exists(
      fs::path(getwd(), "captions_alt_text.csv")
    )
    ) {
      stockplotr::write_captions(
        dat = dat,
        dir = tables_dir,
        year = end_year
      )
    }

    # add more key quantities included as arguments in this fxn
    add_more_key_quants(
      dat,
      topic = topic_label,
      fig_or_table = fig_or_table,
      dir = tables_dir,
      end_year = end_year,
      units = unit_label
    )

    # extract this plot's caption and alt text
    caps_alttext <- extract_caps_alttext(
      topic_label = topic_label,
      fig_or_table = fig_or_table,
      dir = tables_dir
    )
    
    # create LaTeX-based table
    latex_table <- create_latex_table(data = final_df,
                       caption = caps_alttext[1],
                       label = "landings_latex")

    export_rda(
      object = final,
      caps_alttext = caps_alttext,
      figures_tables_dir = tables_dir,
      topic_label = topic_label,
      fig_or_table = fig_or_table,
      latex_table = latex_table
    )
  }
  # Return finished table
  final
}
