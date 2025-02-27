#' Create Indices of Abundance Table
#'
#' @inheritParams plot_recruitment
#' @return Create table of observed annual indices of abundance plus error
#' stratified by fleet.
#' @export
#'
table_indices <- function(
    dat,
    make_rda = FALSE,
    rda_dir = getwd()) {
  # Load data
  output <- dat |>
    dplyr::filter(module_name == "INDEX_2" | module_name == "t.series")
  # Check for U
  if (any(unique(output$module_name == "INDEX_2"))) {
    output <- output |>
      dplyr::filter(
        grepl("expected_indices", label) | grepl("indices_predicted", label)
      ) # grepl("input_indices", label) |
  } else if (any(unique(output$module_name == "t.series"))) {
    output <- output |>
      dplyr::filter(grepl("cpue", label))
  }
  # Extract fleet names
  fleet_names <- unique(as.character(output$fleet))
  factors <- c("year", "fleet", "fleet_name", "age", "sex", "area", "seas", "season", "time", "era", "subseas", "subseason", "platoon", "platoo", "growth_pattern", "gp")
  # re-structure df for table
  indices <- output |>
    tidyr::pivot_wider(
      # id_cols = c(year, uncertainty, uncertainty_label),
      names_from = label,
      values_from = estimate
    ) |>
    dplyr::select(year, fleet, unique(output$label), uncertainty, uncertainty_label) # |>

  # na.omit()
  # check if uncertainty is a measure in the df
  if (all(is.na(indices$uncertainty))) {
    indices <- indices |>
      dplyr::select(-c(uncertainty_label, uncertainty))
  } else {
    uncertainty_col <- paste("uncertainty_", unique(indices$uncertainty_label), sep = "")
    colnames(indices) <- stringr::str_replace(colnames(indices), "^uncertainty$", uncertainty_col)
    indices <- dplyr::select(indices, -uncertainty_label)
  }

  # Check if observed/inital values are in the df
  if (any(grepl("observed", colnames(indices)))) {
    indices <- indices |>
      dplyr::select(-colnames(indices)[grep(c("observed"), colnames(indices))])
  }

  # rename columns to remove cpue/effort
  if (any(grep("_indices", colnames(indices)))) {
    colnames(indices) <- stringr::str_replace_all(colnames(indices), "_indices", "")
  } else if (any(grep("indices_", colnames(indices)))) {
    colnames(indices) <- stringr::str_replace_all(colnames(indices), "indices_", "")
  } else {
    colnames(indices) <- stringr::str_replace_all(colnames(indices), "cpue_", "")
  }

  # Check for which column is U and filter out na values
  if (any(grep("predicted", colnames(indices)))) {
    indices <- indices |>
      dplyr::filter(!is.na(predicted))
  }
  if (any(grep("expected", colnames(indices)))) {
    indices <- indices |>
      dplyr::filter(!is.na(expected))
  }

  # move fleet data into own columns
  indices2 <- indices |>
    tidyr::pivot_wider(
      names_from = fleet,
      values_from = colnames(indices)[!colnames(indices) %in% c("year", "fleet")]
    ) |>
    dplyr::select(year, dplyr::ends_with(fleet_names))

  fleet_col_names <- stringr::str_extract(colnames(indices2)[colnames(indices2) != "year"], "[^_]+$")
  if (any(grepl("[0-9]+", fleet_col_names))) {
    fleet_header_lab <- ""
    fleet_col_names <- paste("Fleet ", fleet_col_names)
  } else {
    fleet_header_lab <- "Fleet"
  }

  tab <- indices2 |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 2)),
      year = as.character(year)
    ) |>
    flextable::flextable() |>
    flextable::set_header_labels(
      # TODO: set uncertainty to the actual value instead of word uncertainty
      values = c("Year", rep(c("Estimated CPUE", "Uncertainty"), (ncol(indices2) - 1) / 2))
    ) |>
    flextable::add_header_row(
      values = c(fleet_header_lab, fleet_col_names)
    ) |>
    flextable::merge_h(part = "header") |>
    flextable::align(part = "header")

  final <- suppressWarnings(add_theme(tab))

  # export table to rda if argument = T
  if (make_rda) {
    # create plot-specific variables to use throughout fxn for naming and IDing
    topic_label <- "indices.abundance"


    # run write_captions.R if its output doesn't exist
    if (!file.exists(
      fs::path(getwd(), "captions_alt_text.csv")
    )
    ) {
      stockplotr::write_captions(
        dat = dat,
        dir = rda_dir,
        year = NULL
      )
    }
    # identify output
    fig_or_table <- "table"

    # extract this plot's caption and alt text
    caps_alttext <- extract_caps_alttext(
      topic_label = topic_label,
      fig_or_table = fig_or_table,
      dir = rda_dir
    )


    export_rda(
      final = final,
      caps_alttext = caps_alttext,
      rda_dir = rda_dir,
      topic_label = topic_label,
      fig_or_table = fig_or_table
    )
  }
  return(final)
}
