#' Export a figure or table to rda
#'
#' Export a figure/table, and its caption and alternative text, to an rda object.
#' Typically used after stockplotr::extract_caps_alttext().
#'
#' @param object The figure (ggplot) or table (flextable) object to be exported.
#' @param caps_alttext The object containing a figure's caption and alternative
#' text, in a list, or a table's caption, likely generated with
#' stockplotr::extract_caps_alttext().
#' @param figures_tables_dir If the user has already created folders containing
#' figures and tables ("figures" and "tables"), figures_tables_dir represents
#' the location of these folders. Otherwise, these two folders will be created
#' automatically, then used to store the exported rda files.
#' @param topic_label A string that describes a figure or table's label. These
#' labels are found in the "label" column of the "captions_alt_text.csv" file
#' and are used to link the figure or table with its caption/alt text.
#' @param fig_or_table A string describing whether the plot is a figure or table.
#'
#' @return An rda file with a figure's ggplot, caption, and alternative text, or
#' a table's flextable and caption.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' export_rda(
#'   object = final_table_object,
#'   caps_alttext = caps_alttext_object,
#'   figures_tables_dir = here::here(),
#'   topic_label = "bnc",
#'   fig_or_table = "table"
#' )
#'
#' export_rda(
#'   object = final_figure_object,
#'   caps_alttext = another_caps_alttext_object,
#'   figures_tables_dir = "my_figures_tables_dir",
#'   topic_label = "landings",
#'   fig_or_table = "figure"
#' )
#' }
export_rda <- function(object = NULL,
                       caps_alttext = NULL,
                       figures_tables_dir = NULL,
                       topic_label = NULL,
                       fig_or_table = NULL) {
  # make rda for figures
  if (fig_or_table == "figure") {
    rda <- list(
      "figure" = object,
      "cap" = caps_alttext[[1]],
      "alt_text" = caps_alttext[[2]]
    )
    rda_loc <- "figures"

    # check if a figures folder already exists; if not, make one
    if (!dir.exists(fs::path(figures_tables_dir, rda_loc))) {
      dir.create(fs::path(figures_tables_dir, rda_loc))
      cli::cli_alert_success("New {rda_loc} folder created in {fs::path(figures_tables_dir)}.")
    }

    # make rda for tables
  } else if (fig_or_table == "table") {
    rda <- list(
      "table" = object,
      "cap" = caps_alttext[[1]]
    )
    rda_loc <- "tables"
    # check if a tables folder already exists; if not, make one
    if (!dir.exists(fs::path(figures_tables_dir, rda_loc))) {
      dir.create(fs::path(figures_tables_dir, rda_loc))
      cli::cli_alert_success("New {rda_loc} folder created in {fs::path(figures_tables_dir)}.")
    }
  }
  output_file_name <- paste0(topic_label, "_", fig_or_table, ".rda")

  # check if rda is already present. If so, check it should be overwritten
  if (file.exists(fs::path(
    figures_tables_dir,
    rda_loc,
    output_file_name
  ))) {
    question1 <- readline(
      paste0(
        "The ",
        output_file_name,
        " already exists within ",
        fs::path(figures_tables_dir, rda_loc),
        ". Would you like to overwrite this file? (Y/N)"
      )
    )

    if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
      # export rda
      save(rda,
        file = fs::path(
          figures_tables_dir,
          rda_loc,
          output_file_name
        )
      )
      cli::cli_alert_success("{output_file_name} was regenerated and overwrote the previous version.",
        wrap = TRUE
      )
    } else {
      cli::cli_alert_warning("{output_file_name} was not regenerated.")
    }
  } else {
    cli::cli_alert_info("{output_file_name} will be newly created.")

    # export rda
    save(rda,
      file = fs::path(
        figures_tables_dir,
        rda_loc,
        output_file_name
      )
    )

    cli::cli_alert_success("{output_file_name} was exported.")
  }
}
