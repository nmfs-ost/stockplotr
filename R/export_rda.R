#' Export a figure or table to rda
#'
#' Export a figure/table, and its caption and alternative text, to an rda object.
#' Typically used after stockplotr::extract_caps_alttext().
#'
#' @param final The final figure (ggplot) or table (flextable) object.
#' @param caps_alttext The object containing a figure's caption and alternative
#' text, in a list, or a table's caption, likely generated with
#' stockplotr::extract_caps_alttext().
#' @param rda_dir If the user has already created a folder containing .rda
#' files with figures, tables, alt text, and captions, rda_dir represents
#' the location of the folder containing these .rda files ("rda_files").
#' Otherwise, an "rda_files" folder will be created automatically, then used
#' to store the exported rda files.
##' @param topic_label A string that describes a figure or table's label. These
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
#'   final = final_table_object,
#'   caps_alttext = caps_alttext_object,
#'   rda_dir = here::here(),
#'   topic_label = "bnc",
#'   fig_or_table = "table"
#' )
#'
#' export_rda(
#'   final = final_figure_object,
#'   caps_alttext = another_caps_alttext_object,
#'   rda_dir = "my_rda_dir",
#'   topic_label = "landings",
#'   fig_or_table = "figure"
#' )
#' }
export_rda <- function(final = NULL,
                       caps_alttext = NULL,
                       rda_dir = NULL,
                       topic_label = NULL,
                       fig_or_table = NULL) {
  # make rda for figures
  if (fig_or_table == "figure") {
    rda <- list(
      "figure" = final,
      "cap" = caps_alttext[[1]],
      "alt_text" = caps_alttext[[2]]
    )
    # make rda for tables
  } else if (fig_or_table == "table") {
    rda <- list(
      "table" = final,
      "cap" = caps_alttext[[1]]
    )
  }
  output_file_name <- paste0(topic_label, "_", fig_or_table, ".rda")

  # check if an rda_files folder already exists; if not, make one
  if (!dir.exists(fs::path(rda_dir, "rda_files"))) {
    dir.create(fs::path(rda_dir, "rda_files"))
  }

  # check if rda is already present. If so, check it should be overwritten
  if (file.exists(fs::path(
    rda_dir,
    "rda_files",
    output_file_name
  ))) {
    question1 <- readline(
      paste0(
        "The ",
        output_file_name,
        " already exists within ",
        fs::path(rda_dir, "rda_files"),
        ". Would you like to overwrite this file? (Y/N)"
      )
    )

    if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
      # export rda
      save(rda,
        file = fs::path(
          rda_dir,
          "rda_files",
          output_file_name
        )
      )
      message(
        paste0(
          output_file_name,
          " was regenerated and overwrote the previous version."
        )
      )
    } else {
      warning(
        paste0(output_file_name, " was not regenerated.")
      )
    }
  } else {
    message(paste0(output_file_name, " will be newly created."))

    # export rda
    save(rda,
      file = fs::path(
        rda_dir,
        "rda_files",
        output_file_name
      )
    )

    message(
      paste0(
        output_file_name,
        " was exported."
      )
    )
  }
}
