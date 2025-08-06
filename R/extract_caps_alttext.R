#' Extract captions and alternative texts
#'
#' Extract a figure or table's caption and alternative text for usage when
#' generating a figure or table. Typically used before stockplotr::export_rda().
#'
#' @param topic_label A string that describes a figure or table's label. These
#' labels are found in the "label" column of the "captions_alt_text.csv" file
#' and are used to link the figure or table with its caption/alt text.
#' @param fig_or_table A string describing whether the plot is a figure or table.
#' @param dir The directory containing the "captions_alt_text.csv" file.
#'
#' @return A figure's caption and alternative text, in a list, or a table's caption.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' extract_caps_alttext(
#'   topic_label = "biomass",
#'   fig_or_table = "figure",
#'   dir = here::here()
#' )
#'
#' extract_caps_alttext(
#'   topic_label = "bnc",
#'   fig_or_table = "table",
#'   dir = getwd()
#' )
#' }
extract_caps_alttext <- function(topic_label = NULL,
                                 fig_or_table = NULL,
                                 dir = getwd()) {
  # import csv with captions and alt text
  captions_alttext_df <- utils::read.csv(
    fs::path(dir, "captions_alt_text.csv")
  )

  # extract plot or table's caption and alt text
  cap <- captions_alttext_df |>
    dplyr::filter(
      label == topic_label,
      type == fig_or_table
    ) |>
    dplyr::select(caption) |>
    as.character()

  if (fig_or_table == "figure") {
    alt_text <- captions_alttext_df |>
      dplyr::filter(
        label == topic_label,
        type == "figure"
      ) |>
      dplyr::select(alt_text) |>
      as.character()

    caps_alttext_list <- list(
      cap,
      alt_text
    )
  } else {
    caps_alttext_list <- list(cap)
  }

  caps_alttext_list
}
