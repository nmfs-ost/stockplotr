#' Add caption and alt text to the captions_alt_text.csv file
#'
#' Adds a new row to an existing captions_alt_text.csv file with a label, type,
#' caption, and alt_text for a figure or table not present in stockplotr.
#'
#' @inheritParams create_rda
#' 
#' @param topic_label String. Figure/table label. For automated matching in 
#' asar-based report, it must match the label in your plot's chunk options, MINUS
#' the "fig-" or "tab-". For example, the label "custom_table1" would match up 
#' properly with the "tbl-custom_table1" code chunk label.
#' 
#' @param caption String. Figure/table caption.
#' 
#' @param alt_text String. Figure alternative text. Unnecessary for text-based tables.
#' 
#' Default: ""
#' 
#' @param dir Path. Path to the existing captions_alt_text.csv file that will be
#' updated with the new caption and alt text.
#'
#' Default: file.path(getwd(), "captions_alt_text.csv")
#'
#' @export
#' 
#' @examples \dontrun{
#' add_caption_alt_text(
#'   topic_label = "my_topic",
#'   fig_or_table = "figure",
#'   caption = "My example figure caption.",
#'   alt_text = "My example figure alt text.",
#'   dir = file.path(getwd(), "captions_alt_text.csv")
#' )
#' }
add_caption_alt_text <- function(topic_label,
                                 fig_or_table,
                                 caption,
                                 alt_text = "",
                                 dir = file.path(getwd(), "captions_alt_text.csv")) {
  captions_alt_text <- utils::read.csv(dir)
  
  if (fig_or_table == tolower("figure") & alt_text == "") {
    cli::cli_alert_warning("No alt text provided for figure {topic_label}.")
    cli::cli_alert_warning("Figure alt text is necessary to pass asar report accessibility checks.")
    cli::cli_alert_info("Learn how to write descriptive alt text with the asar 'Increasing Report Accessibility' vignette: {.url https://nmfs-ost.github.io/asar/articles/accessibility_guide.html#alternative-text}.")
  }
  
  new_row <- data.frame(
    label = topic_label,
    type = fig_or_table,
    caption = caption,
    alt_text = alt_text
  )
  
  captions_alt_text <- rbind(captions_alt_text, new_row)
  
  utils::write.csv(
    captions_alt_text,
    file = dir,
    row.names = FALSE
  )
}
