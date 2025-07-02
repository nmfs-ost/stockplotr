#' Make an html file with all figures and tables
#'
#' Show all tables and figures in a single html file.
#'
#' @param figures_tables_dir The location of the folder containing
#' figures and tables ("figures" and "tables").
#'
#' @return A folder ("all_tables_figures") in your working directory containing
#' html and qmd files that show all tables and figures.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' html_all_figs_tables()
#' html_all_figs_tables(figures_tables_dir = "my_figures_tables_dir")
#' }
html_all_figs_tables <- function(figures_tables_dir = getwd()) {
  if (!dir.exists(fs::path(figures_tables_dir, "figures"))) {
    cli::cli_alert_danger("'figures' folder not found.")
    cli::cli_alert_warning("Figures will not be included in the html.")
    cli::cli_alert_info("Did you enter the correct argument for figures_tables_dir?")
    cli::cli_alert_info("figures_tables_dir entered as {figures_tables_dir}")
  }

  if (!dir.exists(fs::path(figures_tables_dir, "tables"))) {
    cli::cli_alert_danger("'tables' folder not found.")
    cli::cli_alert_warning("Tables will not be included in the html.")
    cli::cli_alert_info("Did you enter the correct argument for figures_tables_dir?")
    cli::cli_alert_info("figures_tables_dir entered as {figures_tables_dir}")
  }

  # check if dir exists and present warning message/option message
  if (dir.exists(fs::path(figures_tables_dir, "all_tables_figures"))) {
    question1 <- readline(
      "The 'all_tables_figures' folder already exists within your working directory. Would you like to overwrite the files within this folder? (Y/N)"
    )
  } else {
    # indicate to proceed with function
    question1 <- "y"
  }

  if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
    # create new folder for the html and qmd files
    doc_path <- fs::path(figures_tables_dir, "all_tables_figures")
    dir.create(doc_path)

    asar::create_tables_doc(
      subdir = figures_tables_dir,
      tables_dir = figures_tables_dir
    )

    asar::create_figures_doc(
      subdir = figures_tables_dir,
      figures_dir = figures_tables_dir
    )

    tab_doc_name <- list.files(figures_tables_dir)[grepl("_tables.qmd", list.files(figures_tables_dir))]
    tab_doc <- readLines(fs::path(figures_tables_dir, tab_doc_name))

    fig_doc_name <- list.files(figures_tables_dir)[grepl("_figures.qmd", list.files(figures_tables_dir))]
    fig_doc <- readLines(fs::path(figures_tables_dir, fig_doc_name))

    tabs_figs_text <- c(
      tab_doc,
      fig_doc
    )

    yaml_text <-
      "---
title: 'All Tables & Figures'
format:
  html:
    toc: true
    embed-resources: true
---
"

    new_html_qmd <- c(yaml_text, tabs_figs_text)

    writeLines(
      new_html_qmd,
      fs::path(figures_tables_dir, "all_tables_figures.qmd")
    )

    withr::with_dir(
      figures_tables_dir,
      quarto::quarto_render(
        input = fs::path(figures_tables_dir, "all_tables_figures.qmd"),
        output_file = fs::path("all_tables_figures.html")
      )
    )

    file.rename(
      from = fs::path(figures_tables_dir, "all_tables_figures.html"),
      to = fs::path(
        doc_path,
        "all_tables_figures.html"
      )
    )

    file.rename(
      from = fs::path(figures_tables_dir, "all_tables_figures.qmd"),
      to = fs::path(
        doc_path,
        "all_tables_figures.qmd"
      )
    )

    cli::cli_alert_success("Generated html and qmd with all tables and figures", wrap = TRUE)
    cli::cli_alert_info("Overwrote previous html and qmd")
  } else {
    cli::cli_alert_warning("Did not generate html and qmd with all tables and figures", wrap = TRUE)
  }
}
