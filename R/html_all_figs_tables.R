#' Make an html file with all figures and tables
#'
#' Show all tables and figures in a single html file.
#'
#' @inheritParams plot_recruitment
#'
#' @return A folder ("all_tables_figures") in your working directory containing
#' html and qmd files that show all tables and figures.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' html_all_figs_tables()
#' html_all_figs_tables(rda_dir = "my_rda_dir")
#' }
html_all_figs_tables <- function(rda_dir = getwd()) {
  if (!dir.exists(fs::path(rda_dir, "rda_files"))) {
    stop("'rda_files' folder not found. Did you enter the correct argument for rda_dir?")
  }
  # check if dir exists and present warning message/option message
  if (dir.exists(fs::path(getwd(), "all_tables_figures"))) {
    question1 <- readline(
      "The 'all_tables_figures' folder already exists within your working directory. Would you like to overwrite the files within this folder? (Y/N)"
    )
  } else {
    # indicate to proceed with function
    question1 <- "y"
    # create new folder for the html and qmd files
    dir.create(fs::path(rda_dir, "all_tables_figures"))
    doc_path <- fs::path(rda_dir, "all_tables_figures")
  }

  if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
    asar::create_tables_doc(
      subdir = tempdir() ,
      include_all = TRUE,
      rda_dir = rda_dir
    )

    asar::create_figures_doc(
      subdir = tempdir(),
      include_all = TRUE,
      rda_dir = rda_dir
    )

    tabs_figs_text <- c(readLines(fs::path(tempdir(), "08_tables.qmd")), readLines(fs::path(tempdir(), "09_figures.qmd")))

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

    writeLines(new_html_qmd,
                fs::path(rda_dir, "all_tables_figures.qmd"))
    quarto::quarto_render(
      input = fs::path(rda_dir, "all_tables_figures.qmd"),
      output_file = "all_tables_figures.html"
    )

    file.rename(
      from = fs::path("all_tables_figures.html"),
      to = fs::path(
        doc_path,
        "all_tables_figures.html"
      )
    )

    file.rename(
      from = fs::path("all_tables_figures.qmd"),
      to = fs::path(
        doc_path,
        "all_tables_figures.qmd"
      )
    )

    message(
      "The html and qmd with all tables and figures were generated and overwrote the previous versions."
    )

  } else {
    warning("The html and qmd with all tables and figures were not generated.")
  }
}
