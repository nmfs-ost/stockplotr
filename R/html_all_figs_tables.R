#' Make an html file with all figures and tables
#'
#' Show all figures and tables in an html file.
#'
#' @inheritParams plot_recruitment
#' @param outdir The location of the folder containing the generated html and qmd
#' files ("all_tables_figures") that will be created. Default is the working directory.
#'
#' @return An html file with all existing figures and tables.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' html_all_figs_tables()
#' html_all_figs_tables(rda_dir = getwd(),
#'                      outdir = getwd())
#' }
html_all_figs_tables <- function(
    rda_dir = getwd(),
    outdir = getwd()
    ) {

  asar::create_tables_doc(subdir = tempdir() ,
                           include_all = TRUE,
                           rda_dir = rda_dir)

  asar::create_figures_doc(subdir = tempdir(),
                           include_all = TRUE,
                           rda_dir = rda_dir)

  tabs_figs_text <- c(readLines(fs::path(tempdir(), "08_tables.qmd")),
                      readLines(fs::path(tempdir(), "09_figures.qmd")))

  yaml_text <-
"---
title: 'All Tables & Figures'
format:
  html:
    toc: true
    embed-resources: true
---
"

  new_html_qmd <- c(yaml_text,
                    tabs_figs_text)

  writeLines(new_html_qmd, fs::path(outdir, "all_tables_figures.qmd"))
  quarto::quarto_render(input = fs::path(outdir, "all_tables_figures.qmd"),
                        output_file = "all_tables_figures.html"
                        )

  # create new folder for the html and qmd files because these can't
  # be saved there before or while rendering
  dir.create(fs::path(outdir, "all_tables_figures"))

  file.rename(from = fs::path("all_tables_figures.html"),
              to = fs::path(outdir, "all_tables_figures", "all_tables_figures.html"))

  file.rename(from = fs::path("all_tables_figures.qmd"),
              to = fs::path(outdir, "all_tables_figures", "all_tables_figures.qmd"))

  }
