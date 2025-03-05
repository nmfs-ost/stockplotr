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
html_all_figs_tables <- function(rda_dir = getwd(),
                                 outdir = getwd()) {
  if (!dir.exists(fs::path(rda_dir, "rda_files"))) {
    stop("'rda_files' folder not found. Did you enter the correct argument for rda_dir?")
  } else {
    if (!dir.exists(fs::path(outdir, "all_tables_figures"))) {
      asar::create_tables_doc(subdir = tempdir() ,
                              include_all = TRUE,
                              rda_dir = rda_dir)

      asar::create_figures_doc(subdir = tempdir(),
                               include_all = TRUE,
                               rda_dir = rda_dir)

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
                 fs::path(outdir, "all_tables_figures.qmd"))
      quarto::quarto_render(
        input = fs::path(outdir, "all_tables_figures.qmd"),
        output_file = "all_tables_figures.html"
      )

      # create new folder for the html and qmd files because these can't
      # be saved there before or while rendering
      dir.create(fs::path(outdir, "all_tables_figures"))

      file.rename(
        from = fs::path("all_tables_figures.html"),
        to = fs::path(
          outdir,
          "all_tables_figures",
          "all_tables_figures.html"
        )
      )

      file.rename(
        from = fs::path("all_tables_figures.qmd"),
        to = fs::path(outdir, "all_tables_figures", "all_tables_figures.qmd")
      )

      message("The html and qmd with all tables and figures were newly created.")

    } else {
      question1 <- readline(
        paste0(
          "The 'all_tables_figures' folder already exists within ",
          fs::path(outdir),
          ". Would you like to overwrite the files within this folder? (Y/N)"
        )
      )

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
                   fs::path(outdir, "all_tables_figures.qmd"))
        quarto::quarto_render(
          input = fs::path(outdir, "all_tables_figures.qmd"),
          output_file = "all_tables_figures.html"
        )

        file.rename(
          from = fs::path("all_tables_figures.html"),
          to = fs::path(
            outdir,
            "all_tables_figures",
            "all_tables_figures.html"
          )
        )

        file.rename(
          from = fs::path("all_tables_figures.qmd"),
          to = fs::path(
            outdir,
            "all_tables_figures",
            "all_tables_figures.qmd"
          )
        )

        message(
          "The html and qmd with all tables and figures were regenerated and overwrote the previous versions."
        )

      } else {
        warning("The html and qmd with all tables and figures were not regenerated.")
      }
    }
  }
}
