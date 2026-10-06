#' Browse the dashboard figure catalog
#'
#' Lists every figure included by [create_dashboard()], its plotting function,
#' category, and preferred data modules. The first available preferred module is
#' used; otherwise a unique module containing the required labels is used.
#' Ambiguous or missing sources are reported in the dashboard, never prompted for.
#'
#' @return A data frame with one row per supported figure.
#' @md
#' @export
#' @examples
#' dashboard_figures()
dashboard_figures <- function() {
  specs <- dashboard_specs()
  do.call(rbind, lapply(specs, function(x) {
    data.frame(id = x$id, title = x$title, category = x$category,
               function_name = paste0("plot_", x$id),
               preferred_modules = paste(x$modules, collapse = ", "),
               stringsAsFactors = FALSE)
  }))
}

#' Create a portable stock assessment dashboard
#'
#' Export stockplotr figures into a searchable, offline HTML dashboard with
#' category navigation, a figure gallery, side-by-side comparison, image zoom,
#' captions, alternative text, and SVG, PNG and source-data downloads.
#'
#' @param dat A standardized data frame, such as [example_data] or the result of
#'   [convert_output()]. Required columns are `label`, `estimate`, `year`, and
#'   `module_name`. Pass one model's standardized output, not a fitted model.
#' @param output_dir Directory to create. Defaults to `"stockplotr-dashboard"`
#'   under the current working directory. It must be absent or empty; existing
#'   reports are never overwritten. All viewer assets are supplied internally.
#' @param title Single character string displayed as the report title.
#' @param subtitle Single character string describing the stock, assessment, or
#'   model. Defaults to `"Stock assessment figure explorer"`.
#' @param figures Character vector of figure IDs from [dashboard_figures()].
#'   `NULL`, the default, attempts every figure in that catalog. Unavailable
#'   figures are recorded with reasons in the report. Unknown IDs are rejected.
#' @param zip Logical. Also create `paste0(output_dir, ".zip")` (default `TRUE`).
#'   This archive must not already exist.
#' @param open Logical. Open the resulting `index.html` in the default browser
#'   (default `FALSE`).
#' @param plot_args Optional named list of argument lists, indexed by figure ID.
#'   For example, `list(biomass = list(module = "TIME_SERIES", unit_label = "mt"))`.
#'   Normally omitted. Arguments are forwarded to the corresponding `plot_*`
#'   function; `dat`, `make_rda`, `figures_dir`, and `interactive` are managed
#'   internally. Module choices are recorded in the report. Unit labels do not
#'   convert values: match them to the units in your data.
#'
#' @details
#' Uses the original stockplotr plots and their exported captions and alternative
#' text. Biomass, landings, discards and biomass-at-age default to metric tons
#' (`mt`); recruitment and abundance-at-age use `fish`. Catch-at-age uses
#' proportional bubbles. These labels assume those source units; override them
#' with `plot_args` when needed. No unit conversions are performed.
#'
#' The viewer uses local HTML, CSS and JavaScript, with no Quarto, Java, server,
#' CDN, or internet connection. Share the ZIP; the recipient must extract the
#' whole archive and open `index.html`. JavaScript must be enabled. All figures
#' also remain available as ordinary image files. Interactivity is in the viewer
#' (search, navigation, comparison and image magnification); it does not recompute
#' model results or provide point-level hover values.
#'
#' Each CSV contains the selected source module, including reference quantities
#' from other modules used by plots. It is not a reconstruction of plotted layers.
#' A JSON manifest records sources, plot arguments, warnings and failures. Missing
#' figures remain visible in the viewer; if every figure fails no report is
#' published and an error explains why. Generation uses temporary directories so
#' the plotting functions' caption files do not alter the working directory.
#'
#' @return Invisibly, a list with absolute paths `index`, `directory`, `zip`
#'   (`NULL` when disabled), and a `figures` data frame containing IDs, status,
#'   selected modules and failure reasons.
#' @seealso [dashboard_figures()], [save_all_plots()]
#' @md
#' @export
#' @examples
#' \dontrun{
#' report <- create_dashboard(example_data, output_dir = "example-dashboard",
#'                            title = "Example assessment")
#' utils::browseURL(report$index)
#' # Send example-dashboard.zip. Recipients extract it and open index.html.
#'
#' create_dashboard(example_data, output_dir = "population-dashboard",
#'   figures = c("spawning_biomass", "biomass", "recruitment"),
#'   plot_args = list(recruitment = list(unit_label = "fish")))
#' }
create_dashboard <- function(dat, output_dir = "stockplotr-dashboard",
                             title = "Stock assessment", subtitle = "Stock assessment figure explorer",
                             figures = NULL, zip = TRUE, open = FALSE,
                             plot_args = list()) {
  required <- c("label", "estimate", "year", "module_name")
  if (!is.data.frame(dat) || !all(required %in% names(dat))) {
    stop("`dat` must be a standardized data frame with: ", paste(required, collapse = ", "), call. = FALSE)
  }
  if (!nrow(dat) || !is.numeric(dat$estimate)) stop("`dat` needs rows and numeric estimates.", call. = FALSE)
  for (value in list(output_dir, title, subtitle)) {
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      stop("`output_dir`, `title`, and `subtitle` must be nonempty strings.", call. = FALSE)
    }
  }
  for (value in list(zip, open)) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) stop("`zip` and `open` must be TRUE or FALSE.", call. = FALSE)
  }
  specs <- dashboard_specs()
  if (is.null(figures)) figures <- names(specs)
  if (!is.character(figures) || !length(figures) || anyNA(figures) ||
      anyDuplicated(figures) || any(!figures %in% names(specs))) {
    stop("Use unique figure IDs from dashboard_figures().", call. = FALSE)
  }
  if (!is.list(plot_args) || (length(plot_args) &&
      (is.null(names(plot_args)) || anyDuplicated(names(plot_args)) ||
       any(!names(plot_args) %in% figures) || !all(vapply(plot_args, is.list, logical(1)))))) {
    stop("`plot_args` must be a named list of argument lists for requested figure IDs.", call. = FALSE)
  }
  for (id in names(plot_args)) {
    args <- plot_args[[id]]
    allowed <- setdiff(names(formals(get(paste0("plot_", id)))),
                       c("dat", "make_rda", "figures_dir", "interactive", "..."))
    if (length(args) && (is.null(names(args)) || anyDuplicated(names(args)) || any(!names(args) %in% allowed))) {
      stop("Unknown or managed plot argument for ", id, ". See its plot_* documentation.", call. = FALSE)
    }
  }
  dependencies <- c("jsonlite", "svglite", if (zip) "zip")
  for (pkg in dependencies) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Install the '", pkg, "' package to create a dashboard.", call. = FALSE)
  }
  output_dir <- fs::path_abs(output_dir)
  zip_path <- paste0(output_dir, ".zip")
  if (file.exists(output_dir) && (!dir.exists(output_dir) ||
      length(list.files(output_dir, all.files = TRUE, no.. = TRUE)))) {
    stop("`output_dir` already exists and is not empty: ", output_dir, call. = FALSE)
  }
  if (zip && file.exists(zip_path)) stop("Archive already exists: ", zip_path, call. = FALSE)
  stage <- tempfile("stockplotr-dashboard-")
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  dir.create(file.path(stage, "figures"))
  dir.create(file.path(stage, "data"))
  assets <- system.file("dashboard", package = "stockplotr")
  files <- c("index.html", "dashboard.css", "dashboard.js")
  if (!nzchar(assets) || !all(file.exists(file.path(assets, files)))) stop("Dashboard assets are missing; reinstall stockplotr.", call. = FALSE)
  if (!all(file.copy(file.path(assets, files), stage))) stop("Could not copy dashboard assets.", call. = FALSE)
  entries <- lapply(figures, function(id) {
    cli::cli_inform("Building dashboard figure: {id}")
    dashboard_export_figure(dat, specs[[id]], plot_args[[id]], stage)
  })
  ok <- vapply(entries, function(x) x$status == "ready", logical(1))
  if (!any(ok)) stop("No figures could be rendered:\n", paste(vapply(entries, function(x) paste(x$title, x$error, sep = ": "), character(1)), collapse = "\n"), call. = FALSE)
  report <- list(schema_version = 1L, title = title, subtitle = subtitle,
                 generated = format(Sys.time(), "%Y-%m-%d %H:%M UTC", tz = "UTC"),
                 package_version = as.character(utils::packageVersion("stockplotr")),
                 figures = entries)
  json <- jsonlite::toJSON(report, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE)
  writeLines(json, file.path(stage, "manifest.json"), useBytes = TRUE)
  # A classic local script works under file://; no fetch(), modules or web server.
  script_json <- gsub("<", "\\u003c", json, fixed = TRUE)
  script_json <- gsub("\u2028", "\\u2028", script_json, fixed = TRUE)
  script_json <- gsub("\u2029", "\\u2029", script_json, fixed = TRUE)
  writeLines(paste0("window.STOCKPLOTR_REPORT = ", script_json, ";"), file.path(stage, "report-data.js"), useBytes = TRUE)
  writeLines(c("STOCKPLOTR DASHBOARD", "", "Extract this entire ZIP, then double-click index.html.",
               "No R, Java, internet connection or server is required. Enable JavaScript in your browser.",
               "Keep the folders together. Figures are also available as SVG and PNG in figures/.",
               "CSV downloads contain source module data, not only the plotted layers.",
               "See manifest.json for plotting arguments, source modules, warnings and unavailable figures."),
             file.path(stage, "START-HERE.txt"))
  staged_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(staged_zip), add = TRUE)
  if (zip) zip::zipr(staged_zip, list.files(stage, all.files = TRUE, no.. = TRUE), root = stage)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!all(file.copy(list.files(stage, full.names = TRUE, all.files = TRUE, no.. = TRUE), output_dir, recursive = TRUE))) stop("Could not copy report to output directory.", call. = FALSE)
  if (zip && !file.copy(staged_zip, zip_path)) stop("Could not write ZIP archive.", call. = FALSE)
  status <- do.call(rbind, lapply(entries, function(x) data.frame(id = x$id, status = x$status,
    module = x$module, reason = x$error, stringsAsFactors = FALSE)))
  if (any(!ok)) warning(sum(!ok), " figure(s) unavailable; see the dashboard or result$figures.", call. = FALSE)
  index <- file.path(output_dir, "index.html")
  if (open) utils::browseURL(index)
  invisible(list(index = index, directory = output_dir, zip = if (zip) zip_path else NULL, figures = status))
}

dashboard_specs <- function() {
  spec <- function(id, title, category, modules, pattern, args = list()) {
    list(id = id, title = title, category = category, modules = modules, pattern = pattern, args = args)
  }
  x <- list(
    spec("spawning_biomass", "Spawning biomass", "Population", c("Population", "DERIVED_QUANTITIES", "TIME_SERIES"), "^spawning_biomass$"),
    spec("biomass", "Total biomass", "Population", c("Population", "TIME_SERIES", "DERIVED_QUANTITIES"), "^biomass$"),
    spec("recruitment", "Recruitment", "Population", c("Population", "TIME_SERIES", "DERIVED_QUANTITIES"), "^recruitment$", list(unit_label = "fish")),
    spec("recruitment_deviations", "Recruitment deviations", "Population", c("Population", "PARAMETERS"), "recruitment_deviations"),
    spec("stock_recruitment", "Stock-recruit relationship", "Population", c("Population", "SPAWN_RECRUIT"), "recruitment", list(recruitment_label = "fish")),
    spec("abundance_at_age", "Abundance at age", "Population", c("Population", "NUMBERS_AT_AGE"), "abundance", list(unit_label = "fish")),
    spec("biomass_at_age", "Biomass at age", "Population", c("Population", "BIOMASS_AT_AGE"), "^biomass"),
    spec("fishing_mortality", "Fishing mortality", "Mortality", c("Population", "TIME_SERIES"), "^fishing_mortality$"),
    spec("natural_mortality", "Natural mortality", "Mortality", c("Population", "Natural_Mortality"), "natural_mortality"),
    spec("selectivity", "Selectivity", "Fisheries", c("Fleet", "AGE_SELEX", "LEN_SELEX"), "selectivity"),
    spec("landings", "Landings", "Fisheries", c("Fleet", "CATCH", "TIME_SERIES"), "landings", list(geom = "area")),
    spec("discard", "Discards", "Fisheries", c("Fleet", "DISCARD_OUTPUT"), "^discard"),
    spec("catch_comp", "Catch at age", "Fisheries", c("Fleet", "CATCH_AT_AGE"), "catch|landings", list(proportional = TRUE)),
    spec("index", "Abundance indices", "Observations", c("Fleet", "INDEX_2"), "^index")
  )
  stats::setNames(x, vapply(x, `[[`, character(1), "id"))
}

dashboard_export_figure <- function(dat, spec, overrides, stage) {
  if (is.null(overrides)) overrides <- list()
  entry <- list(id = spec$id, title = spec$title, category = spec$category,
                function_name = paste0("plot_", spec$id), status = "unavailable",
                module = "", error = "", warnings = list())
  warnings <- character()
  tryCatch(withCallingHandlers({
    modules <- unique(as.character(dat$module_name[grepl(spec$pattern, dat$label, ignore.case = TRUE)]))
    modules <- modules[!is.na(modules) & nzchar(modules)]
    preferred <- spec$modules[spec$modules %in% modules]
    module <- if (!is.null(overrides$module)) overrides$module else if (length(preferred)) preferred[1] else if (length(modules) == 1L) modules else NULL
    if (is.null(module)) stop(if (length(modules)) "Multiple source modules match; select one with plot_args." else "Required data are absent.")
    if (!is.character(module) || length(module) != 1L || is.na(module) || !module %in% modules) stop("Selected module does not contain the required data.")
    entry$module <- module
    # Keep reference quantities while excluding unrelated competing time series.
    source <- dat[dat$module_name %in% module | grepl("_(msy|unfished|target)$", dat$label, ignore.case = TRUE), , drop = FALSE]
    scratch <- tempfile("stockplotr-figure-")
    dir.create(scratch)
    previous <- setwd(scratch)
    on.exit({setwd(previous); unlink(scratch, recursive = TRUE)}, add = TRUE)
    # filter_data currently stores this selection globally. Restore the caller's state.
    had_selection <- exists("selected_module", envir = .GlobalEnv, inherits = FALSE)
    if (had_selection) selection <- get("selected_module", envir = .GlobalEnv)
    on.exit({
      if (had_selection) assign("selected_module", selection, envir = .GlobalEnv)
      else if (exists("selected_module", envir = .GlobalEnv, inherits = FALSE)) rm("selected_module", envir = .GlobalEnv)
    }, add = TRUE)
    args <- utils::modifyList(spec$args, overrides, keep.null = TRUE)
    args$module <- module
    entry$arguments <- args
    # Metadata calculations can need context outside the plotted module (e.g.,
    # recruitment age). Let the plotting function filter the full input explicitly.
    args <- c(list(dat = dat, interactive = FALSE, make_rda = TRUE, figures_dir = scratch), args)
    # Preserve the function's name; plot metadata derives its topic from sys.call().
    do.call(entry$function_name, args, envir = environment(create_dashboard))
    rdas <- list.files(scratch, pattern = "_figure\\.rda$", recursive = TRUE, full.names = TRUE)
    if (length(rdas) != 1L) stop("The plot did not export a unique figure with caption and alternative text.")
    env <- new.env(parent = emptyenv())
    load(rdas, envir = env)
    figure <- env$rda
    if (!inherits(figure$figure, "ggplot")) stop("The exported figure is not a ggplot.")
    entry$caption <- paste(figure$caption, collapse = " ")
    entry$alt_text <- paste(figure$alt_text, collapse = " ")
    if (!nzchar(entry$caption) || !nzchar(entry$alt_text)) stop("The plot exported empty captions or alternative text.")
    entry$svg <- paste0("figures/", spec$id, ".svg")
    entry$png <- paste0("figures/", spec$id, ".png")
    entry$csv <- paste0("data/", spec$id, ".csv")
    ggplot2::ggsave(file.path(stage, entry$svg), figure$figure, device = svglite::svglite, width = 12, height = 8, bg = "white")
    ggplot2::ggsave(file.path(stage, entry$png), figure$figure, device = "png", width = 12, height = 8, dpi = 144, bg = "white")
    utils::write.csv(source, file.path(stage, entry$csv), row.names = FALSE, fileEncoding = "UTF-8")
    entry$status <- "ready"
    entry
  }, warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
    invokeRestart("muffleWarning")
  }), error = function(e) {
    entry$error <<- conditionMessage(e)
    entry
  }) -> result
  result$warnings <- as.list(unique(warnings))
  result
}
