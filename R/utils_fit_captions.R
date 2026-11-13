# Captions, alternative text, and key quantities for the fit diagnostic
# plots. Each plot has a row in captions_alt_text_template.csv whose
# placeholders start with the plot's topic label (e.g., runs.test.summary),
# and rows in key_quantity_template.csv for the quantities it records (e.g.,
# runs.test.n.passed). The plot describes what it shows as tags, which
# fit_export_rda() inserts with insert_kqs(), and its results as key
# quantities, which fit_export_rda() records with export_kqs().

# Export key quantities, insert tags into captions_alt_text.csv, and export
# the figure as an rda
fit_export_rda <- function(plt, topic_label, tags, dat, figures_dir, kqs = list()) {
  if (length(kqs) > 0) {
    fit_add_kq_rows(names(kqs), dir = figures_dir)
    fit_call_with_names(export_kqs, lapply(kqs, fit_tag_value), dir = figures_dir)
  }
  fit_add_caption_rows(topic_label, dir = figures_dir)
  fit_call_with_names(insert_kqs, lapply(tags, fit_tag_value), dir = figures_dir)
  create_rda(
    object = plt,
    topic_label = topic_label,
    fig_or_table = "figure",
    dat = dat,
    dir = figures_dir
  )
  invisible(tags)
}

# Call export_kqs() or insert_kqs(), which take the names of the quantities
# from the names of their arguments, with one variable per value
fit_call_with_names <- function(fun, values, dir) {
  env <- list2env(values, parent = environment())
  do.call(fun, c(rlang::syms(names(values)), list(dir = dir)), envir = env)
}

# Add the template rows for key quantities to an existing key_quantities.csv
# that was made before they existed (e.g., by an older version of
# stockplotr), so that their values are recorded
fit_add_kq_rows <- function(kq_names, dir = getwd()) {
  path <- fs::path(dir, "key_quantities.csv")
  if (!file.exists(path)) {
    return(invisible(NULL))
  }
  kqs <- utils::read.csv(path)
  missing <- setdiff(kq_names, kqs$key_quantity)
  if (length(missing) == 0) {
    return(invisible(NULL))
  }
  template <- utils::read.csv(
    system.file("resources", "key_quantity_template.csv", package = "stockplotr")
  )
  utils::write.csv(
    rbind(kqs, template[template$key_quantity %in% missing, , drop = FALSE]),
    path,
    row.names = FALSE
  )
  invisible(NULL)
}

# Add the template rows for a topic to an existing captions_alt_text.csv
# that was made before the topic existed (e.g., by an older version of
# stockplotr), so that its caption and alternative text are filled in
fit_add_caption_rows <- function(topic_label, dir = getwd()) {
  path <- fs::path(dir, "captions_alt_text.csv")
  if (!file.exists(path)) {
    return(invisible(NULL))
  }
  caps <- utils::read.csv(path)
  if (any(caps$label == topic_label & caps$type == "figure")) {
    return(invisible(NULL))
  }
  template <- utils::read.csv(
    system.file("resources", "captions_alt_text_template.csv", package = "stockplotr")
  )
  new_rows <- template[template$label == topic_label & template$type == "figure", , drop = FALSE]
  utils::write.csv(rbind(caps, new_rows), path, row.names = FALSE)
  invisible(NULL)
}

# One string per tag. Remove characters with special meaning in the
# replacement strings of stringr::str_replace_all().
fit_tag_value <- function(x) {
  x <- paste(x, collapse = " ")
  gsub("[$\\\\]", "", x)
}

#------------------------------------------------------------------------------
# Text helpers
#------------------------------------------------------------------------------

# "A", "A and B", or "A, B, and C"
fit_list_text <- function(x) {
  x <- unique(as.character(x))
  n <- length(x)
  if (n == 0) {
    return("")
  }
  if (n <= 2) {
    return(paste(x, collapse = " and "))
  }
  paste0(paste(x[-n], collapse = ", "), ", and ", x[n])
}

# Description of the panels in a faceted plot
fit_panel_text <- function(panels, what = "fleet") {
  panels <- unique(panels)
  if (length(panels) == 1) {
    paste0("A single panel for ", panels, ".")
  } else {
    paste0(
      "A set of ", length(panels), " panels, one for each ", what,
      " (", fit_list_text(panels), ")."
    )
  }
}
