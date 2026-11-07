# Captions and alternative text for the fit diagnostic plots. Each plot has
# a row in captions_alt_text_template.csv whose placeholders start with the
# plot's topic label (e.g., runs.test.summary). The plot describes what it
# shows as tags, which fit_export_rda() inserts with insert_kqs().

# Insert tags into captions_alt_text.csv and export the figure as an rda
fit_export_rda <- function(plt, topic_label, tags, dat, figures_dir) {
  fit_add_caption_rows(topic_label, dir = figures_dir)
  # insert_kqs() takes the placeholder names from the names of its
  # arguments, so call it with one variable per tag
  tags <- lapply(tags, fit_tag_value)
  env <- list2env(tags, parent = environment())
  do.call(insert_kqs, c(rlang::syms(names(tags)), list(dir = figures_dir)), envir = env)
  create_rda(
    object = plt,
    topic_label = topic_label,
    fig_or_table = "figure",
    dat = dat,
    dir = figures_dir
  )
  invisible(tags)
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
