# Evaluate plotting code with `make_rda = TRUE` in a temporary directory and
# return the saved rda (a list with the figure, caption, and alt text).
# `captions` is an optional captions_alt_text.csv to start from, and `dir` the
# figures_dir the plot was saved to (default: the temporary directory).
fit_rda <- function(code, topic_label, captions = NULL, dir = NULL) {
  wd <- tempfile("fit_rda")
  dir.create(wd)
  old <- setwd(wd)
  on.exit(
    {
      setwd(old)
      unlink(wd, recursive = TRUE)
    },
    add = TRUE
  )
  if (!is.null(captions)) {
    utils::write.csv(captions, "captions_alt_text.csv", row.names = FALSE)
  }
  suppressMessages(invisible(utils::capture.output(force(code))))
  rda <- NULL
  load(file.path(if (is.null(dir)) wd else dir, "figures", paste0(topic_label, "_figure.rda")))
  rda
}

# Expect that every placeholder starting with `prefix` (e.g., "runs.test.")
# was replaced in the caption and alt text
expect_tags_filled <- function(rda, prefix) {
  expect_s3_class(rda$figure, "ggplot")
  expect_no_match(c(rda$caption, rda$alt_text), prefix, fixed = TRUE)
  expect_gt(nchar(rda$caption), 50)
  expect_gt(nchar(rda$alt_text), 100)
}
