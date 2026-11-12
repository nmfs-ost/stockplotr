# Evaluate plotting code with `make_rda = TRUE` in a temporary directory and
# return the saved rda (a list with the figure, caption, and alt text), with
# the key quantities recorded in key_quantities.csv as `kqs` (a named
# vector of the filled-in values).
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
  out <- if (is.null(dir)) wd else dir
  load(file.path(out, "figures", paste0(topic_label, "_figure.rda")))
  kqs <- utils::read.csv(file.path(out, "key_quantities.csv"))
  kqs <- kqs[!is.na(kqs$value) & kqs$value != "", , drop = FALSE]
  rda$kqs <- stats::setNames(as.character(kqs$value), kqs$key_quantity)
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
