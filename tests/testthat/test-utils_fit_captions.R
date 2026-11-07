test_that("caption text helpers format lists and panels", {
  expect_equal(fit_list_text("A"), "A")
  expect_equal(fit_list_text(c("A", "B")), "A and B")
  expect_equal(fit_list_text(c("A", "B", "C", "A")), "A, B, and C")
  expect_equal(fit_panel_text("a"), "A single panel for a.")
  expect_equal(fit_panel_text(c("a", "b")), "A set of 2 panels, one for each fleet (a and b).")
  expect_equal(fit_tag_value(c("costs $5", "a\\b")), "costs 5 ab")
})

test_that("fit_add_caption_rows adds missing rows to an existing captions file", {
  dir <- tempfile("captions")
  dir.create(dir)
  old <- setwd(dir)
  on.exit(
    {
      setwd(old)
      unlink(dir, recursive = TRUE)
    },
    add = TRUE
  )
  template <- utils::read.csv(
    system.file("resources", "captions_alt_text_template.csv", package = "stockplotr")
  )
  utils::write.csv(template[template$label != "runs_test", ], "captions_alt_text.csv", row.names = FALSE)
  fit_add_caption_rows("runs_test")
  caps <- utils::read.csv("captions_alt_text.csv")
  expect_equal(sum(caps$label == "runs_test"), 1)
  expect_equal(nrow(caps), nrow(template))
  # Rows already present are not duplicated
  fit_add_caption_rows("runs_test")
  expect_equal(nrow(utils::read.csv("captions_alt_text.csv")), nrow(template))
})
