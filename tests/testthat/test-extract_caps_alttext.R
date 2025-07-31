# load sample dataset
load(file.path(
  "fixtures", "ss3_models_converted", "Hake_2018",
  "std_output.rda"
))

test_that("extract_caps_alttext works for figures", {
  topic_label <- "biomass"
  fig_or_table <- "figure"

  # run write_captions.R
  stockplotr::write_captions(
    dat = out_new,
    dir = getwd(),
    year = 2022
  )

  # extract this plot's caption and alt text
  caps_alttext <- extract_caps_alttext(
    topic_label = topic_label,
    fig_or_table = fig_or_table,
    dir = getwd()
  )


  # expect that the caps_alttext is a list with two objects (caption, alt text)
  expect_true(length(caps_alttext) == 2)

  # expect the first 4 words of the caption
  expect_true("Biomass (B) time series." == stringr::word(caps_alttext[[1]], 1, 4))

  # expect the first 3 words of the alt text
  expect_true("Line graph showing" == stringr::word(caps_alttext[[2]], 1, 3))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
})

test_that("extract_caps_alttext works for tables", {
  topic_label <- "bnc"
  fig_or_table <- "table"

  # run write_captions.R
  stockplotr::write_captions(
    dat = out_new,
    dir = getwd(),
    year = 2022
  )

  # extract this plot's caption and alt text
  caps_alttext <- extract_caps_alttext(
    topic_label = topic_label,
    fig_or_table = fig_or_table,
    dir = getwd()
  )

  # expect that the caps_alttext is a list with one object (caption)
  expect_true(length(caps_alttext) == 1)

  # expect the first 4 words of the caption
  expect_true("Historical biomass, spawning biomass," == stringr::word(caps_alttext, 1, 4))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
})
