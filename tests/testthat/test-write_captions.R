test_that("write_captions() function imports alt text/captions template
          as a data frame", {
  # import pre-written captions and alt text template
  caps_alttext <- utils::read.csv(system.file("resources", "captions_alt_text_template.csv", package = "stockplotr"))

  expect_s3_class(caps_alttext, "data.frame")

  expect_no_error(caps_alttext)
})

test_that("write_captions() function replaces placeholder text with key quantities as expected", {
  # import data
  dat <- utils::read.csv(
    system.file(
      "resources",
      "sample_data",
      "petrale_sole-after_2020.csv",
      package = "stockplotr"
    )
  )

  # make captions/alt text csv
  write_captions(dat, dir = here::here(), year = 2022)

  # import csv
  caps <- read.csv(here::here("captions_alt_text.csv"))

  # extract alt text from biomass figure
  B_alt_text <- caps |>
    dplyr::filter(
      label == "biomass",
      type == "figure"
    ) |>
    dplyr::select(alt_text) |>
    as.character()

  # expected alt text from end of biomass figure
  expected_alt_text_substring <- "spans from 2021 to B.end.year"

  # test expected alt text within B alt text
  expect_true(
    grepl(expected_alt_text_substring, B_alt_text, fixed = T)
  )

  # erase files placed in here::here()
  on.exit(unlink(fs::path(here::here(), "captions_alt_text.csv"), recursive = TRUE))
})
