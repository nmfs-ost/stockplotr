test_that("export_rda works for figures", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file(
      "resources",
      "sample_data",
      "petrale_sole-after_2020.csv",
      package = "stockplotr"
    )
  )

  topic_label <- "biomass"
  fig_or_table <- "figure"

  # run write_captions.R
  stockplotr::write_captions(dat = dat,
                       dir = getwd(),
                       year = 2022)

  # extract this plot's caption and alt text
  caps_alttext <- extract_caps_alttext(topic_label = topic_label,
                                       fig_or_table = fig_or_table,
                                       dir = getwd())

  # make a simple plot
  library(ggplot2)
  final <- ggplot2::ggplot(Orange, aes(circumference, age)) +
    ggplot2::geom_point()

  # export rda
  export_rda(
    final = final,
    caps_alttext = caps_alttext,
    rda_dir = getwd(),
    topic_label = topic_label,
    fig_or_table = fig_or_table
  )

  # expect that both rda_files dir and the biomass_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "rda_files")))
  expect_true(file.exists(fs::path(
    getwd(), "rda_files", "biomass_figure.rda"
  )))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "rda_files"), recursive = T)

})

test_that("export_rda works for tables", {
  # read in sample dataset
  dat <- utils::read.csv(
    system.file(
      "resources",
      "sample_data",
      "petrale_sole-after_2020.csv",
      package = "stockplotr"
    )
  )

  topic_label <- "bnc"
  fig_or_table <- "table"

  # run write_captions.R
  stockplotr::write_captions(dat = dat,
                       dir = getwd(),
                       year = 2022)

  # extract this plot's caption and alt text
  caps_alttext <- extract_caps_alttext(topic_label = topic_label,
                                       fig_or_table = fig_or_table,
                                       dir = getwd())

  # make a simple table
  final <- flextable::flextable(data = data.frame(x = c(1, 2, 3),
                                                  y = c(4, 5, 6)))

  # export rda
  export_rda(
    final = final,
    caps_alttext = caps_alttext,
    rda_dir = getwd(),
    topic_label = topic_label,
    fig_or_table = fig_or_table
  )

  # expect that both rda_files dir and the bnc_table.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "rda_files")))
  expect_true(file.exists(fs::path(getwd(), "rda_files", "bnc_table.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "rda_files"), recursive = T)

})
