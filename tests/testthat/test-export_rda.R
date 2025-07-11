# read in sample dataset
dat <- asar::convert_output(
  file = fs::path("fixtures", "ss3_models", "models", "Hake_2018", "Report.sso"),
  model = "ss3"
)

test_that("export_rda works for figures", {
  topic_label <- "biomass"
  fig_or_table <- "figure"

  # run write_captions.R
  stockplotr::write_captions(
    dat = dat,
    dir = getwd(),
    year = 2022
  )

  # extract this plot's caption and alt text
  caps_alttext <- extract_caps_alttext(
    topic_label = topic_label,
    fig_or_table = fig_or_table,
    dir = getwd()
  )

  # make a simple plot
  library(ggplot2)
  final <- ggplot2::ggplot(Orange, aes(circumference, age)) +
    ggplot2::geom_point()

  # export rda
  export_rda(
    object = final,
    caps_alttext = caps_alttext,
    figures_tables_dir = getwd(),
    topic_label = topic_label,
    fig_or_table = fig_or_table
  )

  # expect that both figures dir and the biomass_figure.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "figures")))
  expect_true(file.exists(fs::path(
    getwd(), "figures", "biomass_figure.rda"
  )))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "figures"), recursive = T)
})

test_that("export_rda works for tables", {
  topic_label <- "bnc"
  fig_or_table <- "table"

  # run write_captions.R
  stockplotr::write_captions(
    dat = dat,
    dir = getwd(),
    year = 2022
  )

  # extract this plot's caption and alt text
  caps_alttext <- extract_caps_alttext(
    topic_label = topic_label,
    fig_or_table = fig_or_table,
    dir = getwd()
  )

  # make a simple table
  final <- flextable::flextable(data = data.frame(
    x = c(1, 2, 3),
    y = c(4, 5, 6)
  ))

  # export rda
  export_rda(
    object = final,
    caps_alttext = caps_alttext,
    figures_tables_dir = getwd(),
    topic_label = topic_label,
    fig_or_table = fig_or_table
  )

  # expect that both tables dir and the bnc_table.rda file exist
  expect_true(dir.exists(fs::path(getwd(), "tables")))
  expect_true(file.exists(fs::path(getwd(), "tables", "bnc_table.rda")))

  # erase temporary testing files
  file.remove(fs::path(getwd(), "captions_alt_text.csv"))
  unlink(fs::path(getwd(), "tables"), recursive = T)
})
