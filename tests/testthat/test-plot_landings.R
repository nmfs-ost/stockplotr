test_that("plot_landings generates plots without errors", {
  # expect error-free plot with minimal arguments
  expect_no_error(
    plot_landings(
      stockplotr::example_data,
      interactive = FALSE
    )
  )


  # expect ggplot object is returned
  expect_s3_class(
    plot_landings(
      stockplotr::example_data,
      interactive = FALSE
    ),
    "gg"
  )
})

# test_that("rda file made when indicated", {
#   # export rda
#   plot_landings(
#     stockplotr::example_data,
#     interactive = FALSE,
#     make_rda = TRUE,
#     unit_label = "metric tons",
#     figures_dir = getwd()
#   )

#   # expect that both figures dir and the landings_figure.rda file exist
#   check_and_remove_tmp_files("landings_figure.rda")
# })

test_that("plot_landings plots the observed landings", {
  # vermilion_snapper also has predicted landings in numbers and weight, whose
  # labels contain "landings"
  dat <- stockplotr::vermilion_snapper
  p <- suppressMessages(plot_landings(dat, interactive = FALSE))
  obs <- dat[dat$label == "landings_observed" & !is.na(dat$year) & !is.na(dat$estimate), ]
  line <- p$layers[[which(sapply(p$layers, function(l) inherits(l$geom, "GeomLine")))]]$data
  expect_equal(sum(line$estimate, na.rm = TRUE), sum(obs$estimate))
  expect_equal(landings_label_name(dat), "^landings_observed$")
  expect_equal(landings_label_name(stockplotr::example_data), "^landings_observed_weight$")
  # pollock reports its observed landings as "catch"
  expect_equal(landings_label_name(stockplotr::pollock), "^catch$")
  # Without observed landings, the first label containing "landings"
  no_obs <- dat[!grepl("landings_observed", dat$label), ]
  expect_message(label <- landings_label_name(no_obs), "No observed landings")
  expect_equal(label, "landings")
  # Only labels in the module asked for
  expect_message(
    label <- landings_label_name(stockplotr::example_data, module = "SPR_SERIES"),
    "No observed landings"
  )
  expect_equal(label, "landings")
})
