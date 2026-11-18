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
  p <- suppressMessages(plot_landings(dat, predicted = FALSE, interactive = FALSE))
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

# The layers of a plot drawn with a geom
landings_layers <- function(p, geom) {
  Filter(function(l) inherits(l$geom, geom), p$layers)
}

test_that("plot_landings shows the fit: observed points and predicted lines", {
  dat <- stockplotr::vermilion_snapper
  p <- suppressMessages(plot_landings(dat, interactive = FALSE))
  points <- landings_layers(p, "GeomPoint")
  lines <- landings_layers(p, "GeomLine")
  ribbons <- landings_layers(p, "GeomRibbon")
  expect_length(points, 1)
  expect_length(lines, 1)
  expect_length(ribbons, 1)
  expect_gt(nrow(ribbons[[1]]$data), 0)
  obs <- dat[dat$label == "landings_observed" & !is.na(dat$year) & !is.na(dat$estimate), ]
  expect_equal(sum(points[[1]]$data$estimate), sum(obs$estimate))
  expect_true(all(lines[[1]]$data$label == "landings_predicted"))
  # The predicted landings only in the years with observed landings
  expect_equal(nrow(lines[[1]]$data), nrow(obs))
  # No intervals
  p <- suppressMessages(plot_landings(dat, interval = NULL, interactive = FALSE))
  expect_length(landings_layers(p, "GeomRibbon")[[1]]$data$year, 0)
  # A panel per fleet
  p <- suppressMessages(plot_landings(dat, facet = "fleet", interactive = FALSE))
  expect_s3_class(p$facet, "FacetWrap")
  # pollock reports its landings as "catch" and "catch_predicted"
  p <- suppressMessages(plot_landings(stockplotr::pollock, interactive = FALSE))
  expect_true(all(landings_layers(p, "GeomLine")[[1]]$data$label == "catch_predicted"))
  expect_true(all(landings_layers(p, "GeomPoint")[[1]]$data$label == "catch"))
})

test_that("plot_landings shows only the observed landings when they match the predicted", {
  # Stock Synthesis fits the landings almost exactly
  dat <- stockplotr::example_data
  expect_message(
    p <- plot_landings(dat, interactive = FALSE),
    "differ by less than"
  )
  expect_length(landings_layers(p, "GeomPoint"), 0)
  expect_true(all(landings_layers(p, "GeomLine")[[1]]$data$label == "landings_observed_weight"))
  # tolerance = 0 always shows the predicted landings
  p <- suppressMessages(plot_landings(dat, tolerance = 0, interactive = FALSE))
  expect_length(landings_layers(p, "GeomPoint"), 1)
  expect_true(all(landings_layers(p, "GeomLine")[[1]]$data$label == "landings_predicted_weight"))
  # The difference is the range of the differences relative to the largest
  # observed landings, as in r4ss::SSplotCatch()
  observed <- data.frame(model = "m", group_var = "a", year = 1:3, estimate = c(100, 200, 1000))
  predicted <- transform(observed, estimate = estimate + c(0, 1, -1))
  expect_equal(landings_difference(observed, predicted), 2 / 1000)
  expect_true(is.na(landings_difference(observed, predicted[0, ])))
})

test_that("plot_landings draws the observed landings as bars", {
  dat <- stockplotr::vermilion_snapper
  p <- suppressMessages(plot_landings(dat, geom = "bar", interactive = FALSE))
  bars <- landings_layers(p, "GeomCol")
  expect_length(bars, 1)
  expect_length(landings_layers(p, "GeomLine"), 0)
  expect_s3_class(bars[[1]]$position, "PositionDodge")
  obs <- dat[dat$label == "landings_observed" & !is.na(dat$year) & !is.na(dat$estimate), ]
  expect_equal(sum(bars[[1]]$data$estimate), sum(obs$estimate))
  # Stacked, as in r4ss::SSplotCatch()
  p <- suppressMessages(plot_landings(dat, geom = "bar", position = "stack", interactive = FALSE))
  expect_s3_class(landings_layers(p, "GeomCol")[[1]]$position, "PositionStack")
})
