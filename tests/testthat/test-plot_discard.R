# The layers of a plot drawn with a geom
discard_layers <- function(p, geom) {
  Filter(function(l) inherits(l$geom, geom), p$layers)
}

test_that("plot_discard plots the fit to the observed discards", {
  p <- suppressMessages(plot_discard(
    stockplotr::example_data,
    module = "DISCARD_OUTPUT",
    interactive = FALSE
  ))
  expect_s3_class(p, "gg")
  # Observed discards as points and predicted discards as lines, for the
  # same fleets and years
  points <- discard_layers(p, "GeomPoint")[[1]]$data
  line <- discard_layers(p, "GeomLine")[[1]]$data
  expect_true(all(points$label == "discard_observed"))
  expect_true(all(line$label == "discard_predicted"))
  expect_equal(nrow(points), nrow(line))
  # "std" is not a known type of observation error: no intervals
  expect_equal(nrow(discard_layers(p, "GeomRibbon")[[1]]$data), 0)
})

test_that("plot_discard options change the plot", {
  dat <- data.frame(
    model = "model_1",
    module_name = "DISCARD_OUTPUT",
    label = rep(c("discard_observed", "discard_predicted"), each = 4),
    year = rep(c(2000, 2001), 4),
    fleet = rep(c("fleet1", "fleet2"), each = 2, times = 2),
    estimate = c(100, 200, 80, 120, 99.8, 200.2, 79.8, 120.2),
    uncertainty = 0.1,
    uncertainty_label = "cv",
    unit = "kg"
  )
  p <- suppressMessages(plot_discard(dat, interactive = FALSE))
  expect_length(discard_layers(p, "GeomPoint"), 1)
  ribbon <- discard_layers(p, "GeomRibbon")[[1]]$data
  expect_gt(nrow(ribbon), 0)
  expect_true(all(ribbon$estimate_lower < ribbon$estimate))

  # The range of differences is 0.2% of the largest observed discard.
  expect_message(
    p <- plot_discard(dat, tolerance = 0.01, interactive = FALSE),
    "differ by less than"
  )
  expect_length(discard_layers(p, "GeomPoint"), 0)

  p <- suppressMessages(plot_discard(dat, interval = NULL, interactive = FALSE))
  expect_equal(nrow(discard_layers(p, "GeomRibbon")[[1]]$data), 0)

  p <- suppressMessages(plot_discard(dat, facet = "fleet", interactive = FALSE))
  expect_s3_class(p$facet, "FacetWrap")

  p <- suppressMessages(plot_discard(dat, geom = "bar", interactive = FALSE))
  expect_length(discard_layers(p, "GeomCol"), 1)

  expect_error(plot_discard(dat, tolerance = -1, interactive = FALSE), "tolerance")
})
