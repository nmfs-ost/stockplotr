test_that("plot_data_availability works with each example model output", {
  est <- readRDS(test_path("fixtures", "fims_output", "fims_estimates.rds"))
  datasets <- list(
    example_data = stockplotr::example_data,
    pollock = stockplotr::pollock,
    ss3_simple = stockplotr::ss3_simple,
    vermilion_snapper = stockplotr::vermilion_snapper,
    fims = suppressMessages(convert_output(est, models = "fims", start_year = 2001))
  )
  for (name in names(datasets)) {
    p <- plot_data_availability(datasets[[name]])
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
    expect_gt(nrow(p$data), 0)
  }
})

test_that("data_availability finds each type of data once per fleet and year", {
  avail <- data_availability(stockplotr::example_data)
  expect_equal(
    levels(avail$type),
    c("Landings", "Discards", "Index", "Age compositions", "Length compositions")
  )
  expect_equal(nrow(avail), nrow(dplyr::distinct(avail)))
  # Index years match the observed index
  obs <- stockplotr::example_data[stockplotr::example_data$label %in% "index_observed", ]
  index <- avail[avail$type == "Index", ]
  expect_setequal(paste(index$fleet, index$year), unique(paste(obs$fleet, obs$year)))
  # Landings come from one table only (not also from catch_observed, whose
  # fleets are numbered)
  expect_setequal(unique(avail$fleet[avail$type == "Landings"]), c("North", "South"))

  # pollock's catch is observed (it has catch_predicted); its compositions
  # are of unknown type
  avail <- data_availability(stockplotr::pollock)
  expect_true(all(c("Landings", "Compositions") %in% avail$type))
})

test_that("data_availability reports problems", {
  dat <- stockplotr::vermilion_snapper
  expect_error(
    data_availability(dat[!grepl("observed", dat$label), ]),
    "No observed data found"
  )
  # Observations without a fleet are kept, not dropped
  row <- which(dat$label == "index_observed" & dat$estimate > 0 & !is.na(dat$year))[1]
  dat$fleet[row] <- NA
  expect_message(avail <- data_availability(dat), "without a fleet")
  expect_true("unknown" %in% avail$fleet)
})

test_that("plot_data_availability works with FIMS input data", {
  # FIMS input (FIMS::get_data()): one row per observation, -999 = missing
  input <- data.frame(
    type = c("landings", "landings", "index", "index", "age_comp", "weight_at_age"),
    fleet = c("fleet1", "fleet1", "survey1", "survey1", "survey1", "fleet1"),
    timing = c(2001, 2002, 2001, 2002, 2002, 2001),
    observed = c(100, 120, 1.5, -999, 0.2, 0.5)
  )
  avail <- data_availability(input)
  expect_equal(as.character(avail$type), c("Landings", "Landings", "Index", "Age compositions"))
  expect_equal(avail$year, c(2001, 2002, 2001, 2002))

  # A FIMSFrame (an S4 object with the input in its data slot)
  frame_class <- methods::setClass("FIMSFrame", representation(data = "data.frame"), where = environment())
  p <- plot_data_availability(frame_class(data = input))
  expect_equal(nrow(p$data), 4)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_error(data_availability(input[input$type == "weight_at_age", ]), "No observed data")
})

test_that("plot_data_availability saves a caption, alt text, and key quantities", {
  dir <- tempfile("data_availability")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  suppressMessages(invisible(utils::capture.output(
    plot_data_availability(stockplotr::example_data, make_rda = TRUE, figures_dir = dir)
  )))
  rda <- NULL
  load(file.path(dir, "figures", "data_availability_figure.rda"))
  expect_no_match(c(rda$caption, rda$alt_text), "data.availability.", fixed = TRUE)
  expect_match(rda$caption, "landings, discards, index, age compositions, and length compositions", fixed = TRUE)
  expect_match(rda$alt_text, "from 1876 to 2022", fixed = TRUE)
  kqs <- utils::read.csv(file.path(dir, "key_quantities.csv"))
  kqs <- stats::setNames(kqs$value, kqs$key_quantity)
  expect_equal(as.character(kqs[["data.availability.n.fleets"]]), "4")
  expect_equal(as.character(kqs[["data.availability.start.year"]]), "1876")
})
