ss3_likelihood_data <- function() {
  skip_if_not_installed("r4ss")
  models <- c("Simple", "Simple_with_Discard", "Simple_NoCPUE")
  outputs <- lapply(stats::setNames(models, models), function(model) {
    r4ss::SS_output(
      test_path("fixtures", "ss3_models", "models", model),
      verbose = FALSE
    )
  })
  suppressMessages(convert_output(outputs))
}

test_that("plot_likelihoods plots total SS3 likelihoods for one to three models", {
  dat <- ss3_likelihood_data()
  models <- unique(dat$model)

  for (n_models in seq_along(models)) {
    selected <- models[seq_len(n_models)]
    p <- suppressMessages(plot_likelihoods(
      dat, model = selected, interactive = FALSE
    ))
    expected <- dat |>
      dplyr::mutate(label = tolower(label)) |>
      dplyr::filter(
        model %in% selected,
        module_name == "likelihoods",
        type == "values",
        is.na(fleet),
        !grepl("laplace_obj_fun", label),
        !is.na(estimate),
        estimate != 0
      ) |>
      dplyr::select(model, label, estimate) |>
      dplyr::arrange(model, label)

    expect_s3_class(p, "ggplot")
    expect_equal(
      p$data |>
        dplyr::select(model, label, estimate) |>
        dplyr::arrange(model, label),
      expected
    )
    expect_setequal(p$data$model, selected)
    expect_equal(sum(p$data$label == "total"), n_models)
    expect_true(any(p$data$estimate < 0))

    built <- ggplot2::ggplot_build(p)
    expect_equal(nrow(built$layout$layout), 1)
    expect_equal(sort(built$data[[1]]$x), sort(expected$estimate))
    expect_setequal(
      built$plot$scales$get_scales("fill")$get_limits(),
      if (n_models > 1) selected else unique(expected$label)
    )
  }
})

test_that("plot_likelihoods preserves SS3 fleet contributions and adds fleet totals", {
  dat <- ss3_likelihood_data()
  p <- suppressMessages(plot_likelihoods(
    dat, model = unique(dat$model), by_fleet = TRUE, interactive = FALSE
  ))
  expected <- dat |>
    dplyr::mutate(label = tolower(label)) |>
    dplyr::filter(
      module_name == "likelihoods",
      type == "like",
      !is.na(fleet),
      !is.na(estimate),
      estimate != 0
    ) |>
    dplyr::select(model, fleet, label, estimate) |>
    dplyr::arrange(model, fleet, label)
  totals <- expected |>
    dplyr::group_by(model, fleet) |>
    dplyr::summarise(estimate = sum(estimate), .groups = "drop")

  expect_s3_class(p, "ggplot")
  expect_equal(
    p$data |>
      dplyr::ungroup() |>
      dplyr::filter(label != "total") |>
      dplyr::select(model, fleet, label, estimate) |>
      dplyr::arrange(model, fleet, label),
    expected
  )
  expect_equal(
    p$data |>
      dplyr::ungroup() |>
      dplyr::filter(label == "total") |>
      dplyr::select(model, fleet, estimate) |>
      dplyr::arrange(model, fleet),
    totals
  )
  expect_false("Total" %in% p$data$fleet)

  built <- ggplot2::ggplot_build(p)
  expect_setequal(built$layout$layout$fleet, expected$fleet)
  expect_equal(nrow(built$layout$layout), dplyr::n_distinct(expected$fleet))
  expect_equal(
    sort(built$data[[1]]$x),
    sort(c(expected$estimate, totals$estimate))
  )
})

test_that("plot_likelihoods summarises SS3 fleets separately for each model", {
  dat <- ss3_likelihood_data()
  p <- suppressMessages(plot_likelihoods(
    dat, model = unique(dat$model), by_fleet = TRUE,
    summarise_fleets = TRUE, interactive = FALSE
  ))
  source <- dat |>
    dplyr::mutate(label = tolower(label)) |>
    dplyr::filter(
      module_name == "likelihoods",
      type == "like",
      !is.na(fleet),
      !is.na(estimate),
      estimate != 0
    )
  expected <- source |>
    dplyr::group_by(model, label) |>
    dplyr::summarise(estimate = sum(estimate), .groups = "drop")
  totals <- source |>
    dplyr::group_by(model) |>
    dplyr::summarise(label = "total", estimate = sum(estimate), .groups = "drop")
  expected <- dplyr::bind_rows(expected, totals) |>
    dplyr::arrange(model, label)

  expect_s3_class(p, "ggplot")
  expect_equal(
    p$data |>
      dplyr::ungroup() |>
      dplyr::filter(fleet == "Total") |>
      dplyr::select(model, label, estimate) |>
      dplyr::arrange(model, label),
    expected
  )
  expect_equal(
    p$data |>
      dplyr::ungroup() |>
      dplyr::filter(fleet != "Total", label != "total") |>
      dplyr::mutate(fleet = as.character(fleet)) |>
      dplyr::select(model, fleet, label, estimate) |>
      dplyr::arrange(model, fleet, label),
    source |>
      dplyr::select(model, fleet, label, estimate) |>
      dplyr::arrange(model, fleet, label)
  )

  built <- ggplot2::ggplot_build(p)
  expect_equal(as.character(built$layout$layout$fleet[1]), "Total")
  expect_setequal(as.character(built$layout$layout$fleet), c("Total", source$fleet))
  expect_equal(nrow(built$data[[1]]), nrow(p$data))
  expect_equal(sort(built$data[[1]]$x), sort(p$data$estimate))
})

