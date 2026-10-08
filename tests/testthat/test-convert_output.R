test_that("convert_output works for SS3", {
  # Get a list of directories containing SS3 models
  all_models <- list.dirs(
    file.path(test_path("fixtures", "ss3_models"), "models"),
    full.names = TRUE,
    recursive = FALSE
  )

  # Loop through each model directory and check convert_output function
  for (i in seq_along(all_models)) {
    # Ensure no errors occur while converting SS3 output
    expect_no_error(result <- convert_output(
      file = file.path(all_models[i], "Report.sso")
    ))

    # Check that the result has exactly 35 columns
    expect_equal(dim(result)[2], 35)
  }

  # Test saving the output in a global environment
  output <- convert_output(
    file = file.path(all_models[1], "Report.sso")
  )

  expect_equal(dim(output)[2], 35)
})

test_that("convert_output does not emit scalar case_when deprecation warning", {
  expect_no_warning(convert_output(
    file = fs::path("fixtures", "ss3_models", "models", "Hake_2018", "Report.sso")
  ))
})


test_that("convert_output saves model ss3 hake output file", {
  dir.create(fs::path("fixtures", "ss3_models_converted", "Hake_2018"), recursive = TRUE)

  convert_output(
    file = fs::path("fixtures", "ss3_models", "models", "Hake_2018", "Report.sso"),
    save_dir = fs::path("fixtures", "ss3_models_converted", "Hake_2018", "std_output.rda")
  )

  expect_true(list.files(fs::path("fixtures", "ss3_models_converted", "Hake_2018")) == "std_output.rda")

  unlink(fs::path("fixtures", "ss3_models_converted"), recursive = TRUE)
})

test_that("missing arguments trigger warnings or errors", {
  # TODO: Debug why this doesn't work
  # expect_error(
  #   convert_output(
  #     model = "ss3",
  #     save_dir = fs::path("fixtures", "ss3_models_converted", "Hake_2018")
  #   ),
  #   "Missing `file`"
  # )

  # TODO: Debug why this doesn't work
  # expect_error(
  #   convert_output(
  #     file = fs::path("fixtures", "ss3_models", "models", "Hake_2018", "Report.sso"),
  #     model = "fake_model",
  #     save_dir = fs::path("fixtures", "ss3_models_converted", "Hake_2018")
  #   ),
  #   "Missing `model`"
  # )
  dir.create(fs::path("fixtures", "ss3_models_converted", "Hake_2018"), recursive = TRUE)

  expect_error(
    convert_output(
      file = fs::path("fixtures", "ss3_models", "models", "Hake_2018", "Report.rdat"),
      model = "ss3",
      save_dir = fs::path("fixtures", "ss3_models_converted", "Hake_2018")
    ),
    "`file` not found"
  )

  unlink(fs::path("fixtures", "ss3_models_converted"), recursive = TRUE)
})

test_that("r4ss::ss_output object is compatible.", {
  simple_r4ss <- readRDS(fs::path("fixtures", "r4ss_output", "simple_small", "simple_r4ss.rds"))

  expect_no_error(result <- convert_output(simple_r4ss))
  expect_equal(dim(result)[2], 35)
})

test_that("URL input is compatible", {
  expect_no_error(result <- convert_output(
    file = "https://raw.githubusercontent.com/pfmc-assessments/petrale/main/models/2023.a050.003_FIMS_case-study_wtatage/Report.sso"
  ))
  expect_equal(dim(result)[2], 35)
})

test_that("invalid URL input triggers an error", {
  expect_error(
    convert_output(
      file = "https://invalid.invalid/Report.sso"
    ),
    "Invalid URL."
  )
})

test_that("row_match returns matching row positions", {
  row_match <- getFromNamespace("row_match", "stockplotr")

  table <- data.frame(
    a = c("header", "x", "y"),
    b = c("value", "1", "2")
  )

  expect_equal(row_match(c("header", "value"), table), 1)
  expect_equal(row_match(data.frame(a = "y", b = "2"), table), 3)
  expect_true(is.na(row_match(c("missing", "row"), table)))
})

test_that("replace_empty_with_na_all replaces empty strings only", {
  replace_empty_with_na_all <- getFromNamespace("replace_empty_with_na_all", "stockplotr")

  dat <- data.frame(
    a = c("x", "", NA),
    b = c("", "y", ""),
    c = 1:3
  )

  out <- replace_empty_with_na_all(dat)

  expect_equal(out$a, c("x", NA, NA))
  expect_equal(out$b, c(NA, "y", NA))
  expect_equal(out$c, 1:3)
})

test_that("convert_output works for Rceattle", {
  fit_path <- test_path("fixtures", "rceattle_goa_atf.rds")

  # Both documented input modes: a path, and an already-loaded fit.
  expect_no_error(from_path <- convert_output(
    file = fit_path,
    model = "rceattle"
  ))
  expect_no_error(from_object <- convert_output(
    file = readRDS(fit_path),
    model = "rceattle"
  ))

  expect_equal(dim(from_path)[2], 35)
  expect_equal(from_path, from_object)

  # Every row says which module it came from.
  expect_false(any(is.na(from_path[["module_name"]])))

  # Predicted composition binds column-wise to the observations, one row of
  # age_hat per row of comp_data, so reading age_hat row by row reproduces the
  # series exactly. A cross join repeats it once per row of comp_data instead.
  fit <- readRDS(fit_path)
  age_hat <- as.vector(t(fit[["quantities"]][["age_hat"]]))
  expect_equal(
    as.numeric(from_path[from_path[["label"]] == "composition_predicted", ][["estimate"]]),
    age_hat[age_hat != 0]
  )

  # The index series are the fit's own values, in order.
  expect_equal(
    as.numeric(from_path[from_path[["label"]] == "index_predicted", ][["estimate"]]),
    unname(as.numeric(fit[["quantities"]][["index_hat"]]))
  )

  # A derived time series carries its years, rather than falling into the
  # `year = NA` branch.
  biomass <- from_path[from_path[["label"]] == "biomass", ]
  expect_gt(nrow(biomass), 0)
  expect_false(any(is.na(biomass[["year"]])))
  expect_true(all(is.finite(as.numeric(biomass[["estimate"]]))))
})

test_that("convert_output accepts .RDS as well as .rds for Rceattle", {
  # saveRDS() output is conventionally named either way, and real Rceattle
  # assessment files use the upper-case form.
  upper <- tempfile(fileext = ".RDS")
  on.exit(unlink(upper), add = TRUE)
  saveRDS(readRDS(test_path("fixtures", "rceattle_goa_atf.rds")), upper)
  expect_no_error(convert_output(file = upper, model = "rceattle"))
})

test_that("convert_output says so when an Rceattle fit carries no sdrep", {
  # getsd = FALSE leaves sdrep NULL, and every derived time series lives there,
  # so the converted output holds only the input data.
  no_sd <- readRDS(test_path("fixtures", "rceattle_goa_atf.rds"))
  no_sd[["sdrep"]] <- NULL

  # Matched on a single word, since cli wraps a message and a phrase can be
  # split across a line break.
  expect_warning(
    out <- convert_output(file = no_sd, model = "rceattle"),
    "sdrep"
  )
  expect_false("biomass" %in% out[["label"]])
  expect_true("catch" %in% out[["label"]])
})

test_that("convert_output refuses an Rceattle fit of unknown species count", {
  # The standardized output has no species dimension, so an unreadable species
  # count must stop the conversion rather than risk merging two stocks.
  no_nspp <- readRDS(test_path("fixtures", "rceattle_goa_atf.rds"))
  no_nspp[["data_list"]][["nspp"]] <- NULL

  expect_error(
    convert_output(file = no_nspp, model = "rceattle"),
    "species count is unknown"
  )
})

test_that("convert_output refuses a multispecies Rceattle fit", {
  # The standardized output describes one stock and has no species dimension.
  # CEATTLE is a multispecies model, so this must fail rather than return rows
  # in which two stocks share a year.
  multi <- readRDS(test_path("fixtures", "rceattle_goa_atf.rds"))
  multi[["data_list"]][["nspp"]] <- 3L

  # Matched on a phrase short enough that cli's wrapping cannot split it.
  expect_error(
    convert_output(file = multi, model = "rceattle"),
    "Rceattle model has 3 species"
  )
})
