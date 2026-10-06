test_that("dashboard catalog includes every result plotting function", {
  catalog <- dashboard_figures()
  expected <- c("abundance_at_age", "biomass", "biomass_at_age", "catch_comp",
                "discard", "fishing_mortality", "index", "landings",
                "natural_mortality", "recruitment", "recruitment_deviations",
                "selectivity", "spawning_biomass", "stock_recruitment")
  expect_setequal(catalog$id, expected)
  expect_false(anyDuplicated(catalog$id) > 0)
  expect_true(all(nzchar(catalog$preferred_modules)))
})

test_that("dashboard rejects invalid inputs and preserves existing reports", {
  expect_error(create_dashboard(data.frame()), "standardized data frame")
  expect_error(create_dashboard(stockplotr::example_data, figures = "unknown"), "figure IDs")
  expect_error(create_dashboard(stockplotr::example_data, plot_args = list(biomass = list(dat = 1))), "managed plot argument")
  expect_error(create_dashboard(stockplotr::example_data, zip = NA), "TRUE or FALSE")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("svglite")
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  writeLines("existing report", file.path(dir, "index.html"))
  expect_error(create_dashboard(stockplotr::example_data, output_dir = dir, zip = FALSE), "not empty")
  expect_identical(readLines(file.path(dir, "index.html")), "existing report")
})

test_that("example data produces all figures and a self-contained archive", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("svglite")
  skip_if_not_installed("zip")
  dest <- tempfile("dashboard space ")
  extracted <- tempfile("extracted report ")
  on.exit(unlink(c(dest, paste0(dest, ".zip"), extracted), recursive = TRUE))
  cwd <- getwd()
  had_selection <- exists("selected_module", envir = .GlobalEnv, inherits = FALSE)
  if (had_selection) old_selection <- get("selected_module", envir = .GlobalEnv)
  on.exit({
    if (had_selection) assign("selected_module", old_selection, envir = .GlobalEnv)
    else if (exists("selected_module", envir = .GlobalEnv, inherits = FALSE)) rm("selected_module", envir = .GlobalEnv)
  }, add = TRUE)
  assign("selected_module", "caller value", envir = .GlobalEnv)
  report <- create_dashboard(stockplotr::example_data, output_dir = dest,
                             title = "A <script>literal</script> & \"title\"")
  expect_identical(getwd(), cwd)
  expect_identical(get("selected_module", envir = .GlobalEnv), "caller value")
  expect_true(all(report$figures$status == "ready"), info = paste(report$figures$reason, collapse = "\n"))
  expect_equal(nrow(report$figures), 14L)
  expect_true(file.exists(report$zip))
  utils::unzip(report$zip, exdir = extracted)
  expect_true(file.exists(file.path(extracted, "index.html")))
  manifest <- jsonlite::read_json(file.path(extracted, "manifest.json"))
  stock_recruit <- manifest$figures[[match("stock_recruitment", report$figures$id)]]
  expect_false(grepl("age NA", stock_recruit$alt_text, fixed = TRUE))
  expect_equal(manifest$title, "A <script>literal</script> & \"title\"")
  script <- paste(readLines(file.path(extracted, "report-data.js")), collapse = "\n")
  expect_false(grepl("<script>", script, fixed = TRUE))
  expect_match(script, "\\\\u003cscript>")
  for (f in manifest$figures) {
    if (f$status != "ready") next
    expect_true(all(file.exists(file.path(extracted, c(f$svg, f$png, f$csv)))))
    expect_true(nzchar(f$caption))
    expect_true(nzchar(f$alt_text))
    expect_match(paste(readLines(file.path(extracted, f$svg)), collapse = ""), "<svg")
    expect_true(file.info(file.path(extracted, f$png))$size > 1000)
  }
  assets <- paste(readLines(file.path(extracted, "index.html")), collapse = "\n")
  expect_false(grepl('(src|href)="https?://', assets))
  expect_false(grepl("fetch\\(", paste(readLines(file.path(extracted, "dashboard.js")), collapse = "\n")))
})

test_that("missing data gives explicit failures and leaves the caller unchanged", {
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("svglite")
  dest <- tempfile()
  on.exit(unlink(dest, recursive = TRUE))
  dat <- stockplotr::example_data[stockplotr::example_data$module_name %in% "INDEX_2", ]
  cwd <- getwd()
  expect_warning(report <- create_dashboard(dat, dest, zip = FALSE,
    figures = c("index", "biomass")), "1 figure")
  expect_identical(report$figures$status, c("ready", "unavailable"))
  expect_match(report$figures$reason[2], "absent")
  expect_null(report$zip)
  failed <- tempfile()
  expect_error(create_dashboard(dat, failed, zip = FALSE, figures = "biomass"), "No figures could be rendered")
  expect_false(dir.exists(failed))
  expect_identical(getwd(), cwd)
})
