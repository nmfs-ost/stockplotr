test_that("argument descriptions begin with a type prefix", {
  allowed_prefixes <- c(
    "string",
    "number",
    "logical",
    "list",
    "data frame",
    "data frame or list",
    "character vector",
    "path",
    "object",
    "plot object",
    "table object",
    "dots"
  )

  rd_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)
  param_lines <- unlist(lapply(rd_files, function(file) {
    grep("^\\\\item\\{", readLines(file, warn = FALSE), value = TRUE)
  }))

  expect_true(length(param_lines) > 0)

  bad_lines <- param_lines[!vapply(param_lines, function(line) {
    desc <- sub("^\\\\item\\{[^}]+\\}\\{", "", line)
    any(vapply(allowed_prefixes, function(prefix) {
      startsWith(desc, paste0(prefix, ".")) || startsWith(desc, paste0(prefix, " "))
    }, logical(1)))
  }, logical(1))]

  expect_length(bad_lines, 0)
})
