test_that("theme_table applies NOAA formatting correctly", {
  # Test with a gt table object
  gt_obj <- gt::gt(head(cars))
  result_gt <- theme_table(gt_obj)

  expect_true(inherits(result_gt, "gt_tbl"))

  # Test with a kableExtra object
  kable_obj <- knitr::kable(head(cars), format = "html") %>%
    kableExtra::kable_styling()
  result_kable <- theme_table(kable_obj)

  expect_true("kableExtra" %in% class(result_kable))

  # Test with unsupported objects
  unsupported_obj <- data.frame(a = 1, b = 2)
  expect_error(theme_table(unsupported_obj))

  unsupported_obj2 <- list(1, 3, 4)
  expect_error(theme_table(unsupported_obj2))
})
