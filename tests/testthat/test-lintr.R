
context("lintr")

test_that("library/require is OK in vignettes and examples", {
  x <- gp("bad1", checks = "lintr_library_require_linter")
  df <- results(x)
  expect_identical(df$result, TRUE)
})
