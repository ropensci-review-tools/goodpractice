
context("lintr")

test_that("library/require is OK in vignettes and examples", {
  bad1 <- system.file("bad1", package = "goodpractice")
  x <- gp(bad1, checks = "lintr_library_require_linter")
  df <- results(x)
  expect_identical(df$result, TRUE)
})
