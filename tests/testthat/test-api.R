
context("API")

bad1 <- system.file("bad1", package = "goodpractice")
x <- gp(bad1, checks = "description_bugreports")

test_that("access to checks", {
  expect_equal(checks(x), "description_bugreports")
})

test_that("results", {
  # TODO: check for skipped tests (currently returns TRUE, should return NA? -> #74)
  # --> failed_checks
  res <- data.frame(check = "description_bugreports",
                    result = FALSE,
                    stringsAsFactors = FALSE)
  expect_equal(results(x), res)
})
