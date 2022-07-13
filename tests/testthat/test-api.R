
bad1 <- system.file("bad1", package = "goodpractice")

# Warning message checked in test-coverage.R
x <- suppressWarnings(
  gp(bad1, checks = c("covr", "description_bugreports"))
)

test_that("checks", {
  expect_equal(checks(x), c("covr", "description_bugreports"))
})

test_that("results", {
  res <- data.frame(check = c("covr", "description_bugreports"),
                    result = c(NA, FALSE),
                    stringsAsFactors = FALSE)
  expect_equal(results(x), res)
})
