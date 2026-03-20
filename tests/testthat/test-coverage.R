
test_that("warning if prep step fails", {
  bad1 <- system.file("bad1", package = "goodpractice")

  expect_warning(gp(bad1, checks = "covr"),
                 "Prep step for.*covr.*failed")
})

test_that("covr check passes with NaN coverage (no testable code)", {
  state <- list(covr = list(
    coverage = structure(list(), class = "coverage"),
    zero = data.frame(
      filename = character(), functions = character(),
      first_line = integer(), last_line = integer(),
      first_column = integer(), last_column = integer(),
      value = numeric(), stringsAsFactors = FALSE
    ),
    pct_by_line = NaN,
    pct_by_expr = NaN
  ))
  result <- CHECKS$covr$check(state)
  expect_true(result$status)
})

test_that("covr gp message handles NaN coverage", {
  state <- list(covr = list(pct_by_line = NaN))
  msg <- CHECKS$covr$gp(state)
  expect_match(msg, "No testable code")
})

test_that("run_prep_step passes multiple ... args to fn", {
  state <- list()
  state <- run_prep_step(state, "test", function(a, b, c) {
    list(sum = a + b + c)
  }, a = 1, b = 2, c = 3)
  expect_equal(state$test$sum, 6)
})
