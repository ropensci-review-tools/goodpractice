
test_that("cyclocomp check returns NA on try-error state", {
  state <- list(cyclocomp = structure("error", class = "try-error",
    condition = simpleError("test")))

  result <- CHECKS$cyclocomp$check(state)
  expect_identical(result$status, NA)
  expect_identical(result$positions, list())
})

test_that("cyclocomp check passes when all functions are simple", {
  state <- list(cyclocomp = data.frame(
    name = c("f1", "f2"),
    cyclocomp = c(1, 10),
    stringsAsFactors = FALSE
  ))

  result <- CHECKS$cyclocomp$check(state)
  expect_true(result$status)
  expect_identical(result$positions, list())
})

test_that("cyclocomp check fails when a function exceeds limit", {
  state <- list(cyclocomp = data.frame(
    name = c("f1", "complex_fun"),
    cyclocomp = c(1, 50),
    stringsAsFactors = FALSE
  ))

  result <- CHECKS$cyclocomp$check(state)
  expect_false(result$status)
  expect_identical(result$positions, list())
})

test_that("cyclocomp limit respects option", {
  state <- list(cyclocomp = data.frame(
    name = "f1",
    cyclocomp = 20,
    stringsAsFactors = FALSE
  ))

  withr::local_options(goodpractice.cyclocomp_limit = 25)
  result <- CHECKS$cyclocomp$check(state)
  expect_true(result$status)

  withr::local_options(goodpractice.cyclocomp_limit = 10)
  result <- CHECKS$cyclocomp$check(state)
  expect_false(result$status)
})
