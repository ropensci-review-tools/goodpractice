get_result <- function(res, check) res$passed[res$check == check]

test_that("no_missing fails when missing() is used", {
  gp_res <- gp("bad_missing", checks = "no_missing")
  res <- results(gp_res)
  expect_false(get_result(res, "no_missing"))

  pos <- failed_positions(gp_res)$no_missing
  lines <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("missing", lines)))
})

test_that("no_missing passes when missing() is not used", {
  gp_res <- gp("good", checks = "no_missing")
  res <- results(gp_res)
  expect_true(get_result(res, "no_missing"))
})

test_that("no_missing passes when no R directory exists", {
  state <- list(path = tempdir())
  result <- CHECKS$no_missing$check(state)
  expect_true(result$status)
  expect_length(result$positions, 0)
})
