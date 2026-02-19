get_result <- function(res, check) res$passed[res$check == check]

test_that("spelling fails when misspelled words exist", {
  gp_res <- gp("bad_spelling", checks = "spelling")
  res <- results(gp_res)
  expect_false(get_result(res, "spelling"))

  pos <- failed_positions(gp_res)$spelling
  words <- vapply(pos, `[[`, "", "line")
  expect_true(any(grepl("incremnet", words)))
})

test_that("spelling passes when no misspellings exist", {
  gp_res <- gp("good", checks = "spelling")
  res <- results(gp_res)
  expect_true(get_result(res, "spelling"))
})

test_that("spelling returns NA when no inst/WORDLIST exists", {
  state <- list(spelling = "no_wordlist")
  result <- CHECKS$spelling$check(state)
  expect_true(is.na(result$status))
  expect_equal(result$positions, list())
})
