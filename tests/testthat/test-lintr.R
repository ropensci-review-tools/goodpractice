# "lintr_semicolon_linter" fails in bad1
# "lintr_attach_detach_linter"  fails in bad2
# "lintr_setwd_linter"  fails in bad2
# "lintr_sapply_linter"  fails in bad2
# "lintr_library_require_linter" fails in bad2
# "lintr_seq_linter" fails in bad3
# not failing yet:
# "lintr_assignment_linter"
# "lintr_line_length_linter"

gp_lintrs <- c(
  "lintr_assignment_linter",
  "lintr_line_length_linter",
  "lintr_semicolon_linter",
  "lintr_attach_detach_linter",
  "lintr_setwd_linter",
  "lintr_sapply_linter",
  "lintr_library_require_linter",
  "lintr_seq_linter"
)

bad1 <- system.file("bad1", package = "goodpractice")
gp_bad1 <- gp(bad1, checks = gp_lintrs)
res_bad1 <- results(gp_bad1)

gp_bad2 <- gp("bad2", checks = gp_lintrs)
res_bad2 <- results(gp_bad2)

gp_bad3 <- gp("bad3", checks = gp_lintrs)
res_bad3 <- results(gp_bad3)

get_result <- function(res, check) res$passed[res$check == check]


test_that("lintr_assignment_linter", {
  expect_true(get_result(res_bad1, "lintr_assignment_linter"))
  # TODO expectation/example where the check fails
})
test_that("lintr_line_length_linter", {
  expect_true(get_result(res_bad1, "lintr_line_length_linter"))
  # TODO expectation/example where the check fails
})
test_that("lintr_semicolon_linter", {
  expect_true(get_result(res_bad2, "lintr_semicolon_linter"))
  expect_false(get_result(res_bad1, "lintr_semicolon_linter"))
})
test_that("lintr_attach_detach_linter", {
  expect_true(get_result(res_bad1, "lintr_attach_detach_linter"))
  expect_false(get_result(res_bad2, "lintr_attach_detach_linter"))
})
test_that("lintr_setwd_linter", {
  expect_true(get_result(res_bad1, "lintr_setwd_linter"))
  expect_false(get_result(res_bad2, "lintr_setwd_linter"))
})
test_that("lintr_sapply_linter", {
  expect_true(get_result(res_bad1, "lintr_sapply_linter"))
  expect_false(get_result(res_bad2, "lintr_sapply_linter"))
})
test_that("lintr_library_require_linter", {
  # library/require is OK in vignettes and examples
  expect_true(get_result(res_bad1, "lintr_library_require_linter"))
  # but not in the R/ folder
  expect_false(get_result(res_bad2, "lintr_library_require_linter"))
})
test_that("lintr_seq_linter", {
  expect_true(get_result(res_bad1, "lintr_seq_linter"))
  expect_false(get_result(res_bad3, "lintr_seq_linter"))
})

test_that("get_lintr_state returns NA on try-error", {
  state <- list(lintr = structure("error", class = "try-error"))
  result <- get_lintr_state(state, "assignment_linter")
  expect_true(is.na(result$status))
  expect_identical(result$positions, list())
  expect_true("positions" %in% names(result))
  expect_false("position" %in% names(result))
})

test_that("lintr check fns return positions (not position) on try-error", {
  state <- list(lintr = structure("error", class = "try-error"))

  res_assignment <- CHECKS$lintr_assignment_linter$check(state)
  expect_true(is.na(res_assignment$status))
  expect_identical(res_assignment$positions, list())
  expect_true("positions" %in% names(res_assignment))
  expect_false("position" %in% names(res_assignment))

  res_library <- CHECKS$lintr_library_require_linter$check(state)
  expect_true(is.na(res_library$status))
  expect_identical(res_library$positions, list())
  expect_true("positions" %in% names(res_library))
  expect_false("position" %in% names(res_library))
})

test_that("all new lintr checks run and return results", {
  all_lintr <- grep("^lintr_", all_checks(), value = TRUE)
  g <- gp("good", checks = all_lintr)
  res <- results(g)
  expect_gt(nrow(res), 0)
  expect_true(all(all_lintr %in% res$check))
  expect_true(all(vapply(
    res$passed, function(x) is.logical(x) || is.na(x), logical(1)
  )))
})

test_that("make_lintr_check creates valid check", {
  chk <- make_lintr_check(
    "seq_linter", "test desc", "test gp message"
  )
  expect_identical(chk$description, "test desc")
  expect_identical(chk$preps, "lintr")
  expect_true(is.function(chk$check))
})
